# Language notes: filter graphs and bitstream filters

Advice for building [filters.md](../filters.md) and
[bitstream-filters.md](../bitstream-filters.md) in OCaml on top of the FFmpeg
bindings. Nothing here is binding; Part A is.

## 1. Layout

- One optional library for the filter builtins, depending on the
  `ffmpeg-avfilter`, `ffmpeg-avcodec` and `ffmpeg-avutil` bindings. Without
  `ffmpeg-avfilter` the library is not built and every `ffmpeg.filter.*`
  builtin is absent; scripts guard with `%ifdef`. Make the build fail, not
  skip, when the library is expected and its stanza is broken.
- The helper graphs (Fps, AFormat, Fit) live in the base FFmpeg library,
  since decoders, encoders and content use them without the builtins.
- Registration runs at module initialisation, from the bindings' registries
  (`Avfilter.filters`, `Avcodec.BitstreamFilter.filters`). Wrap it in the
  start-up timer so its cost shows in the start-up log.
- Small modules with an `.mli` each: option mapping (§5 of filters.md),
  description and replay (§7, §11.2), graph inputs, sinks and the graph
  source (§8, §9, §12, §13), `parse`, helpers, bitstream operators.

## 2. Option mapping

- `Avutil.Options.opts` gives the options; deduplicate by name keeping the
  first, which is the filter's own class.
- A GADT pairing each ground constructor with its script type and its
  converter keeps the mapping exhaustive and the per-type code in one
  place.
- The flags check is one `Int64.logand value (lognot union) = 0L`, `union`
  the OR of the named flags.
- Duration bounds and defaults come in microseconds: divide by `1_000_000`
  for the documentation and multiply the script value for the range check.
- Render pairs as `` `Pair (name, value) `` for `Avfilter.attach`
  (`avfilter.md` §11.2), and as an `Avutil.opts` table for
  `Avcodec.BitstreamFilter.init`; read the table back after the call to
  report the leftover keys.

## 3. Script values

- `Value.MkCustom` instances for the graph and the two pad types. Give each
  value an `int` from an `Atomic` counter at creation, and make `compare`
  compare those numbers: comparison by identity, never raising on the
  closures inside.

## 4. Description and replay

- Represent the description as an ordered list of elements: instances
  (filter, name, rendered args), graph inputs, graph outputs, links, parsed
  descriptions. A pad value is `(element, pad index)`.
- One function `replay : description -> Avfilter.config -> resolved` attaches
  every element, then links and parses, and returns a table from element to
  attached filter. The probe call and every build call the same function,
  so the two cannot diverge. Run each step of the probe replay as the
  script states it, so errors carry that statement's position.
- Instance names need only be unique within one config: a per-graph counter
  is enough. Use stable names (`aecho_0`) so FFmpeg's log lines stay
  readable across generations.
- The probe's buffer sources: attach `abuffer` with
  `sample_rate=44100:sample_fmt=fltp:channel_layout=stereo:time_base=1/44100`
  and `buffer` with a small `yuv420p` size; the probe is never launched.
- The unlinked-pad check walks the resolved table and the pad counts of
  each attached filter against the links made.

## 5. Running a graph

- Keep the per-graph state as one variant:
  ``[ `Idle | `Running of generation | `Exhausted | `Failed of float ]``,
  with `generation` holding the launched `Avfilter.t`, its source id and the
  per-sink helpers. A drain is a function `generation -> unit` followed by
  the state change; there is no flag to keep in sync. `Failed` carries the time of the fault, so readiness compares it with `fault_retry_delay`.
- `Avfilter.launch` returns every buffer source and sink by instance name;
  map them back through the resolved table.
- One `Mutex` per graph serialises `process_command` with pushes and pulls;
  the bindings' guard would otherwise raise its in-use error.
- The read modes of filters.md §12.2 are two loops over the sink handler:
  one stopping at need, one at ``Error `Eagain``. Both treat
  ``Error `Eof`` as the sink's end.
- Counters for the limits of §12.3 are per fill (input pushed), per drain
  (media read) and per input (held frames). Measure in main ticks and
  compare with `filter_max_buffer × main_rate`.
- Graph inputs are `Output.output` instances with their own passive clock,
  unified with the input clock in `ffmpeg.filter.create`. The graph source
  owns the input clock, registers it on wake-up with `Clock.register` and
  deregisters it on sleep.

## 6. Timestamps

- Input PTS: the input keeps one `int` timeline position. Each frame's PTS
  is that position plus the run start plus the item position; set it with
  `Avutil.Frame.set_pts`, and rescale `duration` with the shared `rescale`.
- Audio sinks keep `s₀` and a sample count `n` per generation; the frame's
  start is `s₀ + rescale n (1/rate) main`.
- Video sinks use the shared duration helper; on a decreasing PTS, flush it
  and create a new one at `end_position`.

## 7. Bitstream filters

- The operator plugs a transform into the shared inline producer; the
  handler is a record `{ source_id; time_base; filter; params; helper }`,
  held in an option.
- The output field's format is created per operator from fresh copy
  parameters, not unified
  with the input field.

## 8. Tests that pin behaviour

- `tests/regression/GH3944.liq`: a `loudnorm` graph behind a `buffer` whose
  input runs dry is rebuilt and resumes.
- `tests/media/test_ffmpeg_raw_format_change.liq`: a graph across a video
  format change.
- `tests/media/test_ffmpeg_filter_changing_rate.liq`: a graph across inputs
  of different rates.
- `tests/media/test_ffmpeg_filter_array_args.liq`: array options.
- `tests/media/ffmpeg_filter_parse.liq`: `parse` with labelled inputs and
  outputs.
- `tests/media/ffmpeg_bitstream_filter.liq`: bitstream filters on copy
  content.
