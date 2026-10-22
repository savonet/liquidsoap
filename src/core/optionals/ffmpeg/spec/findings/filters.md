# Findings: filter graphs and bitstream filters

## Defects

### A graph of endless source filters never leaves its tick

`Ffmpeg_filter_io.sink#drain` (ffmpeg_filter_io.ml:459) loops until the
buffer sink answers `Eagain` or `Eof`. A sink fed only by a generating filter
(`sine`, `testsrc`, … with no duration) never answers either: the generator
produces on demand. The loop never returns, and `check_buffer`
(ffmpeg_filter_graph.ml:78), which runs only after a drain, never fires, so
the buffer grows without bound. Matters: a graph with no liquidsoap input is
a documented use (`is_ready` builds it for that case). **Checked**: `sine` →
`aecho` → `audio.output` → `output.dummy`, shutdown after 1 s: `[clock.ffmpeg.filter:1]
Still running at shutdown: in a tick for 11.01s`.

### `parse` before any graph input builds a graph that is never launched

`builtins_ffmpeg_filter_parse.ml:211` calls `init_graph` at script time. With
no graph input yet, `input_inits` is empty, so the graph is built then, before
the outputs and the launch closure are queued. `current` is now `Some`, so
`is_ready` never builds again; the sinks never get an output; `fill_buffer`
exits through `wait_for_launch` every time. The graph source reports ready
and produces nothing. **Checked**: `parse(description="sine=frequency=220:duration=5[a_out]")`
→ output → `ffmpeg.raw.decode.audio` → `%wav` for 2 s writes a 0-byte file;
`s.is_ready()` is `true` throughout; no `Initializing graph` log.

### Flag options reject combinations

`Ffmpeg_filter_options.mk_options` (ffmpeg_filter_options.ml:354) requires a
value to be one of the named values. For `Flags` options the named values are
single flags, so any combination is refused. **Checked**:
`scale(sws_flags=6)` → `sws_flags should be one of: 1, 2, 4, …`.

### Integer options with named special values reject in-range numbers

Same check: an `Int` option that declares named constants and a numeric range
(common in FFmpeg: `-1 (auto)` plus `0..N`) accepts only the constants.
**Read only.**

### `Duration` options: unit mismatch

`Duration` maps to `int`, rendered in decimal (ffmpeg_filter_options.ml:193).
FFmpeg parses duration option text as seconds, but the documented default is
FFmpeg's stored microseconds (`sine`: `duration : int? … (default: 0)`).
`sine(duration=1)` logs `Setting 'duration' to value '1'` (checked), i.e. 1 s,
while a reader of the doc expects 1 µs. **Read only** for the parse rule.

### Description with its own buffer source or sink fails at launch

The launch closure (builtins_ffmpeg_filters.ml:889–910) looks every buffer
source and sink of the launched graph up with `Hashtbl.find`. A `parse`
description that contains `abuffer`, `buffer`, `abuffersink` or `buffersink`
creates an endpoint with no entry: `Not_found` at build, graph failed.
**Read only.**

### Bitstream flush emits an empty chunk of length 1

`flush` (builtins_ffmpeg_bitstream_filters.ml:104) always calls `process` on
`Duration.flush`, which returns `(max 1 …, packets)` even when no packet is
held (ffmpeg_utils.ml:492). A chunk `{length = 1; data = []}` is put on every
flush, at sleep and at every stream change. **Read only.**

### Bitstream filter on content without parameters

`on_data` (builtins_ffmpeg_bitstream_filters.ml:124) does `Option.get params`
per packet. **Read only.**

### Comparing pad or graph values raises

`compare = Stdlib.compare` on values containing closures. **Checked**:
`print(a == b)` on two `ffmpeg.filter.sine` outputs → runtime error
`compare: functional value`.

## Asymmetries

### Readiness comment versus `is_ready`

`Ffmpeg_filter_graph.source#can_generate_frame` (ffmpeg_filter_graph.ml:69)
says readiness must not tick anything, but it calls `is_ready`, which ticks
the input clock while the graph is not built (builtins_ffmpeg_filters.ml:146).
Related, **to verify**: calling `s.is_ready()` from a `thread.run` on a
`ffmpeg.raw.decode.audio` of a graph raised `Clock.tick: a tick of this clock
is in progress`.

### Only the first metadata of a frame enters a graph

`graph_metadata` (ffmpeg_filter_io.ml:233) takes the head of
`Frame.get_all_metadata`; later metadata of the same liquidsoap frame are
dropped, and their positions are lost. The pending set replaces the FFmpeg
frame's own metadata (`set_metadata`). **Read only.**

### `lavfi.liq.*` timestamps only with other metadata

`sink#emit` adds the timestamps only when the frame carries some other
metadata, so a plain filter's frames produce none. **Read only.**

### Bitstream handler ignores a time-base change

`handler_getters` compares `stream_idx` only; the graph input converter logs
and realigns on a time-base change of the same stream. **Read only.**

### Helper graphs build buffer sources their own way

`Fps.init` passes `video_size`/`pix_fmt`/`time_base`/`pixel_aspect`/`range`/`frame_rate`
by hand and no colour space, while graph inputs and `Fit` use the bindings'
`video_buffer_args` (colour space included). `AFormat` passes `sample_fmt` by
name; graph inputs pass it by numeric id. **Read only.**

### `Fps.convert` lets `Eof` through, `Fit` swallows it

`Fps.flush`/`AFormat.flush` catch `Eagain` only in `convert`; `Fit.deliver`
catches `Eagain` and `Eof`. **Read only.**

### Input/output categories differ by media

`ffmpeg.filter.audio.output` is `Track / FFmpeg filter`,
`ffmpeg.filter.video.output` is `Track / Video processing`. **Checked** (`-h`).

### Link error names the ends in reverse

`Error while connecting filter elements <input> to <output>`
(builtins_ffmpeg_filters.ml:372) prints the destination first. **Read only.**

## Gaps

### Dynamic-input count is checked at build, not at script time

`get_input` runs inside the build closure, so `Invalid number of input` is
raised while streaming, far from the script. **Read only.**

### Options of the same name are duplicated in the signature

`opts` lists child-class options; `scale` shows `param0`/`param1` twice
(**checked**, `-h`). Both read the same script argument. Which one FFmpeg
keeps is not decided here.

### Filters created by `parse` have no handle

No `process_command` for them; they are re-created each generation. **Read only.**

### Doc text

- `.create` descriptions end the filter description with `..`
  (`Add echoing to the audio.. Use this operator …`). **Checked** (`-h`).
- `ffmpeg.filter.audio_video.output` documents `@param id` without `~`, so
  `liquidsoap -h` attaches "Force the value of the source ID." to the graph
  argument. **Checked**.
- `ffmpeg.filter.parse` says it "attaches to the current graph", but it only
  queues a closure (and builds early, see Defects).

### Unbounded holding queue

`base_output.held` has no bound; the code carries its own `ponytail:` note.
**Read only.**

## To verify

### A generation that reaches end of stream with ready inputs

`fill_buffer` ends a generation only when the graph is not ready; when every
sink is at `Eof` and inputs are ready, the loop stops and nothing resets.
Observed: `sine(duration=1)` alone (no input), decoded and written with
`%wav`: 3.02 s of non-silent audio, no `generation 2` log. The same graph
encoded with `%ffmpeg(%audio.raw(codec="pcm_s16le"))` wrote 0.0015 s. Neither
matches "1 s, then rebuild"; which part (sine duration, sink tail, encoder) is
responsible was not isolated.

### Graph outputs with non-monotonic PTS

The sink-side converter raises `Content.Invalid` on decreasing PTS
(`autocue.liq` works around `ebur128` with `asetpts`). Whether this agrees
with what [timestamps.md](../timestamps.md) expects from raw content was not
checked.

### Unconnected pads

A `.create` instance whose `set_input` is never called, or an output pad
never consumed, leaves pads unlinked; launch presumably fails and marks the
graph failed. Not run.
