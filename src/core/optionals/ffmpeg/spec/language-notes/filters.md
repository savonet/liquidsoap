# Language notes: filter graphs and bitstream filters

OCaml structure behind [filters.md](../filters.md) and
[bitstream-filters.md](../bitstream-filters.md).

## 1. Libraries and files

| Library (dune)                                              | Files                                                                                                                                                                                                                                                                      | Notes                                                                                                        |
| ----------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------ |
| `liquidsoap_ffmpeg_filter`, `(optional)`, `(wrapped false)` | `filter/ffmpeg_filter_graph.ml`, `ffmpeg_filter_io.ml`, `ffmpeg_filter_options.ml`, `builtins_ffmpeg_filters.ml`, `builtins_ffmpeg_filter_parse.ml`, `builtins_ffmpeg_bitstream_filters.ml` (and `builtins_ffmpeg_side_data.ml`, owned by [side-data.md](../side-data.md)) | depends on `ffmpeg-avfilter`, `ffmpeg-avcodec`, `ffmpeg-avutil`, `liquidsoap_core`, `liquidsoap_ffmpeg_base` |
| `liquidsoap_ffmpeg_base`                                    | `base/ffmpeg_avfilter_utils.ml(.mli)`                                                                                                                                                                                                                                      | Display, Fps, AFormat, Fit; used by decoder, encoder and content code                                        |

Registration runs from top-level `let () = Startup.time "…" register_filters`
in each builtins module, so linking the library registers the builtins. Being
`(optional)`, a missing `ffmpeg-avfilter` drops the library silently and every
`ffmpeg.filter.*` builtin disappears; scripts guard with `%ifdef`.

The `ffmpeg.filter` module value is `Builtins_ffmpeg_base.ffmpeg_filter`;
`ffmpeg.filter.audio`, `.video` and `.bitstream` are `Lang.add_module`
children.

## 2. Script values

`Graph`, `Audio`, `Video` are `Value.MkCustom` instances. `compare` is
`Stdlib.compare`, which raises on the closures inside (checked: `a == b` on
two pads raises `compare: functional value`). `Audio`/`Video` content is
``[ `Input of unit -> pad | `Output of unit -> pad ]``; only `` `Output ``
is ever built, so the `` `Input `` arms are `assert false` or a `failwith`.

## 3. Graph record

`Builtins_ffmpeg_filters.graph` holds: `probe : Avfilter.config option`
(the script-time config, `None` after `create`), `current : config option`
(the generation's config), `generation : int`, `failed : bool`, `owner :
Clock.owner`, and `Queues.Queue` lists: `init` (build closures), `resets`,
`input_inits` (readiness tests), `graph_inputs`, `input_flushes`,
`input_ends`; `graph_source` (created lazily by the first output);
`audio_outputs`/`video_outputs` counters; `entries` (hash tables from
endpoint instance name to setter, for the launch closure).

Cells (`'a cell = {mutable cell_generation; mutable value; make}`) are the
mechanism that makes a description replayable: the script's wiring is
closures over cells, and a new generation is just `generation + 1` plus a
fresh config.

`uniq_name` is one global `Hashtbl` of counters, not protected by a lock;
graph owners count with an `Atomic`.

## 4. Classes

- `Ffmpeg_filter_graph.source`: inherits `Source.source`; `sinks` is a
  mutable list of `{field; connected; drain; eof}` records so the graph
  source never knows the media type. `fill_buffer` uses `raise Exit` to stop.
- `Ffmpeg_filter_io.base_output`: inherits `Output.output` with
  `~output_kind:"ffmpeg.filter.input"`, `~infallible:false`,
  `~register_telnet:false`, autostart `true`, its own passive clock; plus the
  virtual class `duration_converter`. Mutable state: `input` (function to
  the buffer source or `None`), `held` (`Stdlib.Queue`, unbounded; a
  `ponytail:` comment records the missing cap), `init` (format check,
  `assert false` until set), `pending` metadata, `ended`, `is_up` (`Atomic`).
- `Ffmpeg_filter_io.sink`: `duration_converter` plus `output`, `conform`,
  `stream_idx`, `eof`.
- `('a, 'params) media` records (`audio_media`, `video_media`) carry what
  differs between audio and video so each class is written once.

The duration converter's state (`duration_converter`, `last_duration`) is
mutable per object and reused across generations on the input side.

## 5. Typing

Graph inputs build a frame type with the field set to
`` `Format (lift_params (default_params `Raw)) `` so the track's raw
parameters unify with the upstream source. Outputs use
`Lang.add_track_operator`, which adds `id` and sets it on the returned
source (`lang_source.ml`), hence "last `id` names the graph". Bitstream
operators use `Lang.add_operator` with return type equal to the source type
`Lang.frame_t (univ_t ()) fields`.

`Ffmpeg_filter_options.mk_options` uses a GADT (`ground_opt_descr`) pairing
each `Avutil.Options.ground` with its converter. It folds the options into
`(args, getter)`: `args` is built by consing (reverse order) and the getter
composes so that the first option is evaluated first and its pair ends up
last.

## 6. Clocks

`Clock.create ~id:"ffmpeg.filter" ()`, `Clock.create ~sync:`Passive
~id:"ffmpeg.filter.input" ~parent ~owner ()`, `Clock.unify`, and
`Clock.register ~parent`/`Clock.deregister`in`on_wake_up`/`on_sleep`of the graph source. Bitstream filters use`Child_support.producer
~check_self_sync:false`with`child#set_process_frame`.

## 7. Formats on the wire

| Where                              | Format                                                                                                                                                                                                                                           | Seen in the code at                    |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------- |
| FFmpeg frame metadata into a graph | key `liquidsoap_track_mark`, value `"1"`                                                                                                                                                                                                         | `Ffmpeg_filter_io.track_mark_metadata` |
| liquidsoap metadata out of a graph | `lavfi.liq.pts`, `lavfi.liq.duration`, `lavfi.liq.best_effort_timestamp`, seconds as `string_of_float`                                                                                                                                           | `sink#metadata_timestamps`             |
| avfilter instance names            | `<filter>_<k>`, `abuffer_<k>`, `buffer_<k>`, `abuffersink_<k>`, `buffersink_<k>`, `parse_aformat_out_<label>_<k>`, `parse_format_out_<label>_<k>`; fixed `buffer`, `setpts`, `fps`, `buffersink`, `abuffer`, `aformat`, `abuffersink` in helpers | `uniq_name`, `Ffmpeg_avfilter_utils`   |

## 8. Tests that pin behaviour

- `tests/regression/GH3944.liq`: a graph (`loudnorm`) over `buffer(sine())`
  survives its input running dry and is rebuilt (§11 of filters.md).
- `tests/media/test_ffmpeg_raw_format_change.liq`: a graph across a video
  format change.
- `tests/media/test_ffmpeg_filter_changing_rate.liq`: a graph across inputs
  of different rates.
- `tests/media/test_ffmpeg_filter_array_args.liq`: array options
  (`aformat(sample_fmts=[…])`).
- `tests/media/ffmpeg_filter_parse.liq`: `parse` with labelled inputs and
  outputs.
- `tests/media/ffmpeg_bitstream_filter.liq`: a bitstream filter on copy
  content.
