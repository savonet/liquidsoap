# Language notes: side data, converters, shared utilities

How [side-data.md](../side-data.md), [converters.md](../converters.md) and
[common.md](../common.md) are built in OCaml today. Paths are relative to
`src/core/optionals/ffmpeg/` unless absolute from `src/`.

## 1. Modules

| Module                                | Holds                                                                                                                                                                     |
| ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `base/ffmpeg_utils.ml(i)`             | settings, log bridge, start-up actions, time bases, `convert_time_base`, `Duration`, pixel formats, packing, format comparison, `mk_hardware_context`, `global_side_data` |
| `base/ffmpeg_avfilter_utils.ml(i)`    | `Display` (the display stage of side-data §3); also `Fps`, `AFormat`, `Fit`, owned by other files                                                                         |
| `filter/builtins_ffmpeg_side_data.ml` | `track.ffmpeg.side_data`, `track.ffmpeg.raw.side_data`, `ffmpeg.autorotate.on_undecided`                                                                                  |
| `base/ffmpeg_audio_converter.ml`      | the `ffmpeg` samplerate plug                                                                                                                                              |
| `base/ffmpeg_video_converter.ml`      | the `ffmpeg` video converter plug                                                                                                                                         |
| `src/core/media/audio_converter.ml`   | the samplerate seam (`Samplerate.create`, `resample`, the setting)                                                                                                        |
| `src/core/media/video_converter.ml`   | the video converter seam (`find_converter`, `scaler`, the setting)                                                                                                        |
| `liquidsoap_ffmpeg.ml`                | empty: gives the aggregating library something to compile                                                                                                                 |
| `base/FFMPEG_SIDE_DATA.md`            | in-tree side-data document (MUST/SHOULD wording)                                                                                                                          |

## 2. Settings and life cycle

- Settings are `Dtools.Conf` values plugged under `Configure.conf#plug "ffmpeg"`
  (`conf_ffmpeg`). Other files plug their own keys under `conf_ffmpeg`
  (`filter_max_buffer` in `filter/ffmpeg_filter_graph.ml`, `content` in
  `base/ffmpeg_content_base.ml`, `content.copy` in `base/ffmpeg_copy_content.ml`).
- `settings.ffmpeg.log.capture` (`conf_capture`) is not exported by the
  `.mli`; `conf_format_change_*` are not either, only
  `ignored_video_properties`.
- `Lifecycle.on_init` logs the library versions; `Lifecycle.on_start` sets
  the FFmpeg log level and installs the callback; a `Dtools.Init.make
~before:[Dtools.Log.stop]` initialiser named `ffmpeg-log-release` clears
  it. `Avdevice.init ()` runs at module initialisation.
- The log bridge calls `Avutil.Log.set_level`, `Avutil.Log.set_callback` and
  `Avutil.Log.clear_callback` ([common.md §2](../common.md#2-log-bridge)).
- A commented-out `let level = conf_level#get` sits in the start hook: the
  level is read per message instead.
- `Printexc.register_printer` gives `Avutil.Error `Encoder_not_found` its text.
- Time bases force `Lazy.Mutexed` values of `Frame` (`main_rate`,
  `audio_rate`, `video_rate`), which `Frame_settings` evaluates after script
  parse and asserts are not forced before.

## 3. Side-data operators

- `class virtual passthrough` inherits `Source.operator` and forwards
  `fallible`, `can_generate_frame` (`source#is_ready`), `remaining`,
  `abort_track`, `effective_source`, `self_sync` (`source#cached_self_sync`).
  Subclasses `copy` and `raw` implement `generate_frame`, `side_data` and
  `rotation`.
- Registered with `Lang.add_track_operator`, base `track.ffmpeg` and
  `track.ffmpeg.raw` (from `Builtins_ffmpeg_base`), category `` `Track ``,
  return type the same `Format_type.descr (`Kind …)`as the input, methods
with empty quantifier lists. The`id`argument is added by`add_track_operator`.
- `copy` keeps `entries` and `last_edit` as mutable fields;
  `last_edit` compares parameters with `==` and the rotation with `=`.
  `edit_params` returns the input `params` physically when the edited list is
  structurally equal, and `rewrap` preserves the physical identity of the
  outer `codec_params` variant in that case.
- `edit.rotation` is a closure over `Lang.to_getter`. `copy` calls it twice
  per frame (in `#edit` and in `rotate_entries`); `raw` snapshots it once
  per frame into a constant closure.
- `raw` keeps `last_frame` (an `Avutil.frame`) as a mutable field: the last
  AVFrame stays alive until the next frame replaces it.
- `ffmpeg.autorotate.on_undecided` stores the handler with
  `Ffmpeg_avfilter_utils.Display.set_on_undecided`, an `Atomic.t` of
  `option`; `Display.init` reads it with `Atomic.get`.
- Binding functions behind [side-data.md](../side-data.md): stream-wide copy
  side data is `Avcodec.params_side_data`, rewritten with
  `Avcodec.params_with_side_data`; the `Global` test is
  `Avutil.Frame_side_data.props kind`; the display stage is
  `Avfilter.Utils.init_display_converter` with `Packet_side_data.cropping` of
  the parameters; the geometry readers call `Avfilter.Utils.display_layout`;
  the rotation entry is `Display_matrix.make`; the raw operator duplicates
  frames with `Frame.dup`.
- `filter_spec_of_value` turns each `(option, value)` into
  `` `Pair (option, `String value) ``.

## 4. Converters

- Both converters register with `Plug.register` at module initialisation:
  `Audio_converter.Samplerate.converters "ffmpeg" ~doc:""` and
  `Video_converter.video_converters "ffmpeg"`.
- The samplerate plug uses `Swresample.Make (PlanarFloatArray) (PlanarFloatArray)`;
  the resampler and its output rate are two `ref`s in the closure.
- The video converter's context is a `ref` in the closure returned by
  `create`; there is no table shared between converters. Commit `dba44551a`
  removed a `Weak` hash table of contexts shared by all converters (two
  entries pinned): concurrent `sws_scale` on one context faulted in
  `ff_hscale8to15_X8_neon` once clocks ran on separate domains.
- `Video_converter.find_converter` uses an `Exit of converter` exception to
  leave `Plug.iter` early.

## 5. Duration

- `Duration.t` is a record with mutable `last_ts` and `packets`; `push`
  destructures the record into locals first, so `packets` in the result is
  the list as it was before the call.
- `Content.Invalid` is the exception raised on non-monotonic timestamps.
- `Duration.flush` does not reset `packets`.

## 6. Hardware contexts

`mk_hardware_context` iterates `Avcodec.hw_configs` with `List.iter` and a
local `Found` exception carrying the result; non-returning arms only log.
The error is raised with `Lang_encoder.raise_error ~pos:None`, which is
`Runtime_error.raise` with kind `"encoder"`.

## 7. Build and dune

- `liquidsoap_ffmpeg` (`dune`) is an `(optional)` library that only depends
  on the five sub-libraries `liquidsoap_ffmpeg_{base,decoder,encoder,filter,io}`,
  each `(optional)` and `(wrapped false)`. They are skipped when an
  `ffmpeg-*` binding library is missing.
- `src/core/optionals/config/gen_dune.ml` declares
  `dep "ffmpeg" "liquidsoap_ffmpeg" ~build_config:"ffmpeg"`, generating in
  `dune.inc` a `(select ffmpeg_option.ml from (liquidsoap_ffmpeg -> …enabled.ml) (-> …disabled.ml))`
  and the `("ffmpeg", Ffmpeg_option.enabled)` entry of
  `Optionals_build_config.all`, which `builtins_optionals.ml` turns into
  `liquidsoap.build_config.optionals.ffmpeg`. `Ffmpeg_option.detected`
  (`"yes"` / `"no (requires ffmpeg)"`) feeds `src/bin/runtime/build_summary.ml`.
- `formats/` (`liquidsoap_ffmpeg_formats`) is not optional.
  `formats/dummy/` (`liquidsoap_ffmpeg_formats_dummy`, `-linkall`) is linked
  only by `src/core/typecheck/dummy` (`liquidsoap.typecheck_dummy`); linking
  it together with the real kinds raises.

## 8. Formats seen in the code

- Side-data payloads exposed by `side_data()` (`data` field): FFmpeg's
  in-memory layout, seen in the code at `Packet_side_data.raw.data` and
  `Frame_side_data.raw.data`. The display matrix payload is nine native-endian
  `int32`, 36 bytes, seen in the code at `Avutil.Display_matrix.make` /
  `to_payload` (bindings).
- No dump, wire or disk format is owned by these files.

## 9. Tests that pin behaviour

- `tests/media/test_ffmpeg_side_data.liq`: copy relay keeps the rotation,
  `rotation()` reads it, a set rotation and a removed matrix reach the
  output, internal decoding gives upright 240×320, a rotation set on raw
  reaches the output, rotation 33 calls `on_undecided` with `"odd_rotation"`.
- `tests/media/test_ffmpeg_video_format_change.liq`,
  `tests/media/test_ffmpeg_raw_format_change.liq`: format changes in the
  middle of a stream (the `format_change` settings' consumers).
