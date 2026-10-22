# Language notes: shared utilities, converters, side data

Advice for building [common.md](../common.md), [converters.md](../converters.md)
and the operator side of [side-data.md](../side-data.md) in OCaml on top of
the FFmpeg bindings. Nothing here is binding; Part A is.

## 1. Layout

- One small module per concern, each with an `.mli`: settings and the log
  bridge; time bases, `rescale` and the duration helper; pixel formats and
  image packing; format comparison; hardware contexts. A wrapped library
  keeps the names short.
- The two converters register with the generic plug registries at module
  initialisation. Linking the library is what makes them available.
- The FFmpeg layer is an optional library: it builds only when the
  `ffmpeg-*` binding libraries are present. The build configuration exposes
  `liquidsoap.build_config.optionals.ffmpeg` from whether the library was
  linked.
- The typecheck-only stand-in for the content kinds is a separate library,
  linked with `-linkall` by the typecheck-only executables and never
  together with the real kinds.

## 2. Settings and life cycle

- Settings are `Dtools.Conf` values under one `ffmpeg` node. Files that own a
  key (filters, content) plug it under that node themselves; the index of
  [common.md §1](../common.md#1-settings) stays the reference.
- Liquidsoap's life-cycle hooks give the two set-up points of
  [common.md §2](../common.md#2-log-bridge): module initialisation for the
  defaults, `on_start` for the script's values. The release of the callback
  runs before the log stops (a `Dtools.Init` initialiser ordered before
  `Dtools.Log.stop`).
- The log bridge uses `Avutil.Log.set_level`, `Avutil.Log.set_callback` and
  `Avutil.Log.clear_callback`. The callback type `string -> unit` carries no
  severity: the bindings need a callback that receives the level with the
  message (a request on the bindings, see the Part A rule).
- The main, audio and video rates are lazy values that Liquidsoap forces
  after the script is parsed. Forcing them earlier is a bug; an assertion in
  the frame settings catches it.
- `Printexc.register_printer` can give `Avutil.Error` cases their text; the
  encoder-not-found error is raised where the codec name is known, so the
  message can name it.

## 3. `rescale`

- OCaml's native `int` is 63-bit and has no 128-bit type. Compute `rescale`
  on `Int64` with FFmpeg's own algorithm for `av_rescale_rnd`: split the
  multiplication so that no partial product overflows, and round with the
  "near, away from zero" mode. FFmpeg's implementation is public and short;
  translate it rather than reinvent it. A float shortcut loses precision above
  2^53 and is not acceptable.
- Content lengths are native `int`: convert the `Int64` result with a range
  check rather than `Int64.to_int`, which wraps silently.
- Unit-test the conformance values of
  [common.md §5](../common.md#5-time-base-conversion) directly.

## 4. Duration helper

- A record with mutable fields matches the state list of
  [common.md §6.1](../common.md#61-state). Keep `origin` and `group_pos` as
  options: they are absent until the first timestamp, and the type says so.
- The ended state is a variant
  (``[ `Running of group | `Ended of int ]``, the int being
  `end_position`), so a push after flush is a pattern the compiler shows.
- Build the group in reverse and reverse it when emitting; keep the count
  alongside it for the `max_group_items` bound.
- Return the push result as a polymorphic variant
  (``[ `Chunk of chunk | `Held | `Dropped | `Discontinuity ]``) rather than
  raising, so every caller handles the containment cases in its match.
- `rescale` returns an option (or a variant with `` `Out_of_range ``) for the
  same reason.

## 5. Converters

- The samplerate plug wraps
  `Swresample.Make (PlanarFloatArray) (PlanarFloatArray)`. Keep the resampler and its output rate in one
  `option` value, replaced as a whole on a rate change, after draining the
  old one with `flush`.
- The generic samplerate seam gains `flush` on its converter type; the
  `native` and `libsamplerate` converters implement it too (libsamplerate
  holds samples as well).
- The video converter's context and key live in a `ref` inside the closure
  returned by `create`. Do not share contexts between converters through a
  table: concurrent `sws_scale` on one context crashes.
- `find_converter` can leave the registry walk early with a local exception
  or with `Seq.find` over the registry's entries.

## 6. Hardware contexts

- The candidate list of [common.md §11](../common.md#11-hardware-contexts) is
  a `List.concat_map` over `Avcodec.hw_configs`, then a walk that tries each
  candidate. Writing it as a list first makes the order testable.
- Device and frame contexts come from `Avutil.HwContext`
  ([avutil.md §4.18](../../../../../modules/synced/ffmpeg/spec/avutil.md#418-hardware-contexts)).
  Catch their failure per candidate.
- Raise the `encoder` error with Liquidsoap's runtime error mechanism,
  kind `"encoder"`, at the format's position when one is known.

## 7. Side-data operators

- The two track operators forward every source method to their input except
  frame generation, `side_data` and `rotation`; a shared virtual class gives
  the forwarding once.
- Read the `rotation` getter once per frame and use that value for every
  decision about the frame. Two reads can see two values.
- For copy content, keep the last edited parameters and the inputs they were
  edited from; compare the inputs to skip an edit that would give the same
  result.
- The binding functions behind side-data.md: `Avcodec.params_side_data` and
  `Avcodec.params_with_side_data` for stream-wide copy side data,
  `Avutil.Frame_side_data.props` for the global test,
  `Avfilter.Utils.init_display_converter` and `Avfilter.Utils.display_layout`
  for the display stage, `Avutil.Display_matrix.make` for a rotation entry,
  `Avutil.Frame.dup` for raw frames.
- The handler of `ffmpeg.autorotate.on_undecided` is held in an
  `Atomic.t` of `option`, read when a display stage is created.

## 8. Formats

- Side-data payloads exposed to scripts are FFmpeg's in-memory layout. The
  display matrix payload is nine native-endian `int32`, 36 bytes.
- No dump, wire or disk format is owned by these files.

## 9. Tests that pin behaviour

- `tests/media/test_ffmpeg_side_data.liq`: side-data operators and
  autorotate.
- `tests/media/test_ffmpeg_video_format_change.liq`,
  `tests/media/test_ffmpeg_raw_format_change.liq`: format changes in the
  middle of a stream.
- New unit tests: `rescale` values and overflow, the duration helper's
  conformance items, the samplerate converter's sample counts across rate
  changes.
