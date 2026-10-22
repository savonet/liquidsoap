# Language notes: the `%ffmpeg` encoder (OCaml)

Part B for [encoder-format.md](../encoder-format.md),
[encoder.md](../encoder.md), [encoder-internal.md](../encoder-internal.md),
[encoder-copy.md](../encoder-copy.md) and [timestamps.md](../timestamps.md):
how the current OCaml code is cut, the registries it plugs into, its state
cells, and the `Weak` store of per-source records.

## 1. Modules and libraries

| Module                    | Library / dir                                                                    | Role                                                                                                                                                                       |
| ------------------------- | -------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Ffmpeg_format`           | `src/core/encoder` (core, always built)                                          | the format record and `to_string`; no libav dependency                                                                                                                     |
| `Encoder` (`Ffmpeg` case) | `src/core/encoder`                                                               | `type_of_format`, `extension`, `mime`, `iso_base_file_media_file_format`, `video_size`, `with_file_output`, `with_url_output`, the plug and the per-instance mutex wrapper |
| `Ffmpeg_encoder_type`     | `optionals/ffmpeg/formats` (`liquidsoap.ffmpeg_formats`, linkable without libav) | `type_of_encoder` from the term, registered with `Encoder_types.register "ffmpeg"`; a `codecs` hook (`implement`) for media type by codec name and channels by layout      |
| `Lang_ffmpeg`             | `optionals/ffmpeg/encoder`                                                       | term → record (`make`), registered with `Lang_encoder.register "ffmpeg"`; installs the `codecs` hook with libav lookups                                                    |
| `Ffmpeg_encoder`          | same                                                                             | `Plug.register Encoder.plug "ffmpeg"`; the HLS/global-header defaults (`wrap`); builds stream encoders                                                                     |
| `Ffmpeg_encoder_common`   | same                                                                             | the instance (`encoder`), handler record, `mk_stream_store`, `styp_header`, HLS ops                                                                                        |
| `Ffmpeg_internal_encoder` | same                                                                             | `mk_audio`, `mk_video`, `mk_subtitle`, `write_audio_frame`, resampler/scaler module instances                                                                              |
| `Ffmpeg_copy_encoder`     | same                                                                             | `mk_stream_copy`                                                                                                                                                           |

The split between `Ffmpeg_encoder_type` and `Lang_ffmpeg` exists so that a
process without libav (the JS/LSP build) can type-check `%ffmpeg` terms: it
falls back to the name conventions and the seven-layout table
([encoder-format.md §10.1](../encoder-format.md#101-at-type-checking-time-from-the-term)). Seen in the code at
`Ffmpeg_encoder_type.codecs`.

`Lang_ffmpeg.parse_encoder_name` is defined and never used; the parser uses
`Ffmpeg_encoder_type.parse_encoder_name`.

Inline encoders (`builtins_ffmpeg_encoder.ml`) use
`Ffmpeg_internal_encoder.InternalResampler`, `InternalScaler` and
`write_audio_frame`, and read `Ffmpeg_format` records; they do not build
`Ffmpeg_encoder_common.encoder` values.

## 2. Two content-type computations

`Ffmpeg_encoder_type.type_of_encoder` works on `Term.encoder` parameters
(literal terms only, `term_string`), `Encoder.type_of_format` on the built
`Ffmpeg_format.t`. The record-side one asserts `channels > 0` and builds the
channel layout with `Audio_converter.Channel_layout.layout_of_channels`; the
term side uses `Pcm_format.format_of_channels`. They are kept consistent by
hand; `alpha` and the channel-precedence rule differ (findings).

## 3. Field iteration order

`Frame.Fields` is `Liquidsoap_lang_data.Methods`, an association list whose
`add` prepends. `mk_streams` folds over `ffmpeg.streams` in order with
`Fields.add`, so `Fields.iter` visits the tracks last-written first. This
fixes the order in which stream encoders get each frame
([encoder.md §5](../encoder.md#5-encoding-a-frame)) and the order of `codec_attrs`.
`Ffmpeg_format.to_string` folds with `::` and also prints tracks reversed.

## 4. State cells

- The instance holds the handler in an `Atomic.t` so the Ogg restart can swap
  it; every instance operation is already serialised by the mutex that
  `Encoder.get_factory` wraps around it.
- `started : bool Atomic.t` with `Atomic.exchange` triggers `mk_stream` once
  per handler.
- `pending_on_start : (unit -> unit) Queue.t` holds ID3 writes made before
  the header.
- HLS: `hls_utils.on_keyframe : (unit -> unit) Atomic.t` (the split callback,
  reset to a no-op by `Fun.protect ~finally`), `keyframes : (field * bool
Atomic.t) list` built from all tracks (drop included).
- Rates and sizes are `Lazy.Mutexed.t`, forced at stream creation and by
  `to_string`. Options are a `Hashtbl`; `to_string` prints them in its
  iteration order ([encoder-format.md §11](../encoder-format.md#11-rendering-to_string)).
- [encoder-copy.md §12.2](../encoder-copy.md#122-accepting-a-packet) is `check_stream`, the
  beginning of a source ([timestamps.md §6.2](../timestamps.md#62-beginning-a-source-rebase)) is
  `begin_stream`, the check of §6.3 is `check_dts`.
- Per copy track: `ref`s for the muxer stream, `current_stream`,
  `stream_started`, `waiting_for_keyframe`, `last_dts`, `last_position`,
  `stream_head` (a polymorphic variant ``[ `None | `Pending_start of int64 *
int | `Started of int64 ]``), `intra_only`.
- Per video track: `video_stream_state : video_stream_state option ref`
  (`Option.get` in `get_state` assumes `mk_stream` ran), `converter :
(pixel_format * time_base * stream_idx * Fps.t) option ref`, `start_pts :
int64 ref`.

## 5. The `Weak` store of source records

### 5.1 Structure

```ocaml
module Stream = Weak.Make (struct
  type t = stream_data
  let equal x y = x.idx = y.idx
  let hash x = Int64.to_int x.idx
end)
```

`mk_stream_store total_count` creates one `Stream.t` (initial size 1) per
encoder instance, in `Ffmpeg_encoder`'s factory, before `make`, and returns
`(add, remove)` closures over it. Every copy track of the instance, across Ogg
restarts, gets the same pair.

- `Stream.merge store d` returns the element equal to `d` (same `idx`) if the
  set holds one, else inserts `d` and returns it.
- `Stream.remove store d` removes one element equal to `d`, if any.
- The set holds its elements weakly: an element no one else references can be
  reclaimed by the GC at any time, after which `merge` inserts a fresh record.

### 5.2 Who holds strong references

A `stream_data` is kept alive only by:

1. each copy track's `current_stream` ref (inside the `mk_stream_copy`
   closure, reachable from the handler's `streams`, from the instance's
   `Atomic`, from the `Encoder.encoder` record the output holds);
2. transiently, locals of `check_stream`/`begin_stream`/`remove` during a
   call.

Nothing else: the store does not, and a track drops its previous record when
it switches.

### 5.3 When an entry becomes collectable or disappears

- **Explicit removal**: `remove` at a switch with `ready_count <= 1`. The
  record leaves the set even if another track still holds it as
  `current_stream`. That track keeps using its record (its `position` updates
  and `last_start` reads go to the orphan); when it begins (`begin_stream`
  calls `add` again) `merge` inserts a **fresh** record, and from then on the
  track and any later sibling use the fresh one. The shared `last_start` max
  and readiness are lost for that source ([encoder-copy.md §12.5](../encoder-copy.md#125-release-and-lifetime-of-source-records)).
- **GC**: a record no track holds. While any track has it as
  `current_stream` it cannot be reclaimed, so GC never takes state a running
  track uses. It can take a record that a track will look up later only when
  no track is on that source, which is the case where state would also have
  been removed explicitly.
- **Ogg restart**: the previous handler's tracks become garbage, but their
  `current_stream` records stay in the set until the GC reclaims them. New
  tracks looking up the same source id may find them (with the old
  `last_start`, `position` and a `ready_count` that can now exceed
  `total_count`) or not, depending on GC timing (findings).

### 5.4 `remove` on an entry already gone

`remove d` first does `merge store d`: if `d` was removed, this re-inserts `d`
itself. Then `ready_count <= 1` removes it again (net no-op), and
`ready_count > 1` decrements it and leaves `d` in the set. The re-inserted
record is reachable only from the releasing track, which drops it right after,
so it becomes collectable; nothing observes it unless the same source id is
looked up before the GC runs. Harmless in practice.

### 5.5 No permanent growth

Each track holds at most one record; released records are either removed or
weak. The placeholder `idx = 0L` record each track creates at build is
removed at its first switch (`ready_count` 0). The set therefore holds at most
one live record per copy track plus collectable garbage: no path keeps entries
forever.

### 5.6 Seen in the code: idx 0

`Ffmpeg_content_base.make_data` defaults `stream_idx` to `0L`, the same value
as the copy track's placeholder. No producer of copy content in the tree uses
that default (`new_stream_idx` starts at 1); content with id 0 would never
switch and would wait on a record that cannot become ready.

## 6. Tests that pin behaviour

- `tests/regression/GH3840.liq`: copy audio and video from different sources
  are both muxed (relies on the forced `ready` of
  [encoder-copy.md §12.2](../encoder-copy.md#122-accepting-a-packet) step 1.5).
- `tests/media/test_ffmpeg_copy_concat.liq`: concatenation of copied files.
- `tests/media/subtitle_concat_copy.liq`, `subtitle_concat.liq`: the stream
  head of sparse sources.
- `tests/media/test_ffmpeg_hls_mp4.liq`, `test_ffmpeg_hls_copy_mp4.liq`,
  `ffmpeg_raw_hls.liq`: HLS with mp4 and raw content.

## 7. Runs used for examples

Scripts and outputs under `/var/tmp/claude-ffmpeg-spec/encoder/` (`seq.liq`,
`seq2.liq`, `cut1.liq`, `cut2.liq`, `int.liq`, `hls.liq`, `p.liq`, `t.liq`,
`ch.liq`, `opt.liq`, `tail.liq`), binary at the same commit, libavformat 63.7.100, libavcodec 63.14.100.
