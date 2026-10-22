# Inline encoding and decoding operators (Part B, OCaml)

How [inline-codecs.md](../inline-codecs.md) is laid out in OCaml. It relies
on the Lang track-operator API, `Child_support` and the FFmpeg bindings.

## 1. Files and registration

| File                                  | Holds                                                                                                  |
| ------------------------------------- | ------------------------------------------------------------------------------------------------------ |
| `base/builtins_ffmpeg_base.ml`        | Lang modules `ffmpeg`, `ffmpeg.filter`, `ffmpeg.raw`, `track.ffmpeg`, `track.ffmpeg.raw`               |
| `base/ffmpeg_inline.ml`               | `relay_metadata`, `mk_producer` (§3 of Part A), shared with bitstream filters                          |
| `encoder/builtins_ffmpeg_encoder.ml`  | `encode_audio_frame`, `encode_video_frame`, `mk_encoder`, `SourceIdx`                                  |
| `decoder/builtins_ffmpeg_decoder.ml`  | `decode_audio_frame`, `decode_video_frame`, `mk_decoder`                                               |
| `src/libs/ffmpeg.liq`                 | the twelve source wrappers, under `%ifdef track.ffmpeg.raw.encode.audio` / `track.ffmpeg.decode.audio` |
| `src/core/operators/child_support.ml` | `child_output`, `base`, `producer` (generic)                                                           |

`ffmpeg_encode = Lang.add_module ~base:track_ffmpeg "encode"` and the like
give the `track.ffmpeg.encode.*` names. Each file ends with
`List.iter mk_encoder [`Audio_encoded; `Audio_raw; `Video_encoded; `Video_raw]`
(same for `mk_decoder`), which calls `Lang.add_track_operator name proto
~base ~return_t ~category:`Conversion`. The operator returns
`(field, producer)`; `field` is the input track's field.

Prototype of the encoders: `[("", Lang.format_t (Lang.frame_t (Lang.univ_t ())
(Fields.add format_field format_frame_t empty)), None, Some "Encoding
format."); ("", input_frame_t, None, None)]`, where `format_frame_t` is
`input_frame_t` (encoded) or `output_frame_t` (raw). `Lang.univ_t ()` as the
frame base is what leaves the format's row open.

## 2. Producer

`Ffmpeg_inline.mk_producer ~stack ~name ~field ~input_frame_t
~mk_process_frame source` creates `new Child_support.producer ~stack
~child_frame_type ~check_self_sync:false ~name source` and calls
`producer#child#set_process_frame process_frame`. The transform is a
`ref None` captured by `process_frame`; `` `Flush `` sets it back to `None`.

`Child_support.child_output#output` is where the `Frame.position frame > 0`
skip, the `process_frame` call and the `conf_max_buffer` check
(`Runtime_error.raise … "clock"`) sit. `child_output#flush` is called only
from the `base` class's `on_sleep` handler, after `child#sleep`. `on_wake_up`
does `child_buffer <- Generator.create self#content_type` and
`child#set_generator`.

`Generator.add_metadata ?pos` and `add_track_mark ?pos` take absolute
positions from the generator's head (`default_pos` is `length gen` only when
`pos` is omitted). `relay_metadata` always passes `~pos`.

## 3. Stream index

```ocaml
type source_idx = { source : Source.source; idx : int64 }
module SourceIdx = Weak.Make (struct
  type t = source_idx
  let equal x y = x.source == y.source
  let hash x = Hashtbl.hash x.source#id
end)
let source_idx_map = SourceIdx.create 0
```

`SourceIdx.merge source_idx_map { source; idx = new_stream_idx () }` runs in
`mk_encode_frame`, i.e. per transform build. The returned record is captured
by the transform closures (`source_idx.idx` in every chunk), which is what
keeps the weak entry alive. Before the track split (commit `25f118689`,
"Implement track-level ffmpeg inline encoder", #2869) the hash was
`Obj.magic x.source`.

## 4. Encoder transform

`mk_encode_frame generator` (inside the operator closure):

1. `Type.fresh output_frame_t`, passed as `~type_t`; the encoders run
   `Typing.(effective_t <: type_t)` with `effective_t` built from the
   resolved parameters (`Format_type.descr (`Format (lift_params params))`).
2. `List.assoc format_field format.streams`; the `` `Encode {opts} `` table
   is `Hashtbl.copy`'d (the comment there speaks of "every track boundary";
   the copy matters per build).
3. Match on `` `Encode { mode; codec; options } ``; `_ -> assert false`
   covers `codec = None` in internal mode and `` `Copy ``/`` `Drop ``.
4. `encoded && Hashtbl.length opts > 0` raises `Error.Invalid_value`.

Audio uses `Ffmpeg_internal_encoder.InternalResampler`
(`Swresample.Make (PlanarFloatArray) (Frame)`) and
`Ffmpeg_internal_encoder.write_audio_frame`. Only `Content_audio` input is
supported; the pcm_s16/pcm_f32 resamplers of the internal encoder are not
used here. Video uses `Ffmpeg_internal_encoder.InternalScaler`
(`Swscale.Make (BigArray) (Frame)`), `Ffmpeg_avfilter_utils.Fps.init`
without `?source_fps` or `?start_pts` (hence always the `` `Filter `` case
and the `PTS-max(STARTPTS, 0)` expression), `Ffmpeg_utils.pixel_format`,
`Ffmpeg_utils.mk_hardware_context`. Scaler and fps converter are `ref`s set
by `mk_scaler`/`mk_fps_converter`; `chosen_pixel_format` is a `ref`
initialised to `` `Yuv420p ``. The image counter is `nb_frames : int64 ref`.

## 5. Chunker

`Ffmpeg_utils.Duration`: record `{ mode; convert_ts; get_ts; set_ts;
get_duration; src; dst; offset; mutable last_ts; mutable packets }`. Inline
encoders call `init ~mode ~src ~convert_ts:false ~get_ts ~set_ts
~get_duration ()` (no `?offset`, no `?last_ts`) and `push`; they never call
`Duration.flush`, whose result is `(max(1, max packet duration), packets)`.
`get_ts` is `Avcodec.Packet.get_dts` (encoded) or `Avutil.Frame.pts` (raw).
`convert_time_base` is `Int64.div (Int64.mul pts (of_int (src.num *
dst.den))) (of_int (src.den * dst.num))`; the products are OCaml `int`.

## 6. Decoder transforms

Audio: `InternalResampler = Swresample.Make (Swresample.Frame)
(Swresample.PlanarFloatArray)` (decoder side). Copy decoder state is three
`ref`s (`current_converter`, `current_stream`, `current_params`); raw is one
`ref` holding `(converter, time_base, stream_idx)`. Raw parameters are read
with `Option.get`.

Video: records `video_codec`, `video_stream` (mutable `params`, `display`,
`matrix : [ `Decoder | `Stream of Display_matrix.t option ]`),
`raw_video_stream`. `Ffmpeg_decoder_common.internal_video_converter` provides
`{ convert; flush }`; `Ffmpeg_avfilter_utils.Display` the display stage.
Parameter identity uses `==` on `codec_params`; side data compared with `<>`
on `Avcodec.params_side_data`. Logger `Log.make ["ffmpeg"; "internal";
"decoder"]`.

## 7. Tests pinning behaviour

- `tests/regression/GH5110.liq`: three AAC encoders on one source (shared
  child clock, shared stream index).
- `tests/regression/GH3944.liq`: raw encode → filter → raw decode across an
  input running dry.
- `tests/media/ffmpeg_inline_encode_decode*.liq`, `ffmpeg_raw_hls.liq`,
  `test_ffmpeg_distributed_hls*.liq`, `tests/regression/GH2602.liq`.
