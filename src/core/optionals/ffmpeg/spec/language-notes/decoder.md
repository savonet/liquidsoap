# Language notes: FFmpeg decoder

How the decoder of [decoder.md](../decoder.md) and its per-stream files is
built in OCaml.

## 1. Library and modules

Dune library `liquidsoap_ffmpeg_decoder` in `src/core/optionals/ffmpeg/decoder/`,
`(wrapped false)`, `(optional)`, depending on `liquidsoap_core`,
`liquidsoap_ffmpeg_base`, the `ffmpeg-*` bindings, `mm`, `metadata`,
`menhirLib` and `sedlex` (the last two for parsing `ffmpeg_options`).

| Module                             | Holds                                                                                                                                                                                |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `Ffmpeg_decoder_conf`              | the MIME, extension and priority settings (literals only)                                                                                                                            |
| `Ffmpeg_decoder_common`            | `max_interleave_duration`, the preferred-codec settings and `configure_*_stream`, the `sparse_decoder` record, `mk_subtitle_decoder` (subtitle timeline), `internal_video_converter` |
| `Ffmpeg_internal_decoder` (`.mli`) | `mk_audio_decoder`, `mk_video_decoder`, `mk_text_subtitle_decoder`, `mk_bitmap_subtitle_decoder`                                                                                     |
| `Ffmpeg_raw_decoder` (`.mli`)      | `mk_audio_decoder`, `mk_video_decoder`                                                                                                                                               |
| `Ffmpeg_copy_decoder` (`.mli`)     | `mk_audio_decoder`, `mk_video_decoder`, `mk_subtitle_decoder`                                                                                                                        |
| `Ffmpeg_decoder`                   | probing, stream mapping, decode loop, seek, the three `Plug.register` calls (`Decoder.decoders`, `Request.dresolvers`, `Request.mresolvers`)                                         |
| `Ffmpeg_image_decoder`             | the `Decoder.image_file_decoders` registration                                                                                                                                       |

The container description and content-type derivation live in
`base/ffmpeg_stream_description.ml`, the grouping helper in
`Ffmpeg_utils.Duration`, the display/frame-rate/audio-format/fit stages in
`Ffmpeg_avfilter_utils`.

Exported for other libraries: `Ffmpeg_decoder.get_type` and
`Ffmpeg_decoder.mk_decoder_record` (used by `io/ffmpeg_io.ml` and
`io/ffmpeg_harbor_input.ml`), `Ffmpeg_decoder_common.configure_*_stream`
(`io/ffmpeg_io.ml`) and `Ffmpeg_decoder_common.internal_video_converter`
(`decoder/builtins_ffmpeg_decoder.ml`).

## 2. Registrations

- `Plug.register Decoder.decoders "ffmpeg"` with a `Decoder.decoder_specs`
  record: `priority`, `file_extensions` and `mime_types` are closures reading
  the settings at each call; `file_type` always returns `Some`;
  `file_decoder = Some create_file_decoder`, `stream_decoder = Some
create_stream_decoder`.
- `Plug.register Request.dresolvers "ffmpeg"` with `{dpriority;
file_extensions; dresolver}`; `dresolver` returns a float or raises
  `Not_found`.
- `Plug.register Request.mresolvers "ffmpeg"` with `{priority; resolver}`,
  `resolver ~metadata ~extension ~mime file`.
- `Plug.register Decoder.image_file_decoders "ffmpeg"` with
  `{image_decoder_priority; check_image; decode_image}`.

The `request.duration.<name>` and `file.metadata.<name>` builtins are added by
`builtins_request.ml` and `builtins_files.ml` in a `Lifecycle.on_load` hook,
after every module registered its resolver.

The preferred-codec settings are created at module initialisation by
iterating `Avcodec.Audio.decoders` and `Avcodec.Video.decoders` into a
hashtable keyed by `string_of_id`; the list values are built by consing, which
reverses FFmpeg's order.

## 3. Types

- Per-stream decoders are closures `~buffer:Decoder.buffer -> item -> unit`,
  where `item` is a polymorphic variant including `` `Flush ``. Sparse ones
  are `Ffmpeg_decoder_common.sparse_decoder = {decoder; advance}`.
- `Ffmpeg_decoder.decoder` is an ordinary variant with one constructor per
  (media, mode) pair, each holding the `Av.stream` (typed by media and by
  ``[`Frame]`` or ``[`Packet]``) and the matching decoder closure. Pairing
  them makes the dispatch in `decode_data` total except for a fallback arm the
  construction makes unreachable.
- Because `Av.stream`'s type records the read mode, `mk_streams` walks each
  media type's stream list twice (once at packet type, once at frame type),
  calling `Av.get_*_streams` again for the other instantiation.
- `stream` records hold mutable `seen`, `first_position`, `pts`, `position`.
- The decoder record keeps `(streams, decoder, eof)` in an `Atomic.t`
  replaced on seek; `remaining` and `target_position` are `Atomic.t`. Decoding
  runs under `Mutex_utils.atomic_lock ~state`, seeking under
  `Mutex_utils.mutable_lock ~state`; the rebuild after a seek happens after
  the lock is released.
- The internal audio decoder picks a first-class module (`Converter_type`)
  by PCM kind, over `Swresample.Make (Swresample.Frame)` applied to
  `PlanarFloatArray`, `S16PlanarBigArray` or `FltPlanarBigArray`.
- `parse_input_args` runs the language's encoder-parameter parser
  (`Parser.plain_encoder_params` through `MenhirLib.Convert`, a `Sedlexing`
  lexer and `Preprocessor.mk_tokenizer`) and `Term_reducer.to_encoder_params`;
  the fold has an `assert false` arm, caught by the surrounding `with _`.
- Exceptions: `Ffmpeg_decoder.End_of_file`, `Ffmpeg_decoder.Invalid_file`
  (internal to the metadata resolver), `Ffmpeg_copy_decoder.Corrupt` and
  `Empty` (internal, swallowed).

## 4. Formats seen in the code

- `ffmpeg_options` metadata: the argument list of `%ffmpeg(...)` without the
  parentheses (seen in the code at `Ffmpeg_decoder.parse_input_args`).
- `application/ffmpeg;<args>` MIME type with the same argument syntax after
  the first `;` (seen in the code at `create_stream_decoder`).
- Timed ID3 payloads with and without their `ID3` header; the missing header
  is rebuilt with version bytes `03 00` then `04 00` (seen in the code at
  `parse_timed_id3`).

## 5. Tests that pin behaviour

- `tests/regression/ffmpeg_seek_preroll.liq`: pre-roll decoding after a seek
  (§8.5 of decoder.md).
- `tests/regression/seek_cue_out.liq`, `tests/regression/seek_track_map.liq`,
  `tests/streams/cue-cut.liq`: seek and cue interplay.
- `tests/core/ffmpeg_get_type_test.ml`, `tests/core/ffmpeg_stream_description_test.ml`:
  content type derivation and container description.
