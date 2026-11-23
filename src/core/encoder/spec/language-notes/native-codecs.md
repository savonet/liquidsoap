# Native codecs: OCaml and binding notes (Part B)

Maps [mp3.md](../mp3.md), [shine.md](../shine.md), [fdkaac.md](../fdkaac.md)
and [flac.md](../flac.md) onto the code. Paths are relative to the
repository root; line numbers are at 02fa52985.

## Where things are

| Concern                           | `%mp3`                                                                                                     | `%shine`                              | `%fdkaac`                               | `%flac`                                     |
| --------------------------------- | ---------------------------------------------------------------------------------------------------------- | ------------------------------------- | --------------------------------------- | ------------------------------------------- |
| Format record, printing, estimate | `src/core/encoder/mp3_format.ml`                                                                           | `shine_format.ml`                     | `fdkaac_format.ml`                      | `flac_format.ml`                            |
| Option parser, registration       | `lang_mp3.ml` (`mp3`, `mp3.cbr`, `mp3.abr`, `mp3.vbr`)                                                     | `lang_shine.ml` (`shine`, `mp3.fxp`)  | `lang_fdkaac.ml`                        | `lang_flac.ml` (`flac`; `make_ogg` for Ogg) |
| Instance                          | `src/core/optionals/lame/liquidsoap_lame.ml`                                                               | `optionals/shine/liquidsoap_shine.ml` | `optionals/fdkaac/liquidsoap_fdkaac.ml` | `optionals/flac/flac_encoder.ml`            |
| Binding                           | `src/modules/synced/lame/src/`                                                                             | `synced/shine/src/`                   | `synced/fdkaac/src/`                    | `synced/xiph/flac/`                         |
| Helpers                           | `Encoder.{extension,mime,bitrate,iso_base_file_media_file_format}` in `src/core/encoder/encoder.ml:96-146` |                                       |                                         |                                             |

Each optional registers with `Plug.register Encoder.plug "<name>"` and the
seam's `get_factory` wraps the record with `Encoder.protect`
(`encoder.ml:207-248`). HLS: `src/core/encoder/hls_packed.ml:39-51` accepts
`MP3 _ | Shine _ | FdkAacEnc {transmux = `Adts}`.

## `%mp3`

- `lang_mp3.ml:30-38`: `check_samplerate` wraps the value in
  `Lazy.Mutexed.from_fun`, so even a literal is validated when forced
  (`liquidsoap_lame.ml:73`), raising `Lang_encoder.raise_error` from the
  streaming thread.
- `lang_mp3.ml:74-81`: `id3v2` accepts `true` (3), `false`, `"none"`, any
  `Int`. `channels` is not matched and falls to `raise_generic_error`.
- `liquidsoap_lame.ml:46-62`: `apply_constaints` applies `hard_min` first,
  then `quality`, mean, min, max. Bindings: `Lame.set_vbr_mode` →
  `lame_set_VBR` with `vbr_mode_val = {vbr_off, vbr_rh, vbr_abr, vbr_mtrh,
vbr_max_indicator}`; `set_mode` → `lame_set_mode(Int_val)` with the OCaml
  constructor order `Stereo | Joint_stereo | Dual_channel | Mono` matching
  LAME's `MPEG_mode` 0..3 (`lame.ml:57`, `lame_stubs.c:113`).
- `liquidsoap_lame.ml:74`: `Lame.set_bWriteVbrTag enc false`. Introduced by
  #3064 (96cf5526a, May 2023, "Make encoders thread safe, cleanup mp3
  encoder"), which also replaced per-call encoding with fixed chunks and
  dropped the copyright/original-bit message signalling (`msg`,
  `msg_interval`).
- `liquidsoap_lame.ml:82-114`: `frame_size = Frame.main_of_audio
(Lame.frame_size enc)`; a `Generator` holds pending audio; the loop
  condition is `Generator.length pending_data > frame_size` (strict).
  Before #3064 the id3v2 tag was rendered from the first non-empty
  metadata, via `insert_metadata`, and skipped when empty; now it is
  rendered at creation from the factory's metadata, empty or not
  (`liquidsoap_lame.ml:90-97`).
- `liquidsoap_lame.ml:117`: `stop` = `Lame.encode_flush`; the Generator is
  dropped.
- Binding: `ocaml_lame_encode_buffer_float` (`lame_stubs.c:254-288`) mallocs
  two float buffers (unchecked) and a `LAME_MAXMP3BUFFER` (144 KiB) output
  buffer on the heap (stack is too small for musl threads, comment at
  `lame_stubs.c:49`), scales `32768 × clip(x)` (NaN → 0), releases the
  runtime around `lame_encode_buffer_float`, raises via `raise_enc_err`.
  `ocaml_lame_init_params` prints to stderr on failure
  (`lame_stubs.c:133`).
- ID3v2 rendering: `Utils.id3v2_of_metadata` (`src/core/utils/utils.ml:478-522`)
  and `Metadata.ID3v2.make` (`src/modules/synced/metadata/src/metadataID3v2.ml:569`);
  frame flags from `default_flags`.

## `%shine`

- `lang_shine.ml`: no validation at all; the anonymous `mono`/`stereo`
  cases appear twice (the second pair is dead code).
- `liquidsoap_shine.ml:13-41`: `Audio_converter.Samplerate.create
channels`; `resample` returns `(buf, start, len)`; loop `while
Generator.length buf > samples` (strict).
- `liquidsoap_shine.ml:43`: `stop` = `Shine.flush` (`shine_flush`), which
  returns pending bitstream bytes only.
- Binding `shine.ml:30-37`: `check_config` then channel check, both raising
  plain exceptions; `shine_stubs.c:66-88`: `shine_set_config_mpeg_defaults`,
  mode MONO or JOINT_STEREO; `shine_stubs.c:104-129`: stack arrays of
  `SHINE_MAX_SAMPLES`, `clip` without NaN case, blocking section around
  `shine_encode_buffer`.

## `%fdkaac`

- `lang_fdkaac.ml:103-106`: the HE-AAC v2 channel check is a `failwith`.
- `liquidsoap_fdkaac.ml:6-49`: parameter list and `set` mapping of
  `Invalid_config` / `Unsupported_parameter` to `Lang_encoder.raise_error`.
- `liquidsoap_fdkaac.ml:58-76`: `Utils.buflen` (`src/core/utils/utils.ml:105`,
  `LIQ_BUFLEN`, default 1024); copies `n` bytes out of a `Strings.Mutable`
  per chunk.
- `liquidsoap_fdkaac.ml:78-88`: `stop` maps the remaining buffer through
  `Fdkaac.Encoder.encode` with `Strings.Mutable.map` (which returns a new
  buffer holding the result), then appends `Fdkaac.Encoder.flush`.
- Binding `fdkaac.ml`: `Encoder.t = {enc; mutable info; buffer : Buffer.t}`;
  `get_info` memoises `ocaml_fdkaac_info`, whose
  `aacEncEncode(enc, NULL, NULL, NULL, NULL)` is FDK's initialisation, so
  configuration errors surface at the first `encode`. `chunk_length = 2 ×
input_channels × frame_length`. `flush` pads with `\000` and loops on
  `ocaml_fdkaac_flush` until `Unsupported_parameter | End_of_file`.
- `fdkaac_stubs.c:230-264` (`ocaml_fdkaac_flush`): sets
  `outBuf.bufferIdentifiers/bufSizes/bufElSizes/bufs` but **not**
  `outBuf.numBufs` (zero from the initialiser), so FDK has no output buffer
  and `numOutBytes` stays 0; contrast `ocaml_fdkaac_encode`
  (`fdkaac_stubs.c:211`) which sets it to 1.
- `fdkaac_stubs.c:120-123,146-161`: on errors after `aacEncOpen` the
  handle is `free`d rather than `aacEncClose`d. Output uses a 20 480-byte
  stack buffer.

## `%flac`

- `flac_encoder.ml:15-26`: `total_samples = None`; `Flac.Encoder.create
~comments ~write p` without `~seek`/`~tell`; `write` appends to a
  `Strings.Mutable`.
- Binding `flac.ml:194-201`: `create` wraps `write` with `Bytes.sub b 0
len` (one copy) and installs `Gc.finalise cleanup`.
- `flac_stubs.c:715-787` (`ocaml_flac_encoder_alloc`): VORBIS_COMMENT
  object, `FLAC__stream_encoder_set_metadata(enc, &meta, 1)`.
- `flac_stubs.c:789-801` (`ocaml_flac_encoder_init`): the return value of
  `FLAC__stream_encoder_init_stream` is discarded.
- `flac_stubs.c:667-699`: seek and tell callbacks return `UNSUPPORTED`
  when the OCaml option is `None`.
- `flac_stubs.c:803-867`: `sample_from_double` (clip, multiply, truncate),
  per-call malloc of the planar buffer, process and finish return values
  discarded.
- `flac_stubs.c:639-664`: the write callback registers the thread with the
  runtime, re-acquires it, copies into a reusable OCaml buffer and calls the
  closure; the comment notes that `Flac_ogg` relies on one libFLAC write
  being one OCaml call.

## Shared mechanics

- Resampling: `Audio_converter.Samplerate.resample`
  (`src/core/media/audio_converter.ml:88-91`) copies when the ratio is 1.0;
  otherwise the first available converter of `settings.audio.converter.
samplerate.converters` (default libsamplerate, ffmpeg, native). The
  converter is never flushed.
- `AFrame.pcm` / `AFrame.position` give the whole audio array and the
  piece's length in samples.
- Float to 16-bit for `%fdkaac`: `Audio.S16LE.make` → `s16_clip`
  (`src/modules/synced/mm/src/audio_c.c:96`).
- Initial metadata: the four factories receive
  `Frame.Metadata.Export.t`; only `%mp3` (id3v2) and `%flac` use it.
  `pipe_output.ml:61` declares `current_metadata` but nothing assigns it
  since 2ba7ba739 (#2924, Feb 2023); `icecast2.ml:605`,
  `srt/output_operator.ml:297` and `hls_packed.ml:48` pass
  `Frame.Metadata.Export.empty`; `harbor/dedicated.ml:369-371` passes the
  last metadata.
- `pipe_output.ml:91`: `close_encoder` calls `stop` under `try ... with _ ->
Strings.empty`, so a failing `stop` loses its bytes silently.
