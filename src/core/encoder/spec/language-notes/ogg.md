# Ogg encoder: OCaml notes (Part B)

Maps [ogg.md](../ogg.md), [ogg-streams.md](../ogg-streams.md) and the codec
files onto the code. Not normative; the Part A files govern.

## Files

| Part A                  | Code                                                                                                                                                                     |
| ----------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| ogg.md §1 (format)      | `src/core/encoder/ogg_format.ml`, `lang_ogg.ml`; `Encoder.Ogg` and the Ogg cases of `type_of_format`, `extension`, `mime`, `video_size` in `src/core/encoder/encoder.ml` |
| ogg.md §2–§9 (instance) | `src/core/optionals/ogg/ogg_encoder.ml` (`encoder`, plug `"ogg"` at :152)                                                                                                |
| ogg.md §10 (muxer)      | `src/core/optionals/ogg/ogg_muxer.ml` / `.mli`                                                                                                                           |
| codec §1                | `src/core/encoder/{vorbis,opus,speex,theora,flac}_format.ml`, `lang_{vorbis,opus,speex,theora,flac}.ml` (`Lang_flac.make_ogg`)                                           |
| codec §2–§13            | `src/core/optionals/{vorbis,opus,speex,theora}/*_encoder.ml`, `src/core/optionals/ogg_flac/ogg_flac_encoder.ml` (+ `ogg_flac_encoder_stubs.c`)                           |
| libogg, codec bindings  | `src/modules/synced/xiph/{ogg,vorbis,opus,speex,flac,theora}`                                                                                                            |

## The seam as OCaml values

- Track maker: a function `Ogg_format.audio -> Ogg_encoder.track` stored in
  the hash table `Ogg_encoder.audio_encoders` under `"vorbis"`, `"opus"`,
  `"flac"`, `"speex"` by each module's toplevel `let () =`; Theora sets the
  `Ogg_encoder.theora_encoder` reference. A module not linked in leaves its
  entry absent: `prepare_encoder` (ogg_encoder.ml:80) raises
  `Lang_encoder.raise_error`; the Theora path does `Option.get` (:93).
- `Ogg_encoder.track = { encode; reset; mutable id : nativeint option }`.
  `id = None` means "not registered in the current link".
- Stream encoder: `Ogg_muxer.stream_encoder`, a record of closures
  `header_encoder`, `fisbone_packet`, `stream_start`, `data_encoder`
  (`Audio_encoder`/`Video_encoder` of `'a data -> Ogg.Stream.stream ->
(Ogg.Page.t -> unit) -> unit`), `end_of_page`, `end_of_stream`. Per-link
  codec state lives in the closures (`ref`s such as `started`, `enc`,
  `remaining`).
- The instance is built with `let rec enc = {...} and streams_start () ...
and encode ... and stop ... and encode_metadata ...` (ogg_encoder.ml:113–149).

## Muxer

- `Ogg_muxer.t` (ogg_muxer.ml:68): `encoded` and `header` are
  `Strings.Mutable.t`; each page is added as two fragments (header string,
  body string), so a returned `Strings.t` has 2 fragments per page.
- `get_data` (:122) is `Strings.Mutable.flush` plus the link-start hand-over;
  `last_link_start` reads `flushed_link_start`.
- `state` is `Bos | Streaming | Eos` (Part A: beginning, streaming, ended).
- Held page: `stream.remaining : (float * Ogg.Page.t) option`; queue:
  `stream.available : Ogg.Page.t Queue.t`.
- `Ogg.Not_enough_data` is the control flow for "no page".
- `flush` (:388) runs `Hashtbl.iter (fun id _ -> end_of_track encoder id)`
  while `end_of_track` → `add_available` may `Hashtbl.remove` from the same
  table (:344): mutation during iteration, which the OCaml manual leaves
  unspecified.
- Serials: `Random.State.nativeint random_state 0x3FFFFFFF` with one global
  `random_state` (:114), not domain-safe.
- `eos` (:398) clears the header buffer with `Strings.Mutable.flush`.

## Lazy values

`samplerate` (Vorbis, Speex, FLAC) and `width`/`height`/`picture_*` (Theora)
are `int Lazy.Mutexed.t` defaulting to `Frame.audio_rate` /
`Frame.video_dimensions ()`. The makers force them (once per instance); the
Theora parser forces `width`/`height` while checking `picture_*`; `to_string`
forces them.

## Calls into C

- `Ogg.Stream.terminate` (xiph/ogg/ogg.ml:101) = flush all + the C stub
  `ocaml_ogg_stream_terminate` (ogg_stubs.c:288), which packets in an empty
  EOS packet with `granulepos = os->granulepos + 1`, pages it out, and sets
  `header[26] = 0`, `header_len = 27`, body empty, then recomputes the CRC.
  Used by Opus (opus_encoder.ml:116), Speex (:145), Theora (:151).
- `Ogg.Stream.get_page ?fill` → `ocaml_ogg_stream_pageout`
  (ogg_stubs.c:314) uses `ogg_stream_pageout_fill` only `#ifdef
HAVE_PAGEOUT_FILL`; nothing in the tree defines it (the flag dates from the
  vendoring, d772ceee5), so `?fill` is dropped.
- Ogg FLAC: `liq_ocaml_ogg_stream_set_eos` (ogg_flac_encoder_stubs.c) sets
  `os->e_o_s = 1` on the unused muxer stream.
- `Flac_ogg.Encoder.create` (xiph/flac/flac_ogg.ml) pairs consecutive write
  callbacks as (page header, page body), keeps the pages written during
  `init` as `first_pages`, attaches `Gc.finalise cleanup`, and returns
  `Obj.magic (enc, params)` as a `Flac.Encoder.t`.
- Opus: `Opus.Encoder.create ?(pre_skip = 3840)` (xiph/opus/opus.ml:163);
  the C `pack_header` writes into `static unsigned char header_packet[19]`
  (opus_stubs.c:494) then `value_of_packet` copies it.
- Runtime lock released: `vorbis_analysis*` (`caml_release_runtime_system`),
  `opus_encode_float` (same), libFLAC init/process/finish (same; its write
  callback re-acquires and registers the thread), `speex_encode*` and
  `th_encode_ycbcr_in` (`caml_enter_blocking_section`).
- Speex feed: an OCaml closure called from C (`caml_callback_exn`); it raises
  the local exception `Internal` to stop the binding's page loop, and the
  binding restores its frame counter before re-raising.

## Exceptions

| Part A                  | OCaml                                                                            |
| ----------------------- | -------------------------------------------------------------------------------- |
| usage error             | `Ogg_muxer.Invalid_usage`                                                        |
| invalid data            | `Ogg_muxer.Invalid_data` (also raised by Ogg FLAC when `first_pages` is empty)   |
| not found               | `Not_found` from `Hashtbl.find`                                                  |
| Theora size failure     | `Failure "Invalid theora width/height (should be a multiple of 16)."`            |
| Theora plane too small  | `Theora.Invalid_data` (registered as `theora_exn_inval`)                         |
| Vorbis channel mismatch | `Vorbis.Invalid_channels`                                                        |
| Vorbis setup            | the exception printed `Invalid vorbis setup request, e.g. out of range argument` |
| layout                  | `Audio_layout.Unsupported` escaping `Encoder.type_of_format`                     |

## Serialisation

`Encoder.get_factory` wraps every instance in `Encoder.protect`: one
`Mutex.t` per instance around each of its calls (encoder.ml:186). The Ogg
FLAC page list has its own `Mutex.protect` (ogg_flac_encoder.ml:41).
