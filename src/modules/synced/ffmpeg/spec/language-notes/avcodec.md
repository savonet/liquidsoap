# avcodec — language notes (Part B)

How `avcodec/avcodec_stubs.c`, `avcodec_stubs.h` and `avcodec.ml` implement
the contract in [../avcodec.md](../avcodec.md).

## Build (`avcodec/dune`)

- Library `avcodec`, public name `ffmpeg-avcodec`, depends on
  `ffmpeg-avutil`. Enabled when `../detect/avcodec_available` reads `true`.
- One C stub file, `avcodec_stubs`; flags from
  `../detect/avcodec_c_flags.sexp`, link flags from
  `../detect/avcodec_c_library_flags.sexp`.
- `(install_c_headers avcodec_stubs)` installs `avcodec_stubs.h`.
- An `install`-alias rule runs `../detect/check.exe avcodec` on
  `../detect/avcodec_available` unless
  `LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL=true`.
- Eight rules run `../gen_code/gen_code.exe "%{cc}" <table> h|ml <cflags…>`
  for `hw_config_method`, `codec_capabilities`, `codec_properties`,
  `codec_id`, producing `<table>_stubs.h` and `<table>.ml`. Each `.ml` rule
  depends on its `.h`.
- A `(mode fallback)` rule targets `avcodec_stubs.c` with the four generated
  headers as deps and an action that echoes "this should not happen": it
  exists only to make the headers dependencies of the stub file.
- `avcodec_stubs.c` also includes `media_types_stubs.h`, generated in the
  `avutil` directory.

## Generated headers define functions

Each generated `*_stubs.h` contains a `static const int64_t TAB[][2]` of
`{OCaml value, C value}` pairs, a `*_TAB_LEN` macro, and **non-static**
function definitions `X_val`, `X_val_no_raise`, `Val_X`. They can be included
by one translation unit only. `avcodec_stubs.c` is that unit;
`avcodec_stubs.h` redeclares the eight codec-id converters
(`AudioCodecID_val`/`Val_AudioCodecID`, `Video…`, `Subtitle…`, `Unknown…`)
for sibling stubs. `CodecID_val`/`Val_CodecID`(all ids) are not declared in
the header.`VALUE_NOT_FOUND`is`0xFFFFFFF`.

Bit-flag tables (`AV_CODEC_CAP_T_TAB`, `AV_CODEC_PROP_T_TAB`,
`AV_CODEC_HW_CONFIG_METHOD_T_TAB`) are walked directly with `&` on column 1;
the converters generated for them are unused here.

`ocaml_avcodec_get_next_codec` (`avcodec_stubs.c:1416`) also walks the three
codec-id tables directly, comparing `codec->id` with column 1 and keeping
column 0 in a local declared `enum AVCodecID`.

## Value layouts

| OCaml type           | Block                                                                                                          | Accessor                       | Finaliser                                     |
| -------------------- | -------------------------------------------------------------------------------------------------------------- | ------------------------------ | --------------------------------------------- |
| `codec`              | `Abstract_tag`, 1 word, `const AVCodec *`                                                                      | `AvCodec_val` (header)         | none                                          |
| `params`             | custom `"ocaml_avcodec_parameters"`, `AVCodecParameters *`                                                     | `CodecParameters_val` (header) | `avcodec_parameters_free`                     |
| `Packet.t`           | custom `"ocaml_packet"`, `AVPacket *`                                                                          | `Packet_val` (header)          | `av_packet_free`                              |
| `decoder`, `encoder` | custom `"ocaml_codec_context"`, pointer to an `av_mallocz`'d `codec_context_t {codec; codec_context; flushed}` | `CodecContext_val`             | `avcodec_free_context` if non-null, `av_free` |
| `BitstreamFilter.t`  | custom `"bsf_filter_parameters"`, `AVBSFContext *`                                                             | `BsfFilter_val`                | `av_bsf_free`                                 |
| codec cursor         | `Abstract_tag`, `void *` (`value_of_avobj` from avutil)                                                        | `AvObj_val`                    | none                                          |
| bsf cursor           | `Abstract_tag`, `void *`                                                                                       | `BsfCursor_val`                | none                                          |

All custom operations use the default compare, hash, serialize and
deserialize. Packets are allocated with
`caml_alloc_custom_mem(ops, size, buf ? buf->size : 0)`; the other custom
blocks use `caml_alloc_custom(ops, size, 0, 1)`.

## Stub conventions

- `CAML_NAME_SPACE` is defined. Every stub that allocates uses
  `CAMLparam`/`CAMLlocal`/`CAMLreturn`.
- Exceptions to the frame discipline, all deliberate or harmless:
  `ocaml_avcodec_init`, `ocaml_avcodec_flag_qscale`, `ocaml_avcodec_version`
  (no allocation; `init` and `version` are `[@@noalloc]` externals; `init`
  lacks `CAMLprim`); `ocaml_avcodec_flush_decoder` /
  `ocaml_avcodec_flush_encoder` (tail-call the send stubs);
  `ocaml_avcodec_<media>_descriptor` (converts its argument to an enum and
  calls the shared builder).
- `ocaml_avcodec_descriptor` is declared `CAMLprim` but takes an
  `enum AVCodecID`, not a `value`; it is a C helper called by
  `ocaml_avcodec_params_descriptor` and the per-media descriptor stubs.
- `ocaml_avcodec_create_video_encoder` has five arguments: `CAMLparam4` +
  `CAMLxparam1`. No stub needs a bytecode/native pair.
- Flush is expressed by calling the send stub with the C integer `0` in
  place of a `value`: `ocaml_avcodec_send_packet(_ctx, 0)`
  (`avcodec_stubs.c:733`) and `ocaml_avcodec_send_frame(_ctx, 0)` (`:827`).
  The send stubs test `_packet ? … : NULL` / `_frame ? … : NULL` after
  registering the argument with `CAMLparam2`.
- Options: `ocaml_avutil_dict_of_options(_opts, &dict)` first (allocates
  nothing on the OCaml heap), then `ocaml_avutil_unused_options(&dict)`
  assigned straight into a `CAMLlocal`.
- Lists are built with avutil's `List_init` / `List_add` (cons at head); the
  OCaml wrappers `List.rev` them. `ocaml_avcodec_hw_methods` builds its two
  nested lists by hand and returns the record tuple directly as the OCaml
  record `hw_config` (field order `pixel_format; methods; device_type`).
- Options and `Some` are built as 1-field tuples; `None` is `Val_none`
  (avutil header). Polymorphic variants with an argument are 2-field tuples
  `(hash, payload)`; hashes come from avutil's
  `polymorphic_variant_values_stubs.h` (`PVV_Keyframe`, `PVV_Replaygain`, …).
- Errors: `ocaml_avutil_raise_error(ret)` for FFmpeg codes;
  `Fail(fmt, …)` (avutil macro) formats into a shared static buffer and calls
  the closure registered as `"ffmpeg_exn_failure"`, which raises; control
  does not come back, and the macro itself contains no `return`.
  `caml_raise_out_of_memory`, `caml_raise_not_found`, `caml_failwith` are
  used directly in the places listed in Part A §5.
- Ownership on failure is handled by ordering: the decoder/encoder custom
  block is allocated and given the zeroed record _before_ any fallible FFmpeg
  call, so a later raise leaves cleanup to the finaliser. Local C resources
  (option dictionary, temporary frame/packet) are freed by hand before each
  raise.

## Macro-generated stubs

`CODEC_MEDIA_STUBS(ml, Ml, MEDIA)` (`avcodec_stubs.c:1011`) is instantiated
for `audio`, `video`, `subtitle` and defines
`ocaml_avcodec_get_<ml>_codec_id`,
`ocaml_avcodec_find_<ml>_{encoder,decoder}[_by_name]`,
`ocaml_avcodec_<ml>_descriptor`.

`CODEC_ID_NAME_STUBS(ml, Ml)` (`:1049`) is instantiated for those three plus
`unknown` and defines `ocaml_avcodec_get_<ml>_codec_id_name`,
`ocaml_avcodec_parameters_get_<ml>_codec_id`.

These symbols do not appear literally in the C source.

## Header inline code

`ocaml_avcodec_open_codec` (`avcodec_stubs.h:17`) and `value_of_avcodec`
(`:35`) are `static inline` in the installed header, so each including stub
library gets its own copy. The header includes `caml/threads.h` for the
runtime-lock calls.

## OCaml side

- `external`s shadowed by same-named wrappers: `version`, `flag_qscale`,
  `capabilities` (array to list), `Packet.get_flags` (int to list),
  `Packet.add_side_data` / `side_data` (internal type `_side_data` carrying
  the NUL-joined string), the supported-list getters (`List.rev`),
  `create_encoder` (option handling), `BitstreamFilter.init`.
- `create_decoder` is one external typed
  `?params:'a params -> 'b -> 'a decoder`, re-exported with narrower types by
  `Audio` and `Video` through the `.mli`.
- Descriptors cross the boundary as a 6-tuple with arrays; `mk_descriptor`
  turns it into the record with lists.
- `codecs_of` uses `Obj.magic` to assign the phantom parameters.
- `hw_config`, `profile` and `Packet.replaygain` are filled directly by the
  stubs as tuples matching the record field order.
- `BitstreamFilter.cursor` is an abstract type not exported by the `.mli`.
