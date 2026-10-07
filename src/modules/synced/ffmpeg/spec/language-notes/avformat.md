# `av` — language notes (how the current stubs do it)

Companion to [../avformat.md](../avformat.md). Files: `av/av.ml`, `av/av.mli`,
`av/av_stubs.c`, `av/av_stubs.h`, `av/dune`.

## Build

`av/dune`: library `av`, public name `ffmpeg-av`, enabled when
`../detect/av_available` reads `true`. One foreign stub `av_stubs`, C flags
and link flags included from `../detect/av_c_flags.sexp` and
`../detect/av_c_library_flags.sexp`. `(install_c_headers av_stubs)`.
Libraries: `ffmpeg-avutil ffmpeg-avcodec unix`. A rule on the `install` alias
runs `../detect/check.exe av` unless
`LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL=true`.

The stub defines `CAML_NAME_SPACE` and falls back to `String_val` when
`Bytes_val` is missing.

## Block layouts

| OCaml type                                | Representation                                                                                                                                | Accessor                                                                     |
| ----------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------- |
| `_ container`                             | custom block `"ocaml_av_context"`, payload one `av_t *`; `caml_alloc_custom(ops, sizeof(av_t *), 0, 1)`; finaliser `av_free`s the struct only | `Av_base_val` (raw), `Av_val` (raises when `closed`) — `av_stubs.c:81-87`    |
| internal `avio`                           | custom block `"ocaml_avio_context"`, payload one `avio_t *`; finaliser frees `avio_context->buffer`, the context, the struct                  | `Avio_val` — `av_stubs.c:500-514`                                            |
| `(input, _) format`, `(output, _) format` | `caml_alloc(1, Abstract_tag)` holding the pointer                                                                                             | `InputFormat_val`, `OutputFormat_val` — `av_stubs.h:18,26`                   |
| `(_, _, _) stream`                        | plain OCaml record `{ container; index }`                                                                                                     | `Field(v, 0)`, `StreamIndex_val(v) = Int_val(Field(v, 1))` — `av_stubs.c:91` |
| `uninitialized_stream_copy`               | OCaml tuple `output container * int`                                                                                                          | OCaml only                                                                   |
| `Options.obj` from `input_obj`            | `Obj.magic (abstract_obj, container)` — `av.ml:163`                                                                                           | —                                                                            |

All other custom-operation slots are the defaults (no compare, hash,
serialise).

## Two-stage release

- Container: `Gc.finalise ocaml_av_cleanup_av` (an OCaml-level finaliser, so
  it may release the runtime lock and touch roots) runs `close_av`; the
  custom finaliser `finalize_av` only frees the struct. `av.ml:93,137,314,
329,345`; `av_stubs.c:112-166,2551`.
- I/O object: `Gc.finalise caml_av_io_close` removes the four roots; the
  custom finaliser `finalize_avio` frees C memory. `av.ml:114-117`;
  `av_stubs.c:502-508,595-611`.

## Roots

Generational global roots stored in C structs, with a zero word
(`(value)NULL`, from `av_mallocz`) meaning "not registered":

- `av_t.interrupt_cb`, `av_t.control_message_callback`, `av_t.avio` —
  registered at `av_stubs.c:718,1650,336,963,1814`, removed in `close_av`
  (`:154-161`). `control_message_callback` uses
  `caml_modify_generational_global_root` on replacement (`:338`).
- `avio_t.buffer`, `read_cb`, `write_cb`, `seek_cb` — registered in
  `ocaml_av_create_io` (`:535-567`), removed in `caml_av_io_close`.

Stubs use `CAMLparam`/`CAMLlocal`. Stream stubs copy `Field(_stream, 0)` into
a `CAMLlocal` before `Av_val`. `ocaml_av_open_output` does not register
`_format` (`:1743`); it is read before any allocation. Results of
`ocaml_avutil_unused_options` are assigned straight into a `CAMLlocal`
(contract of that helper).

Static helpers that allocate take `value *` out-parameters pointing at
caller roots (`value_of_inputFormat`, `take_decoded_frame`, the
`CLONE_PACKET` and `STORE_DECODED_CONTENT` macros at `:1299-1319`).

## Runtime lock

`caml_release_runtime_system` / `caml_acquire_runtime_system` pairs around
the calls listed in Part A §6. `write_frame` (`:2232`) runs almost entirely
unlocked and re-acquires before every raise and around `on_keyframe`
(`:2327-2329`). `close_av` releases for the C teardown and re-acquires before
removing roots. `ocaml_avcodec_open_codec` (from `avcodec_stubs.h`) releases
internally, so its callers hold the lock.

C-to-OCaml entry points (`:378,410,437,475`) call
`ocaml_ffmpeg_register_thread()` then `caml_acquire_runtime_system()`, use
`caml_callback*_exn`, and funnel exceptions through `callback_raised`
(`:364`), which formats with `caml_format_exception`, logs with `av_log` and
frees with `caml_stat_free`.

## Raising

- `ocaml_avutil_raise_error(err)` — `Avutil.Error` with the mapped variant.
- `Fail(fmt, ...)` — formats into avutil's shared static buffer and calls
  the registered OCaml closure `"ffmpeg_exn_failure"`, which raises
  ``Avutil.Error (`Failure msg)``. It is a `caml_callback`, so it allocates
  and needs the lock.
- `caml_raise_not_found`, `caml_raise_out_of_memory`, `caml_failwith`.

## Externals

- More than five arguments: `open_input` (`ocaml_av_open_input_bytecode` /
  `ocaml_av_open_input`, `CAMLparam5` + `CAMLxparam2`) and `seek`
  (`ocaml_av_seek_bytecode` / `ocaml_av_seek_native`).
- `[@@noalloc]`: `ocaml_av_init`, `ocaml_avformat_version`.
- Optional arguments reach the stubs as `option` values, tested against
  `Val_none`, unwrapped with `Some_val`. `ocaml_av_new_subtitle_stream`
  tests `Is_block` instead (`:2119`).
- The OCaml `external` for `find_input_format` raises `Not_found`; the
  wrapper turns it into `None` (`av.ml:28`).
- `media_type` (internal variant `MT_audio | MT_video | MT_data |
MT_subtitle`) and `seek_flag` are constant constructors used as indexes
  into C arrays `MEDIA_TYPES` (`:95`) and `seek_flags` (`:1449`).
- `Unix.seek_command` crosses as an `int` 0/1/2; `seek_of_int` converts
  (`av.ml:103`).
- `read_input` results are polymorphic variants with one argument: a
  2-field block `[hash; payload]`, the hash coming from the generated
  `PVV_*` constants in `polymorphic_variant_values_stubs.h` (included via
  `avutil_stubs.h`); the payload is the `(index, content)` tuple.
- The packet selection array holds `(index, media_type)` tuples; the stub
  reads field 0 only (`:1377`).

## Header

`av_stubs.h` declares `ocaml_av_get_format_context(value *)`, the
`avioformat_const` shim, `InputFormat_val` / `value_of_inputFormat`,
`OutputFormat_val` / `value_of_outputFormat`, and
`ocaml_av_get_control_message_callback` /
`ocaml_av_set_control_message_callback`. The setter calls `Av_val` and
registers a root, so it needs the runtime lock.

## Internal constants

`BUFLEN 32768` (`:350`); subtitle packet `4096` (`:2384`); `char attr[32]`
(`:2598`). `ff_nal_unit_extract_rbsp` (`:2561`) is copied from
libavformat's `avc.h`; `ocaml_av_codec_attr` follows libavformat's
`hlsenc.c`.
