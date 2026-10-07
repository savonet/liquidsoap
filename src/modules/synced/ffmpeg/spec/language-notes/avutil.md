# avutil — language notes (Part B)

How the current stubs implement [avutil.md](../avutil.md). Files:
`avutil/avutil.ml`, `avutil/avutil_stubs.c`, `avutil/avutil_stubs.h`,
`avutil/dune`.

## Build

- One dune library `avutil` (public name `ffmpeg-avutil`), enabled when
  `../detect/avutil_available` reads `true`. Depends on `threads`.
- One foreign stub, `avutil_stubs.c`. C flags from
  `../detect/avutil_c_flags.sexp`, link flags from
  `../detect/avutil_c_library_flags.sexp`. On the inspected build these
  were `-Wall -Wextra -Werror=unused-variable -Werror=unused-parameter` and
  `-lavcodec -lavutil`.
- `install_c_headers`: `avutil_stubs`, `polymorphic_variant_values_stubs`,
  `media_types_stubs`. The other generated `*_stubs.h` are not installed.
- An `install`-alias rule runs `../detect/check.exe avutil` unless
  `LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL=true`.
- A `fallback` rule for `avutil_stubs.c` exists only to declare the
  generated headers as dependencies of the stub.
- Generated per table: `X_stubs.h` and `X.ml`, for `hw_device_type`,
  `media_types`, `color_space`, `color_range`, `color_primaries`,
  `color_trc`, `chroma_location`, `pixel_format`, `pixel_format_flag`,
  `sample_format`, `channel_layout`, `subtitle_type`, `subtitle_flag`; and
  `polymorphic_variant_values_stubs.h` alone (`PVV_*` hash constants).

## Generated conversions

- Each generated header defines, as non-static functions, `X_val`,
  `X_val_no_raise` and `Val_X` over a `static const int64_t TAB[][2]` of
  `{variant hash, C constant}` and a `TAB_LEN` macro. They are compiled
  once, by inclusion in `avutil_stubs.c`. Sibling stubs see only the
  prototypes that `avutil_stubs.h` declares (`avutil_stubs.h:90-141`); the
  `_no_raise` variants and the subtitle and channel-layout conversions have
  no prototype there.
- `avutil_stubs.c` scans `AV_PIX_FMT_FLAG_T_TAB` and
  `AV_SUBTITLE_FLAG_T_TAB` directly for bit-set to list conversion
  (`avutil_stubs.c:753-770`, `1335-1339`).
- Polymorphic variants without argument are compared as immediates in C
  `switch` statements on `PVV_*` (`avutil_stubs.c:152`, `257`, `1586`,
  `2025`). Variants with an argument fall to `default` and are told apart
  by `Field(v, 0)`.

## Header (`avutil_stubs.h`)

| Name                                                                                                                                                 | Kind                       | Notes                                                                                                                                                          |
| ---------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Val_none`, `Some_val`                                                                                                                               | macros                     | `Val_int(0)`, `Field(v, 0)`                                                                                                                                    |
| `HAVE_AV_OPT_TYPE_FLAG_ARRAY`                                                                                                                        | macro                      | defined when `LIBAVUTIL_VERSION_INT >= AV_VERSION_INT(59,1,100)`; the enum member cannot be tested with `#ifdef`                                               |
| `ERROR_MSG_SIZE` (256), `EXN_ERROR`                                                                                                                  | macros                     |                                                                                                                                                                |
| `Fail(fmt, ...)`                                                                                                                                     | macro                      | `snprintf` into `ocaml_av_exn_msg`, then `caml_callback` on the named value `ffmpeg_exn_failure` with a fresh string. Not a `do { } while (0)`; a braced block |
| `ocaml_avutil_raise_error(int)`                                                                                                                      | function                   | `caml_raise_with_arg` on the named value `ffmpeg_exn_error`                                                                                                    |
| `ocaml_av_exn_msg`                                                                                                                                   | `char[ERROR_MSG_SIZE + 1]` | process-wide                                                                                                                                                   |
| `ocaml_avutil_dict_of_options`, `ocaml_avutil_unused_options`                                                                                        | functions                  | see below                                                                                                                                                      |
| `List_init`, `List_add(list, cons, val)`                                                                                                             | macros                     | `List_add` allocates the cons cell first, then stores `val`; `val` must be rooted or immediate                                                                 |
| `ocaml_ffmpeg_register_thread`                                                                                                                       | function                   |                                                                                                                                                                |
| `rational_of_value(v)`                                                                                                                               | macro                      | compound literal from `Int_val` of fields 0, 1                                                                                                                 |
| `value_of_rational(const AVRational *, value *)`                                                                                                     | function                   | `caml_alloc_tuple(2)` into the pointed value                                                                                                                   |
| `second_fractions_of_time_format(value)`                                                                                                             | function                   |                                                                                                                                                                |
| `AVChannelLayout_val`, `value_of_channel_layout(value *, const AVChannelLayout *)`                                                                   | macro, function            |                                                                                                                                                                |
| `Sample_format_val`, `AVSampleFormat_of_Sample_format`                                                                                               | macro, prototype           | not used; the prototype has no definition                                                                                                                      |
| `SampleFormat_val`, `Val_SampleFormat`, `bigarray_kind_of_AVSampleFormat`                                                                            | prototypes                 |                                                                                                                                                                |
| `ColorSpace_val`/`Val_ColorSpace`, and the same pair for `ColorRange`, `ColorPrimaries`, `ColorTrc`, `ChromaLocation`, `PixelFormat`, `HwDeviceType` | prototypes                 | generated                                                                                                                                                      |
| `BufferRef_val`                                                                                                                                      | macro                      | `AVBufferRef **` in a custom block                                                                                                                             |
| `Frame_val`, `value_of_frame(value *, AVFrame *)`                                                                                                    | macro, function            |                                                                                                                                                                |
| `Subtitle_val`, `value_of_subtitle(value *, AVSubtitle *)`                                                                                           | macro, function            |                                                                                                                                                                |
| `AvPixFmtDescriptor_val`, `value_of_avpixfmtdescriptor(value, ptr)`                                                                                  | macro, static inline       | `Abstract_tag` block of 1 word; this one takes and returns `value`                                                                                             |
| `AvClass_val`/`value_of_avclass`, `AvOptions_val`/`value_of_avoptions`, `AvObj_val`/`value_of_avobj`                                                 | macro, static inline       | `Abstract_tag` blocks of 1 word; take `value *`, write through it and also return it                                                                           |

The header includes `libavcodec/avcodec.h`.

## Custom blocks

All hold one pointer in the data area. All use the default compare, hash,
serialize and deserialize operations.

| Identifier                      | Payload             | Finaliser                             | Allocation                                                                                   |
| ------------------------------- | ------------------- | ------------------------------------- | -------------------------------------------------------------------------------------------- |
| `ocaml_avframe`                 | `AVFrame *`         | `av_frame_free`                       | `caml_alloc_custom_mem(ops, sizeof(AVFrame *), sum of buf sizes)` (`avutil_stubs.c:873-886`) |
| `ocaml_avchannel_layout`        | `AVChannelLayout *` | `av_channel_layout_uninit`, `av_free` | `caml_alloc_custom(ops, size, 0, 1)`                                                         |
| `ocaml_avchannel_layout_opaque` | `void **`           | `av_free`                             | `caml_alloc_custom(ops, size, 0, 1)`                                                         |
| `ocaml_avsubtitle`              | `AVSubtitle *`      | `avsubtitle_free`, `av_free`          | `caml_alloc_custom(ops, size, 0, 1)`                                                         |
| `ocaml_avutil_buffer_ref`       | `AVBufferRef *`     | `av_buffer_unref`                     | `caml_alloc_custom(ops, size, 0, 1)`                                                         |

## Abstract blocks and hidden fields

- `Pixel_format.descriptor` is allocated with 8 fields; field 7 is the
  abstract block holding the `AVPixFmtDescriptor *`. The OCaml record
  declares 7 fields and is `private` in the `.mli`
  (`avutil.ml:277-288`, `avutil_stubs.c:746`, `796`, `803`).
- The option cursor is `Some ((child_state, option), class)`, three
  abstract blocks (`avutil_stubs.c:1717-1732`), matching the OCaml records
  `_cursor = { _opt_cursor; _class_cursor }` with `_opt_cursor` itself a
  pair.
- `Options.obj` is an OCaml pair `(abstract C pointer, owner)`; the getter
  uses `Obj.magic` to unpack it and `ignore o` after the call to keep the
  owner live (`avutil.ml:773-777`).
- The raw option returned by the iterator is a 6-field tuple matching the
  OCaml record `_opt` (`avutil.ml:586-593`).

## Named values

| Name                          | Registered as                                                      | Used by                      |
| ----------------------------- | ------------------------------------------------------------------ | ---------------------------- |
| `ffmpeg_exn_error`            | exception ``Error `Unknown`` (`avutil.ml:112`)                     | `ocaml_avutil_raise_error`   |
| `ffmpeg_exn_failure`          | closure ``fun s -> raise (Error (`Failure s))`` (`avutil.ml:113`)  | `Fail`                       |
| `av_opt_iter_not_implemented` | exception `Av_opt_iter_not_implemented None` (`avutil.ml:597-599`) | `raise_unimplemented_option` |

`caml_named_value` is looked up on every raise; nothing caches the result.

## Root discipline

- Stubs use `CAMLparam`/`CAMLlocal`/`CAMLreturn` throughout. `version` and
  `version_int` are `[@@noalloc]` and use no frame.
- `ocaml_avutil_dict_of_options` has no frame on purpose: it allocates
  nothing on the OCaml heap between reading the bytes and `av_dict_set`
  copying them (`avutil_stubs.c:104-118`).
- `ocaml_avutil_unused_options` returns an unrooted value; the caller
  assigns it straight into a `CAMLlocal`.
- `value_of_frame`, `value_of_subtitle`, `value_of_channel_layout`,
  `value_of_rational` write through a `value *` that the caller has rooted.
- `Store_field(block, i, alloc())` is used widely. The runtime's
  `Store_field` evaluates the value into a temporary before computing the
  field address, so the pattern is sound; the comment at
  `avutil_stubs.c:130-132` claims otherwise.
- The colour `*_name` stubs declare `CAMLparam0()` and leave their argument
  unrooted; the argument is an immediate and is consumed before the
  allocation (`avutil_stubs.c:639-643` and siblings).
- No generational global roots are used. No OCaml closure is stored on the
  C side.

## Runtime lock

`caml_release_runtime_system` / `caml_acquire_runtime_system` pairs at
`avutil_stubs.c:326-332` (log wait), `1176-1179` (`av_samples_copy`),
`2096-2099` (`av_hwdevice_ctx_create`), `2140-2142`
(`av_hwframe_ctx_init`).

## Thread registration

`avutil_stubs.c:217-236`. A `pthread_key_t` created under `pthread_once`
with destructor `caml_c_thread_unregister`. `ocaml_ffmpeg_register_thread`
calls `caml_c_thread_register()` and, when it returns non-zero and
`pthread_getspecific` is `NULL`, sets the slot to the address of a static
`int`, which arms the destructor for that thread.

## Logging

`avutil_stubs.c:279-393`, `avutil.ml:133-224`.

- `log_msg_t { char msg[1024]; next; }`, `av_malloc`/`av_free`.
- `_Atomic(log_msg_t *) log_head`: Treiber stack push with
  `atomic_compare_exchange_weak`; take-all with `atomic_exchange`.
- `ocaml_ffmpeg_get_pending_logs` reverses the taken chain in place, then
  conses while walking the reversed chain.
- `log_mutex`, `log_condition`, `log_wakeup`: the push signals only on the
  empty-to-non-empty transition, under the mutex; the waiter checks
  `log_head == NULL && !log_wakeup` under the mutex.
- `static _Atomic int print_prefix` inside the callback, passed by copy to
  `av_log_format_line2` and stored back.
- OCaml: `log_m : Mutex.t`, `log_thread : bool ref`,
  `log_thread_should_stop : bool ref`,
  `log_thread_processor : (string -> unit) Atomic.t`, `Thread.create`.
  `mutexify` is `[@inline never]`.
- Primitive names mix two prefixes: `ocaml_avutil_setup_log_callback`,
  `ocaml_avutil_clear_log_callback`, `ocaml_ffmpeg_wait_for_logs`,
  `ocaml_ffmpeg_signal_logs`, `ocaml_ffmpeg_get_pending_logs`.

## Option iteration

- `av_opt_next(&class, option)`: the address of a local `const AVClass *`
  is passed as the object (`avutil_stubs.c:1808`, `1817`).
- `type_of_av_opt_type` returns `-1` for an unmapped type; `_type` is a
  `value` compared against `-1`.
- `AV_OPT_TYPE_CONST` maps to `PVV_Constant` in `type_of_av_opt_type` and
  is then rejected in the `switch` (`avutil_stubs.c:1852-1854`).
- `ocaml_avutil_avopt_default_int64/_double/_string` read `default_val`
  from an abstract `AVOption *`; their only caller is the constant-merging
  code in `avutil.ml:604-688`.
- `ocaml_avutil_av_d2q` is an unexported primitive with no `CAMLprim`
  marker, used only by that same code.

## Other

- `ocaml_avutil_get_opt` receives `?search_children` as an OCaml option
  and applies `Bool_val` to it (`avutil.ml:770`, `avutil_stubs.c:1583`).
- `av_assert2` at `avutil_stubs.c:1168-1170` names `AVFrame.channel_layout`
  and `AVFrame.channels`; the macro discards its argument unless
  `ASSERT_LEVEL > 1`.
- `ocaml_avutil_audio_frame_copy_samples` has five arguments and no
  bytecode wrapper is needed; `ocaml_avutil_create_frame_context` likewise
  has five.
- `Channel_layout.get_native_id` is followed in the `.mli` by a floating
  `[@@@caml.alert deprecated ...]` attribute (`avutil.mli:200`).
