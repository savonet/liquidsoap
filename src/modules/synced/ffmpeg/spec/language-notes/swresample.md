# swresample — language notes (Part B)

How the current stubs implement [../swresample.md](../swresample.md).
Files: `swresample/swresample.ml`, `swresample/swresample_stubs.c`,
`swresample/swresample_stubs.h`, `swresample/dune`.

## Build (`swresample/dune`)

- Install-alias rule for package `ffmpeg-swresample`: runs
  `../detect/check.exe swresample` with stdin from
  `../detect/swresample_available`, unless the environment variable
  `LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL` is `true`.
- Library `swresample`, public name `ffmpeg-swresample`, enabled when
  `../detect/swresample_available` reads `true`. One foreign stub,
  `swresample_stubs`, C flags from `../detect/swresample_c_flags.sexp`, link
  flags from `../detect/swresample_c_library_flags.sexp`. Libraries:
  `ffmpeg-avutil`, `ffmpeg-avcodec`. No `install_c_headers`.
- Two rules run `../gen_code/gen_code.exe "%{cc}" swresample_options h|ml
<lines of ../detect/swresample_c_flags>` to produce
  `swresample_options_stubs.h` and `swresample_options.ml`.
- A `(mode fallback)` rule targets `swresample_stubs.c` with the generated
  header as dependency and an `echo "this should not happen"` action. Its only
  effect is to make the stub depend on the generated header.

## Generated table use

`swresample_options_stubs.h` is included once, in the stub file; it defines
static tables and non-static functions `DitherType_val`,
`DitherType_val_no_raise`, `Val_DitherType` and the same for `Engine` and
`FilterType`, plus `VALUE_NOT_FOUND` (`0xFFFFFFF`). Only the three
`*_val_no_raise` functions are called (`swresample_stubs.c:588-603`). Their
result is stored in an `int64_t` and compared with `VALUE_NOT_FOUND`.

`swresample_options.ml` is a module of the library; `swresample.ml` and
`swresample.mli` `open` it for the three type names.

## Header (`swresample_stubs.h`)

Declares the C enum `vector_kind { Str, P_Str, Fa, P_Fa, Ba, P_Ba, Frm }`,
matching the OCaml constructor order, and the opaque `swr_t`. The stub reads a
kind with `Int_val`.

## Custom block

- `Swr_val(v)` is `*(swr_t **)Data_custom_val(v)`: the block holds one
  pointer to an `av_mallocz`'ed `swr_t`.
- Custom operations: identifier `"ocaml_swresample_context"`, finaliser
  `ocaml_swresample_finalize` → `swresample_free`, all others default.
- Allocated with `caml_alloc_custom(&swr_ops, sizeof(swr_t *), 0, 1)`.

`struct swr_t`: `SwrContext *context`; `struct audio_t in, out`;
`AVChannelLayout out_ch_layout`; `int out_sample_rate`;
`vector_kind in_kind, out_kind`.

`struct audio_t`: `uint8_t **data` (pointer table), `int nb_samples`
(capacity of the scratch buffer, or size of the last output allocation),
`nb_channels`, `sample_fmt`, `is_planar`, `bytes_per_samples`, `owns_data`.

`alloc_data` (`:63-78`) is the single grow function: `av_freep(&data[0])` if
set, `owns_data = 1`, `av_samples_alloc(data, NULL, nb_channels, nb_samples,
sample_fmt, 0)`.

## Dispatch

`VECTOR_OPS[]` (`:388-403`) is a table indexed by `vector_kind` with three
function pointers: `get_in(swr, value *, offset)`,
`alloc_out(swr, nb_samples, value *)`, `store_out(swr, ret, value *)`. A
`_Static_assert` checks the table has `Frm + 1` rows. `Str`, `P_Str`, `Fa`,
`P_Fa` share `alloc_out_data`.

## Roots

- `ocaml_swresample_convert`: `CAMLparam4`, `CAMLlocal1(out_vect)`. The
  helpers take `value *` pointing at these rooted slots (`&_in_vector`,
  `&out_vect`) so they see moved values after an allocation.
- Planar readers open their own `CAMLparam0` / `CAMLlocal1` frame and return
  with `CAMLreturnT(int, ...)`.
- Output allocators write the fresh value into `*out_vect` first, then fill
  it with `Store_field(*out_vect, i, <allocating call>)`
  (`:311-312`, `:340`, `:366-367`).
- `ocaml_swresample_flush` pre-fills `out_vect` with
  `caml_alloc(nb_channels, 0)`; every `alloc_out`/`store_out` overwrites or
  ignores it.
- The option array is copied into `CAMLlocalN(options, NB_OPTIONS_TYPES + 1)`
  with `NB_OPTIONS_TYPES` = 3; the terminator is the word `0`, tested with
  `options[i]` as a C truth value.
- The comment at `:245-249` states the rule behind the two output families:
  scratch kinds build the OCaml value after the lock is re-acquired; `Frm`,
  `Ba`, `P_Ba` let `swr_convert` write into memory outside the OCaml heap
  (frame buffer, `caml_ba_alloc(..., NULL, ...)` region).

## Option values

`_ofs` and `_len` are OCaml options: compared with `Val_none`, payload read
with `Int_val(Field(v, 0))`.

## Bigarrays

- Input: `Caml_ba_array_val(v)->dim[0]` and `Caml_ba_data_val(v)`. The offset
  is added to the `void *` returned by `Caml_ba_data_val` (GNU byte
  arithmetic).
- Output: `caml_ba_alloc(CAML_BA_C_LAYOUT | kind, 1, NULL, &size)` (runtime
  managed), then `Caml_ba_array_val(v)->dim[0] = …` to shrink.

## Float arrays

Lengths via `Wosize_val / Double_wosize`, reads via `Double_field`, writes via
`Store_double_field` into `caml_alloc(len * Double_wosize, Double_array_tag)`.
`filter_nan` is `s != s ? 0 : s`.

## Bytes

`caml_string_length`, `String_val` for reads, `Bytes_val` for writes into
`caml_alloc_string`. `Bytes_val` is defined as `String_val` when the runtime
lacks it.

## Errors

`Fail(...)` and `ocaml_avutil_raise_error` from `avutil_stubs.h`; both raise
and do not return. `caml_raise_out_of_memory` for the three allocation checks.

## Runtime lock

`caml_release_runtime_system` / `caml_acquire_runtime_system` around
`swr_init` (`:612-614`) and `swr_convert` (`:416-419`).

## Externals

| OCaml          | C                                                          | Notes                                          |
| -------------- | ---------------------------------------------------------- | ---------------------------------------------- |
| `version`      | `ocaml_swresample_version`                                 | `[@@noalloc]`, `Val_int(swresample_version())` |
| `Make.create`  | `ocaml_swresample_create_byte` / `ocaml_swresample_create` | 9 arguments; `CAMLparam5` + `CAMLxparam4`      |
| `Make.convert` | `ocaml_swresample_convert`                                 | labelled optionals arrive as `int option`      |
| `Make.flush`   | `ocaml_swresample_flush`                                   |                                                |

The externals are declared inside the functor body, so every instantiation
binds the same three C symbols.

`swresample_create`, `swresample_free` and `ocaml_swresample_finalize` have
external linkage but are declared in no header.
