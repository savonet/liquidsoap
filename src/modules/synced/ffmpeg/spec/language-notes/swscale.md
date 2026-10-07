# swscale — language notes (Part B)

How the current stubs implement [../swscale.md](../swscale.md).
Files: `swscale/swscale.ml`, `swscale/swscale_stubs.c`, `swscale/dune`.

## Build (`swscale/dune`)

- Install-alias rule for package `ffmpeg-swscale`: runs
  `../detect/check.exe swscale` with stdin from `../detect/swscale_available`,
  unless `LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL` is `true`.
- Library `swscale`, public name `ffmpeg-swscale`, enabled when
  `../detect/swscale_available` reads `true`. One foreign stub
  `swscale_stubs`, C flags from `../detect/swscale_c_flags.sexp`, link flags
  from `../detect/swscale_c_library_flags.sexp`. Libraries: `ffmpeg-avutil`.
- No generated file, no header.

## Custom blocks

Two custom operation records share the identifier `"ocaml_swscale_context"`
(`swscale_stubs.c:78-82`, `:527-531`):

| OCaml type    | Accessor      | Payload                       | Finaliser                                 |
| ------------- | ------------- | ----------------------------- | ----------------------------------------- |
| `t`           | `Context_val` | `struct SwsContext *`         | `finalize_context` → `sws_freeContext`    |
| `('i,'o) ctx` | `Sws_val`     | `sws_t *` (from `av_mallocz`) | `ocaml_swscale_finalize` → `swscale_free` |

Both are allocated with `caml_alloc_custom(ops, sizeof(pointer), 0, 1)`.

`struct sws_t`: `context`, `in_frame`, `out_frame`, `struct video_t in, out`,
and three function pointers `get_in_pixels(sws, value *)`,
`alloc_out(sws, value *out, value *tmp)`, `copy_out(sws, value *)` (null
unless the output kind is `Str`).

`struct video_t`: `width`, `height`, `pixel_format`, `nb_planes`,
`uint8_t *slice_tab[4]`, `int stride_tab[4]`, `size_t plane_sizes[4]`,
`int sizes_tab[4]` (scratch capacities), `uint8_t **slice` and `int *stride`
(the "current" tables: `slice_tab`/`stride_tab`, or a frame's
`data`/`linesize`), `owns_data`.

`swscale_free` (`:500-523`) bounds its frees by the table size, 4; the comment
there explains that walking to a null would run into `stride_tab`.

## Kinds and flags

- `vector_kind` is a C enum local to the stub file
  (`PackedBa, Ba, Frm, Str`), read with `Int_val`. The reader and allocator
  are chosen by an `if` chain whose final `else` is `Ba`.
- `Flag_val` indexes the static array `FLAGS[]` with `Int_val`.

## Roots

- `ocaml_swscale_convert`: `CAMLparam2`, `CAMLlocal2(out_vect, tmp)`. Helpers
  receive `&_in_vector`, `&out_vect`, `&tmp`.
- Output allocators build each pair in `*tmp` and store with
  `Store_field(*tmp, 0, <allocating call>)` (`:332`, `:370-372`);
  `alloc_out_packed_ba` does `Store_field(*out_vect, 0, caml_alloc_tuple(..))`
  (`:388-389`) and then allocates each bigarray into `*tmp` first.
- Readers that allocate nothing still open `CAMLparam0` frames.
- `ocaml_swscale_scale`: `CAMLparam5` + `CAMLxparam1`, `CAMLlocal1(v)`.

## Bigarrays

Input planes via `Caml_ba_data_val`. Output planes via
`caml_ba_alloc(CAML_BA_C_LAYOUT | CAML_BA_UINT8, 1, NULL, &size)`. In `scale`
the destination offset is added to the `void *` data pointer.

## Wrapper frames

`wrap_planes` (`:415-452`) fills an `AVFrame` with borrowed planes and
`av_buffer_create(..., free_nothing, NULL, 0)` buffers; `scale` (`:454-467`)
always `av_frame_unref`s both. The comment at `:411-414` gives the reason.

## Runtime lock

`caml_release_runtime_system` / `caml_acquire_runtime_system` around
`get_context` (`:130-132`, `:577-581`), `sws_scale` (`:180-183`) and the
static `scale` (`:484-486`).

## Errors

`Fail(...)` and `ocaml_avutil_raise_error` from `avutil_stubs.h`;
`caml_raise_out_of_memory`.

## Externals

| OCaml           | C                                                              | Notes                                                                                            |
| --------------- | -------------------------------------------------------------- | ------------------------------------------------------------------------------------------------ |
| `version`       | `ocaml_swscale_version`                                        | declared `[@@noalloc]`; the stub uses `CAMLparam0`/`CAMLreturn`                                  |
| `configuration` | `ocaml_swscale_configuration`                                  | `caml_copy_string`                                                                               |
| `license`       | `ocaml_swscale_license`                                        | `caml_copy_string`                                                                               |
| `create`        | `ocaml_swscale_get_context_byte` / `ocaml_swscale_get_context` | 7 arguments                                                                                      |
| `scale`         | `ocaml_swscale_scale_byte` / `ocaml_swscale_scale`             | 6 arguments                                                                                      |
| `Make.create`   | `ocaml_swscale_create_byte` / `ocaml_swscale_create`           | 10 arguments: threads, flags, in kind, in w, in h, in format, out kind, out w, out h, out format |
| `Make.convert`  | `ocaml_swscale_convert`                                        |                                                                                                  |

The functor's externals are declared in its body; every instantiation binds
the same C symbols. `swscale_free` has external linkage and no header.

A commented-out `SwsFilter` custom block remains at `:44-62`.
