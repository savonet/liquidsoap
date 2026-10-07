# avfilter — language notes (Part B)

How `avfilter/avfilter_stubs.c` and `avfilter/avfilter.ml` implement
[../avfilter.md](../avfilter.md).

## Build

`avfilter/dune`: library `avfilter`, public name `ffmpeg-avfilter`,
`(libraries ffmpeg-avutil)`, one foreign stub `avfilter_stubs`. C flags and
link flags are included from `../detect/avfilter_c_flags.sexp` and
`../detect/avfilter_c_library_flags.sexp`. The library is enabled when
`../detect/avfilter_available` reads `true`. An `install`-alias rule runs
`../detect/check.exe avfilter` unless
`LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL=true`.

The stub includes `avutil_stubs.h` and `polymorphic_variant_values_stubs.h`
(both from `avutil`); the `PVV_*` constants for pad media types and filter
flags come from the latter. No generated table is specific to avfilter.
`HAVE_AV_OPT_TYPE_FLAG_ARRAY` is defined by `avutil_stubs.h`
(libavutil >= 59.1.100).

## Value layouts

- **Graph handle** (`_config`): custom block, identifier
  `"ocaml_avfilter_filter_graph"`, payload one `AVFilterGraph *`, finaliser
  `avfilter_graph_free`, default compare/hash/serialise. Allocated with
  `caml_alloc_custom(ops, sizeof(AVFilterGraph *), 1, 0)` (mem 1, max 0)
  (`avfilter_stubs.c:141-168`).
- **Filter context** (`filter_ctx`): `caml_alloc(1, Abstract_tag)` holding
  the raw `AVFilterContext *`; no finaliser
  (`avfilter_stubs.c:19-26`). `'a context = filter_ctx` in the `.ml`.
- **Pad**: a 6-field tuple built in C in the field order of the OCaml record
  `pad_name, filter_name, media_type, idx, filter_ctx, _config`; fields 4
  and 5 are `Val_none` from C (`avfilter_stubs.c:37-80`). The OCaml side
  fills them with a functional record update (`avfilter.ml:239-240`).
- **Raw filter** (`_filter`, internal): 6-field tuple
  `name, description, inputs, outputs, options, flags(int)`
  (`avfilter_stubs.c:112-132`, `avfilter.ml:96-103`).
- **Attached filter**: `ocaml_avfilter_append_context` allocates a tuple one
  word longer than the 5-field `filter` record, copies the fields and stores
  the context last. `ocaml_avfilter_get_content` returns
  `Field(v, Wosize_val(v) - 1)` (`avfilter_stubs.c:345-366`). The OCaml type
  system sees an ordinary ``[`Attached] filter``.
- **Parse nodes**: OCaml passes two arrays of `(string * filter_ctx * int)`
  tuples (`avfilter.ml:320-345`).
- **`create_filter` result**: 3-tuple `(filter_ctx, input pads, output
pads)`.

## Externals

| OCaml                                                                                                                                                        | C symbol                                                                                                                                                                                                                        |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `register_all`                                                                                                                                               | `ocaml_avfilter_register_all`                                                                                                                                                                                                   |
| `get_all_filters`                                                                                                                                            | `ocaml_avfilter_get_all_filters`                                                                                                                                                                                                |
| `int_of_flag`                                                                                                                                                | `ocaml_avfilter_int_of_flag`                                                                                                                                                                                                    |
| `init`                                                                                                                                                       | `ocaml_avfilter_init`                                                                                                                                                                                                           |
| `create_filter ?args ~name filter_name graph`                                                                                                                | `ocaml_avfilter_create_filter`                                                                                                                                                                                                  |
| `append_context`                                                                                                                                             | `ocaml_avfilter_append_context`                                                                                                                                                                                                 |
| `get_context`                                                                                                                                                | `ocaml_avfilter_get_content`                                                                                                                                                                                                    |
| `link`                                                                                                                                                       | `ocaml_avfilter_link`                                                                                                                                                                                                           |
| `process_command ~flags ~cmd ~arg ctx`                                                                                                                       | `ocaml_avfilter_process_commands`                                                                                                                                                                                               |
| `parse ~inputs ~outputs desc graph`                                                                                                                          | `ocaml_avfilter_parse`                                                                                                                                                                                                          |
| `config`                                                                                                                                                     | `ocaml_avfilter_config`                                                                                                                                                                                                         |
| `write_frame graph ctx frame`                                                                                                                                | `ocaml_avfilter_write_frame`                                                                                                                                                                                                    |
| `write_eof_frame graph ctx`                                                                                                                                  | `ocaml_avfilter_write_eof_frame`                                                                                                                                                                                                |
| `get_frame graph ctx`                                                                                                                                        | `ocaml_avfilter_get_frame`                                                                                                                                                                                                      |
| `time_base`, `frame_rate`, `width`, `height`, `pixel_aspect`, `pixel_format`, `channels`, `channel_layout`, `sample_rate`, `sample_format`, `set_frame_size` | `ocaml_avfilter_buffersink_get_time_base`, `_get_frame_rate`, `_get_w`, `_get_h`, `_get_pixel_aspect`, `_get_pixel_format`, `_get_channels`, `_get_channel_layout`, `_get_sample_rate`, `_get_sample_format`, `_set_frame_size` |
| `get_array_separator`                                                                                                                                        | `ocaml_avfilter_get_array_separator`                                                                                                                                                                                            |

None is `[@@noalloc]` or unboxed. No bytecode/native split is needed (at
most four arguments).

## Root discipline

- Every primitive uses `CAMLparam`/`CAMLlocal`/`CAMLreturn`.
- `ocaml_avfilter_alloc_pads` is a C helper (not called from OCaml) with its
  own `CAMLparam0`/`CAMLlocal2`; callers pass its result straight to
  `Store_field`, which evaluates the value into a temporary before computing
  the destination address.
- `value_of_avfiltercontext(tmp, ctx)` takes its scratch value by copy and
  returns the fresh block.
- `ocaml_avfilter_get_array_separator` raises on every path except one
  `CAMLreturn`; in the no-array build it references `caml__frame` to silence
  the unused-variable warning (`avfilter_stubs.c:615-619`).

## Runtime lock

`caml_release_runtime_system` / `caml_acquire_runtime_system` bracket the
seven FFmpeg calls listed in Part A section 6. OCaml strings are copied
(`av_strndup` / `av_malloc`+`memcpy` / `av_strdup`) before the release.

Three stubs read an OCaml block _after_ releasing the lock, because the
accessor macro is written inside the argument list of the C call:
`Filter_graph_val(_graph)` in `ocaml_avfilter_config`
(`avfilter_stubs.c:510-511`), `Frame_val(_frame)` in
`ocaml_avfilter_write_frame` (`:525-526`), and `Int_val(_srcpad)`,
`Int_val(_dstpad)`, `Int_val(_flags)` in `ocaml_avfilter_link` (`:375`) and
`ocaml_avfilter_process_commands` (`:274-275`). See findings.

## Keeping the graph alive

`write_frame`, `write_eof_frame` and `get_frame` take the graph handle as an
unused first argument and register it with `CAMLparam`, so it is a root for
the duration of the call (`avfilter.ml:349-364`). The closures built by
`launch` capture `graph.c`. Attached pads store `Some graph.c`. The other
stubs that take a `filter_ctx` (sink accessors, `link`, `process_command`)
receive no graph.

## Module initialisation

`let () = register_all ()` then the top-level binding that calls
`get_all_filters ()` and folds it into `filters` and the four dedicated
filters (`avfilter.ml:105-185`). `int_of_flag` is an external called five
times per filter.

## Misc

- `ocaml_avfilter_process_commands` uses a stack buffer `char buf[4096] =
{0}` and `caml_copy_string(buf)`.
- `append_avfilter_in_out` (`avfilter_stubs.c:225-249`) appends at the tail
  of the list and calls `caml_raise_out_of_memory` itself after freeing the
  list it was given.
- `` `Fast `` is mapped to the literal `2` in the `.ml` with a comment that
  it is `AVFILTER_CMD_FLAG_FAST` (`avfilter.ml:290-292`).
