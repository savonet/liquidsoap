# swscale — findings

Each finding carries its verification verdict. Sections **Gaps** and **To
verify** stay **read only**. Paths are relative to `swscale/`.

## Defects

### `scale`: the last argument is a byte offset, documented as a row

`swscale_stubs.c:176` against `swscale.mli:24-27`. The `.mli` says the rows
are scaled "to the [off] row of the [out_planes] output". The stub adds
`off` bytes to every destination plane pointer, with no multiplication by the
stride and the same value for luma and subsampled chroma planes.

**checked** — Run: throwaway program against the built bindings, linked with FFmpeg master (N-126774). Gray8 4x4 to gray8 4x4, rows 10..13, 20..23, 30..33, 40..43, destination zeroed: `off = 1` wrote `0 10 11 12 13 20 ...` (one byte), `off = 4` wrote the image from byte 4 (one row only because the stride is 4).

### `scale`: more than four planes overflow the stack tables

`swscale_stubs.c:161-178`. The four-slot local tables are indexed by the
length of the OCaml arrays with no bound. The typed readers bound the same
loop by 4 (`:243`, `:267`, `:280`).

**confirmed (second read)** — looked for a bound in `swscale.ml` and in the stub: `scale` is a direct external on an unbounded `planes` array and both loops run to `Wosize_val`. Not run: the outcome is a stack smash.

### `Make.create` leaks when a pixel format has no C value

`swscale_stubs.c:547-572`. The record is allocated at `:547`; the two pixel
format conversions at `:557` and `:572` can raise through avutil with the
record (and, for the second, both wrapper frames) not yet owned by any value.

**narrowed** — the raise exists (`PixelFormat_val` in the generated `pixel_format_stubs.h` ends in `Fail`) and nothing would free the record, but the closed polymorphic variant `Pixel_format.t` and the C table are generated together from the same headers, so a well-typed caller cannot supply a value missing from the table. The leak needs a binding built against other headers than the stubs, or an unsafe cast.

## Asymmetries

### Padding on output planes, none on input scratch

`swscale_stubs.c:252` against `:326-327`, `:365-367`. The stated reason for
the 16 extra bytes is that swscale "can read up to 16 bytes beyond the
planes". The bytes input scratch buffer, which is what swscale reads, is
allocated at exactly the string length.

**confirmed (second read)** — `:252` reallocates to `str_len` exactly; no other allocation feeds `in.slice` for the bytes kind.

### `av_realloc` results are unchecked

`swscale_stubs.c:252`, `:327`. Every other allocation in the file is checked.
A failure stores null (losing the old buffer) and `memcpy` then writes to it
on the input side.

**confirmed (second read)** — both results are stored straight into the slot; `sizes_tab` is raised regardless, so a later call does not retry.

### Only the typed context can use threads

`swscale_stubs.c:131` against `:578-580`. Plain `create` hard-codes one
thread; `Make.create` takes `?threads`.

**confirmed (second read)** — the literal `1` at `:131`; `swscale.ml` plain `create` has no thread argument.

### Scaler construction loses the error code

`swscale_stubs.c:84-108`. Both `create` functions report a fixed `Failure`
string; `scale` and `convert` report the FFmpeg code through avutil.

**confirmed (second read)** — `get_context` returns only a pointer and discards the `sws_init_context` result.

## Gaps

### Frame input is not checked against the context

`swscale_stubs.c:231-238`, `:415-428`. The reader takes `data` and
`linesize`; the wrapper frame then declares the context's width, height and
format over them. A frame smaller than, or of another format than, the
context was created for is read out of bounds.

### Bigarray and bytes inputs are not size-checked

`swscale_stubs.c:169-178`, `:240-286`. No plane length is compared with
stride × height, and the number of planes supplied is not compared with what
the format needs. Wrapping fails with `EINVAL` only when a needed slot is
still null.

### Stale plane slots survive between calls

`swscale_stubs.c:243-257`, `:265-271`, `:278-283`. Slots beyond the planes
supplied keep the pointer and stride of an earlier call. For bigarray kinds
that pointer may belong to a collected bigarray.

### `PackedBa`: the linesize array length is not checked

`swscale_stubs.c:278-283`. The loop is bounded by the bigarray array and
reads the same indices from the integer array.

### `av_opt_set_int` results are ignored

`swscale_stubs.c:93-100`. A rejected option (for example `threads` on a
library without it) is silent.

### `scale`: unused stride slots are uninitialised

`swscale_stubs.c:162-167`. Only the pointer tables are zeroed.

### No mutual exclusion on a context

`swscale_stubs.c:484-486`. The lock is released while the context's tables,
scratch buffers and wrapper frames are in use; a second thread on the same
context rewrites them.

### Output plane count and wrapped plane sizes come from different functions

`swscale_stubs.c:631` against `:427-440`. Allocators create
`av_pix_fmt_count_planes` planes; wrapping requires a pointer wherever
`av_image_fill_plane_sizes` is non-zero. The comment at `:434-435` notes that
paletted formats have a buffer that is not a plane.

### Contexts report no size to the garbage collector

`swscale_stubs.c:137`, `:633`. `caml_alloc_custom(..., 0, 1)`; collection is
the only release.

### Two custom operation records share one identifier

`swscale_stubs.c:79`, `:528`. Both use `"ocaml_swscale_context"` for
different payloads.

## API gaps

- **`scale`'s `off`** cannot express a destination row for a multi-plane
  format: one byte offset is applied to planes with different strides and
  heights (first Defect).
  **checked** — see the first Defect.
- **Bigarray outputs are 16 bytes longer than the plane.** `BigArray` and
  `PackedBigArray` results expose the padding in their dimension; the true
  plane size is not returned.
  **confirmed (second read)** — `out_size` at `:367` and `:394` is the bigarray dimension and nothing shrinks it afterwards.
- **`VideoData` is an open signature.** `Make` accepts any user module and
  the stubs trust `vk` to describe `t` (`swscale.mli:36-40`). A mismatched
  module is memory-unsafe.
  **confirmed (second read)** — `vector_kind` and `VideoData` are exported by the `.mli`.
- **`convert` on `Frame` input accepts any frame** although the context is
  bound to one geometry and format; the mismatch is undetected rather than an
  error.
  **confirmed (second read)** — `wrap_planes` overwrites the wrapper frame's width, height and format with the context's, so `sws_scale_frame` cannot see the mismatch either.
- **`flag` covers four constants**; nothing else of the scaler (other
  algorithms, colour space details, source/destination range) is reachable.
  **confirmed (second read)** — `FLAGS` at `:66-67`.
- **Plain `t` and `Make(BigArray)(BigArray).t` overlap** (same data shape)
  with different semantics: `scale` writes into caller buffers with slice
  arguments and one thread; `convert` allocates its output and can thread.
  **confirmed (second read)** — `BigArray.t = planes` in `swscale.ml`.

## To verify

- `sws_scale_frame` accepts a destination frame with caller-provided,
  non-owning buffers and writes into them rather than allocating.
- `av_image_fill_plane_sizes` reports a non-zero size at index 1 for
  paletted formats while `av_pix_fmt_count_planes` reports 1; and whether any
  such format is a supported swscale output.
- `threads = 0` means "one per core" for the scaler option, as the `.mli`
  says.
- `sws_scale` returns the output slice height and never a positive error.
- Minimum FFmpeg versions for `sws_scale_frame`, the scaler `threads` option
  and `av_image_fill_plane_sizes`; the code states none.
- Whether libswscale's logging from its worker threads reaches an OCaml log
  callback installed through avutil.

## Refuted

### Allocation inside `Store_field` arguments

`swscale_stubs.c:332`, `:370-372`, `:388-389`. `Store_field(*tmp, 0,
caml_alloc_string(len))`, `Store_field(*tmp, 0, caml_ba_alloc(...))` and
`Store_field(*out_vect, 0, caml_alloc_tuple(...))` allocate inside the macro
argument. `avutil/avutil_stubs.c:130-132` documents the project's rule
against this. `alloc_out_packed_ba` follows the rule for the bigarrays
(`:396-397`) and breaks it two lines earlier.

**refuted** — `Store_field` in the installed OCaml 5.5.0 `caml/memory.h` first assigns `value caml__temp_val = (val);` as its own statement and only then evaluates `&Field((block), offset)`, so the allocation completes before the destination is read; here `block` is `*out_vect` / `*tmp`, a registered root re-read after the allocation. What remains is a departure from the house rule stated in `avutil/avutil_stubs.c:130-132`, whose stated reason does not apply to this macro.
