# swscale — as-built specification (Part A)

Mechanics of the current stubs are in
[language-notes/swscale.md](language-notes/swscale.md). Observations and
judgement are in [findings/swscale.md](findings/swscale.md).

## 1. Scope

- Binds `libswscale` (image scaling, pixel format and colour space
  conversion). It also calls `libavutil` directly (`av_opt_set_int`,
  `av_frame_*`, `av_buffer_create`, `av_image_fill_linesizes`,
  `av_image_fill_plane_sizes`, `av_pix_fmt_count_planes`).
- OCaml library `swscale`, public name `ffmpeg-swscale`. It depends on the
  sibling library `ffmpeg-avutil` only (types `Pixel_format.t`, `data`,
  `video frame`, `version`, exception `Error`).
- Minimum version: none is stated. The code uses `sws_alloc_context` +
  `sws_init_context`, `sws_scale_frame`, the `"threads"` option of the
  scaler context and `av_image_fill_plane_sizes` unconditionally.
- The library has no C header and exports nothing to sibling stubs.
- It consumes no generated table of its own; pixel formats convert through
  avutil's generated table.

## 2. Objects

### 2.1 Plain scaler context: `t`

- C object: one `SwsContext`.
- Creation: `create`.
- Ownership: the OCaml value owns the `SwsContext`. It keeps nothing else
  alive.
- Release: by garbage collection only; the finaliser calls
  `sws_freeContext`. No explicit close. No memory pressure is reported.
- Always created with one thread.

### 2.2 Typed scaler context: `('i, 'o) ctx`, `Make(I)(O).t`

One OCaml value wraps one C record holding:

| Part                   | Content                                                                                                                                                    |
| ---------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `SwsContext`           | initialised scaler                                                                                                                                         |
| two wrapper `AVFrame`s | one for input, one for output; empty between calls                                                                                                         |
| input side             | width, height, pixel format; a 4-slot plane pointer table and a 4-slot stride table; a 4-slot table of scratch capacities; an "owns the plane memory" flag |
| output side            | the same, plus the plane count and a 4-slot table of plane byte sizes                                                                                      |
| three behaviours       | input reader, output allocator, optional output copier, chosen from the kinds at creation                                                                  |

The type parameters are phantom (`I.t`, `O.t`). The kinds are fixed at
creation.

Each side has a "current" plane pointer table and stride table. They are the
record's own 4-slot tables, except for the frame kind, where they are
redirected to the `data` and `linesize` arrays of the frame being read or the
frame just allocated.

**Ownership.**

- The context owns the `SwsContext` and the two wrapper frames.
- For the bytes kind (`Str`) the context owns up to four scratch plane
  buffers per side. Each grows on demand, never shrinks, and is reused by
  later calls.
- For the bigarray kinds the plane pointers point into the caller's bigarrays
  (input) or into the bigarrays being returned (output); the context does not
  own them.
- For the frame kind the plane pointers are those of the frame; the context
  does not own them.

**What it keeps alive.** Nothing on the OCaml side. After a call the context
holds stale raw pointers to the last input and output planes and does not use
them before overwriting them, except as noted for stale slots in 4.5.1.

**Release.** By garbage collection only. The finaliser:

1. `sws_freeContext`;
2. `av_frame_free` on both wrapper frames;
3. for each side that owns its plane memory, frees the four scratch slots
   (unused slots are null);
4. frees the record.

No explicit close. No memory pressure is reported.

### 2.3 Values produced

`convert` returns fresh values (section 8). A returned `video frame` is an
ordinary avutil frame value.

## 3. Enumerations and constants

### 3.1 `flag` (hand-written)

`type flag = Fast_bilinear | Bilinear | Bicubic | Print_info`, converted by
constructor index, OCaml to C only:

| Index | Constructor     | C constant          |
| ----: | --------------- | ------------------- |
|     0 | `Fast_bilinear` | `SWS_FAST_BILINEAR` |
|     1 | `Bilinear`      | `SWS_BILINEAR`      |
|     2 | `Bicubic`       | `SWS_BICUBIC`       |
|     3 | `Print_info`    | `SWS_PRINT_INFO`    |

A flag list becomes the bitwise OR of its constants; the empty list is 0.

### 3.2 `vector_kind` (hand-written)

`type vector_kind = PackedBa | Ba | Frm | Str`, passed by constructor index:

| Index | Constructor | OCaml data type                                |
| ----: | ----------- | ---------------------------------------------- |
|     0 | `PackedBa`  | `data array * int array` (planes, linesizes)   |
|     1 | `Ba`        | `(data * int) array` (plane, linesize) pairs   |
|     2 | `Frm`       | `video frame`                                  |
|     3 | `Str`       | `(string * int) array` (plane, linesize) pairs |

`data` is avutil's unsigned 8-bit C-layout one-dimensional bigarray.

### 3.3 Other constants

| Value | Use                                                                                                                                                                            |
| ----: | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
|     4 | maximum number of planes handled                                                                                                                                               |
|    16 | bytes added after each output plane (bigarray dimension, bytes scratch buffer); the code gives the reason "some filters and swscale can read up to 16 bytes beyond the planes" |
|    32 | alignment passed to `av_frame_get_buffer` for output frames                                                                                                                    |
|     1 | default thread count                                                                                                                                                           |

## 4. Operations

### 4.1 `version`, `configuration`, `license`

```ocaml
val version : version
val configuration : string
val license : string
```

Computed once at module initialisation. `version` is `swscale_version()`
decoded as `major = v lsr 16`, `minor = (v lsr 8) land 0xff`,
`micro = v land 0xff`. `configuration` and `license` are copies of
`swscale_configuration()` and `swscale_license()`.

### 4.2 `type pixel_format`, `type flag`, `type t`

```ocaml
type pixel_format = Avutil.Pixel_format.t
type flag = Fast_bilinear | Bilinear | Bicubic | Print_info
type t
```

### 4.3 `create`

```ocaml
val create :
  flag list -> int -> int -> pixel_format -> int -> int -> pixel_format -> t
```

Arguments: flags, input width, input height, input pixel format, output width,
output height, output pixel format.

1. Convert the flag list to an array, then OR the constants.
2. Convert both pixel formats through avutil's table.
3. With the runtime lock released, build the scaler (4.3.1) with 1 thread.
4. A null result raises ``Error (`Failure "Failed to get sws context!")``.
5. Wrap the `SwsContext` in a garbage-collected value.

#### 4.3.1 Scaler construction (shared with `Make.create`)

1. `sws_alloc_context()`; null gives a null result.
2. `av_opt_set_int(ctx, name, value, 0)` for, in order: `"srcw"`, `"srch"`,
   `"src_format"`, `"dstw"`, `"dsth"`, `"dst_format"`, `"sws_flags"`,
   `"threads"`. The return values are ignored.
3. `sws_init_context(ctx, NULL, NULL)` (no source or destination filter). A
   negative result frees the context with `sws_freeContext` and gives a null
   result.

No other option is set. The specific FFmpeg error code is lost.

### 4.4 `type planes`, `scale`

```ocaml
type planes = (data * int) array
val scale : t -> planes -> int -> int -> planes -> int -> unit
```

`scale ctx src y h dst off`. Each `planes` element is a plane bigarray and its
stride in bytes.

1. Zero two local 4-slot plane pointer tables. The two stride tables are not
   initialised.
2. For each element `i` of `src`: pointer `i` := the bigarray's data address,
   stride `i` := the integer. The element count is not bounded by 4.
3. For each element `i` of `dst`: pointer `i` := the bigarray's data address
   plus `off` **bytes** (the same `off` for every plane), stride `i` := the
   integer.
4. With the runtime lock released:
   `sws_scale(ctx, src_ptrs, src_strides, y, h, dst_ptrs, dst_strides)`.
5. A negative result raises `Error` (avutil mapping). Otherwise return `()`;
   the number of rows written is discarded.

Nothing is copied: libswscale reads and writes the bigarrays directly. The
bigarray sizes are not compared with the strides, heights or the context's
geometry.

### 4.5 `vector_kind`, `VideoData`, `ctx`, `Make`

```ocaml
type vector_kind = PackedBa | Ba | Frm | Str
module type VideoData = sig
  type t
  val vk : vector_kind
end
type ('i, 'o) ctx
module Make (I : VideoData) (O : VideoData) : sig
  type t = (I.t, O.t) ctx
  val create : ?threads:int -> flag list -> int -> int -> pixel_format ->
               int -> int -> pixel_format -> t
  val convert : t -> I.t -> O.t
end
```

`vector_kind` is hidden from the generated documentation but public. The
stubs trust `vk` to describe `t`; nothing checks it.

The `.mli` and `.ml` contain commented-out `from_codec`, `to_codec` and
`from_codec_to_codec`; they are not part of the surface.

#### `Make.create`

`?threads` defaults to 1. It is passed unchanged as the scaler's `"threads"`
option; the `.mli` states that 0 lets swscale pick one per core.

1. Allocate the zero-filled record (`Out_of_memory` on failure).
2. Point the input side's current tables at the record's own tables. Record
   input width, height and pixel format.
3. `av_frame_alloc` twice for the wrapper frames. If either fails, release
   everything and raise `Out_of_memory`.
4. Same as step 2 for the output side.
5. OR the flags.
6. With the runtime lock released, build the scaler (4.3.1) with `threads`.
   A null result releases everything and raises
   ``Error (`Failure "Failed to create Swscale context")``.
7. Choose the input reader from `I.vk`. For `Str` the input side owns its
   plane memory.
8. Choose the output allocator from `O.vk`. For `Str` also set the output
   copier, and the output side owns its plane memory.
9. `av_image_fill_linesizes(out_strides, out_format, out_width)` into the
   record's output stride table. Negative: release everything and raise
   ``Error (`Failure "Failed to create Swscale context")``.
10. `av_image_fill_plane_sizes(out_plane_sizes, out_format, out_height,
out_strides)`. Negative: same release and error.
11. Output plane count := `av_pix_fmt_count_planes(out_format)`.
12. Wrap the record in a garbage-collected value.

The output strides of step 9 are the minimal ones (no padding). They are the
strides reported for the bytes and bigarray output kinds.

#### `Make.convert`

1. Run the input reader (4.5.1).
2. Run the output allocator (4.5.2). A negative result raises `Error`.
3. With the runtime lock released, scale (4.5.3).
4. A negative result raises `Error` (avutil mapping). The context stays
   usable; the allocated output value is dropped.
5. If there is an output copier, run it (4.5.2).
6. Return the output value.

#### 4.5.1 Input readers

| Kind       | Behaviour                                                                                                                                                                                                                                                                              |
| ---------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Frm`      | current plane table := `frame->data`; current stride table := `frame->linesize`. Nothing is copied. The frame's width, height and format are not compared with the context's.                                                                                                          |
| `Ba`       | for each of the first 4 pairs: plane slot := the bigarray's data address; stride slot := the integer. Nothing is copied.                                                                                                                                                               |
| `PackedBa` | plane count := length of the bigarray array; for each of the first 4: plane slot := data address of bigarray `i`; stride slot := element `i` of the integer array. Nothing is copied. The integer array's length is not checked.                                                       |
| `Str`      | for each of the first 4 pairs: stride slot := the integer; if the scratch capacity of slot `i` is below the string's length, re-allocate that scratch buffer to exactly the string's length (`av_realloc`) and record the new capacity; copy the whole string into the scratch buffer. |

Slots beyond the number of planes supplied keep their previous content: zero
on a fresh context, or the pointer and stride of an earlier call.

#### 4.5.2 Output allocators and copier

`P` = output plane count, `size[i]` and `stride[i]` from `Make.create`.

| Kind       | Allocation before scaling                                                                                                                                                                                          | Result                                                                                            |
| ---------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------- |
| `Frm`      | `av_frame_alloc`; `width`, `height`, `format` from the output side; `av_frame_get_buffer(frame, 32)`; current plane and stride tables := `frame->data`, `frame->linesize`; wrap as an avutil frame value           | the frame; its linesizes are FFmpeg's aligned ones                                                |
| `Ba`       | array of `P` pairs: fresh runtime-owned `data` bigarray of `size[i] + 16` bytes, and `stride[i]`; plane slot := the bigarray's data                                                                                | the array                                                                                         |
| `PackedBa` | pair of an array of `P` fresh bigarrays of `size[i] + 16` bytes and an array of the `P` strides; plane slots := the bigarrays' data                                                                                | the pair                                                                                          |
| `Str`      | array of `P` pairs: fresh string of `size[i]` bytes, and `stride[i]`; if the scratch capacity of slot `i` is below `size[i]`, re-allocate the scratch buffer to `size[i] + 16` bytes and record capacity `size[i]` | after scaling, the copier copies `length(string i)` bytes from scratch buffer `i` into string `i` |

For `Frm`, a failed `av_frame_alloc` raises `Out_of_memory`; a failed
`av_frame_get_buffer` frees the frame and raises `Error`. The frame carries
only width, height and format: no timestamp, no colour property.

Bigarray outputs are 16 bytes longer than the plane they hold; the extra
bytes are part of the visible dimension and are not initialised.

Because the plane sizes are fixed per context, the bytes output scratch
buffers are allocated on the first `convert` and reused unchanged afterwards.
Bigarray and frame outputs are allocated anew on every call.

#### 4.5.3 Scaling

1. Wrap the input side in the input wrapper frame, then the output side in
   the output wrapper frame. Wrapping a side:
   1. set the wrapper frame's `width`, `height`, `format` from the context's
      configuration for that side;
   2. compute four plane sizes with `av_image_fill_plane_sizes(sizes, format,
height, current_strides)`; a negative result is the error;
   3. for each index 0..3 with a non-zero size: a null plane pointer is
      `AVERROR(EINVAL)`; otherwise `frame->data[i]` := the pointer,
      `frame->linesize[i]` := the stride, `frame->buf[i]` :=
      `av_buffer_create(pointer, size, no-op free, NULL, 0)`; a null buffer is
      `AVERROR(ENOMEM)`.
2. If both wraps succeed:
   `sws_scale_frame(ctx, out_wrapper, in_wrapper)`.
3. `av_frame_unref` both wrapper frames, in every case.

The wrapper frames exist because slice threading is reachable only through
`sws_scale_frame`, which wants reference-counted buffers. The buffers never
free the planes. A user frame given as input, or the frame returned as
output, is never itself passed to libswscale; only its plane pointers and
linesizes are.

A format with a non-zero size at an index that holds something other than an
image plane (the palette of a paletted format) needs a pointer at that index
on both sides.

### 4.6 Predefined `VideoData` modules

| Module           | `t`                             | `vk`       |
| ---------------- | ------------------------------- | ---------- |
| `BigArray`       | `planes` = `(data * int) array` | `Ba`       |
| `PackedBigArray` | `data array * int array`        | `PackedBa` |
| `Frame`          | `video frame`                   | `Frm`      |
| `Bytes`          | `(string * int) array`          | `Str`      |

The functor is applied by the user; the library ships no instantiation.

## 5. Errors

| Raised value                                                 | From                                                                                                 |
| ------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------- |
| ``Error (`Failure "Failed to get sws context!")``            | `create`: allocation or `sws_init_context` failure                                                   |
| ``Error (`Failure "Failed to create Swscale context")``      | `Make.create`: scaler construction, `av_image_fill_linesizes` or `av_image_fill_plane_sizes` failure |
| `Out_of_memory`                                              | `Make.create` record or wrapper frames; output frame allocation                                      |
| `Error e`, avutil mapping                                    | `scale` (`sws_scale`); `Make.convert` (`av_frame_get_buffer`, plane wrapping, `sws_scale_frame`)     |
| ``Error (`Failure …)`` from avutil's pixel format conversion | any `create`, for a format with no C value                                                           |
| ``Error (`Failure "Failed to get input pixels")``            | `Make.convert` if a reader reports a negative count; no reader does                                  |

After an error from `scale` or `convert` the context remains valid.

## 6. Blocking and concurrency

- The runtime lock is released around scaler construction (both `create`
  functions), `sws_scale`, and the wrap + `sws_scale_frame` + unref sequence
  of `convert`. All OCaml allocation and all copying happen with the lock
  held.
- While the lock is released the C side touches only memory outside the OCaml
  heap: scratch buffers, bigarray data, frame buffers. Input and output
  values are rooted for the duration of the call.
- With `threads` other than 1, libswscale runs its own worker threads inside
  `sws_scale_frame`. They never call into OCaml from this library.
- A context has no lock. Two threads using one context at once share its
  tables, scratch buffers and wrapper frames.
- Bigarray and frame inputs are read in place while the lock is released.
- No thread registration and no global state.

## 7. Callbacks

Nothing in this library. `Print_info` and other libswscale messages go
through FFmpeg's logging, whose routing belongs to the avutil specification.

## 8. Data transfer

| Path             | Input                                                   | Output                                                                                               |
| ---------------- | ------------------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| `scale`          | shared bigarrays                                        | written in place into the caller's bigarrays, each plane pointer advanced by `off` bytes             |
| `Ba`, `PackedBa` | shared                                                  | fresh runtime-owned bigarrays of plane size + 16, written directly                                   |
| `Frm`            | shared: the frame's plane pointers and linesizes        | fresh frame, `av_frame_get_buffer` alignment 32, written directly                                    |
| `Str`            | copied into context scratch buffers sized to the string | scaled into context scratch buffers of plane size + 16, then copied into fresh strings of plane size |

- Strides: taken from the caller on input. On output, minimal linesizes from
  `av_image_fill_linesizes` for bytes and bigarrays, the frame's own for
  frames.
- Plane sizes on output come from `av_image_fill_plane_sizes`, which accounts
  for chroma subsampling.
- At most four planes per image.
- No alignment is requested for bigarray or scratch planes.

## 9. Options

No option dictionary and no generic AVOption access. Eight integer options
are set on every scaler: `srcw`, `srch`, `src_format`, `dstw`, `dsth`,
`dst_format`, `sws_flags`, `threads`. Failures to set them are not detected.

## 10. Version-dependent behaviour

Nothing. The stubs contain no version or feature conditional.

## 11. Logic on the OCaml side

- `version` decoding; `configuration` and `license` evaluated once.
- Flag list to array in both `create` functions.
- `Make.create`: default `threads = 1`, passing `I.vk` and `O.vk`.
- The four `VideoData` modules: pure constants.
- `scale` and `Make.convert` are direct externals.
