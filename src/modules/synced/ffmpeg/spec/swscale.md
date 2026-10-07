# swscale

Image scaling and pixel-format conversion. It follows
[binding-contract.md](binding-contract.md); section numbers match.

## 1. Scope

`Swscale` binds libswscale. It depends on `avutil` only: pixel formats,
`data`, video frames, the error exception.

It installs no C header and provides nothing to other libraries. Module
initialisation reads the version, configuration string and licence string of
the libswscale loaded at run time and cannot fail.

## 2. Objects

### 2.1 Plain scaler — `t`

- **Native object**: one initialised scaler, bound to an input geometry and
  format and an output geometry and format.
- **Creation**: `create`.
- **Ownership**: the value owns the scaler and keeps nothing else alive.
- **Release**: by collection only.
- **Guard**: §6.2.
- It scales on one thread.

### 2.2 Typed scaler — `('i, 'o) ctx`, `Make(I)(O).t`

- **Native object**: one initialised scaler, with whatever working memory the
  implementation keeps beside it.
- **Creation**: `Make(I)(O).create`.
- **Ownership**: the value owns the scaler and its working memory. It retains
  nothing of the values passed to it or returned by it.
- **Accounting**: SHOULD report its working memory (L9).
- **Release**: by collection only.
- **Guard**: §6.2.

The type parameters record the input and output data types. They are fixed at
creation.

### 2.3 Values produced

`convert` returns fresh values (A5). A value returned by one call is never
written by a later call.

### 2.4 Data kinds (B3)

As in [swresample.md](swresample.md) §2.3: `'a kind` is abstract, only the
modules of §4.6 provide values of it, and a caller cannot define a data module
of its own.

The remaining run-time checks are on the values themselves: §4.4 and §4.5.

## 3. Enumerations and constants

### 3.1 `flag`

`type flag = Fast_bilinear | Bilinear | Bicubic | Print_info`, OCaml to C,
OR-ed together:

| Constructor     | C constant          |
| --------------- | ------------------- |
| `Fast_bilinear` | `SWS_FAST_BILINEAR` |
| `Bilinear`      | `SWS_BILINEAR`      |
| `Bicubic`       | `SWS_BICUBIC`       |
| `Print_info`    | `SWS_PRINT_INFO`    |

### 3.2 Data layouts

| Layout          | OCaml data type          | Content                               |
| --------------- | ------------------------ | ------------------------------------- |
| bigarray planes | `(data * int) array`     | one (plane, line size) pair per plane |
| packed planes   | `data array * int array` | the planes, then their line sizes     |
| frame           | `video frame`            | the frame's planes                    |
| byte planes     | `(string * int) array`   | one (plane, line size) pair per plane |

`data` is avutil's unsigned 8-bit bigarray. A line size is in bytes.

### 3.3 Parameters

| Name                | Recommended | Meaning                                                                                                                                    |
| ------------------- | ----------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| `PLANE_PADDING`     | 16 bytes    | Extra bytes after each plane buffer the binding allocates for libswscale, whose routines may read past a plane. Not visible in any length. |
| `SCALE_FRAME_ALIGN` | 32          | Buffer alignment of frames `convert` returns. Visible in their line sizes.                                                                 |
| default `threads`   | 1           | `Make.create`                                                                                                                              |

An image has at most 4 buffers (FFmpeg's limit).

## 4. Operations

### 4.1 `version`, `configuration`, `license`

```ocaml
val version : version
val configuration : string
val license : string
```

The version, build configuration and licence of the libswscale loaded at run
time, read once at module load.

### 4.2 `pixel_format`, `flag`, `t`

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

Arguments: flags, input width, input height, input pixel format, output
width, output height, output pixel format.

Creates a scaler with those settings, one thread, and FFmpeg's default for
everything else.

- A width or height below 1 raises a failure.
- A setting FFmpeg rejects, and a failed initialisation, raise the mapped
  FFmpeg error (A7) and release the scaler.

### 4.4 `planes`, `scale`

```ocaml
type planes = (data * int) array
val scale : t -> planes -> int -> int -> planes -> int -> unit
```

`scale ctx src y h dst off` scales the slice of `h` rows starting at row `y`
of the source image into the destination image (`sws_scale`).

- `src` holds the planes of the source slice, `dst` the planes of the
  destination image. Each element is a plane and its line size.
- `off` is a number of **rows**: the destination image starts at row `off` of
  the `dst` planes. For a plane with vertical chroma subsampling the row
  count is reduced accordingly; an `off` that is not a multiple of the
  subsampling factor raises a failure.
- libswscale reads and writes the bigarrays in place. Nothing is copied.

It raises a failure, before anything is read or written, when:

- `src` or `dst` does not hold exactly the buffers its pixel format needs;
- `y`, `h` or `off` is negative, or the slice lies outside the input height;
- a line size is smaller than the format needs for the image width;
- a plane is shorter than the rows libswscale will read from or write to it.

A scaling failure raises the mapped error.

### 4.5 `Make`

```ocaml
type 'a kind
module type VideoData = sig
  type t
  val kind : t kind
end
type ('i, 'o) ctx
module Make (I : VideoData) (O : VideoData) : sig
  type t = (I.t, O.t) ctx
  val create : ?threads:int -> flag list -> int -> int -> pixel_format ->
               int -> int -> pixel_format -> t
  val convert : t -> I.t -> O.t
end
```

#### `Make.create`

As `create` (§4.3), with:

- `threads` (default 1): the number of threads scaling a frame is split
  across. It is passed to the scaler; with more than one thread the output is
  byte for byte the output of one thread.
- a failure when the output pixel format is paletted and the output kind is
  not `Frame`: the palette cannot be returned.

Any failure releases everything (L2).

#### `Make.convert`

Converts one image.

**Input value**, checked against the scaler's input side before anything is
read:

| Kind                                        | Failure when                                                                                                                                                 |
| ------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| frame                                       | its width, height or pixel format is not the scaler's                                                                                                        |
| bigarray planes, packed planes, byte planes | it does not hold exactly the buffers the format needs; a line size is too small for the width; a buffer is shorter than its line size and the height require |
| packed planes                               | the two arrays differ in length                                                                                                                              |

A paletted input format needs its palette as the buffer after the plane.

**Output value**: fresh on every call.

| Kind            | Result                                                                                                                                |
| --------------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| bigarray planes | one (bigarray, line size) pair per plane of the output format                                                                         |
| packed planes   | the same bigarrays and line sizes, as two arrays                                                                                      |
| byte planes     | one (string, line size) pair per plane                                                                                                |
| frame           | a frame of the output width, height and format, buffers aligned to `SCALE_FRAME_ALIGN`; it carries no timestamp and no other property |

- The number of planes is the plane count of the output format
  (`av_pix_fmt_count_planes`).
- For bigarrays and strings, the line size of each plane is the smallest the
  format allows for the width (`av_image_fill_linesizes`), and the length of
  each plane is exactly that plane's size (`av_image_fill_plane_sizes`): a
  chroma plane is as short as the format's subsampling makes it.
- The sizes are the same on every call of one scaler.

A scaling failure raises the mapped error; the scaler stays usable.

### 4.6 Predefined data modules

Each has `type t` as below and `val kind : t kind`.

| Module           | `t`                             | Layout          |
| ---------------- | ------------------------------- | --------------- |
| `BigArray`       | `planes` = `(data * int) array` | bigarray planes |
| `PackedBigArray` | `data array * int array`        | packed planes   |
| `Frame`          | `video frame`                   | frame           |
| `Bytes`          | `(string * int) array`          | byte planes     |

The functor is applied by the user; the library ships no instantiation.

## 5. Errors

| Raised                                    | By                                         |
| ----------------------------------------- | ------------------------------------------ |
| ``Error (`Failure msg)``                  | the argument checks of §4.3, §4.4 and §4.5 |
| `Error e`, `e` mapped from an FFmpeg code | creation; `scale`; `convert`               |
| ``Error (`Failure msg)`` (E3)             | a pixel format with no C value             |
| `Out_of_memory`                           | any failed allocation                      |

No operation raises `Not_found`. After an error from `scale` or `convert` the
scaler is valid.

## 6. Blocking and concurrency

### 6.1 The runtime lock

Contract M1 covers the creation of a scaler, `scale` and `convert`. Strings
live on the OCaml heap: byte planes are copied out before the lock is
released and the result is copied in after it is retaken (M2). Bigarray and
frame data are read and written in place.

With `threads` other than 1, libswscale runs worker threads of its own inside
a conversion. They never enter OCaml.

### 6.2 Guards

`scale` and `convert` take the scaler's guard exclusively.

### 6.3 Global state

None.

## 7. Callbacks

None of either kind. Messages libswscale logs, `Print_info` among them, go
through FFmpeg's logging ([avutil.md](avutil.md) §7.1).

## 8. Data transfer

| Path                           | Input                                        | Output                                              |
| ------------------------------ | -------------------------------------------- | --------------------------------------------------- |
| `scale`                        | shared bigarrays                             | written in place into the caller's bigarrays        |
| bigarray planes, packed planes | shared: read in place                        | written directly into fresh runtime-owned bigarrays |
| frame                          | shared: the frame's planes are read in place | written directly into a fresh frame                 |
| byte planes                    | copied                                       | copied into fresh strings                           |

Buffers the binding allocates for libswscale to read from or write to carry
`PLANE_PADDING`. A bigarray or frame the caller supplies is used as it is.

## 9. Options

No option table. A scaler is configured by the typed arguments of its
creation, which all take effect or fail (contract O2).

## 10. Version-dependent behaviour

None.

## 11. Composite operations

None.
