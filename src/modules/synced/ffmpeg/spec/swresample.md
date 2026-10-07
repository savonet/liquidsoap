# swresample — as-built specification (Part A)

Mechanics of the current stubs are in
[language-notes/swresample.md](language-notes/swresample.md). Observations and
judgement are in [findings/swresample.md](findings/swresample.md).

## 1. Scope

- Binds `libswresample` (audio resampling, rematrixing, sample format
  conversion). It also calls `libavutil` directly (`av_opt_set_*`,
  `av_samples_alloc`, `av_frame_*`, `av_channel_layout_copy`,
  `av_get_bytes_per_sample`, `av_sample_fmt_is_planar`,
  `av_get_sample_fmt_name`).
- OCaml library `swresample`, public name `ffmpeg-swresample`. It depends on
  the sibling libraries `ffmpeg-avutil` (types `Channel_layout.t`,
  `Sample_format.t`, `audio frame`, `version`, exception `Error`) and
  `ffmpeg-avcodec` (type `audio Avcodec.params` and its three audio getters).
- Minimum version: no version is stated. The code uses the `AVChannelLayout`
  API unconditionally (`av_opt_set_chlayout` with `"in_chlayout"` /
  `"out_chlayout"`, `AVFrame.ch_layout`, `av_channel_layout_copy`) and
  `swr_get_out_samples`.
- The library exports nothing to sibling stubs. Its C header is private to the
  library and is not installed.
- It consumes one generated enum table set, `swresample_options` (section 3).

## 2. Objects

### 2.1 Resampler context: `('i, 'o) ctx`, `Make(I)(O).t`

One OCaml value wraps one C record holding:

| Part                    | Content                                                                                                                                                                                                                          |
| ----------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `SwrContext`            | the libswresample context, initialised                                                                                                                                                                                           |
| input side              | channel count, sample format, bytes per sample, "is planar" flag, a pointer table with one slot per channel, the capacity (in samples per channel) of the scratch buffer behind that table, and an "owns the sample memory" flag |
| output side             | the same fields                                                                                                                                                                                                                  |
| output channel layout   | a private copy (`av_channel_layout_copy`)                                                                                                                                                                                        |
| output sample rate      | integer                                                                                                                                                                                                                          |
| input kind, output kind | the `vector_kind` of `I` and `O`                                                                                                                                                                                                 |

The type parameters `'i` and `'o` are phantom: they record `I.t` and `O.t`.
The input and output kinds are fixed at creation and never change.

**Creation.** Only through `Make(I)(O).create` and its three wrappers.

**Ownership.**

- The context owns the `SwrContext`.
- For every kind except frame, the context owns the per-channel pointer table
  of that side.
- For the kinds that copy (bytes, planar bytes, float array, planar float
  array) the context also owns one scratch sample buffer per side. The buffer
  is one allocation reached from slot 0 of the pointer table; for planar
  formats the other slots point inside it. It is allocated lazily, grows on
  demand, never shrinks, and is reused by every later call.
- For the bigarray kinds the pointer table slots point into the caller's
  bigarrays (input) or into the bigarrays being returned (output). The context
  does not own that memory and never frees it.
- For the frame kind the context has no pointer table of its own. During a
  call it borrows the frame's `extended_data` table.

**What it keeps alive.** Nothing on the OCaml side. The channel layouts passed
to `create` are copied (into the `SwrContext` options, and the output one also
into the context record). No input or output value is retained after a call
returns; the context keeps stale raw pointers to the last bigarray or frame
buffers but does not read them again before overwriting them, with the one
exception described for `flush`.

**Release.** By garbage collection only. The finaliser:

1. calls `swr_free` on the `SwrContext`;
2. for each side whose kind is not frame and whose pointer table exists: frees
   the scratch buffer (slot 0) when the side owns its sample memory, then
   frees the pointer table;
3. frees the record.

The private copy of the output channel layout is not uninitialised. There is
no explicit close and so no use-after-close state. The context reports no
memory pressure to the garbage collector.

**State machine.** None beyond "initialised". Internally libswresample buffers
samples between calls.

### 2.2 Values produced

`convert` and `flush` return fresh OCaml values (section 8). A returned
`audio frame` is an ordinary avutil frame value, owned and finalised as avutil
specifies.

## 3. Enumerations and constants

### 3.1 Generated: `swresample_options`

A generated OCaml module `Swresample_options` and a generated C header provide
three polymorphic variant types and their conversion tables:

| OCaml type    | C enum               | Option name it sets |
| ------------- | -------------------- | ------------------- |
| `dither_type` | `enum SwrDitherType` | `"dither_method"`   |
| `engine`      | `enum SwrEngine`     | `"resampler"`       |
| `filter_type` | `enum SwrFilterType` | `"filter_type"`     |

On the build that was inspected the generated members are
`` `Dither_rectangular ``, `` `Dither_triangular ``,
`` `Dither_triangular_highpass ``; `` `Engine_swr ``, `` `Engine_soxr ``;
`` `Filter_type_cubic ``, `` `Filter_type_blackman_nuttall ``,
`` `Filter_type_kaiser ``. `SWR_DITHER_NONE` and the noise-shaping dithers are
outside the generated range. The generated module also defines one list of all
members per type (`dither_type`, `engine`, `filter_type`).

Only the OCaml-to-C direction is used, and only through the non-raising
lookup, which returns the sentinel `0xFFFFFFF` when the value is not in the
table. `Swresample.options` is the union
`[ dither_type | engine | filter_type ]`; the stub finds the category of an
option by trying the three tables in the order dither, engine, filter. A value
found in none of them is skipped silently.

### 3.2 Hand-written: `vector_kind`

`type vector_kind = Str | P_Str | Fa | P_Fa | Ba | P_Ba | Frm`, passed to C as
the constructor index.

| Index | Constructor | OCaml data type           | Layout                               |
| ----: | ----------- | ------------------------- | ------------------------------------ |
|     0 | `Str`       | `bytes`                   | interleaved raw samples              |
|     1 | `P_Str`     | `bytes array`             | one raw-sample `bytes` per channel   |
|     2 | `Fa`        | `float array`             | interleaved doubles                  |
|     3 | `P_Fa`      | `float array array`       | one `float array` per channel        |
|     4 | `Ba`        | `Bigarray.Array1.t`       | interleaved, element type per format |
|     5 | `P_Ba`      | `Bigarray.Array1.t array` | one bigarray per channel             |
|     6 | `Frm`       | `audio frame`             | whatever the frame holds             |

### 3.3 Sample formats and bigarray kinds

Sample formats convert through avutil's generated table. The bigarray element
kind for an output bigarray comes from avutil's sample-format-to-bigarray-kind
helper (U8 → unsigned 8-bit, S16 → signed 16-bit, S32 → int32, FLT → float32,
DBL → float64, planar variants alike; it raises for a format with no kind).

## 4. Operations

### 4.1 `val version : version`

`swresample_version()` decoded at module initialisation:
`major = v lsr 16`, `minor = (v lsr 8) land 0xff`, `micro = v land 0xff`.

### 4.2 `type vector_kind`, `module type AudioData`

```ocaml
type vector_kind = Str | P_Str | Fa | P_Fa | Ba | P_Ba | Frm
module type AudioData = sig
  type t
  val vk : vector_kind
  val sf : Avutil.Sample_format.t
end
```

`vector_kind` is hidden from the generated documentation but is public. `vk`
tells the stubs how to read or build a value of type `t`. `sf` is the sample
format of the module, or `` `None `` when the format must be given to
`create`. The stubs trust `vk` and `sf` to describe `t`; nothing checks it.

### 4.3 `type options`, `type ('i, 'o) ctx`

```ocaml
type options = [ dither_type | engine | filter_type ]
type ('i, 'o) ctx
```

### 4.4 `module Make (I : AudioData) (O : AudioData)`

`type t = (I.t, O.t) ctx`

#### `create`

```ocaml
val create :
  ?options:options list -> Channel_layout.t -> ?in_sample_format:Sample_format.t ->
  int -> Channel_layout.t -> ?out_sample_format:Sample_format.t -> int -> t
```

Arguments in order: options, input channel layout, input sample format, input
sample rate, output channel layout, output sample format, output sample rate.

OCaml side:

1. `options` becomes an array (empty when absent).
2. Input sample format: `I.sf` when `I.sf` is not `` `None ``; otherwise the
   `?in_sample_format` argument; otherwise raise
   ``Error (`Failure "Swresample input sample format undefined")``. The module's
   format takes precedence over the argument; an argument given to a module
   with a defined format is ignored.
3. Output sample format: the same rule with `O.sf` and
   `"Swresample output sample format undefined"`.
4. Call the stub with `I.vk`, input layout, input format, input rate, `O.vk`,
   output layout, output format, output rate, options array.

Stub side:

1. Convert the formats through avutil's table.
2. Keep the first three elements of the options array. Elements after the
   third are dropped.
3. Allocate the zero-filled context record (`Out_of_memory` on failure).
4. `swr_alloc()` (`Out_of_memory` on failure).
5. `av_opt_set_chlayout(ctx, "in_chlayout", in_layout, 0)`. Record the input
   channel count from the layout.
6. If the input format is not `AV_SAMPLE_FMT_NONE`:
   `av_opt_set_sample_fmt(ctx, "in_sample_fmt", fmt, 0)` and record the
   format. Otherwise nothing is set and the recorded format stays at the
   zero value.
7. If the input rate is not 0: `av_opt_set_int(ctx, "in_sample_rate", rate, 0)`.
8. `av_opt_set_chlayout(ctx, "out_chlayout", out_layout, 0)`, then
   `av_channel_layout_copy` into the record. Record the output channel count.
9. If the output format is not `AV_SAMPLE_FMT_NONE`:
   `av_opt_set_sample_fmt(ctx, "out_sample_fmt", fmt, 0)` and record it.
10. If the output rate is not 0:
    `av_opt_set_int(ctx, "out_sample_rate", rate, 0)` and record it.
11. For each kept option, in order: look it up as a dither type and set
    `"dither_method"`; else as an engine and set `"resampler"`; else as a
    filter type and set `"filter_type"`; all with `av_opt_set_int(..., 0)`. A
    non-zero result raises.
12. `swr_init(ctx)` with the runtime lock released.
13. For each side whose kind is not `Frm`: allocate a zero-filled pointer
    table with one slot per channel and record
    `av_sample_fmt_is_planar(format)`. For a `Frm` side there is no table and
    the planar flag stays false.
14. Record `av_get_bytes_per_sample(format)` for each side, and both kinds.
15. Wrap the record in a garbage-collected value.

Every `av_opt_set_*`, `av_channel_layout_copy` and `swr_init` failure raises
`Error` with avutil's mapping of the FFmpeg code. No other option is set; all
other libswresample options keep their defaults.

Failure state: when steps 4 to 12 raise, the record, the `SwrContext` and the
layout copy made so far are not released.

A later option of the same category overrides an earlier one (it sets the same
AVOption again).

#### `from_codec`, `to_codec`, `from_codec_to_codec`

```ocaml
val from_codec :
  ?options:options list -> audio Avcodec.params -> Channel_layout.t ->
  ?out_sample_format:Sample_format.t -> int -> t
val to_codec :
  ?options:options list -> Channel_layout.t -> ?in_sample_format:Sample_format.t ->
  int -> audio Avcodec.params -> t
val from_codec_to_codec :
  ?options:options list -> audio Avcodec.params -> audio Avcodec.params -> t
```

Each calls `create`, taking the codec-side triple from
`Avcodec.Audio.get_channel_layout`, `Avcodec.Audio.get_sample_format` (passed
as the explicit `?in_sample_format` / `?out_sample_format`) and
`Avcodec.Audio.get_sample_rate`. Because of step 2 of `create`, the codec's
sample format is used only when the corresponding module's `sf` is
`` `None `` (`Bytes`, `Frame`).

#### `convert`

```ocaml
val convert : ?offset:int -> ?length:int -> t -> I.t -> O.t
```

`offset` and `length` count samples per channel. Default offset is 0. Default
length is "everything after the offset".

1. If the input side's planar flag is set (non-frame kind with a planar
   format): the number of fields of the input value must equal the input
   channel count, else ``Error (`Failure "Swresample failed to convert %d
channels : %d channels were expected")`` (given, expected).
2. Read the input with the reader of the input kind (4.4.1). It returns the
   number of input samples per channel, `n_in`, and leaves the input pointer
   table pointing at the samples.
3. If `length` is given: when `n_in < length` raise
   ``Error (`Failure "Input vector too small!")``; otherwise `n_in := length`.
4. `n_out := swr_get_out_samples(ctx, n_in)`. The result is used unchecked.
5. Prepare the output with the allocator of the output kind for `n_out`
   samples (4.4.2).
6. With the runtime lock released:
   `r = swr_convert(ctx, out_table, out_capacity, in_table, n_in)`, where
   `out_capacity` is the capacity recorded for the output side (for the
   scratch kinds this is the high-water mark, at least `n_out`; for the other
   kinds exactly `n_out`).
7. `r < 0` raises `Error` (avutil mapping). The context stays usable; the
   prepared output value is dropped.
8. Build the result with the finisher of the output kind for `r` samples
   (4.4.2) and return it.

Neither `offset` nor `length` is checked for being negative.

#### `flush`

```ocaml
val flush : t -> O.t
```

1. `n_out := swr_get_out_samples(ctx, 0)`.
2. Run steps 5 to 8 of `convert` with `n_in = 0`. The input pointer passed to
   `swr_convert` is the context's input pointer table as it stands: the owned
   table for non-frame kinds; for the frame kind, a null pointer before the
   first `convert` and the last input frame's (possibly stale) data table
   afterwards. The input count is 0.

`flush` takes no input and can be called any number of times.

#### 4.4.1 Input readers

Notation: `C` = input channel count, `B` = bytes per sample of the input
format, `o` = offset. "Grow" means: if the sample count exceeds the capacity
of the input scratch buffer, free it and allocate a new one with
`av_samples_alloc(table, NULL, C, n, format, 0)`; the capacity becomes `n`.
An allocation failure raises `Error`.

**Frame (`Frm`).**

1. `o <> 0` raises ``Error (`Failure "Cannot use offset with frame data!")``.
2. The frame's channel count (`ch_layout.nb_channels`) must equal `C`, else
   `Failure "Swresample failed to convert %d channels : %d channels were
expected"`.
3. The frame's format must equal the input format, else `Failure "Swresample
failed to convert %s sample format : %s sample format were expected"` with
   the two `av_get_sample_fmt_name` strings.
4. The input table is the frame's `extended_data`. `n_in = frame->nb_samples`.

The frame's sample rate and channel layout (beyond the count) are not
compared. No timestamp is read.

**Bytes (`Str`), interleaved.** `L` = byte length of the value.

1. `n_in = L / (B * C) - o` (integer division). Negative raises
   `Failure "Invalid offset!"`.
2. Grow to `n_in`.
3. Copy `L` bytes from the value, starting at byte `o * B * C`, to the start
   of the scratch buffer.

The copy length is the whole length `L` for every offset.

**Planar bytes (`P_Str`).** `L` = byte length of channel 0.

1. `n_in = L / B - o`. Negative raises `Failure "Invalid offset!"`.
2. Grow to `n_in`.
3. For each channel `i` in `0 .. C-1`: require
   `L = length(channel i) - o * B`, else `Failure "Swresample failed to
convert channel %d's %lu bytes : %d bytes were expected"` (index, that
   channel's length, `L`). Copy `L` bytes from byte `o * B` of the channel to
   plane `i` of the scratch buffer.

With `o = 0` this requires all channels to have the same length. With
`o > 0` the test fails on channel 0.

**Float array (`Fa`), interleaved.** `N` = number of elements.

1. `n_in = N / C - o`. Negative raises `Failure "Invalid offset!"`.
2. Grow to `n_in`.
3. For `i` in `0 .. N-1`: scratch double `i` := element `i + o`, with NaN
   replaced by 0.

The loop length is `N` and the read index is shifted by `o` elements (not
`o * C`) for every offset.

**Planar float array (`P_Fa`).** `N` = number of elements of channel 0.

1. `n_in = N - o`. Negative raises `Failure "Invalid offset!"`.
2. Grow to `n_in`.
3. For each channel `i`: require the same element count as channel 0, else
   `Failure "Swresample failed to convert channel %d's %lu bytes : %d bytes
were expected"` (the numbers are element counts). For `j` in
   `0 .. n_in-1`: plane `i` double `j` := element `j + o`, NaN replaced by 0.

**Bigarray (`Ba`), interleaved.** `D` = dimension.

1. `n_in = D / C - o`. Negative raises `Failure "Invalid offset!"`.
2. Slot 0 of the input table := the bigarray's data address plus `o * C`
   **bytes**. Nothing is copied.

**Planar bigarray (`P_Ba`).** `D` = dimension of channel 0.

1. `n_in = D - o`. Negative raises `Failure "Invalid offset!"`.
2. For each channel `i`: require `dimension(i) = n_in`, else
   `Failure "Swresample failed to convert channel %d's %ld bytes : %d bytes
were expected"` (index, that dimension, `n_in`). Slot `i` := that
   bigarray's data address plus `o` **bytes**. Nothing is copied.

With `o > 0` the test fails on channel 0.

Worked layout, stereo (`C = 2`), signed 16-bit (`B = 2`), three samples per
channel `L0 L1 L2` / `R0 R1 R2`, each sample two bytes in native byte order:

| Kind           | Value                                                | `n_in` arithmetic |
| -------------- | ---------------------------------------------------- | ----------------- |
| `Str` (S16)    | 12 bytes `L0 R0 L1 R1 L2 R2`                         | `12 / (2*2) = 3`  |
| `P_Str` (S16P) | array of two: 6 bytes `L0 L1 L2`, 6 bytes `R0 R1 R2` | `6 / 2 = 3`       |
| `Fa` (DBL)     | float array `l0 r0 l1 r1 l2 r2`                      | `6 / 2 = 3`       |
| `P_Fa` (DBLP)  | array of two float arrays: `l0 l1 l2`, `r0 r1 r2`    | `3`               |
| `Ba` (S16)     | dimension 6: `L0 R0 L1 R1 L2 R2`                     | `6 / 2 = 3`       |
| `P_Ba` (S16P)  | two bigarrays of dimension 3                         | `3`               |

Channel order is the order of the channel layout. An interleaved value whose
length is not a multiple of one frame (`B * C` bytes, or `C` elements) has its
trailing partial frame ignored by the sample count; the copying readers still
copy it.

#### 4.4.2 Output allocators and finishers

Notation: `C` = output channel count, `B` = bytes per sample, `n` = `n_out`,
`r` = samples per channel written by `swr_convert`.

| Kind    | Before `swr_convert`                                                                         | After `swr_convert`                                                           |
| ------- | -------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------- |
| `Str`   | grow output scratch to `n`                                                                   | fresh `bytes` of `r * C * B` bytes, copied from plane 0                       |
| `P_Str` | grow output scratch to `n`                                                                   | array of `C` fresh `bytes` of `r * B` bytes, plane `i` copied to element `i`  |
| `Fa`    | grow output scratch to `n`                                                                   | fresh `float array` of `r * C` elements read as doubles from plane 0, NaN → 0 |
| `P_Fa`  | grow output scratch to `n`                                                                   | array of `C` fresh `float array`s of `r` elements, NaN → 0                    |
| `Ba`    | fresh C-layout bigarray of `n * C` elements, kind from the output format; slot 0 := its data | its dimension is overwritten with `r * C`                                     |
| `P_Ba`  | array of `C` fresh bigarrays of `n` elements; slot `i` := data of bigarray `i`               | each dimension is overwritten with `r`                                        |
| `Frm`   | fresh frame (below); table := its `extended_data`                                            | `frame->nb_samples := r`                                                      |

"Grow" for output: if `n` exceeds the output scratch capacity, free and
re-allocate it with `av_samples_alloc(table, NULL, C, n, format, 0)`.

Frame allocation: `av_frame_alloc`; `nb_samples = n`; `ch_layout` copied from
the context's output layout; `format` = output format; `sample_rate` = output
rate; `av_frame_get_buffer(frame, 0)`. A failed `av_frame_alloc` raises
`Out_of_memory`; a failed layout copy or buffer allocation frees the frame and
raises `Error`. The frame carries no timestamp and no other property.

The bigarrays are allocated and owned by the OCaml runtime. Their memory stays
sized for `n` samples; only the visible dimension shrinks to `r`.

### 4.5 Predefined `AudioData` modules

All have `val vk : vector_kind` and `val sf : Sample_format.t`.

| Module              | `t`                 | `vk`    | `sf`        |
| ------------------- | ------------------- | ------- | ----------- |
| `Bytes`             | `bytes`             | `Str`   | `` `None `` |
| `U8Bytes`           | `bytes`             | `Str`   | `` `U8 ``   |
| `S16Bytes`          | `bytes`             | `Str`   | `` `S16 ``  |
| `S32Bytes`          | `bytes`             | `Str`   | `` `S32 ``  |
| `FltBytes`          | `bytes`             | `Str`   | `` `Flt ``  |
| `DblBytes`          | `bytes`             | `Str`   | `` `Dbl ``  |
| `U8PlanarBytes`     | `bytes array`       | `P_Str` | `` `U8p ``  |
| `S16PlanarBytes`    | `bytes array`       | `P_Str` | `` `S16p `` |
| `S32PlanarBytes`    | `bytes array`       | `P_Str` | `` `S32p `` |
| `FltPlanarBytes`    | `bytes array`       | `P_Str` | `` `Fltp `` |
| `DblPlanarBytes`    | `bytes array`       | `P_Str` | `` `Dblp `` |
| `FloatArray`        | `float array`       | `Fa`    | `` `Dbl ``  |
| `PlanarFloatArray`  | `float array array` | `P_Fa`  | `` `Dblp `` |
| `U8BigArray`        | `u8ba`              | `Ba`    | `` `U8 ``   |
| `S16BigArray`       | `s16ba`             | `Ba`    | `` `S16 ``  |
| `S32BigArray`       | `s32ba`             | `Ba`    | `` `S32 ``  |
| `FltBigArray`       | `f32ba`             | `Ba`    | `` `Flt ``  |
| `DblBigArray`       | `f64ba`             | `Ba`    | `` `Dbl ``  |
| `U8PlanarBigArray`  | `u8ba array`        | `P_Ba`  | `` `U8p ``  |
| `S16PlanarBigArray` | `s16ba array`       | `P_Ba`  | `` `S16p `` |
| `S32PlanarBigArray` | `s32ba array`       | `P_Ba`  | `` `S32p `` |
| `FltPlanarBigArray` | `f32ba array`       | `P_Ba`  | `` `Fltp `` |
| `DblPlanarBigArray` | `f64ba array`       | `P_Ba`  | `` `Dblp `` |
| `Frame`             | `audio frame`       | `Frm`   | `` `None `` |
| `U8Frame`           | `audio frame`       | `Frm`   | `` `U8 ``   |
| `S16Frame`          | `audio frame`       | `Frm`   | `` `S16 ``  |
| `S32Frame`          | `audio frame`       | `Frm`   | `` `S32 ``  |
| `FltFrame`          | `audio frame`       | `Frm`   | `` `Flt ``  |
| `DblFrame`          | `audio frame`       | `Frm`   | `` `Dbl ``  |
| `U8PlanarFrame`     | `audio frame`       | `Frm`   | `` `U8p ``  |
| `S16PlanarFrame`    | `audio frame`       | `Frm`   | `` `S16p `` |
| `S32PlanarFrame`    | `audio frame`       | `Frm`   | `` `S32p `` |
| `FltPlanarFrame`    | `audio frame`       | `Frm`   | `` `Fltp `` |
| `DblPlanarFrame`    | `audio frame`       | `Frm`   | `` `Dblp `` |

Bigarray type abbreviations, declared between `PlanarFloatArray` and
`U8BigArray`:

```ocaml
type u8ba  = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type s16ba = (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t
type s32ba = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
type f32ba = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
type f64ba = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
```

The typed frame modules differ from `Frame` only by fixing the sample format
given to `create`. The functor is applied by the user; the library ships no
instantiation.

### 4.6 Absent operations

The library exposes no delay query (`swr_get_delay`), no compensation or
timestamp function, no reconfiguration and no explicit close.

## 5. Errors

All errors are `Avutil.Error`, or `Out_of_memory`.

| Raised value                                                                                          | From                                                                                                                                                      |
| ----------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| ``Error (`Failure "Swresample input sample format undefined")``                                       | `create`, OCaml side                                                                                                                                      |
| ``Error (`Failure "Swresample output sample format undefined")``                                      | `create`, OCaml side                                                                                                                                      |
| `Out_of_memory`                                                                                       | record allocation, `swr_alloc`, `av_frame_alloc`                                                                                                          |
| `Error e`, `e` mapped by avutil from the FFmpeg code                                                  | `av_opt_set_*`, `av_channel_layout_copy`, `swr_init`, `av_samples_alloc`, `av_frame_get_buffer`, `swr_convert`, unsupported format for an output bigarray |
| ``Error (`Failure "Cannot use offset with frame data!")``                                             | `convert`, frame input                                                                                                                                    |
| ``Error (`Failure "Swresample failed to convert %d channels : %d channels were expected")``           | `convert`, planar field count or frame channel count                                                                                                      |
| ``Error (`Failure "Swresample failed to convert %s sample format : %s sample format were expected")`` | `convert`, frame input                                                                                                                                    |
| ``Error (`Failure "Invalid offset!")``                                                                | `convert`, every non-frame reader                                                                                                                         |
| ``Error (`Failure "Swresample failed to convert channel %d's … bytes : %d bytes were expected")``     | `convert`, planar readers                                                                                                                                 |
| ``Error (`Failure "Input vector too small!")``                                                        | `convert`, `length` larger than available                                                                                                                 |

After any error from `convert` or `flush` the context remains valid and its
scratch buffers remain owned.

## 6. Blocking and concurrency

- The runtime lock is released around `swr_init` and around `swr_convert`.
  Everything else runs with the lock held, including `swr_alloc`,
  `swr_get_out_samples`, all allocation and all copying.
- While the lock is released the C side reads and writes only memory outside
  the OCaml heap: context-owned scratch buffers, bigarray data, frame buffers.
  The input and output values are rooted for the duration of the call.
- A context has no lock. Two threads using the same context at once operate on
  the same pointer tables and scratch buffers.
- Bigarray and frame inputs are read in place while the lock is released; a
  concurrent writer is visible to the conversion.
- No thread registration. No global state besides the generated tables.

## 7. Callbacks

Nothing. The library registers no callback and makes no C-to-OCaml call other
than raising exceptions through avutil's helpers.

## 8. Data transfer

| Kind           | Input                                                | Output                                                                             |
| -------------- | ---------------------------------------------------- | ---------------------------------------------------------------------------------- |
| `Str`, `P_Str` | copied into the context's input scratch buffer       | converted into the context's output scratch buffer, then copied into fresh `bytes` |
| `Fa`, `P_Fa`   | copied element by element as native doubles, NaN → 0 | scratch buffer, then copied element by element, NaN → 0                            |
| `Ba`, `P_Ba`   | shared: libswresample reads the bigarray memory      | libswresample writes directly into a fresh runtime-owned bigarray                  |
| `Frm`          | shared: libswresample reads the frame's buffers      | libswresample writes directly into a fresh frame's buffers                         |

- Raw byte kinds use the machine's native sample representation; no
  byte-order conversion is done.
- Scratch buffers come from `av_samples_alloc` with alignment argument 0
  (FFmpeg's default alignment). Frame buffers come from
  `av_frame_get_buffer(frame, 0)`.
- Sizes are those of sections 4.4.1 and 4.4.2.
- The output size estimate is `swr_get_out_samples`; the returned value is
  trimmed to what `swr_convert` reports.

## 9. Options

- No option dictionary and no generic AVOption access.
- Options set on the `SwrContext`: `in_chlayout`, `in_sample_fmt`,
  `in_sample_rate`, `out_chlayout`, `out_sample_fmt`, `out_sample_rate`, then
  at most three of `dither_method`, `resampler`, `filter_type` from the
  `?options` list, in list order.
- No unused-option reporting. Unrecognised values are skipped; list elements
  after the third are dropped.

## 10. Version-dependent behaviour

One conditional, in the frame input reader, selects how the channel count of a
frame is read:

| Condition                      | Source of the channel count    |
| ------------------------------ | ------------------------------ |
| `libavcodec < 56.0.100`        | `av_frame_get_channels(frame)` |
| else `libavformat < 59.19.100` | `frame->channels`              |
| otherwise                      | `frame->ch_layout.nb_channels` |

The rest of the library requires the `AVChannelLayout` API, so only the last
branch is reachable in a build that compiles.

## 11. Logic on the OCaml side

- `version` decoding (4.1).
- The 34 `AudioData` modules (4.5): pure constants.
- `Make.create`: sample format resolution (module format first, then
  argument, then failure) and option list to array.
- `from_codec`, `to_codec`, `from_codec_to_codec`: argument plumbing through
  three `Avcodec.Audio` getters.
- `convert` and `flush` are direct externals.
