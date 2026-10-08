# swresample

Audio resampling, rematrixing and sample-format conversion. It follows
[binding-contract.md](binding-contract.md); section numbers match.

## 1. Scope

`Swresample` binds libswresample. It depends on `avutil` (channel layouts,
sample formats, frames, the error exception) and on `avcodec` (audio codec
parameters, for the three constructors that take them).

It installs no C header and provides nothing to other libraries. Module
initialisation reads the version of the libswresample loaded at run time and
cannot fail.

## 2. Objects

### 2.1 Converter — `('i, 'o) ctx`, `Make(I)(O).t`

- **Native object**: one initialised resampling context, with whatever
  working memory the implementation keeps beside it.
- **Creation**: `Make(I)(O).create` and its three variants.
- **Ownership**: the value owns the context and its working memory. It
  retains nothing of the values passed to it or returned by it.
- **Accounting**: SHOULD report its working memory (L9).
- **Release**: by collection only.
- **Guard**: §6.2.
- **State**: none visible, other than the samples libswresample buffers
  between calls.

The type parameters record the input and output data types. They are fixed at
creation.

### 2.2 Values produced

`convert` and `flush` return fresh values (A5). **A value returned by one call
is never written by a later call.** Reusing output storage between calls would
pass any test that consumes each result before the next call.

### 2.3 Data kinds (B3)

A data kind describes how a value of an OCaml type holds audio samples. The
type `'a kind` is abstract: only the modules of §4.5 provide values of it, so
a `t kind` always describes `t` truthfully. A caller cannot define a data
module of its own.

## 3. Enumerations and constants

### 3.1 Generated: resampler options

Three generated variant types ([build.md](build.md) §3.5), each setting one
option of the resampler:

| OCaml type    | Resampler option |
| ------------- | ---------------- |
| `dither_type` | `dither_method`  |
| `engine`      | `resampler`      |
| `filter_type` | `filter_type`    |

`options` is their union. Only the OCaml-to-C direction is used; it is total
(E2).

### 3.2 Data layouts

| Layout               | OCaml data type           | Content                                              |
| -------------------- | ------------------------- | ---------------------------------------------------- |
| interleaved bytes    | `bytes`                   | raw samples, channels interleaved                    |
| planar bytes         | `bytes array`             | one `bytes` of raw samples per channel               |
| interleaved floats   | `float array`             | doubles, channels interleaved                        |
| planar floats        | `float array array`       | one `float array` per channel                        |
| interleaved bigarray | `Bigarray.Array1.t`       | channels interleaved, element type per sample format |
| planar bigarray      | `Bigarray.Array1.t array` | one bigarray per channel                             |
| frame                | `audio frame`             | whatever the frame holds                             |

- Raw samples in `bytes` use the machine's native representation of the
  sample format. No byte-order conversion is done.
- Channel order is the order of the channel layout.
- The bigarray element kind of a sample format is given by
  [avutil.md](avutil.md) §3.2.

Worked layout, stereo, signed 16-bit, three samples per channel `L0 L1 L2` and
`R0 R1 R2`, each sample two bytes:

| Layout               | Value                                                |
| -------------------- | ---------------------------------------------------- |
| interleaved bytes    | 12 bytes `L0 R0 L1 R1 L2 R2`                         |
| planar bytes         | array of two: 6 bytes `L0 L1 L2`, 6 bytes `R0 R1 R2` |
| interleaved floats   | `l0 r0 l1 r1 l2 r2`                                  |
| planar floats        | array of two float arrays: `l0 l1 l2`, `r0 r1 r2`    |
| interleaved bigarray | dimension 6: `L0 R0 L1 R1 L2 R2`                     |
| planar bigarray      | two bigarrays of dimension 3                         |

## 4. Operations

### 4.1 `version`

```ocaml
val version : version
```

The libswresample loaded at run time, read once at module load.

### 4.2 Data kinds

```ocaml
type 'a kind
module type AudioData = sig
  type t
  val kind : t kind
end
```

§2.3. A kind carries a layout of §3.2 and either a sample format or the fact
that the format is given at creation.

### 4.3 `options`, `ctx`

```ocaml
type options = [ dither_type | engine | filter_type ]
type ('i, 'o) ctx
```

### 4.4 `Make (I : AudioData) (O : AudioData)`

```ocaml
type t = (I.t, O.t) ctx
```

#### `create`

```ocaml
val create :
  ?options:options list -> Channel_layout.t -> ?in_sample_format:Sample_format.t ->
  int -> Channel_layout.t -> ?out_sample_format:Sample_format.t -> int -> t
```

Arguments in order: options, input channel layout, input sample format, input
sample rate, output channel layout, output sample format, output sample rate.

**Sample format of a side.** For each of the input and the output:

| The data kind…         | `?…_sample_format` omitted | given                                     |
| ---------------------- | -------------------------- | ----------------------------------------- |
| fixes a format         | that format                | it must equal that format, else a failure |
| leaves the format open | a failure                  | that format                               |

A format given to interleaved bytes must be a packed format, else a failure.

**Other arguments.**

- Each sample rate must be positive, else a failure.
- Each layout holds at most 64 channels, else a failure.
- Every element of `options` is applied, in list order. A later element of
  the same type overrides an earlier one.
- Both layouts are copied; the converter keeps no reference to the values.
- Every other setting of the resampler keeps FFmpeg's default.

Any failure, of validation, of a setting FFmpeg rejects or of the resampler's
initialisation, releases everything and raises (L2).

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

Each is `create` with the layout, sample format and sample rate of a side
taken from codec parameters. The format taken from the parameters is given as
that side's sample format, with the rule above: a data kind that fixes
another format makes the creation fail.

#### `convert`

```ocaml
val convert : ?offset:int -> ?length:int -> t -> I.t -> O.t
```

Converts samples of the input value and returns what the resampler produces.

**The range.** `offset` and `length` count samples per channel. `offset`
defaults to 0 and `length` to everything after `offset`. They select the
samples `offset` to `offset + length - 1` of every channel, for every input
kind, frames included. With `n` the number of samples per channel the value
holds, a failure is raised when `offset < 0`, `length < 0` or
`offset + length > n`.

**The input value** is checked against the converter's input side:

| Kind                         | `n`                                         | Failure when                                                           |
| ---------------------------- | ------------------------------------------- | ---------------------------------------------------------------------- |
| interleaved bytes            | byte length / (bytes per sample × channels) |                                                                        |
| planar bytes                 | byte length of a plane / bytes per sample   | the number of planes is not the channel count; planes differ in length |
| interleaved floats, bigarray | element count / channels                    |                                                                        |
| planar floats, bigarray      | element count of a plane                    | the number of planes is not the channel count; planes differ in length |
| frame                        | the frame's sample count                    | the frame's channel count or sample format is not the converter's      |

A trailing partial frame of an interleaved value is ignored. A frame's sample
rate and the arrangement of its layout are not compared: they are the
caller's to match.

**The output value** holds exactly the samples the resampler produced, `r`
per channel:

| Kind                 | Result                                                                           |
| -------------------- | -------------------------------------------------------------------------------- |
| interleaved bytes    | `bytes` of `r × channels × bytes per sample`                                     |
| planar bytes         | one `bytes` of `r × bytes per sample` per channel                                |
| interleaved floats   | `float array` of `r × channels` elements                                         |
| planar floats        | one `float array` of `r` elements per channel                                    |
| interleaved bigarray | a bigarray of `r × channels` elements                                            |
| planar bigarray      | one bigarray of `r` elements per channel                                         |
| frame                | a frame of `r` samples with the output layout, format and rate, and no timestamp |

`r` may be 0. The result is then an empty value of the same shape: empty
planes, or a frame of zero samples.

**NaN.** A NaN read from a float array, and a NaN written to one, becomes 0.
A NaN that entered a filter or an encoder would poison everything after it.
Bigarrays and frames are shared memory and are not scanned.

On failure the converter stays usable.

#### `flush`

```ocaml
val flush : t -> O.t
```

Returns the samples the resampler still holds: the tail a rate conversion
keeps back until it knows no more input follows. After `flush`, the samples
returned by all calls since creation are everything the input produces.

- A second `flush` with no conversion in between returns an empty value.
- `flush` does not end the converter: `convert` may be called again.

### 4.5 Predefined data modules

Each module has `type t` as below and `val kind : t kind`.

| Module              | `t`                 | Layout               | Sample format |
| ------------------- | ------------------- | -------------------- | ------------- |
| `Bytes`             | `bytes`             | interleaved bytes    | open          |
| `U8Bytes`           | `bytes`             | interleaved bytes    | `` `U8 ``     |
| `S16Bytes`          | `bytes`             | interleaved bytes    | `` `S16 ``    |
| `S32Bytes`          | `bytes`             | interleaved bytes    | `` `S32 ``    |
| `FltBytes`          | `bytes`             | interleaved bytes    | `` `Flt ``    |
| `DblBytes`          | `bytes`             | interleaved bytes    | `` `Dbl ``    |
| `U8PlanarBytes`     | `bytes array`       | planar bytes         | `` `U8p ``    |
| `S16PlanarBytes`    | `bytes array`       | planar bytes         | `` `S16p ``   |
| `S32PlanarBytes`    | `bytes array`       | planar bytes         | `` `S32p ``   |
| `FltPlanarBytes`    | `bytes array`       | planar bytes         | `` `Fltp ``   |
| `DblPlanarBytes`    | `bytes array`       | planar bytes         | `` `Dblp ``   |
| `FloatArray`        | `float array`       | interleaved floats   | `` `Dbl ``    |
| `PlanarFloatArray`  | `float array array` | planar floats        | `` `Dblp ``   |
| `U8BigArray`        | `u8ba`              | interleaved bigarray | `` `U8 ``     |
| `S16BigArray`       | `s16ba`             | interleaved bigarray | `` `S16 ``    |
| `S32BigArray`       | `s32ba`             | interleaved bigarray | `` `S32 ``    |
| `FltBigArray`       | `f32ba`             | interleaved bigarray | `` `Flt ``    |
| `DblBigArray`       | `f64ba`             | interleaved bigarray | `` `Dbl ``    |
| `U8PlanarBigArray`  | `u8ba array`        | planar bigarray      | `` `U8p ``    |
| `S16PlanarBigArray` | `s16ba array`       | planar bigarray      | `` `S16p ``   |
| `S32PlanarBigArray` | `s32ba array`       | planar bigarray      | `` `S32p ``   |
| `FltPlanarBigArray` | `f32ba array`       | planar bigarray      | `` `Fltp ``   |
| `DblPlanarBigArray` | `f64ba array`       | planar bigarray      | `` `Dblp ``   |
| `Frame`             | `audio frame`       | frame                | open          |
| `U8Frame`           | `audio frame`       | frame                | `` `U8 ``     |
| `S16Frame`          | `audio frame`       | frame                | `` `S16 ``    |
| `S32Frame`          | `audio frame`       | frame                | `` `S32 ``    |
| `FltFrame`          | `audio frame`       | frame                | `` `Flt ``    |
| `DblFrame`          | `audio frame`       | frame                | `` `Dbl ``    |
| `U8PlanarFrame`     | `audio frame`       | frame                | `` `U8p ``    |
| `S16PlanarFrame`    | `audio frame`       | frame                | `` `S16p ``   |
| `S32PlanarFrame`    | `audio frame`       | frame                | `` `S32p ``   |
| `FltPlanarFrame`    | `audio frame`       | frame                | `` `Fltp ``   |
| `DblPlanarFrame`    | `audio frame`       | frame                | `` `Dblp ``   |

Bigarray type abbreviations, part of the interface:

```ocaml
type u8ba  = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type s16ba = (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t
type s32ba = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
type f32ba = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
type f64ba = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
```

The functor is applied by the user; the library ships no instantiation.

## 5. Errors

| Raised                                    | By                                                                                                                      |
| ----------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| ``Error (`Failure msg)``                  | `create`: sample format missing, conflicting or not packed for bytes; a rate below 1; a layout of more than 64 channels |
| ``Error (`Failure msg)``                  | `convert`: range, shape of the input value, frame mismatch                                                              |
| `Error e`, `e` mapped from an FFmpeg code | a setting FFmpeg rejects, the resampler's initialisation, a conversion                                                  |
| `Out_of_memory`                           | a failed allocation (F3)                                                                                                |

No operation raises `Not_found`. After any error from `convert` or `flush`
the converter is valid.

## 6. Blocking and concurrency

### 6.1 The runtime lock

Contract M1 covers the resampler's initialisation and every conversion and
flush. Bytes and float arrays live on the OCaml heap, so their samples are
copied out before the lock is released and copied in after it is retaken
(M2). Bigarray and frame data are outside the OCaml heap and are read and
written in place.

### 6.2 Guards

`convert` and `flush` take the converter's guard exclusively.

A bigarray given as input is read in place while other threads run: a
concurrent writer's bytes are visible to the conversion.

### 6.3 Global state

None.

## 7. Callbacks

None of either kind.

## 8. Data transfer

| Kind                      | Input                                         | Output                                              |
| ------------------------- | --------------------------------------------- | --------------------------------------------------- |
| bytes, planar bytes       | copied                                        | copied into fresh `bytes`                           |
| floats, planar floats     | copied, NaN to 0                              | copied into fresh float arrays, NaN to 0            |
| bigarray, planar bigarray | shared: read in place                         | written directly into fresh runtime-owned bigarrays |
| frame                     | shared: the frame's buffers are read in place | written directly into a fresh frame's buffers       |

The number of samples a conversion will produce is known only after it ran.
How the implementation sizes its output beforehand is its own business; the
value returned has exactly the produced length (S2).

## 9. Options

No option table. The resampler is configured by the typed arguments of
`create`, which all take effect or fail (contract O2), and by the `options`
list (§3.1).

## 10. Version-dependent behaviour

None.

## 11. Composite operations

`from_codec`, `to_codec`, `from_codec_to_codec` are `create` with one or both
sides read from codec parameters through
`Avcodec.Audio.get_channel_layout`, `get_sample_format` and
`get_sample_rate`.
