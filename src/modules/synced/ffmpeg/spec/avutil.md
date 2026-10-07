# avutil

The base library. It follows [binding-contract.md](binding-contract.md);
section numbers match.

## 1. Scope

### 1.1 What it binds

`Avutil` binds libavutil: errors, logging, rationals, channel layouts, sample
and pixel formats, colour properties, frames, option introspection and reads,
hardware device and frame contexts, expression evaluation.

It also binds the subtitle structure of libavcodec (`AVSubtitle`) and the
constant `FF_QP2LAMBDA`, so it compiles and links against libavcodec
([build.md](build.md) §1).

It depends on no binding library. Every other binding library depends on it.

### 1.2 Module initialisation

Loading the module:

1. reads the version of the libavutil loaded at run time into `version`;
2. reads `FF_QP2LAMBDA` into `qp2lambda`;
3. registers a printer so that an uncaught `Error e` prints as
   `Avutil.Error(` followed by `string_of_error e` and `)`;
4. builds `Channel_layout.standard_layouts`;
5. builds `Channel_layout.mono`, `stereo` and `five_point_one`.

Initialisation fails only when FFmpeg knows no layout named `mono`, `stereo`
or `5.1` (rule I2).

### 1.3 What it provides to dependent libraries

Through its installed C header ([build.md](build.md) §4) the library gives the
stubs of its dependents:

| Service                       | Contract                                                                                                                                                  |
| ----------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| raise an FFmpeg error         | Takes a negative FFmpeg code and raises `Error e`, `e` per §5.1. Does not return. Needs the runtime lock.                                                 |
| raise a binding failure       | Takes a formatted message and raises ``Error (`Failure msg)``. The message storage belongs to the call. Does not return. Needs the lock.                  |
| option dictionary: fill       | §9.1 step 2. Allocates nothing on the OCaml heap.                                                                                                         |
| option dictionary: report     | §9.1 step 4. Frees the dictionary.                                                                                                                        |
| thread registration           | §6.3.                                                                                                                                                     |
| rationals                     | Both directions between `rational` and `AVRational`.                                                                                                      |
| time formats                  | Units per second of a `Time_format.t` (§3.2).                                                                                                             |
| channel layouts               | The native layout of a `Channel_layout.t`; a constructor that wraps a **copy** of a native layout (§2.2).                                                 |
| enumerations                  | Both conversions for sample format, pixel format, the five colour enumerations, hardware device type and media type, with the rules of the contract's §3. |
| sample format to element kind | §3.2.                                                                                                                                                     |
| frames                        | The native frame of a frame value; a constructor that wraps a native frame and **takes ownership** of it (§2.1).                                          |
| subtitles                     | The native subtitle of a subtitle value; a constructor that wraps a native subtitle and takes ownership of it (§2.3).                                     |
| hardware contexts             | The native buffer reference of a device or frame context value.                                                                                           |
| option classes and objects    | Constructors for `Options.t` and `Options.obj` (§2.5).                                                                                                    |

The two wrapping constructors raise ``Error (`Failure msg)`` when given a null
pointer.

## 2. Objects

### 2.1 Frame — `'media Frame.t` (= `'media frame`)

- **Native object**: one `AVFrame`.
- **Creation**: `Audio.create_frame`, `Video.create_frame`, and dependent
  libraries through the wrapping constructor (decoders, filters, converters).
- **Ownership**: the value owns the `AVFrame`. One value per `AVFrame`. The
  data buffers are reference counted by FFmpeg and may be shared with other
  frames.
- **Accounting**: the value reports the total size of the frame's data
  buffers at the time it is wrapped (L9).
- **Release**: by collection only. No closed state.
- **Concurrent use**: §6.2.
- **Phantom parameter**: `'media` is `audio` or `video`. Every producer of
  frames gives the parameter that matches the content.
- **Dependents**: the plane bigarrays of `Video.frame_visit` (§8.2).

### 2.2 Channel layout — `Channel_layout.t`

- **Native object**: one `AVChannelLayout`.
- **Creation**: always by copy of a source layout. A copy failure releases
  the allocation and raises.
- **Ownership**: the value owns its copy, including the channel map of a
  custom-order layout, and shares nothing with its source.
- **Release**: by collection only.
- Immutable from OCaml. No guard.

Every temporary native layout the binding fills on the way to a value is
initialised before FFmpeg writes it and released after it is copied, on
success and on failure.

### 2.3 Subtitle — `Subtitle.frame`

- **Native object**: one `AVSubtitle`, with its rectangles and each
  rectangle's planes, text and ASS line.
- **Creation**: `Subtitle.create_frame`, and dependent libraries through the
  wrapping constructor (subtitle decoding).
- **Ownership**: the value owns all of it.
- **Release**: by collection only. The release MUST be correct for a subtitle
  whose creation failed half-way (L7).
- Immutable from OCaml after creation. No guard.

### 2.4 Hardware contexts — `HwContext.device_context`, `HwContext.frame_context`

- **Native object**: one buffer reference each, to an `AVHWDeviceContext` or
  an `AVHWFramesContext`.
- **Creation**: `HwContext.create_device_context`,
  `HwContext.create_frame_context`.
- **Ownership**: the value owns one reference. A frame context holds FFmpeg's
  own reference to its device context, so it stays valid when the device
  context value is collected. An encoder given either context takes a
  reference of its own.
- **Release**: by collection only.
- Immutable from OCaml. No guard.

### 2.5 Borrowed and dependent handles

| Type                      | Designates                                              | Lifetime                                                   |
| ------------------------- | ------------------------------------------------------- | ---------------------------------------------------------- |
| `Options.t`               | an FFmpeg option class, or no class                     | static                                                     |
| `Pixel_format.descriptor` | a pixel-format descriptor, copied into a private record | plain data                                                 |
| `Options.obj`             | an option-bearing native object inside an owner         | dependent: keeps its owner alive (L3) and shares its guard |

`Avutil` creates no `Options.t` and no `Options.obj`; dependent libraries do.
An `Options.t` with no class lists no option.

### 2.6 Types with no native object

`input`, `output`, `'a container`, `('line, 'media) format` are abstract types
declared here and given meaning by dependent libraries. `audio`, `video`,
`subtitle` are the single-constructor variant types used as phantom
parameters. `data` is a one-dimensional unsigned-8-bit C-layout bigarray.
`rational`, `version`, `value`, `opts` are plain OCaml data.

### 2.7 Where the types do not protect (B3)

| Place                                  | Run-time check                              |
| -------------------------------------- | ------------------------------------------- |
| `Subtitle.pict.planes`                 | array lengths and plane sizes (§4.14)       |
| `Video.frame_visit` on any video frame | the frame holds software pixel data (§4.13) |

## 3. Enumerations and constants

### 3.1 Generated tables

| Table                   | OCaml type               | Used for                            |
| ----------------------- | ------------------------ | ----------------------------------- |
| pixel format            | `Pixel_format.t`         | both directions                     |
| pixel format flag       | `Pixel_format.flag`      | bit mask to list                    |
| colour space            | `Color_space.t`          | both directions                     |
| colour range            | `Color_range.t`          | both directions                     |
| colour primaries        | `Color_primaries.t`      | both directions                     |
| transfer characteristic | `Color_trc.t`            | both directions                     |
| chroma location         | `Chroma_location.t`      | both directions                     |
| sample format           | `Sample_format.t`        | both directions                     |
| channel layout names    | `Channel_layout.layout`  | the type only; no operation uses it |
| hardware device type    | `HwContext.device_type`  | OCaml to C                          |
| subtitle type           | `Subtitle.subtitle_type` | both directions                     |
| subtitle flag           | `Subtitle.subtitle_flag` | both directions, bit mask           |
| media type              | `media_type`             | C to OCaml, for dependent libraries |

### 3.2 Hand-written tables

**Log levels** (`Log.level` to FFmpeg's level):

| level          | constant         |
| -------------- | ---------------- |
| `` `Quiet ``   | `AV_LOG_QUIET`   |
| `` `Panic ``   | `AV_LOG_PANIC`   |
| `` `Fatal ``   | `AV_LOG_FATAL`   |
| `` `Error ``   | `AV_LOG_ERROR`   |
| `` `Warning `` | `AV_LOG_WARNING` |
| `` `Info ``    | `AV_LOG_INFO`    |
| `` `Verbose `` | `AV_LOG_VERBOSE` |
| `` `Debug ``   | `AV_LOG_DEBUG`   |
| `` `Trace ``   | `AV_LOG_TRACE`   |

**Time formats** (`Time_format.t` to units per second): `` `Second `` 1,
`` `Millisecond `` 1000, `` `Microsecond `` 1000000, `` `Nanosecond `` 1000000000.

**Sample format to bigarray element kind**, for dependent libraries. The
packed and planar variants of a format map alike:

| sample format | element kind     |
| ------------- | ---------------- |
| U8, U8P       | unsigned 8-bit   |
| S16, S16P     | signed 16-bit    |
| S32, S32P     | 32-bit integer   |
| S64, S64P     | 64-bit integer   |
| FLT, FLTP     | 32-bit float     |
| DBL, DBLP     | 64-bit float     |
| any other     | a failure (§5.2) |

**Errors**: §5.1.

**Option flags** (`Options.flag` to FFmpeg's flag):

| flag                   | constant                      |
| ---------------------- | ----------------------------- |
| `` `Encoding_param ``  | `AV_OPT_FLAG_ENCODING_PARAM`  |
| `` `Decoding_param ``  | `AV_OPT_FLAG_DECODING_PARAM`  |
| `` `Audio_param ``     | `AV_OPT_FLAG_AUDIO_PARAM`     |
| `` `Video_param ``     | `AV_OPT_FLAG_VIDEO_PARAM`     |
| `` `Subtitle_param ``  | `AV_OPT_FLAG_SUBTITLE_PARAM`  |
| `` `Export ``          | `AV_OPT_FLAG_EXPORT`          |
| `` `Readonly ``        | `AV_OPT_FLAG_READONLY`        |
| `` `Bsf_param ``       | `AV_OPT_FLAG_BSF_PARAM`       |
| `` `Runtime_param ``   | `AV_OPT_FLAG_RUNTIME_PARAM`   |
| `` `Filtering_param `` | `AV_OPT_FLAG_FILTERING_PARAM` |
| `` `Deprecated ``      | `AV_OPT_FLAG_DEPRECATED`      |
| `` `Child_consts ``    | `AV_OPT_FLAG_CHILD_CONSTS`    |

**Option types** (FFmpeg's option type, after the array flag is removed, to
the constructor of `Options.ground`):

| FFmpeg type              | Constructor           | Default                                                          | Minimum and maximum  |
| ------------------------ | --------------------- | ---------------------------------------------------------------- | -------------------- |
| `AV_OPT_TYPE_FLAGS`      | `` `Flags ``          | the integer default                                              | saturated to 64 bits |
| `AV_OPT_TYPE_INT`        | `` `Int ``            | the integer default                                              | as OCaml integers    |
| `AV_OPT_TYPE_INT64`      | `` `Int64 ``          | the integer default                                              | saturated            |
| `AV_OPT_TYPE_UINT64`     | `` `UInt64 ``         | the integer default                                              | saturated            |
| `AV_OPT_TYPE_DURATION`   | `` `Duration ``       | the integer default                                              | saturated            |
| `AV_OPT_TYPE_DOUBLE`     | `` `Double ``         | the floating default                                             | as given             |
| `AV_OPT_TYPE_FLOAT`      | `` `Float ``          | the floating default                                             | as given             |
| `AV_OPT_TYPE_RATIONAL`   | `` `Rational ``       | the floating default as the nearest rational                     | likewise             |
| `AV_OPT_TYPE_STRING`     | `` `String ``         | the string default when present                                  | none                 |
| `AV_OPT_TYPE_BINARY`     | `` `Binary ``         | the string default when present                                  | none                 |
| `AV_OPT_TYPE_DICT`       | `` `Dict ``           | the string default when present                                  | none                 |
| `AV_OPT_TYPE_IMAGE_SIZE` | `` `Image_size ``     | the string default when present                                  | none                 |
| `AV_OPT_TYPE_VIDEO_RATE` | `` `Video_rate ``     | the string default when present                                  | none                 |
| `AV_OPT_TYPE_COLOR`      | `` `Color ``          | the string default when present                                  | none                 |
| `AV_OPT_TYPE_PIXEL_FMT`  | `` `Pixel_fmt ``      | the integer default converted, when it names a format            | none                 |
| `AV_OPT_TYPE_SAMPLE_FMT` | `` `Sample_fmt ``     | the integer default converted, when it names a format            | none                 |
| `AV_OPT_TYPE_CHLAYOUT`   | `` `Channel_layout `` | the string default parsed as a layout, when present and valid    | none                 |
| `AV_OPT_TYPE_BOOL`       | `` `Bool ``           | the integer default, non-zero is `true`, when it is not negative | none                 |
| `AV_OPT_TYPE_CONST`      | not an option         | a named value of another option (§4.15)                          |                      |
| any other                | not listed            |                                                                  |                      |

FFmpeg stores every bound as a floating-point number. "Saturated" means: a
bound at or below the smallest 64-bit integer becomes that integer, a bound at
or above the largest becomes that one, any other is truncated. "none" means
the field is `None`. A default that does not meet its condition is `None`.
FFmpeg's "none" format names no format: a pixel-format or sample-format
option whose default is "none" has the default `None`.

### 3.3 Parameters

| Name                | Recommended | Meaning                                                                                                  |
| ------------------- | ----------- | -------------------------------------------------------------------------------------------------------- |
| `LOG_LINE_MAX`      | 1024 bytes  | Longest log message delivered; longer ones are truncated. It bounds the memory one queued message holds. |
| `VIDEO_FRAME_ALIGN` | 32          | Buffer alignment of frames made by `Video.create_frame`. Visible in their line sizes.                    |

A subtitle rectangle has exactly 4 plane slots. Plane 1 of a bitmap rectangle
is its palette, of `AVPALETTE_SIZE` bytes.

## 4. Operations

### 4.1 Line, container, media types, format

```ocaml
type input
type output
type 'a container
type audio = [ `Audio ]
type video = [ `Video ]
type subtitle = [ `Subtitle ]
type media_type = Media_types.t
type ('line, 'media) format
```

Type declarations only.

### 4.2 Version

```ocaml
type version = { major : int; minor : int; micro : int }
val version : version
val version_string : version -> string
```

- `version`: the libavutil loaded at run time, read once at module load.
- `version_string v` is `major.minor.micro` in decimal.

### 4.3 Frame

```ocaml
module Frame : sig
  type 'media t
  val pts : _ t -> Int64.t option
  val set_pts : _ t -> Int64.t option -> unit
  val duration : _ t -> Int64.t option
  val set_duration : _ t -> Int64.t option -> unit
  val pkt_dts : _ t -> Int64.t option
  val set_pkt_dts : _ t -> Int64.t option -> unit
  val metadata : _ t -> (string * string) list
  val set_metadata : _ t -> (string * string) list -> unit
  val best_effort_timestamp : _ t -> Int64.t option
end
type 'media frame = 'media Frame.t
```

| Function                | Behaviour                                                                                           |
| ----------------------- | --------------------------------------------------------------------------------------------------- |
| `pts`                   | `None` when the frame's timestamp is `AV_NOPTS_VALUE`, else `Some`.                                 |
| `set_pts`               | Writes the timestamp, `AV_NOPTS_VALUE` for `None`, and the same value as the best-effort timestamp. |
| `duration`              | `None` when the frame's duration is 0, else `Some`.                                                 |
| `set_duration`          | Writes the duration, 0 for `None`.                                                                  |
| `pkt_dts`               | `None` for `AV_NOPTS_VALUE`, else `Some`.                                                           |
| `set_pkt_dts`           | Writes the value, `AV_NOPTS_VALUE` for `None`.                                                      |
| `best_effort_timestamp` | `None` for `AV_NOPTS_VALUE`, else `Some`.                                                           |

After `set_pts`, every timestamp accessor of the frame that FFmpeg components
read agrees with the value set. FFmpeg fills the best-effort timestamp only
when decoding, and its components read that one.

`metadata frame` returns the frame's metadata as `(key, value)` pairs in the
dictionary's order.

`set_metadata frame l` replaces the whole metadata of the frame with `l`.

- Entries present before and absent from `l` are gone.
- A key that appears twice in `l` keeps its last value.
- An empty list leaves the frame with no metadata.
- On failure the frame's metadata is unchanged and nothing leaks.

### 4.4 Exception

```ocaml
type error = [ `Bsf_not_found | `Decoder_not_found | `Demuxer_not_found
  | `Encoder_not_found | `Eof | `Exit | `Filter_not_found | `Invalid_data
  | `Muxer_not_found | `Option_not_found | `Patch_welcome
  | `Protocol_not_found | `Stream_not_found | `Bug | `Eagain | `Unknown
  | `Experimental | `Other of int | `Failure of string ]
exception Error of error
val string_of_error : error -> string
```

- `error`, `Error`: §5.
- `string_of_error e`: for `` `Failure s `` returns `s`. For every other
  constructor, the text FFmpeg gives for the corresponding code (§5.1).

### 4.5 Expression evaluation, data, rational, constants

```ocaml
val expr_parse_and_eval : string -> float
type data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
val create_data : int -> data
type rational = { num : int; den : int }
val string_of_rational : rational -> string
val qp2lambda : int
```

- `expr_parse_and_eval s` evaluates `s` with FFmpeg's expression evaluator,
  with no variable and no user function. A parse error raises the mapped
  error and writes nothing to the log.
- `create_data len` returns a fresh bigarray of `len` bytes owned by the OCaml
  runtime. Its content is unspecified. A negative `len` raises a failure.
- `string_of_rational {num; den}` is `num/den` in decimal.
- `qp2lambda` is `FF_QP2LAMBDA`.

### 4.6 Timestamp

```ocaml
module Time_format : sig
  type t = [ `Second | `Millisecond | `Microsecond | `Nanosecond ]
end
val time_base : unit -> rational
```

- `Time_format.t` selects the unit of durations and positions in the
  dependent libraries.
- `time_base ()` returns FFmpeg's internal time base, `AV_TIME_BASE_Q`.

### 4.7 Logging

```ocaml
module Log : sig
  type level = [ `Quiet | `Panic | `Fatal | `Error | `Warning | `Info
               | `Verbose | `Debug | `Trace ]
  val set_level : level -> unit
  val set_callback : (string -> unit) -> unit
  val clear_callback : unit -> unit
end
```

- `set_level l` sets FFmpeg's process-wide log level (§3.2).
- `set_callback`, `clear_callback`: §7.1.

### 4.8 Channel layout

```ocaml
module Channel_layout : sig
  type layout = Channel_layout.t
  type t
  val standard_layouts : t list
  val stereo : t
  val mono : t
  val five_point_one : t
  val compare : t -> t -> bool
  val find : string -> t
  val get_description : t -> string
  val get_nb_channels : t -> int
  val get_default : int -> t
  val get_mask : t -> int64 option

end
```

- `layout` is the generated variant of FFmpeg's named layout masks. No
  operation takes or returns it.
- `standard_layouts`: every layout FFmpeg's standard-layout iteration yields,
  in its order, each an independent copy. Built once at module load.
- `mono`, `stereo`, `five_point_one`: the layouts named `mono`, `stereo` and
  `5.1`.
- `compare a b` is an equality test, despite its name: `true` when FFmpeg
  finds the two layouts equal (`av_channel_layout_compare`), `false` when
  they differ. An invalid layout raises the mapped error.
- `find name` parses `name` as FFmpeg does
  (`av_channel_layout_from_string`). A name FFmpeg does not accept raises
  `Not_found`.
- `get_description l` returns FFmpeg's description of the layout, in full
  whatever its length.
- `get_nb_channels l` returns the channel count.
- `get_default n` returns FFmpeg's default layout for `n` channels. It raises
  `Not_found` when FFmpeg has no standard layout of `n` channels, and for
  `n < 1`.
- `get_mask l` returns `Some` of the channel mask when the layout has native
  order, else `None`.

### 4.9 Sample format

```ocaml
module Sample_format : sig
  type t = Sample_format.t
  val get_name : t -> string option
  val find : string -> t
  val get_id : t -> int
  val find_id : int -> t
end
```

- `get_name f`: FFmpeg's name of the format; `None` for `` `None `` and for a
  format FFmpeg does not name.
- `find name`: the format FFmpeg names `name`. An unknown name raises
  `Not_found`.
- `get_id f`: the integer value of the C enumeration member.
- `find_id n`: the format whose C value is `n`. A value with no constructor
  raises `Not_found`.

### 4.10 Colour properties

```ocaml
module Color_space      : sig type t = Color_space.t
  val name : t -> string  val from_name : string -> t option end
module Color_range      : sig type t = Color_range.t
  val name : t -> string  val from_name : string -> t option end
module Color_primaries  : sig type t = Color_primaries.t
  val name : t -> string  val from_name : string -> t option end
module Color_trc        : sig type t = Color_trc.t
  val name : t -> string  val from_name : string -> t option end
module Chroma_location  : sig type t = Chroma_location.t
  val name : t -> string  val from_name : string -> t option end
```

| Module            | Naming functions of FFmpeg                                |
| ----------------- | --------------------------------------------------------- |
| `Color_space`     | `av_color_space_name`, `av_color_space_from_name`         |
| `Color_range`     | `av_color_range_name`, `av_color_range_from_name`         |
| `Color_primaries` | `av_color_primaries_name`, `av_color_primaries_from_name` |
| `Color_trc`       | `av_color_transfer_name`, `av_color_transfer_from_name`   |
| `Chroma_location` | `av_chroma_location_name`, `av_chroma_location_from_name` |

- `name v` returns FFmpeg's name of `v`, and the empty string when FFmpeg has
  none (A3).
- `from_name s` returns `None` when FFmpeg does not know `s`, and when it
  knows it as a value the type has no constructor for. Otherwise `Some`.
- A string that is exactly the name of a constructor converts to that
  constructor, whatever FFmpeg's own lookup answers: FFmpeg may match a name
  by its prefix and return another value.
- For every constructor `v` that FFmpeg names, `from_name (name v) = Some v'`
  where `v'` has the same C value as `v`.

### 4.11 Pixel format

```ocaml
module Pixel_format : sig
  type t = Pixel_format.t
  type flag = Pixel_format_flag.t
  type component_descriptor = {
    plane : int; step : int; offset : int; shift : int; depth : int }
  type descriptor = private {
    name : string; nb_components : int;
    log2_chroma_w : int; log2_chroma_h : int;
    flags : flag list; comp : component_descriptor list;
    alias : string option }
  val descriptor : t -> descriptor
  val bits : descriptor -> int
  val planes : t -> int
  val to_string : t -> string option
  val of_string : string -> t
  val get_id : t -> int
  val find_id : int -> t
end
```

- `descriptor f` returns FFmpeg's descriptor of the format
  (`av_pix_fmt_desc_get`). A format with no descriptor, `` `None `` among
  them, raises `Not_found`.
  - `flags`: the flags set in the descriptor (E6).
  - `comp`: one entry per component, `nb_components` entries, in order.
  - `alias`: `None` when the descriptor has none.
- `bits d` is the number of bits per pixel of the format `d` describes
  (`av_get_bits_per_pixel`).
- `planes f` is the number of planes of the format
  (`av_pix_fmt_count_planes`). A format with no descriptor raises the mapped
  error.
- `to_string f`: FFmpeg's name; `None` for `` `None `` and for a format
  FFmpeg does not name.
- `of_string s`: the format FFmpeg names `s`. An unknown name raises
  ``Error (`Failure msg)``.
- `get_id f`: the integer value of the C enumeration member.
- `find_id n`: the format whose C value is `n`. A value with no constructor
  raises `Not_found`.

### 4.12 Audio frames

```ocaml
module Audio : sig
  val create_frame :
    Sample_format.t -> Channel_layout.t -> int -> int -> audio frame
  val frame_get_sample_format : audio frame -> Sample_format.t
  val frame_get_sample_rate : audio frame -> int
  val frame_get_channels : audio frame -> int
  val frame_get_channel_layout : audio frame -> Channel_layout.t
  val frame_nb_samples : audio frame -> int
end
```

`create_frame sample_format channel_layout sample_rate nb_samples` returns a
new frame with that format, a copy of that layout, that rate, and buffers for
`nb_samples` samples per channel allocated by FFmpeg with its default
alignment. Sample data is unspecified. A sample count below 1 raises a
failure. Any FFmpeg failure releases the frame and raises.

| Function                   | Returns                                   |
| -------------------------- | ----------------------------------------- |
| `frame_get_sample_format`  | the frame's sample format (E3)            |
| `frame_get_sample_rate`    | the frame's sample rate                   |
| `frame_get_channels`       | the channel count of the frame's layout   |
| `frame_get_channel_layout` | an independent copy of the frame's layout |
| `frame_nb_samples`         | the number of samples per channel         |

### 4.13 Video frames

```ocaml
module Video : sig
  type planes = (data * int) array
  val create_frame : int -> int -> Pixel_format.t -> video frame
  val frame_get_linesize : video frame -> int -> int
  val frame_visit :
    make_writable:bool -> (planes -> unit) -> video frame -> video frame
  val frame_get_width : video frame -> int
  val frame_get_height : video frame -> int
  val frame_get_pixel_format : video frame -> Pixel_format.t
  val frame_get_pixel_aspect : video frame -> rational option
  val frame_get_color_space : video frame -> Color_space.t
  val frame_get_color_range : video frame -> Color_range.t
  val frame_get_color_primaries : video frame -> Color_primaries.t
  val frame_get_color_trc : video frame -> Color_trc.t
  val frame_get_chroma_location : video frame -> Chroma_location.t
end
```

`create_frame width height pixel_format` returns a new frame of that geometry
and format, with buffers allocated by FFmpeg with alignment
`VIDEO_FRAME_ALIGN`. Pixel data is unspecified. A width or height below 1
raises a failure. Any FFmpeg failure releases the frame and raises.

`frame_get_linesize frame n` returns the line size of plane `n`. It raises a
failure when `n` is not the index of a plane the frame holds.

`frame_visit ~make_writable f frame`:

1. Raises a failure when the frame holds no software pixel data (a hardware
   frame), or a plane with a negative line size.
2. When `make_writable` is true, makes the frame writable
   (`av_frame_make_writable`): its buffers are then private to this frame.
3. Builds one `(data, linesize)` pair per plane of the pixel format
   (`av_pix_fmt_count_planes`), per §8.2.
4. Calls `f planes` (§7.2).
5. Returns `frame`.

The palette of a paletted format is not a plane and is not exposed.

Writing through the planes of a frame that was not made writable changes
every frame that shares its buffers, such as the frames a decoder still
references. A caller that writes passes `~make_writable:true`.

| Function                    | Returns                                                               |
| --------------------------- | --------------------------------------------------------------------- |
| `frame_get_width`           | the frame's width                                                     |
| `frame_get_height`          | the frame's height                                                    |
| `frame_get_pixel_format`    | the frame's pixel format (E3)                                         |
| `frame_get_pixel_aspect`    | `None` when the sample aspect ratio has a zero numerator, else `Some` |
| `frame_get_color_space`     | the frame's colour space (E3)                                         |
| `frame_get_color_range`     | the frame's colour range (E3)                                         |
| `frame_get_color_primaries` | the frame's colour primaries (E3)                                     |
| `frame_get_color_trc`       | the frame's transfer characteristic (E3)                              |
| `frame_get_chroma_location` | the frame's chroma location (E3)                                      |

### 4.14 Subtitles

```ocaml
module Subtitle : sig
  type frame
  type subtitle_type = Subtitle_type.t
  type subtitle_flag = Subtitle_flag.t
  val header_ass_default : unit -> string
  type pict = { x : int; y : int; w : int; h : int; nb_colors : int;
                planes : data array * int array }
  type rectangle = { pict : pict option; flags : subtitle_flag list;
                     rect_type : subtitle_type; text : string; ass : string }
  type content = { format : int; start_display_time : int;
                   end_display_time : int; rectangles : rectangle list;
                   pts : int64 option }
  val create_frame : content -> frame
  val get_content : frame -> content
  val get_pts : frame -> int64 option
end
```

`header_ass_default ()` returns this text, each line ended by CR LF, the last
included:

```
[Script Info]
; Script generated by ocaml-ffmpeg
ScriptType: v4.00+
PlayResX: 384
PlayResY: 288
ScaledBorderAndShadow: yes

[V4+ Styles]
Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding
Style: Default,Arial,16,&Hffffff,&Hffffff,&H0,&H0,0,0,0,0,100,100,0,0,1,1,0,2,10,10,10,1

[Events]
Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text
```

**Plane sizes.** The native rectangle records no plane size. The size of
plane `i` of a rectangle is derived, and is the same for both operations
below:

- `AVPALETTE_SIZE` for plane 1 of a rectangle of type bitmap;
- otherwise `h * linesize[i]` when both are positive;
- otherwise 0.

`create_frame content` builds a native subtitle that holds a copy of
`content`:

- `format`, the two display times, and `pts` (`AV_NOPTS_VALUE` for `None`);
- one rectangle per element of `rectangles`, in order, with its flags, type,
  and copies of `text` and `ass`; an empty string is stored as absent;
- for a rectangle with `pict = Some p`: `x`, `y`, `w`, `h`, `nb_colors`, the
  four line sizes, and a copy of each non-empty plane.

It raises a failure, and leaves nothing behind, when:

- a display time is negative or exceeds 32 unsigned bits;
- `format`, or an integer of a `pict`, does not fit the native field it is
  stored in;
- either array of `planes` does not have exactly 4 elements;
- a plane's byte length is neither 0 nor the derived size of that plane;
- the first plane of a `pict` is empty: `get_content` tells a rectangle with
  a picture by its first plane.

`get_content frame` returns a fresh `content`:

- `pict` is `Some` when the rectangle has a first plane, else `None`. Both
  arrays of `planes` have 4 elements. Each plane is a fresh bigarray, owned
  by the OCaml runtime, holding a copy of the derived size; an absent plane
  is empty.
- `text` and `ass` are the empty string when absent.
- `flags` per E6, `rect_type` per E3.

For every `content` that `create_frame` accepts,
`get_content (create_frame content)` equals `content`.

`get_pts frame`: `None` for `AV_NOPTS_VALUE`, else `Some`. It equals the
`pts` field of `get_content frame`.

### 4.15 Option introspection

```ocaml
module Options : sig
  type t
  type flag = [ `Encoding_param | `Decoding_param | `Audio_param
    | `Video_param | `Subtitle_param | `Export | `Readonly | `Bsf_param
    | `Runtime_param | `Filtering_param | `Deprecated | `Child_consts ]
  type 'a entry = { default : 'a option; min : 'a option; max : 'a option;
                    values : (string * 'a) list }
  type ground = [ `Flags of int64 entry | `Int of int entry
    | `Int64 of int64 entry | `Float of float entry
    | `Double of float entry | `String of string entry
    | `Rational of rational entry | `Binary of string entry
    | `Dict of string entry | `UInt64 of int64 entry
    | `Image_size of string entry | `Pixel_fmt of Pixel_format.t entry
    | `Sample_fmt of Sample_format.t entry | `Video_rate of string entry
    | `Duration of int64 entry | `Color of string entry
    | `Channel_layout of Channel_layout.t entry | `Bool of bool entry ]
  type spec = [ ground | `Array of ground ]
  type opt = { name : string; help : string option; flags : flag list;
               spec : spec }
  val opts : t -> opt list
```

`opts class` lists the options of an option class and of its child classes,
recursively.

- **Order**: the class's own options in FFmpeg's order, then each child
  class's, in FFmpeg's child-class order.
- **Which options**: one `opt` per FFmpeg option whose type is in the table
  of §3.2. An option of any other type is left out. The call never fails
  because of an option's type: listing runs over whatever the installed
  FFmpeg declares.
- `name`: the option's name. `help`: `None` when absent or empty.
- `flags`: every flag of §3.2 set on the option.
- `spec`: the constructor, default, minimum and maximum of §3.2. An
  array-typed option is `` `Array `` of the constructor of its element type;
  its `default` is `None`.
- `values`: the named values of the option. FFmpeg declares them as entries
  of type `AV_OPT_TYPE_CONST` that share the option's unit, in the same
  class. Each gives `(name, value)`, in FFmpeg's order, with the value
  converted to the option's type:

  | Option constructor                                         | Value of a named constant              |
  | ---------------------------------------------------------- | -------------------------------------- |
  | `` `Flags ``, `` `Int64 ``, `` `UInt64 ``, `` `Duration `` | its integer value                      |
  | `` `Int ``                                                 | its integer value                      |
  | `` `Float ``, `` `Double ``                                | its floating value                     |
  | `` `Bool ``                                                | `true` when its integer value is not 0 |
  | any other, and array-typed options                         | no named value: `values` is `[]`       |

An `Options.t` with no class gives `[]`.

### 4.16 Option reads

```ocaml
  type obj
  type 'a getter = ?search_children:bool -> name:string -> obj -> 'a
  val get_string : string getter
  val get_int : int getter
  val get_int64 : int64 getter
  val get_float : float getter
  val get_rational : rational getter
  val get_image_size : (int * int) getter
  val get_pixel_fmt : Pixel_format.t getter
  val get_sample_fmt : Sample_format.t getter
  val get_video_rate : rational getter
  val get_channel_layout : Channel_layout.t getter
  val get_dictionary : (string * string) list getter
end
```

Each getter reads option `name` from the native object `obj` designates
(§9.4).

- `search_children` defaults to `false`. With `true` the object's child
  objects are searched too (`AV_OPT_SEARCH_CHILDREN`).
- An unknown name raises ``Error `Option_not_found``. Any other FFmpeg failure
  raises the mapped error.

| Getter               | Result                                                                      |
| -------------------- | --------------------------------------------------------------------------- |
| `get_string`         | the option's value as text (`av_opt_get`)                                   |
| `get_int`            | the integer value; a value outside the OCaml integer range raises a failure |
| `get_int64`          | the integer value                                                           |
| `get_float`          | the floating value                                                          |
| `get_rational`       | the rational value                                                          |
| `get_image_size`     | `(width, height)`                                                           |
| `get_pixel_fmt`      | the pixel format (E3)                                                       |
| `get_sample_fmt`     | the sample format (E3)                                                      |
| `get_video_rate`     | the rate as a rational                                                      |
| `get_channel_layout` | an independent copy of the layout                                           |
| `get_dictionary`     | the `(key, value)` pairs in the dictionary's order                          |

Everything FFmpeg allocates for a read is released before the getter returns.

### 4.17 Option tables

```ocaml
type value =
  [ `String of string | `Int of int | `Int64 of int64 | `Float of float ]
type opts = (string, value) Hashtbl.t
val string_of_opts : opts -> string
val filter_opts : string array -> opts -> unit
```

An `opts` table maps option names to values. A key bound several times in the
table counts once, with its most recent binding.

Value rendering:

| value           | text                                                     |
| --------------- | -------------------------------------------------------- |
| `` `String s `` | `s`                                                      |
| `` `Int i ``    | `i` in decimal                                           |
| `` `Int64 i ``  | `i` in decimal                                           |
| `` `Float f ``  | a decimal text that FFmpeg parses back to the same value |

- `string_of_opts t`: the `key=text` forms of all keys joined with `,`. The
  order is unspecified and nothing is escaped.
- `filter_opts unused t`: removes from `t`, in place, every key that is not an
  element of `unused`.

### 4.18 Hardware contexts

```ocaml
module HwContext : sig
  type device_type = Hw_device_type.t
  type device_context
  type frame_context
  val create_device_context :
    ?device:string -> ?opts:opts -> device_type -> device_context
  val create_frame_context :
    width:int -> height:int ->
    src_pixel_format:Pixel_format.t -> dst_pixel_format:Pixel_format.t ->
    device_context -> frame_context
end
```

`create_device_context ?device ?opts device_type` opens a hardware device of
that type (`av_hwdevice_ctx_create`).

- `device` names the device; omitted or empty, FFmpeg picks its default.
- `opts` is handed to FFmpeg. FFmpeg does not report which entries it used,
  so the caller's table is left as it was: this operation is the one exception
  to contract O1.
- A failure releases everything and raises the mapped error.

`create_frame_context ~width ~height ~src_pixel_format ~dst_pixel_format
device` creates and initialises a pool of hardware frames on `device`: frames
of hardware format `dst_pixel_format` that carry software data of format
`src_pixel_format`, of the given size. Every other parameter of the pool keeps
FFmpeg's default. A failure releases the pool and raises.

## 5. Errors

### 5.1 FFmpeg error codes

A negative code from FFmpeg raises `Error e`:

| C code                       | `e`                                            |
| ---------------------------- | ---------------------------------------------- |
| `AVERROR_BSF_NOT_FOUND`      | `` `Bsf_not_found ``                           |
| `AVERROR_DECODER_NOT_FOUND`  | `` `Decoder_not_found ``                       |
| `AVERROR_DEMUXER_NOT_FOUND`  | `` `Demuxer_not_found ``                       |
| `AVERROR_ENCODER_NOT_FOUND`  | `` `Encoder_not_found ``                       |
| `AVERROR_EOF`                | `` `Eof ``                                     |
| `AVERROR_EXIT`               | `` `Exit ``                                    |
| `AVERROR_FILTER_NOT_FOUND`   | `` `Filter_not_found ``                        |
| `AVERROR_INVALIDDATA`        | `` `Invalid_data ``                            |
| `AVERROR_MUXER_NOT_FOUND`    | `` `Muxer_not_found ``                         |
| `AVERROR_OPTION_NOT_FOUND`   | `` `Option_not_found ``                        |
| `AVERROR_PATCHWELCOME`       | `` `Patch_welcome ``                           |
| `AVERROR_PROTOCOL_NOT_FOUND` | `` `Protocol_not_found ``                      |
| `AVERROR_STREAM_NOT_FOUND`   | `` `Stream_not_found ``                        |
| `AVERROR_BUG`                | `` `Bug ``                                     |
| `AVERROR(EAGAIN)`            | `` `Eagain ``                                  |
| `AVERROR_UNKNOWN`            | `` `Unknown ``                                 |
| `AVERROR_EXPERIMENTAL`       | `` `Experimental ``                            |
| any other                    | `` `Other code `` with the raw (negative) code |

`string_of_error` reads the same table right to left. `AVERROR_EXTERNAL`,
which a failing user callback produces ([avformat.md](avformat.md) §7), has
no constructor of its own and is `` `Other ``.

### 5.2 Binding failures

``Error (`Failure msg)`` is raised for every condition the library detects
itself: an invalid argument (A1), a value with no table entry (E3), a null
object handed to a wrapping constructor, a sample format with no bigarray
element kind, an unknown pixel format name.

### 5.3 Not_found

| Raised by                    | When                                                |
| ---------------------------- | --------------------------------------------------- |
| `Channel_layout.find`        | FFmpeg does not accept the name                     |
| `Sample_format.find`         | FFmpeg knows no format of that name                 |
| `Sample_format.find_id`      | no constructor has that C value                     |
| `Pixel_format.find_id`       | no constructor has that C value                     |
| `Pixel_format.descriptor`    | the format has no descriptor                        |
| `Channel_layout.get_default` | FFmpeg has no standard layout of that channel count |

### 5.4 State after a failure

No operation of this library leaves a handle changed after raising, with one
exception: `Video.frame_visit ~make_writable:true` may have given the frame
private buffers before the visitor raises.

## 6. Blocking and concurrency

### 6.1 The runtime lock

`HwContext.create_device_context` and `HwContext.create_frame_context` fall
under contract M1. The delivery thread of §7.1 waits with the lock released.

### 6.2 Concurrent use

Frames have no guard (contract M8). Operations that change a frame are
`Frame.set_pts`, `set_duration`, `set_pkt_dts`, `set_metadata` and
`Video.frame_visit ~make_writable:true`; every other operation that takes a
frame, in any library, only reads its structure.

Channel layouts, subtitles, hardware contexts and descriptors are immutable.

### 6.3 Thread registration

One service, used by dependent libraries at the start of every function of
contract §7.1:

- it registers the calling thread with the OCaml runtime when it is not
  registered yet;
- a thread it registered is unregistered when that thread exits, not before;
- it is cheap and has no effect on a thread already registered;
- it does not take the runtime lock.

### 6.4 Global state

| State                               | Rule               |
| ----------------------------------- | ------------------ |
| FFmpeg's log level and log callback | process-wide; §7.1 |
| pending log messages                | §7.1               |
| the installed log callback          | §7.1               |
| thread registration bookkeeping     | per thread         |

There is no other process-wide state. In particular no failure message is
formatted into storage shared between threads.

## 7. Callbacks

### 7.1 Log messages

FFmpeg logs from any thread, at any time: from its worker threads, from
threads that hold its internal locks, and from inside calls the binding made
with the runtime lock held. The log path is therefore indirect.

**Capture.** While a callback is installed, the function FFmpeg calls for each
log message:

- **N1.** touches no OCaml state, calls no OCaml code, and takes no lock that
  an OCaml thread can hold while it is inside an FFmpeg call;
- **N2.** drops a message whose level is above FFmpeg's current log level;
- **N3.** formats the message as FFmpeg's default logger does, prefix
  included, truncates it to `LOG_LINE_MAX`, and queues it. One queued message
  per log call; a message may be a partial line;
- **N4.** returns without waiting for delivery.

**Delivery.**

- **N5.** Queued messages are delivered to the installed callback by a thread
  the library runs for that purpose, never by the thread that logged.
- **N6.** Messages are delivered in the order they were queued.
- **N7.** A message is delivered at most once. A message queued while a
  callback is installed is delivered unless its allocation failed.
- **N8.** A queued message is delivered without waiting for another one to be
  logged: no wake-up is lost.
- **N9.** The callback runs with the runtime lock held and with no lock of the
  library held. It may call `set_callback`, `clear_callback` and any other
  operation of the bindings, including ones that log.
- **N10.** An exception raised by the callback is caught and reported on
  standard error. Delivery continues with the next message.

**`Log.set_callback f`.** From its return on, messages are captured and
delivered to `f`. It replaces any callback installed before, and it works
whatever happened earlier, including immediately after `clear_callback`.

**`Log.clear_callback ()`.** Restores FFmpeg's default logging and drops the
callback.

- Messages queued before the call are delivered to the outgoing callback
  before `clear_callback` returns. When `clear_callback` is called from the
  callback itself, they are delivered after the callback returns.
- No message is delivered to the outgoing callback after that, and no message
  queued before the call is delivered to a callback installed later.

Before any `set_callback`, FFmpeg's default logging applies.

The callback closure is kept alive from `set_callback` until it is replaced or
cleared.

### 7.2 The visitor of `Video.frame_visit`

Called by the binding on the caller's thread (contract §7.2). The frame is not
in use while it runs. An exception propagates; the frame keeps the buffers it
had when the visitor was called.

## 8. Data transfer

### 8.1 Text

Every string crossing the boundary is copied, in both directions: option
names and values, metadata, format names, descriptions, error and log
messages, subtitle text.

### 8.2 Video planes — shared

`Video.frame_visit` hands out bigarrays that share memory with the frame's
buffers.

- Writes through a bigarray write the frame's buffer.
- Each bigarray keeps the buffer it points into alive (L4). It stays valid
  after the visitor returns and after the frame is collected. After a later
  make-writable step gave the frame other buffers, it still designates the
  buffer it was built on.
- The length of plane `i` is exactly `linesize[i]` times the height of that
  plane. The height of a chroma plane is the frame's height reduced by the
  format's vertical chroma subsampling, rounded up
  (`av_image_fill_plane_sizes` gives the sizes).
- The stride is `linesize[i]`, returned alongside. Rows include FFmpeg's
  padding.

### 8.3 Audio samples

`Avutil` exposes no bigarray view of audio samples.
[swresample.md](swresample.md) owns the bigarray paths.

### 8.4 Subtitle bitmaps — copied

Both directions copy each plane (§4.14).

### 8.5 Frames

A frame handed to a dependent library is passed by reference (A4).

### 8.6 Channel layouts

Always copied, in both directions (§2.2).

### 8.7 `data`

`create_data` returns an ordinary runtime-owned bigarray; the binding keeps no
reference to it.

## 9. Options

### 9.1 The option-table protocol

An operation that takes `?opts` (contract O1):

1. Renders the caller's table to `(key, text)` pairs (§4.17).
2. Builds an FFmpeg dictionary from the pairs. A failure frees the dictionary
   and raises.
3. Gives the dictionary to the FFmpeg calls that consume it, in the order the
   library's §9 states. Each removes the entries it recognises.
4. Collects the keys left in the dictionary and frees it.
5. On success, removes from the caller's table every key not left over.

- After a successful call, a key still in the caller's table was not used.
- When `?opts` is omitted, nothing is reported.
- On failure the caller's table is untouched (F7) and the dictionary is
  freed.
- The table handed to FFmpeg holds the caller's entries only. A setting the
  operation derives from a typed argument never appears in it, so the
  leftover keys are exactly the caller's unused ones.

### 9.2 Within `Avutil`

Only `HwContext.create_device_context` takes an option table (§4.18).

### 9.3 Listing options

§4.15.

### 9.4 Reading options

An `Options.obj` is built by a dependent library. It designates a native
object that carries FFmpeg options, inside an owner handle.

- The owner is kept alive while the `obj` is reachable (L3).
- A getter takes the owner's guard shared for the read, when the owner has
  one.
- A getter on an `obj` whose owner is closed raises the closed error.

There is no option setter on a live object.

## 10. Version-dependent behaviour

None.

## 11. Composite operations

- `version_string` (§4.2).
- `string_of_opts`, `filter_opts` (§4.17): pure functions on tables.
- `Video.frame_visit` is: prepare the planes, call the visitor, return the
  frame.
