# avutil — as-built specification (Part A)

Mechanism notes for the current stubs are in
[language-notes/avutil.md](language-notes/avutil.md). Observations and
judgement are in [findings/avutil.md](findings/avutil.md).

## 1. Scope

### 1.1 What it binds

`Avutil` binds libavutil: errors, logging, rationals, channel layouts, sample
and pixel formats, colour properties, `AVFrame`, `AVOption` introspection and
reads, hardware device and frame contexts, expression evaluation.

It also binds a small part of libavcodec: `AVSubtitle` (allocation, content,
`avsubtitle_free`) and the constant `FF_QP2LAMBDA`. The library therefore
compiles against `libavcodec/avcodec.h` and links against both `libavcodec`
and `libavutil`.

It depends on no sibling binding library. It depends on the OCaml `threads`
library. Every sibling binding library depends on it.

### 1.2 Minimum versions

The code states no minimum version. It uses unconditionally:

- the `AVChannelLayout` API (`av_channel_layout_copy`, `_compare`,
  `_describe`, `_default`, `_from_string`, `_standard`, `_uninit`,
  `AVFrame.ch_layout`, `av_opt_get_chlayout`, `AV_OPT_TYPE_CHLAYOUT`);
- `av_opt_child_class_iterate`;
- `av_log_format_line2`;
- `av_color_space_from_name` and its four siblings;
- `av_opt_get_dict_val`.

Conditional features are listed in section 10.

### 1.3 Module initialisation

Loading the OCaml module performs, in order:

1. reads `avutil_version()` into `version`;
2. reads `FF_QP2LAMBDA` into `qp2lambda`;
3. registers a `Printexc` printer for `Error`;
4. registers the `Error` exception and the failure-raising closure so that C
   code can raise them (section 5);
5. registers the internal "option not implemented" exception (section 9.3);
6. builds `Channel_layout.standard_layouts`;
7. builds `Channel_layout.mono`, `stereo`, `five_point_one` by name lookup
   (`"mono"`, `"stereo"`, `"5.1"`). A failure here fails module loading.

### 1.4 What the C header exports to sibling stubs

The header is installed with the library, together with the generated
polymorphic-variant hash table header and the generated media-type table
header. Sibling stubs rely on the following. The exact macros are in Part B;
the behaviour is specified here.

**Error raising**

- _Raise an FFmpeg error_: takes a negative `AVERROR` code and raises
  `Avutil.Error e` with `e` given by the table in section 5.1. Never returns.
- _Raise a binding failure_: takes a `printf` format and arguments, formats
  them into a process-wide 256-byte buffer (messages are truncated to 255
  bytes), and raises ``Avutil.Error (`Failure msg)``. Never returns.
- The message buffer itself is exported.

**Option dictionaries** (section 9.1)

- _Fill a dictionary_: takes an OCaml `(string * string) array` and an
  `AVDictionary **`. Calls `av_dict_set(dict, key, value, 0)` for each pair
  in array order (keys and values are copied by FFmpeg). On a negative
  return it frees the dictionary with `av_dict_free` and raises the mapped
  error. It allocates nothing on the OCaml heap.
- _Report unused options_: takes an `AVDictionary **`. Returns a fresh OCaml
  `string array` holding the key of every entry left in the dictionary, in
  `av_dict_get(dict, "", prev, AV_DICT_IGNORE_SUFFIX)` iteration order, then
  frees the dictionary and sets the pointer to `NULL`. A `NULL` dictionary
  gives an empty array.

**List building**: two helpers to initialise an OCaml list to empty and to
prepend an element.

**Thread registration**: one function, specified in section 6.3.

**Rational**

- OCaml to C: reads the two fields of a `rational` record as C `int`s and
  builds an `AVRational {num, den}`.
- C to OCaml: allocates a 2-field record `{num; den}` from an `AVRational`.

**Time format**: maps a `Time_format.t` to the number of units per second
as `int64_t`:

| OCaml              | value      |
| ------------------ | ---------- |
| `` `Second ``      | 1          |
| `` `Millisecond `` | 1000       |
| `` `Microsecond `` | 1000000    |
| `` `Nanosecond ``  | 1000000000 |
| anything else      | 1          |

**Channel layout**: accessor from an OCaml `Channel_layout.t` to its
`AVChannelLayout *`, and a constructor that wraps a _copy_ of a given
`const AVChannelLayout *` (section 2.2).

**Sample format**: OCaml `Sample_format.t` to `enum AVSampleFormat` and
back (generated table, section 3.1), and a map from `enum AVSampleFormat` to
a bigarray element kind:

| sample format (packed or planar) | bigarray kind                             |
| -------------------------------- | ----------------------------------------- |
| U8, U8P                          | unsigned 8-bit                            |
| S16, S16P                        | signed 16-bit                             |
| S32, S32P                        | int32                                     |
| S64, S64P                        | int64                                     |
| FLT, FLTP                        | float32                                   |
| DBL, DBLP                        | float64                                   |
| any other                        | raises ``Error (`Other AVERROR(EINVAL))`` |

**Colour space, colour range, colour primaries, transfer characteristic,
chroma location, pixel format, hardware device type**: for each, OCaml
variant to C enum and back (generated tables, section 3.1).

**Buffer reference**: accessor from an OCaml hardware device or frame
context value to its `AVBufferRef *` (section 2.5).

**Frame**: accessor from an OCaml frame to its `AVFrame *`, and a
constructor that wraps an `AVFrame *` and takes ownership of it
(section 2.1).

**Subtitle**: accessor from an OCaml subtitle frame to its `AVSubtitle *`,
and a constructor that wraps an `AVSubtitle *` and takes ownership of it
(section 2.3).

**Borrowed pointers**: four wrap/unwrap pairs for pointers the OCaml value
does not own and never frees: `const AVPixFmtDescriptor *`,
`const AVClass *` (this is `Options.t`), `const AVOption *`, and an untyped
`void *` (this is the C half of `Options.obj`, section 9.4).

**Constants**: the OCaml encoding of `None`, an accessor for the content of
`Some`, and a feature flag telling whether `AV_OPT_TYPE_FLAG_ARRAY` exists
(section 10).

## 2. Objects

### 2.1 Frame — `'media Frame.t` (= `'media frame`)

- **C object**: one `AVFrame`, held by pointer.
- **Creation**: `Audio.create_frame`, `Video.create_frame`, and sibling
  libraries through the exported constructor (decoders, filters,
  converters). The exported constructor raises
  ``Error (`Failure "Empty frame")`` when given `NULL`.
- **Ownership**: the OCaml value owns the `AVFrame`. One OCaml value per
  `AVFrame`; the binding never wraps the same `AVFrame` twice.
- **Garbage-collector accounting**: at wrap time the constructor sums
  `frame->buf[n]->size` over the leading non-`NULL` entries of `frame->buf`
  (at most `AV_NUM_DATA_POINTERS`) and declares that many bytes of
  out-of-heap memory for the value. The figure is not updated afterwards.
- **Keeps alive**: nothing on the OCaml side. Data buffers are kept alive by
  FFmpeg's reference counting inside the `AVFrame`.
- **Release**: garbage collection only. The finaliser calls `av_frame_free`.
  There is no explicit close, so no use-after-close state.
- **Phantom parameter**: `'media` is `audio` or `video`; nothing checks at
  run time that an `audio frame` holds audio.

### 2.2 Channel layout — `Channel_layout.t`

- **C object**: one heap-allocated `AVChannelLayout`.
- **Creation**: always by copy. The constructor allocates a zeroed
  `AVChannelLayout` with `av_mallocz`, then `av_channel_layout_copy` from
  the source. Allocation failure raises `Out_of_memory`. A copy failure
  frees the allocation and raises the mapped error.
- **Ownership**: the OCaml value owns its copy. It shares nothing with the
  layout it was copied from.
- **Release**: garbage collection only. The finaliser calls
  `av_channel_layout_uninit` then `av_free`.
- Values are immutable from OCaml.

### 2.3 Subtitle — `Subtitle.frame`

- **C object**: one heap-allocated `AVSubtitle`.
- **Creation**: `Subtitle.create_frame`, and sibling libraries through the
  exported constructor (subtitle decoding). The exported constructor raises
  ``Error (`Failure "Empty subtitle")`` when given `NULL`.
- **Ownership**: the OCaml value owns the `AVSubtitle`, its rectangle
  array, each rectangle and each rectangle's data, text and ASS strings.
- **Release**: garbage collection only. The finaliser calls
  `avsubtitle_free` then `av_free` on the struct.
- Immutable from OCaml after creation.

### 2.4 Standard-layout iterator (internal)

Used only while building `Channel_layout.standard_layouts`. It owns one
heap-allocated `void *` initialised to `NULL`, the iteration state of
`av_channel_layout_standard`. Released by garbage collection (`av_free`).

### 2.5 Hardware contexts — `HwContext.device_context`, `HwContext.frame_context`

- **C object**: one `AVBufferRef *` for each; a device context references an
  `AVHWDeviceContext`, a frame context an `AVHWFramesContext`.
- **Creation**: `HwContext.create_device_context`,
  `HwContext.create_frame_context`.
- **Ownership**: the OCaml value owns one reference.
- **Keeps alive**: a frame context holds FFmpeg's own reference to its
  device context (taken by `av_hwframe_ctx_alloc`). The OCaml frame-context
  value holds no OCaml reference to the OCaml device-context value.
- **Release**: garbage collection only. The finaliser calls
  `av_buffer_unref`.

### 2.6 Borrowed handles

| OCaml type                                    | C pointer                                                                       | Lifetime                                                       |
| --------------------------------------------- | ------------------------------------------------------------------------------- | -------------------------------------------------------------- |
| `Options.t`                                   | `const AVClass *`                                                               | static FFmpeg data; never freed                                |
| hidden 8th field of `Pixel_format.descriptor` | `const AVPixFmtDescriptor *`                                                    | static FFmpeg data; never freed                                |
| `Options.obj`                                 | pair of (`void *` to an AVOptions-enabled object, the OCaml value that owns it) | the second component keeps the owner alive during a read (9.4) |

`Avutil` creates no `Options.t` and no `Options.obj`; sibling libraries do.
An `Options.t` may hold `NULL`; listing its options then gives `[]`.

### 2.7 Types with no C object

`input`, `output`, `'a container`, `('line, 'media) format` are abstract
types declared here and given meaning by sibling libraries. `audio`,
`video`, `subtitle` are the single-constructor polymorphic variant types
used as phantom parameters. `media_type` is the generated media-type variant.
`data` is a one-dimensional unsigned-8-bit C-layout bigarray.
`rational`, `version`, `value`, `opts` are plain OCaml data.

## 3. Enumerations and constants

### 3.1 Generated tables consumed

Each generated table pairs an OCaml polymorphic variant with a C constant.
For each table the generator provides three conversions: OCaml to C
(raising), OCaml to C (non-raising, returns the sentinel `0xFFFFFFF`), and C
to OCaml (raising). The raising conversions raise
``Error (`Failure "Could not find C value for N in TABLE. Do you need to
recompile the ffmpeg binding?")`` (respectively `"... OCaml value for ..."`)
when the value has no entry.

| Table                      | OCaml type                 | C type                               | Used by `Avutil` for                         |
| -------------------------- | -------------------------- | ------------------------------------ | -------------------------------------------- |
| polymorphic variant hashes | every hand-matched variant | —                                    | errors, time formats, option types and flags |
| pixel format               | `Pixel_format.t`           | `enum AVPixelFormat`                 | both directions                              |
| pixel format flag          | `Pixel_format.flag`        | `AV_PIX_FMT_FLAG_*` bits             | C to OCaml, by scanning the table            |
| colour space               | `Color_space.t`            | `enum AVColorSpace`                  | both directions                              |
| colour range               | `Color_range.t`            | `enum AVColorRange`                  | both directions                              |
| colour primaries           | `Color_primaries.t`        | `enum AVColorPrimaries`              | both directions                              |
| transfer characteristic    | `Color_trc.t`              | `enum AVColorTransferCharacteristic` | both directions                              |
| chroma location            | `Chroma_location.t`        | `enum AVChromaLocation`              | both directions                              |
| sample format              | `Sample_format.t`          | `enum AVSampleFormat`                | both directions                              |
| channel layout             | `Channel_layout.layout`    | `AV_CH_LAYOUT_*` masks               | type only; no conversion is called           |
| hardware device type       | `HwContext.device_type`    | `enum AVHWDeviceType`                | OCaml to C                                   |
| subtitle type              | `Subtitle.subtitle_type`   | `enum AVSubtitleType`                | both directions                              |
| subtitle flag              | `Subtitle.subtitle_flag`   | `AV_SUBTITLE_FLAG_*` bits            | both directions                              |
| media type                 | `media_type`               | `enum AVMediaType`                   | type only; header re-exported                |

The sample-format table contains `AV_SAMPLE_FMT_NONE` (as `` `None ``).

### 3.2 Hand-written tables

**Log levels** (`Log.level` to the integer passed to `av_log_set_level`):

| level          | integer |
| -------------- | ------- |
| `` `Quiet ``   | -8      |
| `` `Panic ``   | 0       |
| `` `Fatal ``   | 8       |
| `` `Error ``   | 16      |
| `` `Warning `` | 24      |
| `` `Info ``    | 32      |
| `` `Verbose `` | 40      |
| `` `Debug ``   | 48      |
| `` `Trace ``   | 56      |

**Errors**: section 5.1.

**Time formats**: section 1.4.

**Option flags** (`Options.flag` to bit mask; a flag whose macro is absent
maps to 0 and is then never reported):

| flag                   | mask                                           |
| ---------------------- | ---------------------------------------------- |
| `` `Encoding_param ``  | `AV_OPT_FLAG_ENCODING_PARAM`                   |
| `` `Decoding_param ``  | `AV_OPT_FLAG_DECODING_PARAM`                   |
| `` `Audio_param ``     | `AV_OPT_FLAG_AUDIO_PARAM`                      |
| `` `Video_param ``     | `AV_OPT_FLAG_VIDEO_PARAM`                      |
| `` `Subtitle_param ``  | `AV_OPT_FLAG_SUBTITLE_PARAM`                   |
| `` `Export ``          | `AV_OPT_FLAG_EXPORT`                           |
| `` `Readonly ``        | `AV_OPT_FLAG_READONLY`                         |
| `` `Bsf_param ``       | `AV_OPT_FLAG_BSF_PARAM` if defined, else 0     |
| `` `Runtime_param ``   | `AV_OPT_FLAG_RUNTIME_PARAM` if defined, else 0 |
| `` `Filtering_param `` | `AV_OPT_FLAG_FILTERING_PARAM`                  |
| `` `Deprecated ``      | `AV_OPT_FLAG_DEPRECATED` if defined, else 0    |
| `` `Child_consts ``    | always 0 (see findings)                        |

Any other value raises `Failure "Invalid option flag!"`.

**Option types** (`enum AVOptionType`, after clearing the array bit, to the
tag of `Options.spec`):

| C type                   | OCaml tag             | default                                                                              | min / max                                      |
| ------------------------ | --------------------- | ------------------------------------------------------------------------------------ | ---------------------------------------------- |
| `AV_OPT_TYPE_FLAGS`      | `` `Flags ``          | `default_val.i64` as int64                                                           | `min`/`max` as int64, saturated                |
| `AV_OPT_TYPE_INT`        | `` `Int ``            | `default_val.i64` as OCaml int                                                       | `(int)min`, `(int)max`                         |
| `AV_OPT_TYPE_INT64`      | `` `Int64 ``          | `default_val.i64`                                                                    | saturated                                      |
| `AV_OPT_TYPE_UINT64`     | `` `UInt64 ``         | `default_val.i64`                                                                    | saturated                                      |
| `AV_OPT_TYPE_DURATION`   | `` `Duration ``       | `default_val.i64`                                                                    | saturated                                      |
| `AV_OPT_TYPE_DOUBLE`     | `` `Double ``         | `default_val.dbl`                                                                    | `min`, `max`                                   |
| `AV_OPT_TYPE_FLOAT`      | `` `Float ``          | `default_val.dbl`                                                                    | `min`, `max`                                   |
| `AV_OPT_TYPE_RATIONAL`   | `` `Rational ``       | `av_d2q(default_val.dbl, INT_MAX)`                                                   | `av_d2q(min, INT_MAX)`, `av_d2q(max, INT_MAX)` |
| `AV_OPT_TYPE_STRING`     | `` `String ``         | `default_val.str` if non-`NULL`                                                      | none                                           |
| `AV_OPT_TYPE_BINARY`     | `` `Binary ``         | same                                                                                 | none                                           |
| `AV_OPT_TYPE_DICT`       | `` `Dict ``           | same                                                                                 | none                                           |
| `AV_OPT_TYPE_IMAGE_SIZE` | `` `Image_size ``     | same                                                                                 | none                                           |
| `AV_OPT_TYPE_VIDEO_RATE` | `` `Video_rate ``     | same                                                                                 | none                                           |
| `AV_OPT_TYPE_COLOR`      | `` `Color ``          | same                                                                                 | none                                           |
| `AV_OPT_TYPE_PIXEL_FMT`  | `` `Pixel_fmt ``      | converted `default_val.i64` if `av_get_pix_fmt_name` of it is non-`NULL`             | none                                           |
| `AV_OPT_TYPE_SAMPLE_FMT` | `` `Sample_fmt ``     | converted `default_val.i64` if `av_get_sample_fmt_name` of it is non-`NULL`          | none                                           |
| `AV_OPT_TYPE_CHLAYOUT`   | `` `Channel_layout `` | parsed `default_val.str` if non-`NULL` and `av_channel_layout_from_string` returns 0 | none                                           |
| `AV_OPT_TYPE_BOOL`       | `` `Bool ``           | `default_val.i64 != 0` if `default_val.i64 >= 0`                                     | none                                           |
| `AV_OPT_TYPE_CONST`      | —                     | the option is skipped                                                                | —                                              |
| any other                | —                     | the option is skipped                                                                | —                                              |

"Saturated" means: a `double` bound at or below `INT64_MIN` becomes
`Int64.min_int`, at or above `INT64_MAX` becomes `Int64.max_int`, otherwise
it is truncated to `int64_t`. "none" means the field is `None`. A default
that does not meet its condition is `None`.

**`Options` getter selectors**: section 4.17.

### 3.3 Constants

| Name                              | Value                   | Meaning                                                |
| --------------------------------- | ----------------------- | ------------------------------------------------------ |
| log line buffer                   | 1024 bytes              | each log message is truncated to 1023 bytes            |
| failure message buffer            | 256 bytes               | each binding failure message is truncated to 255 bytes |
| channel layout description buffer | 1024 bytes              |                                                        |
| video frame buffer alignment      | 32                      | passed to `av_frame_get_buffer`                        |
| audio frame buffer alignment      | 0                       | FFmpeg chooses                                         |
| rational denominator bound        | `INT_MAX`               | for `av_d2q`                                           |
| subtitle planes                   | 4                       | fixed number of data/linesize slots per rectangle      |
| bitmap subtitle palette size      | `AVPALETTE_SIZE` (1024) | size of plane 1 of a `SUBTITLE_BITMAP` rectangle       |

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
val compare_version : version -> version -> int
```

- `version`: computed once at module load from `avutil_version()` (the
  version of the libavutil library linked at run time): `major = v lsr 16`,
  `minor = (v lsr 8) land 0xff`, `micro = v land 0xff`.
- `version_string v` is `"major.minor.micro"` in decimal.
- `compare_version a b` computes `AV_VERSION_INT(major, minor, micro)`
  (`major lsl 16 lor minor lsl 8 lor micro`) for each side and returns the
  standard integer comparison of the two.

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
  val copy : 'a t -> 'b t -> unit
end
type 'media frame = 'media Frame.t
```

All of these read or write fields of the `AVFrame` directly. None can fail
unless stated.

| Function                | Behaviour                                                                                                                |
| ----------------------- | ------------------------------------------------------------------------------------------------------------------------ |
| `pts`                   | `None` when `frame->pts == AV_NOPTS_VALUE`, else `Some frame->pts`.                                                      |
| `set_pts`               | Writes `AV_NOPTS_VALUE` for `None`, the value for `Some`. Then copies the new `pts` into `frame->best_effort_timestamp`. |
| `duration`              | `None` when `frame->duration == 0`, else `Some frame->duration`. Always `None` on old libavutil (section 10).            |
| `set_duration`          | Writes 0 for `None`, the value for `Some`. No effect on old libavutil.                                                   |
| `pkt_dts`               | `None` when `frame->pkt_dts == AV_NOPTS_VALUE`, else `Some`.                                                             |
| `set_pkt_dts`           | Writes `AV_NOPTS_VALUE` for `None`, the value for `Some`.                                                                |
| `best_effort_timestamp` | `None` when the field equals `AV_NOPTS_VALUE`, else `Some`.                                                              |

`metadata frame`:

1. Counts entries with `av_dict_count(frame->metadata)`.
2. Walks the dictionary with
   `av_dict_get(metadata, "", prev, AV_DICT_IGNORE_SUFFIX)`.
3. Returns the `(key, value)` pairs as fresh strings, in that order.

`set_metadata frame l`:

1. Builds a new dictionary with `av_dict_set(&d, key, value, 0)` for each
   pair in list order. Later duplicates of a key replace earlier ones
   (FFmpeg's default for flags 0).
2. On a negative return raises the mapped error; the frame's metadata is
   unchanged.
3. Frees the frame's previous dictionary if any, then stores the new one.
   An empty list leaves the frame with no metadata.

`copy src dst`: calls `av_frame_copy(dst, src)`. This copies sample or
pixel data only; `dst` must already be allocated with the same format and
dimensions (or sample count and layout). A negative return raises the mapped
error. The runtime lock is held.

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

- `error`, `Error`: section 5.
- `string_of_error e`: for `` `Failure s `` returns `s` itself. For every
  other constructor, maps it back to its `AVERROR` code with the table of
  section 5.1 (`` `Other n `` gives `n`) and returns `av_err2str(code)`.
- A `Printexc` printer renders `Error e` as
  `"Avutil.Error(" ^ string_of_error e ^ ")"`.

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

- `expr_parse_and_eval s` calls `av_expr_parse_and_eval` with the result
  pointer, `s`, `NULL` for the constant names, constant values, both
  function-name tables, both function tables and the opaque pointer,
  `AV_LOG_MAX_OFFSET` as log offset and `NULL` as log context. With that log
  offset, parse errors are not logged. A negative return raises the mapped
  error. Otherwise returns the value.
- `create_data len` allocates a fresh OCaml-managed bigarray of `len` bytes.
  Contents are uninitialised.
- `string_of_rational {num; den}` is `"num/den"` in decimal.
- `qp2lambda` is `FF_QP2LAMBDA`, read once at module load.

### 4.6 Timestamp

```ocaml
module Time_format : sig
  type t = [ `Second | `Millisecond | `Microsecond | `Nanosecond ]
end
val time_base : unit -> rational
```

- `Time_format.t` is consumed by sibling libraries through the C helper of
  section 1.4.
- `time_base ()` returns `AV_TIME_BASE_Q` as a fresh record (`{num = 1;
den = 1000000}`).

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

- `set_level l` calls `av_log_set_level` with the integer of section 3.2.
- `set_callback` and `clear_callback`: section 7.1.

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
  val get_native_id : t -> int64 option
end
```

- `layout` is the generated variant of `AV_CH_LAYOUT_*` names. No function
  of this module takes or returns it.
- `standard_layouts`: built once at module load. Calls
  `av_channel_layout_standard` repeatedly until it returns `NULL`, copying
  each result into a new value and prepending it to the list. The list is
  therefore in the reverse of FFmpeg's iteration order.
- `mono`, `stereo`, `five_point_one`: `find "mono"`, `find "stereo"`,
  `find "5.1"`, evaluated once at module load.
- `compare a b` calls `av_channel_layout_compare`. A negative return raises
  the mapped error. Returns `true` when the result is 0 (layouts equal),
  `false` when it is positive (layouts differ).
- `find name` calls `av_channel_layout_from_string`. A non-zero return
  raises `Error` with the mapped code (an unknown name gives
  ``Error (`Other AVERROR(EINVAL))``). On success, copies the result into a
  new value and uninitialises the temporary.
- `get_description l` calls `av_channel_layout_describe` into a 1024-byte
  buffer. A negative return raises the mapped error.
- `get_nb_channels l` returns `l->nb_channels`.
- `get_default n` calls `av_channel_layout_default(&tmp, n)` and returns a
  copy of the result. It does not fail: when FFmpeg has no default for `n`
  channels the result has unspecified order and `n` channels.
- `get_mask l` returns `Some l->u.mask` when
  `l->order == AV_CHANNEL_ORDER_NATIVE`, else `None`.
- `get_native_id` is `get_mask`.

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

- `get_name f`: `None` when `f` is `AV_SAMPLE_FMT_NONE` or
  `av_get_sample_fmt_name` returns `NULL`; else `Some name`.
- `find name`: copies `name` (`av_strndup`, `Out_of_memory` on failure),
  calls `av_get_sample_fmt`, frees the copy. `AV_SAMPLE_FMT_NONE` raises
  `Not_found`. Otherwise converts through the generated table.
- `get_id f`: the integer value of the C enum.
- `find_id n`: converts `n` through the generated table. A value with no
  entry raises ``Error (`Failure "Could not find OCaml value for ...")``.

### 4.10 Colour properties

```ocaml
module Color_space      : sig type t = Color_space.t
  val name : t -> string  val from_name : string -> t option end
module Color_range      : sig (* same shape *) end
module Color_primaries  : sig (* same shape *) end
module Color_trc        : sig (* same shape *) end
module Chroma_location  : sig (* same shape *) end
```

Each module has exactly `type t`, `val name : t -> string` and
`val from_name : string -> t option`.

| Module            | `name` calls              | `from_name` calls              |
| ----------------- | ------------------------- | ------------------------------ |
| `Color_space`     | `av_color_space_name`     | `av_color_space_from_name`     |
| `Color_range`     | `av_color_range_name`     | `av_color_range_from_name`     |
| `Color_primaries` | `av_color_primaries_name` | `av_color_primaries_from_name` |
| `Color_trc`       | `av_color_transfer_name`  | `av_color_transfer_from_name`  |
| `Chroma_location` | `av_chroma_location_name` | `av_chroma_location_from_name` |

- `name v` converts `v` to its C enum and returns FFmpeg's name as a fresh
  string.
- `from_name s` returns `None` when FFmpeg returns a negative value;
  otherwise converts the returned enum through the generated table and
  returns `Some`.

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

`descriptor f`:

1. Converts `f` and calls `av_pix_fmt_desc_get`. `NULL` raises `Not_found`.
2. Copies `name`, `nb_components`, `log2_chroma_w`, `log2_chroma_h`.
3. `flags`: every entry of the generated flag table whose bit is set in
   `desc->flags`, in the reverse of table order.
4. `comp`: always four entries, for `desc->comp[0]` to `desc->comp[3]` in
   that order, whatever `nb_components` says.
5. `alias`: `Some` when `desc->alias` is non-`NULL`.
6. Stores the `const AVPixFmtDescriptor *` in a hidden extra field after
   `alias`. The record type is `private` so that only this function builds
   it.

Other functions:

- `bits d` calls `av_get_bits_per_pixel` on the hidden pointer.
- `planes f` returns `av_pix_fmt_count_planes` unchanged, including a
  negative `AVERROR` for a format with no descriptor.
- `to_string f`: `None` when `f` is `AV_PIX_FMT_NONE` or
  `av_get_pix_fmt_name` returns `NULL`; else `Some name`.
- `of_string s` calls `av_get_pix_fmt`. `AV_PIX_FMT_NONE` raises
  ``Error (`Failure "Invalid format name")``.
- `get_id f`: the integer value of the C enum.
- `find_id n`: converts through the generated table; a value with no entry
  raises ``Error (`Failure "Could not find OCaml value for ...")``.

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
  val frame_copy_samples :
    audio frame -> int -> audio frame -> int -> int -> unit
end
```

`create_frame sample_format channel_layout sample_rate nb_samples`:

1. Converts `sample_format`.
2. `av_frame_alloc`; `NULL` raises `Out_of_memory`.
3. Sets `frame->format`.
4. `av_channel_layout_copy(&frame->ch_layout, layout)`. A negative return
   frees the frame and raises the mapped error.
5. Sets `frame->sample_rate` and `frame->nb_samples`.
6. `av_frame_get_buffer(frame, 0)`. A negative return frees the frame and
   raises the mapped error.
7. Wraps the frame (section 2.1). Sample data is uninitialised.

Getters:

| Function                   | Returns                                                   |
| -------------------------- | --------------------------------------------------------- |
| `frame_get_sample_format`  | `frame->format` converted through the sample-format table |
| `frame_get_sample_rate`    | `frame->sample_rate`                                      |
| `frame_get_channels`       | `frame->ch_layout.nb_channels`                            |
| `frame_get_channel_layout` | a new value holding a copy of `frame->ch_layout`          |
| `frame_nb_samples`         | `frame->nb_samples`                                       |

`frame_copy_samples src src_offset dst dst_offset len`:

1. Reads from `dst`: whether its format is planar
   (`av_sample_fmt_is_planar`), its channel count, and the number of planes
   (channel count if planar, else 1).
2. Raises ``Error (`Other AVERROR(EINVAL))`` when
   `src->nb_samples < src_offset + len`, or
   `dst->nb_samples < dst_offset + len`, or
   `av_channel_layout_compare(&dst->ch_layout, &src->ch_layout)` is
   non-zero.
3. Raises the same error when `extended_data[i]` is `NULL` in either frame
   for any plane index below the plane count.
4. Releases the runtime lock, calls
   `av_samples_copy(dst->extended_data, src->extended_data, dst_offset,
src_offset, len, channels, dst->format)`, re-acquires the lock.

Offsets and length are in samples per channel. The sample format of `src`
is not read.

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

`create_frame width height pixel_format`:

1. `av_frame_alloc`; `NULL` raises `Out_of_memory`.
2. Sets `frame->format` (converted), `frame->width`, `frame->height`.
3. `av_frame_get_buffer(frame, 32)`. A negative return frees the frame and
   raises the mapped error.
4. Wraps the frame. Pixel data is uninitialised.

`frame_get_linesize frame n`: raises
``Error (`Failure "Failed to get linesize from video frame : line (n) out of
boundaries")`` when `n < 0`, `n >= AV_NUM_DATA_POINTERS` or `frame->data[n]` is `NULL`. Otherwise returns `frame->linesize[n]`.

`frame_visit ~make_writable f frame`:

1. When `make_writable` is true, calls `av_frame_make_writable(frame)`. A
   negative return raises the mapped error.
2. Calls `av_pix_fmt_count_planes(frame->format)`. A negative return raises
   the mapped error.
3. Builds an array with one `(data, linesize)` pair per plane. `linesize` is
   `frame->linesize[i]`. `data` is a bigarray of length
   `frame->linesize[i] * frame->height` whose storage is `frame->data[i]`
   itself (shared, not copied; section 8.2).
4. Calls `f planes`.
5. Returns `frame`. An exception from `f` propagates unchanged.

Getters:

| Function                    | Returns                                                              |
| --------------------------- | -------------------------------------------------------------------- |
| `frame_get_width`           | `frame->width`                                                       |
| `frame_get_height`          | `frame->height`                                                      |
| `frame_get_pixel_format`    | `frame->format` converted through the pixel-format table             |
| `frame_get_pixel_aspect`    | `None` when `frame->sample_aspect_ratio.num == 0`, else `Some` of it |
| `frame_get_color_space`     | `frame->colorspace` converted                                        |
| `frame_get_color_range`     | `frame->color_range` converted                                       |
| `frame_get_color_primaries` | `frame->color_primaries` converted                                   |
| `frame_get_color_trc`       | `frame->color_trc` converted                                         |
| `frame_get_chroma_location` | `frame->chroma_location` converted                                   |

A converted getter raises ``Error (`Failure "Could not find OCaml value
for ...")`` when the C value has no table entry.

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

`header_ass_default ()` returns this constant string, with lines ended by
CR LF and a final CR LF:

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

`create_frame content`:

1. Counts the rectangles.
2. Allocates a zeroed `AVSubtitle` (`Out_of_memory` on failure) and wraps it
   immediately, so the finaliser owns everything allocated afterwards.
3. Sets `format`, `start_display_time`, `end_display_time` (as `uint32_t`),
   `pts` (`AV_NOPTS_VALUE` for `None`) and `num_rects`.
4. When there is at least one rectangle, allocates the zeroed pointer array
   `rects` (`av_calloc`), then for each rectangle in list order:
   1. allocates a zeroed `AVSubtitleRect` and stores it in `rects[i]`;
   2. when `pict` is `Some`: copies `x`, `y`, `w`, `h`, `nb_colors`; for
      each of the four plane slots, when the bigarray at that index has a
      non-zero byte size, allocates that many bytes with `av_malloc` and
      copies the bigarray content into `data[p]`; stores the matching
      element of the linesize array into `linesize[p]`;
   3. when `pict` is `None`: geometry, data and linesizes stay zero;
   4. sets `flags` to the bitwise OR of the listed flags;
   5. sets `type` from `rect_type`;
   6. when `text` is non-empty, stores an `av_strdup` copy; when `ass` is
      non-empty, stores an `av_strdup` copy. Empty strings leave `NULL`.
5. Any allocation failure raises `Out_of_memory`.

Both arrays of `planes` must have at least four elements.

`get_content frame` returns a fresh `content`:

- `format`, `start_display_time`, `end_display_time` as integers;
- `pts`: `None` for `AV_NOPTS_VALUE`;
- `rectangles`: one per `rects[i]`, in order `0 .. num_rects - 1`:
  - `pict`: `Some` when `rect->data[0]` is non-`NULL`, else `None`. A `pict`
    holds `x`, `y`, `w`, `h`, `nb_colors` and `planes = (data, linesizes)`,
    both arrays of length 4. `linesizes.(i)` is `rect->linesize[i]`.
    `data.(i)` is a fresh OCaml-managed bigarray holding a copy of plane
    `i`. Its size is: `AVPALETTE_SIZE` when the rectangle type is
    `SUBTITLE_BITMAP` and `i = 1`; otherwise `h * linesize[i]` when both are
    positive; otherwise 0. A `NULL` plane pointer also gives size 0.
  - `flags`: every entry of the generated flag table whose bit is set;
  - `rect_type`: `rect->type` converted;
  - `text`, `ass`: copies, `""` for `NULL`.

`get_pts frame`: `None` for `AV_NOPTS_VALUE`, else `Some subtitle->pts`.

### 4.15 AVOption introspection

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
  ...
end
```

`opts class` lists the options of an `AVClass` and of its direct child
classes. Algorithm: section 9.3. Result properties:

- one `opt` per `AVOption` whose type is in the table of section 3.2 and is
  not `AV_OPT_TYPE_CONST`;
- the list is in the reverse of iteration order (root class options first
  in iteration, then each child class in `av_opt_child_class_iterate`
  order);
- `name` is `option->name`; `help` is `None` for a `NULL` or empty help;
- `flags` holds every flag of the list in section 3.2 whose mask intersects
  `option->flags`, in the reverse of that list's order;
- `spec` carries `default`, `min`, `max` per section 3.2, wrapped in
  `` `Array `` when the option type has the array bit (section 10). For such
  an option the default is read from the same member of the default-value
  union as for a scalar of the element type;
- `values` is always `[]` in the current code (section 11.3).

### 4.16 AVOption reads

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
```

Each getter reads option `name` from the C object inside `obj`. The search
flags are `AV_OPT_SEARCH_CHILDREN` when `search_children` is passed (with
either boolean value) and 0 when it is omitted. A negative return from the
FFmpeg call raises the mapped error (an unknown name gives
`` `Option_not_found ``). The owner half of `obj` is kept alive until the
read returns.

| Getter               | FFmpeg call             | Result                                                                              |
| -------------------- | ----------------------- | ----------------------------------------------------------------------------------- |
| `get_string`         | `av_opt_get`            | copy of the returned string, which is then freed with `av_free`                     |
| `get_int`            | `av_opt_get_int`        | the `int64_t` narrowed to an OCaml `int`                                            |
| `get_int64`          | `av_opt_get_int`        | the `int64_t`                                                                       |
| `get_float`          | `av_opt_get_double`     | the `double`                                                                        |
| `get_rational`       | `av_opt_get_q`          | `{num; den}`                                                                        |
| `get_image_size`     | `av_opt_get_image_size` | `(width, height)`                                                                   |
| `get_pixel_fmt`      | `av_opt_get_pixel_fmt`  | converted through the table                                                         |
| `get_sample_fmt`     | `av_opt_get_sample_fmt` | converted through the table                                                         |
| `get_video_rate`     | `av_opt_get_video_rate` | `{num; den}`                                                                        |
| `get_channel_layout` | `av_opt_get_chlayout`   | a new value holding a copy                                                          |
| `get_dictionary`     | `av_opt_get_dict_val`   | all `(key, value)` pairs in dictionary order; the returned dictionary is then freed |

### 4.17 Option tables

```ocaml
type value =
  [ `String of string | `Int of int | `Int64 of int64 | `Float of float ]
type opts = (string, value) Hashtbl.t
val opts_default : opts option -> opts
val mk_opts_array : opts -> (string * string) array
val string_of_opts : opts -> string
val mk_audio_opts :
  ?opts:opts -> ?channels:int -> ?channel_layout:Channel_layout.t ->
  sample_rate:int -> sample_format:Sample_format.t ->
  time_base:rational -> unit -> opts
val mk_video_opts :
  ?opts:opts -> ?frame_rate:rational -> ?color_space:Color_space.t ->
  ?color_range:Color_range.t -> pixel_format:Pixel_format.t ->
  width:int -> height:int -> time_base:rational -> unit -> opts
val filter_opts : string array -> opts -> unit
```

Value rendering, used by `mk_opts_array` and `string_of_opts`:

| value           | text                |
| --------------- | ------------------- |
| `` `String s `` | `s`                 |
| `` `Int i ``    | `string_of_int i`   |
| `` `Int64 i ``  | `Int64.to_string i` |
| `` `Float f ``  | `string_of_float f` |

- `opts_default o`: `Some t` gives `t` itself (not a copy); `None` gives a
  new empty table.
- `mk_opts_array t`: one `(key, text)` pair per binding of the table,
  including shadowed bindings of the same key, in the reverse of
  `Hashtbl.fold` order.
- `string_of_opts t`: the `key=text` forms of all bindings joined with
  `","`, in the reverse of `Hashtbl.fold` order. No escaping.
- `filter_opts unused t`: removes from `t`, in place, every binding whose
  key is not an element of `unused`. What remains is the set of options
  FFmpeg did not consume.

`mk_audio_opts`:

1. Raises ``Error (`Failure "At least one of channels or channel_layout must
be passed!")`` when both `channels` and `channel_layout` are omitted.
2. Copies the caller's table (or starts from an empty one). The caller's
   table is not modified.
3. Adds, with `Hashtbl.add` (so an existing binding of the same key is
   shadowed, not replaced):

| key                | value                                           | when                   |
| ------------------ | ----------------------------------------------- | ---------------------- |
| `"ar"`             | `` `Int sample_rate ``                          | always                 |
| `"ac"`             | `` `Int channels ``                             | `channels` given       |
| `"channel_layout"` | `` `Int64 mask ``                               | `channel_layout` given |
| `"sample_fmt"`     | `` `Int (Sample_format.get_id sample_format) `` | always                 |
| `"time_base"`      | `` `String "num/den" ``                         | always                 |

`mask` is `get_mask channel_layout` when the layout has native order.
Otherwise it is the mask of `get_default (get_nb_channels
   channel_layout)`; if that default has no native mask either, the
function raises `Invalid_argument` (from `Option.get`).

`mk_video_opts`:

1. Copies the caller's table (or starts from an empty one).
2. Adds, with `Hashtbl.add`:

| key              | value                                         | when                                        |
| ---------------- | --------------------------------------------- | ------------------------------------------- |
| `"pixel_format"` | `` `Int (Pixel_format.get_id pixel_format) `` | always                                      |
| `"video_size"`   | `` `String "WxH" ``                           | always                                      |
| `"time_base"`    | `` `String "num/den" ``                       | always                                      |
| `"colorspace"`   | `` `String (Color_space.name cs) ``           | `color_space` given and not `` `Reserved `` |
| `"color_range"`  | `` `String (Color_range.name cr) ``           | `color_range` given                         |
| `"r"`            | `` `String "num/den" ``                       | `frame_rate` given                          |

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

`create_device_context ?device ?opts device_type`:

1. `device` defaults to `""`. An empty string becomes a `NULL` device name.
2. Renders `opts` (default: a new empty table) with `mk_opts_array` and
   fills an `AVDictionary` from it (section 1.4).
3. Releases the runtime lock, calls
   `av_hwdevice_ctx_create(&ctx, type, device, dict, 0)`, re-acquires it.
4. On a negative return: frees the dictionary and raises the mapped error.
5. Collects the keys still in the dictionary, frees it, wraps the context.
6. Calls `filter_opts unused opts`, so the caller's table keeps only the
   keys reported as unused.

`create_frame_context ~width ~height ~src_pixel_format ~dst_pixel_format
device`:

1. `av_hwframe_ctx_alloc(device)`; `NULL` raises `Out_of_memory`.
2. Sets, on the new `AVHWFramesContext`: `format = dst_pixel_format`,
   `sw_format = src_pixel_format`, `width`, `height`. Nothing else is set
   (`initial_pool_size` stays 0).
3. Releases the runtime lock, calls `av_hwframe_ctx_init`, re-acquires it.
4. On a negative return: unreferences the frames context and raises the
   mapped error.
5. Wraps the reference.

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

The same table read right to left is used by `string_of_error`.

### 5.2 Binding failures

``Error (`Failure msg)`` is raised by the binding itself:

| Message                                                                  | Raised by                                                                                                        |
| ------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------- |
| the generated "no C value" message ([build.md](build.md) §3.6)           | any OCaml-to-C enum conversion with no table entry                                                               |
| the generated "no OCaml value" message ([build.md](build.md) §3.6)       | any C-to-OCaml enum conversion with no table entry, including `Sample_format.find_id` and `Pixel_format.find_id` |
| `"Invalid format name"`                                                  | `Pixel_format.of_string`                                                                                         |
| `"Empty frame"`                                                          | wrapping a `NULL` `AVFrame` (sibling libraries)                                                                  |
| `"Empty subtitle"`                                                       | wrapping a `NULL` `AVSubtitle` (sibling libraries)                                                               |
| `"Failed to get linesize from video frame : line (N) out of boundaries"` | `Video.frame_get_linesize`                                                                                       |
| `"At least one of channels or channel_layout must be passed!"`           | `mk_audio_opts`                                                                                                  |

### 5.3 Other exceptions

| Exception                          | Raised by                                                                                                                                                       |
| ---------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Not_found`                        | `Sample_format.find`, `Pixel_format.descriptor`                                                                                                                 |
| `Out_of_memory`                    | every failed FFmpeg allocation the stubs check: frames, channel layouts, subtitles, name copy in `Sample_format.find`, hardware frames context, layout iterator |
| `Failure "Invalid option type!"`   | internal option read with an unknown selector; unreachable through the public getters                                                                           |
| `Failure "Invalid option flag!"`   | internal flag conversion with an unknown flag; unreachable through the public API                                                                               |
| `Failure "Incompatible constant!"` | constant merging in `Options.opts`; unreachable in the current code (section 11.3)                                                                              |
| `Invalid_argument`                 | `mk_audio_opts` with a non-native layout whose channel count has no native default                                                                              |

### 5.4 State after an error

No operation of this library leaves an OCaml-visible object in a different
state after raising, with one exception: `Video.frame_visit
~make_writable:true` may have replaced the frame's buffers with a private
copy before the plane count check or the visitor raises.

## 6. Blocking and concurrency

### 6.1 Calls that release the runtime lock

| Operation                           | Lock released around        |
| ----------------------------------- | --------------------------- |
| `Audio.frame_copy_samples`          | `av_samples_copy`           |
| `HwContext.create_device_context`   | `av_hwdevice_ctx_create`    |
| `HwContext.create_frame_context`    | `av_hwframe_ctx_init`       |
| the log thread's wait (section 7.1) | the condition-variable wait |

Every other operation holds the runtime lock for its whole duration,
including `Frame.copy`, `Video.create_frame`, `Audio.create_frame` and
`Video.frame_visit ~make_writable:true`.

### 6.2 Global state

| State                                                     | Protection                                    |
| --------------------------------------------------------- | --------------------------------------------- |
| FFmpeg's log level and log callback                       | FFmpeg's own                                  |
| pending log message list                                  | lock-free push; atomic take-all               |
| log wake-up flag                                          | a C mutex and condition variable              |
| "last line ended with newline" flag used for log prefixes | atomic load and store, not a critical section |
| OCaml log state: thread-running flag, stop-request flag   | one OCaml mutex                               |
| current log callback                                      | an atomic reference                           |
| failure message buffer                                    | none; one process-wide buffer                 |
| thread registration key                                   | created once (`pthread_once`)                 |

### 6.3 Thread registration (exported to sibling stubs)

One function, called by sibling stubs at the start of every C callback that
FFmpeg may run on a thread the OCaml runtime does not know (I/O callbacks,
interrupt callbacks, device callbacks). `Avutil` itself never calls it.

1. Creates, once per process, a thread-specific key with a destructor.
2. Registers the calling thread with the OCaml runtime.
3. When that registration reports that the thread was newly registered, and
   the thread-specific slot is empty, stores a non-`NULL` marker in the
   slot.
4. At thread exit, the destructor runs for threads that have the marker and
   unregisters the thread from the OCaml runtime.

The function is idempotent and cheap on an already-registered thread. It
does not acquire the runtime lock; the caller does that next.

### 6.4 Thread safety of objects

No object carries a lock. Frames, subtitles and channel layouts are safe to
share between threads only for reads. `Audio.frame_copy_samples` reads and
writes sample data while other OCaml threads run.

## 7. Callbacks

### 7.1 Log messages

This is the only C-to-OCaml path of the library, and it is indirect: FFmpeg
never calls OCaml code.

**C side, on whichever thread FFmpeg logs from, with any lock state:**

1. Returns when the message level is above `av_log_get_level()`.
2. Allocates a node holding a 1024-byte buffer; returns silently when the
   allocation fails (the message is lost).
3. Formats the message with `av_log_format_line2` into the buffer, using
   and updating the shared "print prefix" flag. One node per `av_log` call;
   a node may hold a partial line.
4. Pushes the node on the head of the pending list (lock-free).
5. When the list was empty before the push, locks the C mutex, signals the
   condition variable, unlocks.

This path touches no OCaml state and needs no thread registration.

**`Log.set_callback f`:**

1. Stores `f` as the current callback (atomically, outside the mutex).
2. Under the OCaml log mutex: when the thread-running flag is false,
   installs the C callback with `av_log_set_callback`, starts the log
   thread, clears the stop-request flag and sets the thread-running flag.
   When the flag is true, does nothing more.

**The log thread** (an OCaml thread) loops:

1. Under the OCaml log mutex: takes the whole pending list, frees the
   nodes, and calls the current callback once per message. Then, when the
   stop-request flag is set: restores `av_log_default_callback`, clears the
   thread-running flag and ends the thread.
2. Otherwise waits, with the runtime lock released, until the pending list
   is non-empty or the wake-up flag is set; clears the wake-up flag; loops.

**`Log.clear_callback ()`**, under the OCaml log mutex:

1. Restores `av_log_default_callback`.
2. Takes the pending list and calls the current callback once per message,
   on the calling thread.
3. Sets the stop-request flag.
4. Sets the wake-up flag and signals the condition variable.

**Observable properties:**

- The callback runs on the log thread, or on the thread calling
  `clear_callback`. It always runs with the runtime lock held and with the
  OCaml log mutex held.
- Messages of one take are delivered newest first. Successive takes are in
  chronological order.
- A message is delivered at most once. Messages longer than 1023 bytes are
  truncated.
- An exception raised by the callback on the log thread ends that thread;
  the mutex is released; the thread-running flag stays set. An exception
  raised by the callback inside `clear_callback` propagates to the caller
  after the mutex is released; steps 3 and 4 are skipped.
- The callback closure is kept alive by the atomic reference until replaced.
  It is never released: `clear_callback` leaves it in place.
- Calling `set_callback` while the thread-running flag is set only replaces
  the closure. This includes the interval between `clear_callback` and the
  log thread acting on the stop request.
- Before any `set_callback`, the stored callback is a function that fails
  an assertion.

### 7.2 Failure raising

Raising ``Error (`Failure msg)`` from C is implemented by calling an OCaml
closure registered at module load that raises the exception. It runs on the
calling thread with the runtime lock held.

## 8. Data transfer

### 8.1 Strings

Every string crossing the boundary is copied, in both directions: option
names and values, metadata, format names, descriptions, error messages, log
messages, subtitle text. OCaml strings are passed as C strings, so an
embedded NUL byte ends them.

### 8.2 Video planes — shared

`Video.frame_visit` hands out bigarrays that alias `frame->data[i]`.

- No copy. Writes through the bigarray write the frame.
- The bigarray does not keep the frame alive and does not own the memory.
  It is valid during the visitor call. After the frame is collected, or
  after its buffers are replaced (`av_frame_make_writable` on a later
  visit), a retained bigarray points at released memory.
- Length is `linesize[i] * height` for every plane, with the luma height.
- Stride is `linesize[i]`, returned alongside. Rows include FFmpeg's
  padding.
- Frames created by `Video.create_frame` have buffers aligned to 32 bytes.

### 8.3 Audio samples

`Avutil` exposes no bigarray view of audio samples. `frame_copy_samples`
copies between two frames inside C. Sibling libraries (resampler) own the
bigarray paths and use the sample-format-to-kind map of section 1.4.

### 8.4 Subtitle bitmaps — copied

`Subtitle.get_content` copies each plane into a new OCaml-managed bigarray.
`Subtitle.create_frame` copies each non-empty bigarray into new C memory.
Sizes: section 4.14.

### 8.5 Frames

`Frame.copy` copies data between two already-allocated frames
(`av_frame_copy`); it copies no properties. A frame handed to a sibling
library is passed by its `AVFrame *`; whether that library references or
copies it is specified there.

### 8.6 Channel layouts

Always copied on the way into an OCaml value (section 2.2). Copied again
into `frame->ch_layout` by `Audio.create_frame`.

### 8.7 `data`

`create_data` returns an ordinary OCaml-managed bigarray; C code holds no
reference to it.

## 9. Options

### 9.1 Option dictionaries

The user-facing form is `opts`, a `Hashtbl` from option name to `value`.
The path to FFmpeg is:

1. `mk_opts_array` renders it to `(string * string) array`.
2. The exported C helper turns the array into an `AVDictionary`.
3. The FFmpeg call receives the dictionary.
4. The exported C helper returns the keys left in the dictionary and frees
   it.
5. `filter_opts` prunes the user's table down to those keys.

After a call that takes `?opts`, the caller inspects its own table: any key
still present was not consumed. When `?opts` is omitted the report is
discarded.

`mk_audio_opts` and `mk_video_opts` return a copy of the user's table with
derived entries added; the comment in the code gives the reason: derived
options must not be mixed into the caller's table, or they would be
indistinguishable from the caller's own when the unused report comes back.

### 9.2 Within `Avutil`

Only `HwContext.create_device_context` takes an option table.

### 9.3 Listing options (`Options.opts`)

Each step of the iteration takes a cursor (absent on the first step) and
the root class, and returns the next raw option with a new cursor, or the
end.

A cursor holds three borrowed pointers: the child-class iteration state,
the last `AVOption` returned, and the class currently being walked.

One step:

1. Without a cursor: current class is the root class, last option and
   child state are `NULL`. With a cursor: restore all three.
2. When the current class is `NULL`: end.
3. `option = av_opt_next(&class, option)` (the address of the class pointer
   stands in for an object of that class).
4. While `option` is `NULL`: `class = av_opt_child_class_iterate(root,
&child_state)`; when that is `NULL`: end; else
   `option = av_opt_next(&class, NULL)`.
5. When the option's type (array bit cleared) is `AV_OPT_TYPE_CONST` or is
   not in the table of section 3.2: signal "not implemented", carrying the
   cursor positioned on this option.
6. Otherwise return: name, help, type tag with default/min/max, the raw
   `flags` integer, the unit (`None` for `NULL` or empty), and the cursor.

Children of child classes are not visited.

The OCaml driver:

1. Starts with no cursor and an empty accumulator.
2. On an option: prepends it and continues from its cursor.
3. On "not implemented": continues from the carried cursor, dropping the
   option.
4. At the end: converts each accumulated raw option to an `opt` (flags
   decoded per section 3.2, `values = []`).

### 9.4 Reading options (`Options.obj`)

An `obj` is built by a sibling library as a pair: a borrowed pointer to an
object whose first member is an `AVClass *`, and the OCaml value that owns
that object. A getter unpacks the pair, performs the read on the pointer,
and only then drops the owner, so the owner cannot be collected during the
read.

There is no option setter on a live object in `Avutil`.

## 10. Version-dependent behaviour

| Condition                                              | When true                                                                                                          | When false                                                                     |
| ------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------ |
| libavutil ≥ 59.1.100 (`AV_OPT_TYPE_FLAG_ARRAY` exists) | The array bit is cleared before mapping an option type; an option with the bit is reported as `` `Array ground ``. | No masking; `` `Array `` is never produced.                                    |
| libavutil ≥ 57.30.100 (`AVFrame.duration` exists)      | `Frame.duration` and `set_duration` read and write `frame->duration`.                                              | `duration` returns `None`; `set_duration` does nothing.                        |
| `AV_OPT_FLAG_BSF_PARAM` defined                        | `` `Bsf_param `` maps to it.                                                                                       | Maps to 0; never reported.                                                     |
| `AV_OPT_FLAG_DEPRECATED` defined                       | `` `Deprecated `` maps to it.                                                                                      | Maps to 0.                                                                     |
| `AV_OPT_FLAG_RUNTIME_PARAM` defined                    | `` `Runtime_param `` maps to it.                                                                                   | Maps to 0.                                                                     |
| `AV_OPT_FLAG_AV_OPT_FLAG_CHILD_CONSTS` defined         | `` `Child_consts `` would map to `AV_OPT_FLAG_CHILD_CONSTS`.                                                       | Maps to 0. FFmpeg defines no macro of that name, so this side is always taken. |

`version` reflects the libavutil linked at run time; the conditions above
are decided at compile time.

The enum tables are generated from the headers present at build time, so
the set of constructors of every generated variant type depends on the
FFmpeg version built against.

## 11. Logic on the OCaml side

### 11.1 Version

Decoding of the packed version integer and `compare_version` (section 4.2).

### 11.2 Log thread

The state machine of section 7.1 lives in OCaml: one mutex, two booleans
(thread running, stop requested), one atomic callback reference, one
thread. The C side provides five primitives: install callback, restore
default callback, take pending messages, wait, wake.

A helper runs a function under the mutex and releases it on both normal and
exceptional exit, re-raising with the original backtrace.

### 11.3 Option listing

The driver of section 9.3, the flag decoding, and a constant-merging pass:

- The driver keeps a table from unit name to `(constant name, raw
constant)`, filled when the iteration returns an option of constant kind.
- When converting an option that has a unit present in that table, each
  constant of the unit is appended to the option's `values`, converted
  according to the option's type.

In the current code the iteration never returns an option of constant kind
(step 5 of section 9.3 signals "not implemented" for it), so the table
stays empty and `values` is always `[]`.

### 11.4 Option tables

`opts_default`, `mk_opts_array`, `string_of_opts`, `filter_opts`,
`mk_audio_opts`, `mk_video_opts` are pure OCaml (section 4.17).

### 11.5 Other

- `Frame.metadata` / `set_metadata` and `Options.get_dictionary` convert
  between lists and the arrays the stubs use.
- `Video.frame_visit` is: build planes, call the visitor, return the frame.
- `Channel_layout.standard_layouts` is an OCaml loop over a C iterator.
- `HwContext.create_device_context` applies defaults and the
  render/prune steps of section 9.1 around the stub.
- `Subtitle.header_ass_default` is an OCaml string constant.
