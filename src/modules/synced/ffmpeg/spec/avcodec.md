# avcodec — as-built specification (Part A)

Mechanism notes (block layouts, rooting, macros, build) are in
[language-notes/avcodec.md](language-notes/avcodec.md). Observations and
judgement are in [findings/avcodec.md](findings/avcodec.md).

## 1. Scope

The library binds **libavcodec**: codec lookup and introspection, codec
parameters, packets, decoders, encoders and bitstream filters. It also
includes `libavutil/replaygain.h` for the replay-gain side-data layout.
It binds no parser (`AVCodecParser`): there is no parser object and no parser
operation.

It depends on the `avutil` binding library for: the `Error` exception and the
shared error-raising path, the shared failure path (raises
``Avutil.Error (`Failure msg)``), the shared option-dictionary conversion and
unused-option report, rationals, channel layouts, sample formats, pixel
formats, colour spaces and ranges, hardware device types, hardware
device/frame context values, frame values, the media-type table, the `AVClass`
wrapper used by `Avutil.Options.t`, and the OCaml helpers `opts_default`,
`mk_audio_opts`, `mk_video_opts`, `mk_opts_array`, `filter_opts`.

Minimum version. The code carries one guard for libavcodec older than
58.9.100 (see §10), and uses unconditionally `av_codec_iterate`,
`av_bsf_iterate`, `avcodec_get_hw_config`, the send/receive codec API, and
`AVChannelLayout` (`AVCodecContext.ch_layout`, `AVCodecParameters.ch_layout`,
`AVCodec.ch_layouts`). The effective floor is therefore the first libavcodec
with `AVChannelLayout` (FFmpeg 5.1).

The installed C header exports to sibling stub libraries:

| Export                                   | Meaning                                                                                                                   |
| ---------------------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| `AV_PROFILE_UNKNOWN`, `AV_LEVEL_UNKNOWN` | defined as `FF_PROFILE_UNKNOWN`, `FF_LEVEL_UNKNOWN` when libavcodec < 60.26.100                                           |
| codec open helper                        | sets `thread_count = 0`, then calls `avcodec_open2` with the runtime lock released; returns the FFmpeg return code (§4.9) |
| codec accessor and wrapper               | read the `const AVCodec *` out of a codec value; wrap one into a fresh codec value                                        |
| parameters accessor and copy constructor | read the `AVCodecParameters *`; build a parameters value holding a _copy_ of a given `AVCodecParameters`                  |
| packet accessor and constructor          | read the `AVPacket *`; build a packet value that _takes ownership_ of a given `AVPacket`                                  |
| codec-id converters                      | both directions for the audio, video, subtitle and unknown id families                                                    |

## 2. Objects

### 2.1 Codec — `('media, 'mode) codec`

- C object: `const AVCodec *`, a static table entry owned by libavcodec.
- Creation: the `find_*` functions and the module-load enumeration (§11).
- Ownership: none. The value is a plain holder of the pointer. Nothing is
  released, there is no finaliser.
- The two type parameters are phantom. `'media` is the media kind, `'mode` is
  `encode` or `decode`. The lookup functions enforce the media kind at run
  time; the enumeration assigns it by table membership (§11).
- Structural comparison and hashing act on the holder, not on the codec.

### 2.2 Codec parameters — `'media params`

- C object: `AVCodecParameters *`, always a private copy made with
  `avcodec_parameters_alloc` + `avcodec_parameters_copy`.
- Creation: `Avcodec.params` (from an encoder), `BitstreamFilter.init`
  (output parameters), and sibling libraries through the exported copy
  constructor.
- Ownership: the OCaml value owns its copy. It keeps nothing else alive and
  nothing keeps it alive.
- Release: garbage collection only; the finaliser calls
  `avcodec_parameters_free`. No explicit close.
- Immutable from OCaml: the library exposes getters only.
- The copy constructor fails with ``Error (`Failure "Failed to get codec
parameters")`` when given a null source, raises `Out_of_memory` when the
  allocation fails, and raises the mapped error (after freeing the new
  structure) when the copy fails.

### 2.3 Packet — `'media Packet.t`

- C object: `AVPacket *` allocated with `av_packet_alloc`.
- Creation: `Packet.create`, `Packet.dup`, the encoder receive path,
  `BitstreamFilter.receive_packet`, and sibling libraries through the exported
  constructor.
- Ownership: the OCaml value owns the `AVPacket` structure. The payload is a
  reference-counted `AVBufferRef`; `Packet.dup` and FFmpeg itself may hold
  further references to the same buffer.
- Release: garbage collection only; the finaliser calls `av_packet_free`
  (drops one buffer reference and frees side data). No explicit close.
- GC accounting: the value reports `packet->buf->size` bytes of external
  memory at creation (0 when the packet has no buffer). The figure is fixed
  at creation.
- Mutable from OCaml: stream index, pts, dts, duration, position, side data.
  Not settable: flags, size, payload.
- `BitstreamFilter.send_packet` empties the packet on success (§4.8).
- The exported constructor fails with ``Error (`Failure "Empty packet")`` on a
  null pointer.

### 2.4 Decoder and encoder — `'media decoder`, `'media encoder`

Both are the same C object: a small record holding

| Field         | Content                                                       |
| ------------- | ------------------------------------------------------------- |
| codec         | the `const AVCodec *` it was created from                     |
| codec context | `AVCodecContext *`, null until allocated                      |
| flushed       | integer flag, initially 0, used by the encoder send path only |

- Creation: `Audio.create_decoder`, `Video.create_decoder`,
  `Audio.create_encoder`, `Video.create_encoder`. The context is always
  opened before the value reaches the user; there is no "created but not
  opened" state visible from OCaml.
- Ownership: the OCaml value owns the record and the codec context. A video
  encoder's codec context owns its own `av_buffer_ref` references to the
  hardware device context and hardware frame context it was given, so the
  OCaml `HwContext` values may be collected independently.
- Release: garbage collection only; the finaliser calls
  `avcodec_free_context` when the context is non-null, then frees the record.
  No explicit close. A creation that fails after the record exists leaves the
  cleanup to this finaliser.
- No GC accounting: the value reports no external memory.
- Encoder states:

  | State                 | Reached by      | `encode`                                     | `flush_encoder`                     |
  | --------------------- | --------------- | -------------------------------------------- | ----------------------------------- |
  | open (flushed = 0)    | creation        | sends and drains                             | drains, sends end of stream, drains |
  | flushed (flushed = 1) | `flush_encoder` | raises ``Error `Eof`` before touching FFmpeg | drains, then raises ``Error `Eof``  |

  No operation leaves the flushed state.

- Decoder states are those of libavcodec: after `flush_decoder` the context
  is draining; `decode` then raises whatever `avcodec_send_packet` returns
  (``Error `Eof``), and a further `flush_decoder` returns silently. No
  operation leaves the drained state (`avcodec_flush_buffers` is not bound).

### 2.5 Bitstream filter descriptor — `BitstreamFilter.filter`

A private OCaml record `{ name; codecs; options }` built once at module load.
`options` wraps the filter's `priv_class` pointer (static, possibly null) as
an `Avutil.Options.t`. It owns nothing.

### 2.6 Bitstream filter instance — `'a BitstreamFilter.t`

- C object: `AVBSFContext *` from `av_bsf_alloc`, initialised with
  `av_bsf_init` before the value reaches the user.
- Ownership: the OCaml value owns the context. The input parameters are
  copied into `par_in`; the returned output parameters are a copy of
  `par_out`. Neither parameters value is tied to the filter's lifetime.
- Release: garbage collection only; the finaliser calls `av_bsf_free`. No
  explicit close. No GC accounting.
- States are those of libavcodec (accepting input, output pending, end of
  stream signalled).

### 2.7 Iteration cursors

Codec and bitstream-filter enumeration carry FFmpeg's opaque iteration
pointer between calls in a plain holder value. They are internal to module
initialisation and own nothing.

## 3. Enumerations and constants

### 3.1 Generated tables consumed

| Table                                                  | OCaml type             | Used for                | Direction            |
| ------------------------------------------------------ | ---------------------- | ----------------------- | -------------------- |
| codec id, audio family                                 | `Codec_id.audio`       | `Audio.id`              | both                 |
| codec id, video family                                 | `Codec_id.video`       | `Video.id`              | both                 |
| codec id, subtitle family                              | `Codec_id.subtitle`    | `Subtitle.id`           | both                 |
| codec id, unknown family (includes `AV_CODEC_ID_NONE`) | `Codec_id.unknown`     | `Unknown.id`            | both                 |
| codec id, all (includes `AV_CODEC_ID_NONE`)            | `Codec_id.codec_id`    | `Avcodec.id`            | both                 |
| codec capabilities (`AV_CODEC_CAP_*`)                  | `Codec_capabilities.t` | `capabilities`          | C to OCaml, bit test |
| codec properties (`AV_CODEC_PROP_*`)                   | `Codec_properties.t`   | `descriptor.properties` | C to OCaml, bit test |
| hardware config method (`AV_CODEC_HW_CONFIG_METHOD_*`) | `Hw_config_method.t`   | `hw_configs`            | C to OCaml, bit test |

It also consumes the `avutil` media-type table (C to OCaml, for
`descriptor.media_type`).

The OCaml side of the codec-id table also provides the value lists
`Codec_id.audio`, `Codec_id.video`, `Codec_id.subtitle`, `Codec_id.unknown`
that `codec_ids` re-exports.

Missing mapping:

- Value conversions (codec ids in both directions, and the `avutil`
  conversions used here) raise ``Error (`Failure "Could not find ... value for
N in <table>. Do you need to recompile the ffmpeg binding?")``.
- Bit-test conversions (capabilities, properties, hardware methods) walk the
  generated table and emit the variants whose bit is set. A bit with no table
  entry is dropped silently.
- The codec enumeration matches ids by direct table scan and yields "no id"
  for a codec whose id is in none of the audio, video and subtitle families.

### 3.2 Hand-written mappings

Packet flags (`Packet.flag`):

| Variant           | C constant                                                             |
| ----------------- | ---------------------------------------------------------------------- |
| `` `Keyframe ``   | `AV_PKT_FLAG_KEY`                                                      |
| `` `Corrupt ``    | `AV_PKT_FLAG_CORRUPT`                                                  |
| `` `Discard ``    | `AV_PKT_FLAG_DISCARD`                                                  |
| `` `Trusted ``    | `AV_PKT_FLAG_TRUSTED`                                                  |
| `` `Disposable `` | `AV_PKT_FLAG_DISPOSABLE`, defined as `0x0010` when the headers lack it |

Direction: variant to bit mask only. Any other value raises the standard
`Failure "Invalid flag type!"` (unreachable through the typed API).

Packet side data (`Packet.side_data`):

| Variant                 | C type                         | Payload                                      |
| ----------------------- | ------------------------------ | -------------------------------------------- |
| `` `Replaygain ``       | `AV_PKT_DATA_REPLAYGAIN`       | `AVReplayGain` (four fields)                 |
| `` `Strings_metadata `` | `AV_PKT_DATA_STRINGS_METADATA` | byte string of NUL-separated keys and values |
| `` `Metadata_update ``  | `AV_PKT_DATA_METADATA_UPDATE`  | same                                         |

Both directions. On read, side data of any other type is skipped.

"Absent" sentinels:

| Field                        | C value read as `None` / written for `None` |
| ---------------------------- | ------------------------------------------- |
| packet pts, dts              | `AV_NOPTS_VALUE`                            |
| packet duration              | `0`                                         |
| packet position              | `-1`                                        |
| parameters pixel format      | `AV_PIX_FMT_NONE`                           |
| parameters pixel aspect      | `sample_aspect_ratio.num == 0`              |
| codec long name (descriptor) | null pointer                                |

Other constants: `flag_qscale` is `AV_CODEC_FLAG_QSCALE`. The codec context
thread count is forced to `0` (automatic) before every open (§4.9).

## 4. Operations

Unless stated otherwise an operation holds the runtime lock, cannot fail
except by `Out_of_memory`, and copies every string it returns.

### 4.1 Top level: version, types, constants, encoder accessors

```ocaml
val version : version
```

Computed once at module load from `avcodec_version()` (the run-time library
version): `major = v lsr 16`, `minor = (v lsr 8) land 0xff`,
`micro = v land 0xff`.

```ocaml
type ('media, 'mode) codec
type 'media params
type 'media decoder
type 'media encoder
type encode = [ `Encoder ]
type decode = [ `Decoder ]
type profile = { id : int; profile_name : string }
type descriptor = {
  media_type : Avutil.media_type;
  name : string;
  long_name : string option;
  properties : Codec_properties.t list;
  mime_types : string list;
  profiles : profile list;
}
```

See §2. `descriptor` mirrors `AVCodecDescriptor`.

```ocaml
val flag_qscale : int
```

`AV_CODEC_FLAG_QSCALE`, read once at module load.

```ocaml
val params : 'media encoder -> 'media params
```

1. Allocate a temporary `AVCodecParameters`.
2. `avcodec_parameters_from_context(temp, codec_context)`. On error free the
   temporary and raise the mapped error.
3. Build the result as a copy of the temporary (§2.2), then free the
   temporary.

Each call returns a fresh, independent snapshot.

```ocaml
val descriptor : 'media params -> descriptor option
```

The codec descriptor for `params->codec_id`; see "Descriptor construction"
below.

```ocaml
val time_base : 'media encoder -> Avutil.rational
```

The codec context's `time_base`, as set at open time by the `time_base`
option.

```ocaml
val name : _ codec -> string
```

`codec->name`.

```ocaml
type capability = Codec_capabilities.t
val capabilities : ([< `Audio | `Video ], encode) codec -> capability list
```

Every table variant whose bit is set in `codec->capabilities`, in generated
table order.

```ocaml
type hw_config_method = Hw_config_method.t
type hw_config = {
  pixel_format : Pixel_format.t;
  methods : hw_config_method list;
  device_type : HwContext.device_type;
}
val hw_configs : ([< `Audio | `Video ], _) codec -> hw_config list
```

1. Call `avcodec_get_hw_config(codec, i)` for `i = 0, 1, …` until it returns
   null.
2. For each config: `pixel_format` is `pix_fmt` converted with the `avutil`
   pixel-format conversion; `methods` holds every table variant whose bit is
   set in `methods`; `device_type` is converted with the `avutil`
   device-type conversion.
3. The result lists configs in **reverse** index order (last config first),
   and each `methods` list in reverse table order.

Empty list when index 0 is null. A conversion with no mapping raises the
failure described in §3.1.

**Descriptor construction** (shared by `descriptor`, `Audio.descriptor`,
`Video.descriptor`, `Subtitle.descriptor`):

1. `avcodec_descriptor_get(id)`. Null gives `None`.
2. `media_type`: `type` through the `avutil` media-type table.
3. `name`: copy of `name`. `long_name`: `Some` copy, or `None` when null.
4. `properties`: every table variant whose bit is set in `props`, table
   order.
5. `mime_types`: copies of the NULL-terminated `mime_types` array, in order;
   empty when the array pointer is null.
6. `profiles`: one `{ id = profile; profile_name = name }` per entry of
   `profiles`, in order, stopping at the entry whose `profile` is
   `AV_PROFILE_UNKNOWN`; empty when the pointer is null.

### 4.2 `Packet`

```ocaml
type 'media t
type flag = [ `Keyframe | `Corrupt | `Discard | `Trusted | `Disposable ]
type replaygain = {
  track_gain : int; track_peak : int; album_gain : int; album_peak : int;
}
type side_data =
  [ `Replaygain of replaygain
  | `Strings_metadata of (string * string) list
  | `Metadata_update of (string * string) list ]
```

```ocaml
val add_side_data : 'media t -> side_data -> unit
```

1. OCaml side: a metadata list is encoded as
   `k1 NUL v1 NUL k2 NUL v2 …` — each pair as `key NUL value`, pairs joined
   with a single NUL, **no trailing NUL**. An empty list gives the empty
   string.
2. Metadata variants: allocate `len` bytes with `av_malloc`, copy the encoded
   string, call `av_packet_add_side_data(packet, type, data, len)`.
3. Replay gain: allocate `sizeof(AVReplayGain)` bytes with `av_malloc`, store
   the four record fields (each truncated to the C field type), call
   `av_packet_add_side_data`.
4. The return value of `av_packet_add_side_data` is not examined.

Failure: `Out_of_memory` when `av_malloc` fails; the packet is unchanged.
FFmpeg keeps one entry per side-data type: a call for a type the packet
already carries replaces that entry.

```ocaml
val side_data : 'media t -> side_data list
```

1. Walk `packet->side_data[0 .. side_data_elems-1]` in order, keeping only
   the three supported types.
2. Metadata entries: copy the raw bytes; the OCaml side splits them on NUL
   and pairs consecutive fields `(k, v)`. A final unpaired field (the empty
   string after a trailing NUL, or an odd leftover) is dropped. The pairs of
   one entry come out in **reverse** order of their position in the bytes.
3. Replay gain: fails with ``Error (`Failure "Invalid side_data")`` when the
   entry is smaller than `sizeof(AVReplayGain)`; otherwise the four fields as
   integers.
4. Entries appear in packet order.

```ocaml
val dup : 'media t -> 'media t
```

Allocates a new `AVPacket` and calls `av_packet_ref(new, old)`: the payload
buffer is shared by reference count (a non-reference-counted source is copied
into a new buffer by FFmpeg), properties and side data are copied. The return
value of `av_packet_ref` is not examined. `Out_of_memory` when the packet
allocation fails.

```ocaml
val get_flags : 'media t -> flag list
```

Reads `packet->flags`, then on the OCaml side tests the five flags in the
order keyframe, corrupt, discard, trusted, disposable and conses each set
flag onto the result; the list is therefore in the reverse of that order.
Unknown bits are ignored.

```ocaml
val get_size : 'media t -> int
val get_stream_index : 'media t -> int
val set_stream_index : 'media t -> int -> unit
```

Direct field access to `size` and `stream_index`.

```ocaml
val get_pts : 'media t -> Int64.t option
val set_pts : 'media t -> Int64.t option -> unit
val get_dts : 'media t -> Int64.t option
val set_dts : 'media t -> Int64.t option -> unit
val get_duration : 'media t -> Int64.t option
val set_duration : 'media t -> Int64.t option -> unit
val get_position : 'media t -> Int64.t option
val set_position : 'media t -> Int64.t option -> unit
```

Direct access to `pts`, `dts`, `duration`, `pos` with the sentinels of §3.2.
`set_duration p (Some 0L)` reads back as `None`; `set_position p (Some (-1L))`
reads back as `None`. No time-base conversion happens here.

```ocaml
val to_bytes : 'media t -> bytes
val content : 'media t -> string
```

Each returns a fresh copy of `packet->data[0 .. size-1]`.

```ocaml
val create : string -> 'media t
```

1. `av_packet_alloc`. `Out_of_memory` on failure.
2. `av_new_packet(packet, length)`. On a non-zero return free the packet and
   raise the mapped error.
3. Copy the string into `packet->data`.

All other fields keep their `av_packet_alloc` / `av_new_packet` defaults (no
timestamps, position −1, duration 0, stream index 0, no flags). The
`'media` parameter is chosen freely by the caller.

### 4.3 `Audio`

```ocaml
type 'mode t = (audio, 'mode) codec
type id = Codec_id.audio
val descriptor : id -> descriptor option
val codec_ids : Codec_id.audio list
val encoders : encode t list
val decoders : decode t list
```

`descriptor`: §4.1. `codec_ids`: the generated list. `encoders`, `decoders`:
computed at module load, §11.

```ocaml
val find_encoder_by_name : string -> encode t
val find_encoder : id -> encode t
val find_decoder_by_name : string -> decode t
val find_decoder : id -> decode t
```

1. Call `avcodec_find_encoder_by_name`, `avcodec_find_encoder`,
   `avcodec_find_decoder_by_name` or `avcodec_find_decoder` (the id converted
   through the audio family).
2. When the result is null or `codec->type != AVMEDIA_TYPE_AUDIO`, raise
   ``Error `Encoder_not_found`` (encoder variants) or
   ``Error `Decoder_not_found`` (decoder variants).
3. Otherwise wrap the pointer.

```ocaml
val get_supported_channel_layouts : _ t -> Avutil.Channel_layout.t list
val get_supported_sample_formats : _ t -> Avutil.Sample_format.t list
val get_supported_sample_rates : _ t -> int list
```

Obtain the codec's array (§10 for how), walk it to its terminator, convert
each element, return the list **in the codec's order**. A null array gives
the empty list. Each channel layout is an independent copy.

| List            | Terminator         |
| --------------- | ------------------ |
| channel layouts | `nb_channels == 0` |
| sample formats  | `-1`               |
| sample rates    | `0`                |

On the newer-API side of §10 an error from `avcodec_get_supported_config` is
raised as the mapped error.

```ocaml
val find_best_channel_layout :
  _ t -> Avutil.Channel_layout.t -> Avutil.Channel_layout.t
val find_best_sample_format :
  _ t -> Avutil.Sample_format.t -> Avutil.Sample_format.t
val find_best_sample_rate : _ t -> int -> int
```

OCaml logic, §11: the default when the codec lists it, else the first listed
value, else the default.

```ocaml
val create_decoder : ?params:audio params -> decode t -> audio decoder
```

Decoder creation, §4.9.

```ocaml
val sample_format : audio decoder -> Sample_format.t
```

The codec context's current `sample_fmt`, through the `avutil` conversion.

```ocaml
val create_encoder :
  ?opts:opts -> channel_layout:Channel_layout.t -> sample_rate:int ->
  sample_format:Avutil.Sample_format.t -> time_base:Avutil.rational ->
  encode t -> audio encoder
```

Audio encoder creation, §4.9.

```ocaml
val frame_size : audio encoder -> int
```

The codec context's `frame_size` (0 for codecs accepting any size).

```ocaml
val get_name : _ codec -> string
val get_description : _ codec -> string
val string_of_id : id -> string
val get_id : _ t -> id
```

`get_name`: `codec->name`. `get_description`: `codec->long_name`, or `""`
when null. `string_of_id`: `avcodec_get_name(id)`. `get_id`: `codec->id`
converted through the audio family (failure of §3.1 when the id is not in
that family).

```ocaml
val get_params_id : audio params -> id
val get_channel_layout : audio params -> Avutil.Channel_layout.t
val get_nb_channels : audio params -> int
val get_sample_format : audio params -> Avutil.Sample_format.t
val get_bit_rate : audio params -> int
val get_sample_rate : audio params -> int
```

| Getter               | Source                                                              |
| -------------------- | ------------------------------------------------------------------- |
| `get_params_id`      | `codec_id` through the audio family; failure of §3.1 when not in it |
| `get_channel_layout` | independent copy of `ch_layout`                                     |
| `get_nb_channels`    | `ch_layout.nb_channels`                                             |
| `get_sample_format`  | `format` cast to `AVSampleFormat`, `avutil` conversion              |
| `get_bit_rate`       | `bit_rate` (64-bit, returned as OCaml `int`)                        |
| `get_sample_rate`    | `sample_rate`                                                       |

### 4.4 `Video`

```ocaml
type 'mode t = (video, 'mode) codec
type id = Codec_id.video
val descriptor : id -> descriptor option
val codec_ids : Codec_id.video list
val encoders : encode t list
val decoders : decode t list
val find_encoder_by_name : string -> encode t
val find_encoder : id -> encode t
val find_decoder_by_name : string -> decode t
val find_decoder : id -> decode t
```

As in `Audio`, with the video id family and `AVMEDIA_TYPE_VIDEO`.

```ocaml
val get_supported_frame_rates : _ t -> Avutil.rational list
val get_supported_color_spaces : _ t -> Avutil.Color_space.t list
val get_supported_color_ranges : _ t -> Avutil.Color_range.t list
val get_supported_pixel_formats : _ t -> Avutil.Pixel_format.t list
```

Same scheme as the audio lists; codec order; empty for a null array.

| List          | Terminator                |
| ------------- | ------------------------- |
| frame rates   | `num == 0`                |
| pixel formats | `-1`                      |
| colour spaces | `AVCOL_SPC_UNSPECIFIED`   |
| colour ranges | `AVCOL_RANGE_UNSPECIFIED` |

Colour spaces and ranges are always empty on the older-API side of §10.

```ocaml
val find_best_frame_rate : _ t -> Avutil.rational -> Avutil.rational
val find_best_pixel_format :
  ?hwaccel:bool -> _ t -> Avutil.Pixel_format.t -> Avutil.Pixel_format.t
```

OCaml logic, §11.

```ocaml
val create_decoder : ?params:video params -> decode t -> video decoder
type hardware_context =
  [ `Device_context of HwContext.device_context
  | `Frame_context of HwContext.frame_context ]
val create_encoder :
  ?opts:opts -> ?frame_rate:Avutil.rational ->
  ?hardware_context:hardware_context -> pixel_format:Avutil.Pixel_format.t ->
  width:int -> height:int -> time_base:Avutil.rational ->
  encode t -> video encoder
```

§4.9.

```ocaml
val get_name : _ codec -> string
val get_description : _ codec -> string
val string_of_id : id -> string
val get_id : _ t -> id
```

As in `Audio`, video family.

```ocaml
val get_params_id : video params -> id
val get_width : video params -> int
val get_height : video params -> int
val get_sample_aspect_ratio : video params -> Avutil.rational
val get_pixel_format : video params -> Avutil.Pixel_format.t option
val get_pixel_aspect : video params -> Avutil.rational option
val get_bit_rate : video params -> int
```

| Getter                    | Source                                                |
| ------------------------- | ----------------------------------------------------- |
| `get_params_id`           | `codec_id` through the video family                   |
| `get_width`, `get_height` | `width`, `height`                                     |
| `get_sample_aspect_ratio` | `sample_aspect_ratio` as stored, including `0/1`      |
| `get_pixel_format`        | `format`; `None` for `AV_PIX_FMT_NONE`                |
| `get_pixel_aspect`        | `sample_aspect_ratio`; `None` when its numerator is 0 |
| `get_bit_rate`            | `bit_rate`                                            |

### 4.5 `Subtitle`

```ocaml
type 'mode t = (subtitle, 'mode) codec
type id = Codec_id.subtitle
val descriptor : id -> descriptor option
val codec_ids : Codec_id.subtitle list
val encoders : encode t list
val decoders : decode t list
val find_encoder_by_name : string -> encode t
val find_encoder : id -> encode t
val find_decoder_by_name : string -> decode t
val find_decoder : id -> decode t
val get_name : _ codec -> string
val get_description : _ codec -> string
val string_of_id : id -> string
val get_id : _ t -> id
val get_params_id : subtitle params -> id
```

As in `Audio`, with the subtitle id family and `AVMEDIA_TYPE_SUBTITLE`. This
library offers no subtitle decoder or encoder constructor.

### 4.6 `Unknown`

```ocaml
type 'mode t = ([ `Data ], 'mode) codec
type id = Codec_id.unknown
val codec_ids : Codec_id.unknown list
val string_of_id : id -> string
val get_params_id : [ `Data ] params -> id
```

`codec_ids`: the generated list. `string_of_id`: `avcodec_get_name`.
`get_params_id`: `codec_id` through the unknown family (which includes
`AV_CODEC_ID_NONE`).

### 4.7 All-codec ids

```ocaml
type id = Codec_id.codec_id
val string_of_id : id -> string
```

`avcodec_get_name` on the id converted through the all-ids table.

### 4.8 `BitstreamFilter`

```ocaml
type filter = private { name : string; codecs : id list; options : Avutil.Options.t }
type 'a t
val filters : filter list
```

`filters` is computed at module load (§11). For each filter returned by
`av_bsf_iterate`: `name` is a copy of `filter->name`; `codecs` is
`filter->codec_ids` up to `AV_CODEC_ID_NONE`, each converted through the
all-ids table, in order, empty when the pointer is null; `options` wraps
`filter->priv_class`.

```ocaml
val init : ?opts:opts -> filter -> 'a params -> 'a t * 'a params
```

1. OCaml side: take the caller's option table (a fresh empty one when
   absent) and flatten it to an array of string pairs.
2. `av_bsf_get_by_name(filter.name)`. Null raises the standard `Not_found`.
3. Build the option dictionary (shared conversion).
4. `av_bsf_alloc`. On error free the dictionary and raise the mapped error.
5. `avcodec_parameters_copy(bsf->par_in, params)`. On error free the
   dictionary and the context, raise.
6. `av_opt_set_dict(bsf, &options)` (no child search). On error free both,
   raise.
7. `av_bsf_init(bsf)` with the runtime lock released. On error free both,
   raise.
8. Collect the keys left in the dictionary (shared unused-option report,
   which frees the dictionary).
9. Wrap the context; build the output parameters as a copy of
   `bsf->par_out`.
10. OCaml side: reduce the caller's option table to the unused keys (§9).

`time_base_in` is left at its default. The input parameters value is not
modified.

```ocaml
val send_packet : 'a t -> 'a Packet.t -> unit
```

`av_bsf_send_packet(bsf, packet)` with the runtime lock released. A negative
return raises the mapped error (``Error `Eagain`` when output must be read
first). On success FFmpeg has moved the packet's content into the filter:
the OCaml packet value is left **empty** (no data, size 0, default fields).
A packet with no data and no side data signals end of stream.

```ocaml
val send_eof : 'a t -> unit
```

`av_bsf_send_packet(bsf, NULL)` with the runtime lock released; a negative
return raises the mapped error.

```ocaml
val receive_packet : 'a t -> 'a Packet.t
```

1. Allocate a packet (`Out_of_memory` on failure).
2. `av_bsf_receive_packet` with the runtime lock released.
3. A negative return frees the packet and raises the mapped error. This
   includes ``Error `Eagain`` (more input needed) and ``Error `Eof`` (drained).
4. Otherwise return the packet.

The library provides no drain loop for filters; the caller loops on
`receive_packet` and handles `` `Eagain `` and `` `Eof ``.

### 4.9 Creation and open path

**Open helper** (also exported to sibling stubs). Before every
`avcodec_open2` the helper sets `codec_context->thread_count = 0`.
libavcodec runs a codec on a single thread unless told otherwise; FFmpeg's
own tools raise that to automatic before opening, and the binding does the
same. Codecs that thread badly opt out through their own defaults. Options
passed to `avcodec_open2` are applied by FFmpeg after this assignment, so a
caller's `threads` option wins. `avcodec_open2` runs with the runtime lock
released.

**Decoder** (`Audio.create_decoder`, `Video.create_decoder`):

1. Allocate the zeroed decoder record and wrap it (context still null).
2. `avcodec_alloc_context3(codec)`; `Out_of_memory` when null.
3. When `params` is given, `avcodec_parameters_to_context(ctx, params)`. On
   error free the context and raise the mapped error.
4. Open helper with a null option dictionary. On error free the context and
   raise the mapped error.

No options, no hardware context, no time base and no `pkt_timebase` are set.
The `params` value is read, not retained.

**Audio encoder** (`Audio.create_encoder`):

1. OCaml side: build the effective options (§9): the caller's entries plus
   `ar`, `channel_layout`, `sample_fmt`, `time_base`.
2. Build the option dictionary.
3. Allocate and wrap the record; `avcodec_alloc_context3(codec)`.
4. `ctx->sample_fmt` = the sample format's integer id.
5. `av_channel_layout_copy(&ctx->ch_layout, layout)`. On error free the
   dictionary and raise.
6. Open helper with the dictionary. On error free the dictionary and raise.
7. Collect the unused keys (frees the dictionary).
8. OCaml side: reduce the caller's option table to the unused keys.

**Video encoder** (`Video.create_encoder`):

1. OCaml side: build the effective options (§9): the caller's entries plus
   `pixel_format`, `video_size`, `time_base`, and `r` when `frame_rate` is
   given. Split `hardware_context` into an optional device context and an
   optional frame context (at most one is present).
2. Build the option dictionary.
3. Allocate and wrap the record; `avcodec_alloc_context3(codec)`.
4. `ctx->pix_fmt` = the pixel format's integer id.
5. Device context given: `ctx->hw_device_ctx = av_buffer_ref(device)`;
   `Out_of_memory` (dictionary freed) when null.
6. Frame context given: `ctx->hw_frames_ctx = av_buffer_ref(frames)`; same
   failure handling.
7. Open helper with the dictionary. On error free the dictionary and raise.
8. Unused keys and option-table reduction as for audio.

Width, height, time base, sample rate and frame rate reach the codec context
only through the option dictionary.

On every encoder failure after step 3 the half-built context stays attached
to the record and is freed when the record is collected. A failure leaves the
caller's option table untouched.

### 4.10 Decode, encode, flush

```ocaml
val decode : 'media decoder -> ('media frame -> unit) -> 'media Packet.t -> unit
```

1. `avcodec_send_packet(ctx, packet)` with the runtime lock released. Any
   negative return raises the mapped error (including ``Error `Eagain`` and
   ``Error `Eof``). The packet is not modified; FFmpeg takes its own reference
   to the payload.
2. Receive loop: call the receive step until it yields nothing, passing each
   frame to `f` as soon as it is received.

Receive step (decoder):

1. Allocate an `AVFrame` (`Out_of_memory` on failure).
2. `avcodec_receive_frame` with the runtime lock released.
3. `AVERROR(EAGAIN)`: free the frame, yield nothing.
4. Any other negative return, **including `AVERROR_EOF`**: free the frame,
   raise the mapped error.
5. When the codec context has a hardware frames context: allocate a second
   frame, `av_hwframe_transfer_data(second, first, 0)` (download; FFmpeg
   picks the software format), free the first frame, continue with the
   second. On failure free both and raise. No frame properties are copied
   onto the second frame by the binding. This step holds the runtime lock.
6. Wrap the frame in an `avutil` frame value, which takes ownership.

```ocaml
val flush_decoder : 'media decoder -> ('media frame -> unit) -> unit
```

1. `avcodec_send_packet(ctx, NULL)` (enter draining).
2. Receive loop as above.
3. ``Error `Eof`` raised anywhere in steps 1–2 — by the send, by the receive
   step at end of stream, or by `f` — ends the call normally.

Any other exception propagates. Calling it again returns immediately (the
send reports end of stream).

```ocaml
val encode : 'media encoder -> ('media Packet.t -> unit) -> 'media frame -> unit
```

1. Send step with the frame.
2. Receive loop: call the receive step until it yields nothing, passing each
   packet to `f`.

Send step (encoder), with a frame or with "end of stream":

1. When the flushed flag is set, raise ``Error `Eof``.
2. Set the flushed flag to "this is the end-of-stream send".
3. When the codec context has a hardware frames context and a frame is
   given: allocate a frame, `av_hwframe_get_buffer(hw_frames_ctx, new, 0)`,
   check that the new frame carries a frames context (`Out_of_memory`
   otherwise), `av_hwframe_transfer_data(new, frame, 0)` (upload), and use
   the new frame in step 4. On an FFmpeg error free the new frame and raise.
   No frame properties (timestamps included) are copied onto the new frame
   by the binding. This step holds the runtime lock.
4. `avcodec_send_frame` with the runtime lock released.
5. Free the uploaded frame if any.
6. A negative return raises the mapped error.

The caller's frame is not modified; FFmpeg takes its own reference.

Receive step (encoder):

1. Allocate a packet (`Out_of_memory` on failure).
2. `avcodec_receive_packet` with the runtime lock released.
3. `AVERROR(EAGAIN)` or `AVERROR_EOF`: free the packet, yield nothing.
4. Any other negative return: free the packet, raise the mapped error.
5. Wrap the packet (§2.3). Its timestamps are in the encoder's time base and
   its stream index is whatever the encoder left (0).

```ocaml
val flush_encoder : 'media encoder -> ('media Packet.t -> unit) -> unit
```

1. Receive loop (drain what is already available).
2. Send step with "end of stream".
3. Receive loop until end of stream.

A second call drains nothing in step 1 and raises ``Error `Eof`` in step 2.

In all four functions an exception raised by `f` propagates to the caller
(except ``Error `Eof`` in `flush_decoder`); output still buffered inside FFmpeg
stays there.

## 5. Errors

| Exception                                                                                        | Raised by                                                                                                                                                                                                                                                                   |
| ------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Avutil.Error e`, `e` mapped from a negative FFmpeg return code by the shared error-raising path | every operation that calls a fallible FFmpeg function: `Packet.create`, `params`, creation and open, send/receive, supported-config queries (newer API), bitstream filter init/send/receive, channel-layout copies                                                          |
| ``Avutil.Error `Encoder_not_found`` / `` `Decoder_not_found ``                                   | `find_*`, when the codec is missing or of another media type                                                                                                                                                                                                                |
| ``Avutil.Error `Eof``                                                                            | `encode` and `flush_encoder` on a flushed encoder (raised by the binding itself); `decode` after `flush_decoder`; `BitstreamFilter.receive_packet` when drained                                                                                                             |
| ``Avutil.Error `Eagain``                                                                         | `decode` / `encode` when FFmpeg refuses input; `BitstreamFilter.send_packet` and `receive_packet`                                                                                                                                                                           |
| ``Avutil.Error (`Failure msg)``                                                                  | enum conversion with no mapping (§3.1); `"Invalid side_data"` (short replay-gain entry); `"Empty packet"`, `"Failed to get codec parameters"` (null pointers handed to the exported constructors); `"Invalid value"` (unknown side-data variant, unreachable through types) |
| `Out_of_memory`                                                                                  | any failed FFmpeg allocation of a structure or buffer                                                                                                                                                                                                                       |
| `Not_found`                                                                                      | `BitstreamFilter.init` when `av_bsf_get_by_name` returns null                                                                                                                                                                                                               |
| `Failure "Invalid flag type!"`                                                                   | flag conversion on an unknown variant (unreachable through types)                                                                                                                                                                                                           |
| `Invalid_argument` (from `Option.get`)                                                           | `Audio.create_encoder`, inside the `avutil` option derivation, when neither the given layout nor the default layout for its channel count has a native mask                                                                                                                 |

Silent on failure: `Packet.add_side_data` (return of
`av_packet_add_side_data`) and `Packet.dup` (return of `av_packet_ref`).

Objects stay valid after any error. No error closes or invalidates a
decoder, encoder, packet or filter.

## 6. Blocking and concurrency

The runtime lock is released around exactly these calls, and other OCaml
threads run meanwhile:

| Call                     | From                         |
| ------------------------ | ---------------------------- |
| `avcodec_open2`          | decoder and encoder creation |
| `avcodec_send_packet`    | `decode`, `flush_decoder`    |
| `avcodec_receive_frame`  | decode receive step          |
| `avcodec_send_frame`     | `encode`, `flush_encoder`    |
| `avcodec_receive_packet` | encode receive step          |
| `av_bsf_init`            | `BitstreamFilter.init`       |
| `av_bsf_send_packet`     | `send_packet`, `send_eof`    |
| `av_bsf_receive_packet`  | `receive_packet`             |

Everything else holds the lock, including `av_hwframe_get_buffer`,
`av_hwframe_transfer_data` (upload and download), parameter copies, packet
allocation and data copies, and all introspection.

The library registers no thread and creates none; codec worker threads are
FFmpeg's and never enter OCaml.

Nothing serialises access to an object. While the lock is released another
OCaml thread can call into the same decoder, encoder, filter or packet.

Global state: none in C. On the OCaml side, module load runs the legacy
codec registration (§10), enumerates all codecs and all bitstream filters
once, and reads `version` and `flag_qscale`. These values are immutable
afterwards.

## 7. Callbacks

FFmpeg never calls back into OCaml through this library.

The only C-to-OCaml call is the shared failure path: the stub calls the
OCaml closure registered by `avutil` under a global name, on the calling
thread, with the runtime lock held; the closure raises
``Avutil.Error (`Failure msg)`` and the exception propagates out of the stub.

The user functions passed to `decode`, `flush_decoder`, `encode` and
`flush_encoder` are ordinary OCaml calls made from OCaml code between stub
calls, on the caller's thread.

## 8. Data transfer

| Path                                           | Behaviour                                                                                                                                                                                      |
| ---------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Packet.create` string to packet               | copied into a new buffer of `length` bytes plus FFmpeg's input padding                                                                                                                         |
| `Packet.content`, `Packet.to_bytes`            | fresh copy of `size` bytes                                                                                                                                                                     |
| `Packet.dup`                                   | payload shared by reference count; properties and side data copied                                                                                                                             |
| side data, write                               | bytes copied into an `av_malloc` buffer whose ownership passes to the packet                                                                                                                   |
| side data, read                                | bytes copied into OCaml strings / integers                                                                                                                                                     |
| packet to decoder                              | FFmpeg references the payload; packet value unchanged                                                                                                                                          |
| packet to bitstream filter                     | content moved into the filter; packet value emptied                                                                                                                                            |
| packet from encoder / filter                   | the OCaml value takes ownership of the `AVPacket` FFmpeg filled; no copy                                                                                                                       |
| frame to encoder                               | FFmpeg references the frame's buffers; frame value unchanged. With a hardware frames context the data is uploaded into a pool frame that is freed after the send                               |
| frame from decoder                             | the OCaml frame value takes ownership of the `AVFrame` FFmpeg filled; no copy. With a hardware frames context the data is downloaded into a new software frame and the hardware frame is freed |
| codec parameters                               | always deep-copied, in both directions                                                                                                                                                         |
| channel layouts                                | always deep-copied                                                                                                                                                                             |
| hardware device/frame contexts                 | shared by a new buffer reference owned by the codec context                                                                                                                                    |
| names, descriptions, MIME types, profile names | copied into OCaml strings                                                                                                                                                                      |
| codec, descriptor, filter class pointers       | shared pointers to FFmpeg static data                                                                                                                                                          |

No bigarray, stride or plane handling happens in this library.

## 9. Options

All three option-taking operations (`Audio.create_encoder`,
`Video.create_encoder`, `BitstreamFilter.init`) follow one scheme:

1. `opts` is the caller's mutable table, or a fresh empty table.
2. Encoders: the `avutil` helpers return a **copy** of the table with the
   derived entries added. Bitstream filters use the table as is.
3. The table is flattened to string pairs (numbers printed in decimal) and
   converted to an `AVDictionary` by the shared conversion.
4. FFmpeg consumes entries: `avcodec_open2` for encoders (one pass with
   child search; for each key the codec-private options are searched before
   the generic context options); `av_opt_set_dict` on the `AVBSFContext`
   itself for filters.
5. The keys still in the dictionary are returned and the dictionary is
   freed.
6. The **caller's** table is reduced in place to the entries whose key is in
   that set. After a successful call it holds exactly the caller's options
   FFmpeg did not consume. Derived entries FFmpeg did not consume are
   dropped without notice, because they live only in the copy.

Derived entries (from the `avutil` helpers):

| Operation     | Key              | Value                                                                     |
| ------------- | ---------------- | ------------------------------------------------------------------------- |
| audio encoder | `ar`             | sample rate                                                               |
| audio encoder | `channel_layout` | native mask of the layout, or of the default layout for its channel count |
| audio encoder | `sample_fmt`     | integer id of the sample format                                           |
| audio encoder | `time_base`      | `"num/den"`                                                               |
| video encoder | `pixel_format`   | integer id of the pixel format                                            |
| video encoder | `video_size`     | `"WxH"`                                                                   |
| video encoder | `time_base`      | `"num/den"`                                                               |
| video encoder | `r`              | `"num/den"`, only when `frame_rate` is given                              |

A derived entry is added after the caller's entries. When the caller's table
already has the same key, both bindings are flattened and the dictionary
keeps the last one set; the `avutil` specification owns that ordering.

For bitstream filters, `av_opt_set_dict` searches the `AVBSFContext` only,
not its private data, so a filter's private options stay in the dictionary
and come back as unused.

Decoders take no options. No AVOption getter or setter is exposed on
decoders, encoders or filters; `BitstreamFilter.filter.options` only
describes the filter's private option class.

## 10. Version-dependent behaviour

| Condition                          | When true                                                                                                                                                                                                                                                                   | Otherwise                                                                                                                                                                                                                                                       |
| ---------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| libavcodec < 58.9.100              | module load calls `avcodec_register_all()`                                                                                                                                                                                                                                  | module load does nothing in C                                                                                                                                                                                                                                   |
| libavcodec < 60.26.100             | `AV_PROFILE_UNKNOWN` / `AV_LEVEL_UNKNOWN` are aliases of the `FF_` names                                                                                                                                                                                                    | FFmpeg's own definitions                                                                                                                                                                                                                                        |
| `AV_PKT_FLAG_DISPOSABLE` undefined | defined locally as `0x0010`                                                                                                                                                                                                                                                 | FFmpeg's definition                                                                                                                                                                                                                                             |
| libavcodec ≤ 61.13.100             | supported channel layouts, sample formats, sample rates, frame rates, pixel formats are read from the `AVCodec` fields `ch_layouts`, `sample_fmts`, `supported_samplerates`, `supported_framerates`, `pix_fmts`; supported colour spaces and colour ranges are always empty | each list comes from `avcodec_get_supported_config(NULL, codec, AV_CODEC_CONFIG_*, 0, &array, NULL)` with `CHANNEL_LAYOUT`, `SAMPLE_FORMAT`, `SAMPLE_RATE`, `FRAME_RATE`, `PIX_FORMAT`, `COLOR_SPACE`, `COLOR_RANGE`; a negative return raises the mapped error |

`version` reports the run-time library, not the headers the stubs were
compiled against.

## 11. Logic on the OCaml side

**Module initialisation**, in order: read the version; read `flag_qscale`;
run the C init; enumerate all codecs; later, enumerate all bitstream
filters.

**Codec enumeration.** One pass over `av_codec_iterate`. For each codec the
stub returns the codec value, an optional id and `av_codec_is_encoder`. The
id is found by scanning the audio, then video, then subtitle id tables for
`codec->id`; it is absent when none matches. The matched OCaml id passes
through a C variable of type `enum AVCodecID` before it is returned; where
that type is unsigned 32-bit, an id whose OCaml representation is negative
comes back as a different integer that equals no member of any `codec_ids`
list (see findings). The pass conses, so the stored list is in reverse
iteration order.

`Audio.encoders`, `Audio.decoders` and the `Video` and `Subtitle`
equivalents are each a filter of that one list: keep a codec when it has an
id, its encoder flag equals the wanted mode, and its id is a member of the
module's `codec_ids`. The media and mode phantom types are assigned by an
unchecked cast. Codecs of the unknown/data family never appear in any list.

**Bitstream filter enumeration.** One pass over `av_bsf_iterate`, consing:
`filters` is in reverse iteration order.

**Supported lists.** The stubs build each list by consing (reverse order)
and the OCaml wrappers reverse it, giving codec order. `hw_configs` has no
such wrapper.

**Best-value selection.**

| Function                   | Rule                                                                                                                                                                                                                                |
| -------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `find_best_channel_layout` | default when some supported layout equals it under `Avutil.Channel_layout.compare`; else the first supported layout; else default. `Not_found` raised in between also gives default                                                 |
| `find_best_sample_format`  | default when it is a member (structural equality); else the first; else default. `Not_found` also gives default                                                                                                                     |
| `find_best_sample_rate`    | same rule, no exception handler                                                                                                                                                                                                     |
| `find_best_frame_rate`     | same rule; membership is structural equality on `{num; den}`, with no reduction                                                                                                                                                     |
| `find_best_pixel_format`   | default when it is a member, whatever its kind; else, unless `~hwaccel:true`, drop every format whose `avutil` descriptor has the `` `Hwaccel `` flag; then the first remaining format; else default. `hwaccel` defaults to `false` |

**Packet helpers.** Flag-list construction (§4.2), metadata encoding and
decoding (§4.2).

**Drain loops.** `decode`, `flush_decoder`, `encode`, `flush_encoder` are
OCaml functions composing the send, receive and flush stubs as described in
§4.10. The loops are recursive and call the user function between receives,
so frames and packets are delivered one at a time, in order.

**Option bookkeeping.** §9.
