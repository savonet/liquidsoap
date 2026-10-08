# avcodec

Codecs, codec parameters, packets, decoders, encoders and bitstream filters.
It follows [binding-contract.md](binding-contract.md); section numbers match.

## 1. Scope

`Avcodec` binds libavcodec: codec lookup and introspection, codec parameters,
packets, decoders, encoders and bitstream filters. It binds no parser.

It depends on `avutil` for the error exception, option tables, rationals,
channel layouts, sample and pixel formats, colour properties, hardware
contexts, frames, the media-type table and option classes.

**Module initialisation** reads the version of the libavcodec loaded at run
time, reads `flag_qscale`, and enumerates every codec and every bitstream
filter FFmpeg registers (§11). It cannot fail (I1).

**Provided to dependent libraries**, through its installed C header:

| Service           | Contract                                                                                                               |
| ----------------- | ---------------------------------------------------------------------------------------------------------------------- |
| open a codec      | Opens a codec context per §4.9: thread count automatic unless the options say otherwise, runtime lock released.        |
| codecs            | The native codec of a codec value; a constructor that wraps one.                                                       |
| codec parameters  | The native parameters of a parameters value; a constructor that builds a value holding a **copy** of given parameters. |
| packets           | The native packet of a packet value; a constructor that **takes ownership** of a given packet.                         |
| codec identifiers | Both conversions for the audio, video, subtitle and unknown families.                                                  |

The two constructors raise a failure when given a null pointer.

## 2. Objects

### 2.1 Codec — `('media, 'mode) codec`

- **Native object**: a codec FFmpeg registers. Static: a borrowed handle.
- **Creation**: the `find_*` functions and the lists built at module load.
- The parameters are phantom: `'media` is the media kind, `'mode` is `encode`
  or `decode`. Every producer gives the parameters that match the codec.
- Equality between two codec values is not defined; compare names.

### 2.2 Codec parameters — `'media params`

- **Native object**: one `AVCodecParameters`, always a private deep copy.
- **Creation**: `Avcodec.params`, `BitstreamFilter.init`, and dependent
  libraries through the copy constructor.
- **Ownership**: the value owns its copy and shares nothing.
- **Release**: by collection only.
- Immutable from OCaml. No guard.

### 2.3 Packet — `'media Packet.t`

- **Native object**: one `AVPacket`.
- **Creation**: `Packet.create`, `Packet.dup`, encoders,
  `BitstreamFilter.receive_packet`, and dependent libraries through the
  wrapping constructor.
- **Ownership**: the value owns the packet structure. The payload is a
  reference-counted buffer that other packets, and FFmpeg, may also
  reference.
- **Accounting**: the value reports the size of its payload (L9), whichever
  operation produced it.
- **Release**: by collection only.
- **Mutable from OCaml**: stream index, timestamps, duration, position,
  flags, side data. Not the payload.
- **Concurrent use**: §6.2.
- No operation empties a caller's packet (A4).
- The `'media` parameter of `Packet.create` is chosen by the caller. Nothing
  depends on it for memory safety: a packet is opaque bytes to every
  consumer.

### 2.4 Decoder and encoder — `'media decoder`, `'media encoder`

- **Native object**: one opened codec context.
- **Creation**: `Audio.create_decoder`, `Video.create_decoder`,
  `Audio.create_encoder`, `Video.create_encoder`. A decoder or encoder value
  is always opened. The interface has no constructor for subtitle or data
  decoders and encoders; those types have no value.
- **Ownership**: the value owns the codec context. A video encoder given a
  hardware context holds a reference of its own to it.
- **Release**: by collection only.
- **Guard**: §6.2.
- **States**, the same for both:

  | State    | Reached by                                       | `decode` / `encode`   | `flush_decoder` / `flush_encoder`            |
  | -------- | ------------------------------------------------ | --------------------- | -------------------------------------------- |
  | open     | creation                                         | performed             | signals end of stream, delivers what remains |
  | draining | end of stream accepted, output not yet exhausted | raises ``Error `Eof`` | delivers what remains                        |
  | drained  | the codec reported end of stream                 | raises ``Error `Eof`` | returns at once, delivers nothing            |

  The state changes to draining only when the codec accepted the
  end-of-stream signal. No operation leaves the drained state: a caller that
  needs the codec again creates a new one.

### 2.5 Bitstream filter descriptor — `BitstreamFilter.filter`

A private record `{ name; codecs; options }` built at module load. Plain data
plus a borrowed option class. It owns nothing.

### 2.6 Bitstream filter instance — `'a BitstreamFilter.t`

- **Native object**: one initialised bitstream filter context.
- **Ownership**: the value owns the context. The input parameters are copied
  in; the output parameters returned by `init` are an independent copy.
- **Release**: by collection only.
- **Guard**: §6.2.
- **States** are libavcodec's: accepting input, output pending, end of stream
  signalled.

## 3. Enumerations and constants

### 3.1 Generated tables

| Table                     | OCaml type             | Used for                                  |
| ------------------------- | ---------------------- | ----------------------------------------- |
| codec id, audio family    | `Codec_id.audio`       | `Audio.id`, both directions               |
| codec id, video family    | `Codec_id.video`       | `Video.id`, both directions               |
| codec id, subtitle family | `Codec_id.subtitle`    | `Subtitle.id`, both directions            |
| codec id, unknown family  | `Codec_id.unknown`     | `Unknown.id`, both directions             |
| codec id, all             | `Codec_id.codec_id`    | `Avcodec.id`, both directions             |
| codec capabilities        | `Codec_capabilities.t` | `capabilities`, bit mask to list          |
| codec properties          | `Codec_properties.t`   | `descriptor.properties`, bit mask to list |
| hardware config method    | `Hw_config_method.t`   | `hw_configs`, bit mask to list            |

[build.md](build.md) §3.5 defines the five codec identifier types. The
generated value lists of the four families are re-exported as `codec_ids`.

### 3.2 Hand-written tables

Packet flags (`Packet.flag`), both directions:

| Variant           | C constant               |
| ----------------- | ------------------------ |
| `` `Keyframe ``   | `AV_PKT_FLAG_KEY`        |
| `` `Corrupt ``    | `AV_PKT_FLAG_CORRUPT`    |
| `` `Discard ``    | `AV_PKT_FLAG_DISCARD`    |
| `` `Trusted ``    | `AV_PKT_FLAG_TRUSTED`    |
| `` `Disposable `` | `AV_PKT_FLAG_DISPOSABLE` |

Packet side data (`Packet.side_data`), both directions:

| Variant                 | C type                         | Payload                           |
| ----------------------- | ------------------------------ | --------------------------------- |
| `` `Replaygain ``       | `AV_PKT_DATA_REPLAYGAIN`       | the four fields of `AVReplayGain` |
| `` `Strings_metadata `` | `AV_PKT_DATA_STRINGS_METADATA` | a packed dictionary (below)       |
| `` `Metadata_update ``  | `AV_PKT_DATA_METADATA_UPDATE`  | a packed dictionary               |

**Packed dictionary.** FFmpeg's format (`av_packet_pack_dictionary`): for each
pair in order, the key, a NUL byte, the value, a NUL byte. Every string is
terminated, the last included. FFmpeg's own reader
(`av_packet_unpack_dictionary`) rejects a payload whose last byte is not NUL.

- Writers MUST produce this format.
- Readers SHOULD also accept a payload whose final NUL is missing.

Absent values:

| Field                   | C value that is `None` |
| ----------------------- | ---------------------- |
| packet pts, dts         | `AV_NOPTS_VALUE`       |
| packet duration         | 0                      |
| packet position         | -1                     |
| parameters pixel format | `AV_PIX_FMT_NONE`      |
| parameters pixel aspect | a zero numerator       |

These are FFmpeg's own sentinels: a duration of 0 and a position of -1 mean
"unknown" to FFmpeg too, so `Some 0L` and `Some (-1L)` read back as `None`.

`flag_qscale` is `AV_CODEC_FLAG_QSCALE`.

## 4. Operations

Unless stated otherwise an operation fails only by `Out_of_memory` and
returns copies of the text it reads.

### 4.1 Top level

```ocaml
val version : version
```

The libavcodec loaded at run time, read once at module load.

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

§2. `descriptor` mirrors FFmpeg's codec descriptor.

```ocaml
val flag_qscale : int
```

§3.2.

```ocaml
val params : 'media encoder -> 'media params
```

A fresh, independent snapshot of the encoder's parameters
(`avcodec_parameters_from_context`). A failure releases everything and raises.

```ocaml
val descriptor : 'media params -> descriptor option
```

The descriptor of the parameters' codec identifier; see "Descriptors" below.

```ocaml
val time_base : 'media encoder -> Avutil.rational
```

The encoder's time base, as given at creation.

```ocaml
val name : _ codec -> string
```

The codec's name.

```ocaml
type capability = Codec_capabilities.t
val capabilities : ([< `Audio | `Video ], _) codec -> capability list
```

The capabilities of the codec (E6). It accepts encoders and decoders.

```ocaml
type hw_config_method = Hw_config_method.t
type hw_config = {
  pixel_format : Pixel_format.t;
  methods : hw_config_method list;
  device_type : HwContext.device_type;
}
val hw_configs : ([< `Audio | `Video ], _) codec -> hw_config list
```

The hardware configurations of the codec, in FFmpeg's index order
(`avcodec_get_hw_config`). `methods` per E6. A configuration whose pixel
format or device type has no constructor is left out (E4).

**Descriptors** (`descriptor`, `Audio.descriptor`, `Video.descriptor`,
`Subtitle.descriptor`): FFmpeg's descriptor for a codec identifier
(`avcodec_descriptor_get`), `None` when there is none.

- `media_type` per E3; `name`; `long_name` per A3.
- `properties` per E6.
- `mime_types`: in order; empty when FFmpeg lists none.
- `profiles`: one `{ id; profile_name }` per profile FFmpeg lists, in order;
  empty when it lists none.

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

Attaches the side data to the packet, encoded per §3.2. A packet holds one
entry per side-data type: adding a type it already carries replaces that
entry. A gain outside the range of a C `int`, or a peak outside 32 unsigned
bits, raises ``Error (`Failure _)``; a failed allocation raises
`Out_of_memory`. On failure the packet is unchanged and nothing leaks.

```ocaml
val side_data : 'media t -> side_data list
```

The packet's side data of the three supported types, in the packet's order.
The pairs of a metadata entry are in the payload's order. An entry of another
type, and an entry too short for its type, are left out.

`side_data p` after `add_side_data p d` on a packet with no side data returns
`[d]`.

```ocaml
val dup : 'media t -> 'media t
```

A new packet with the same properties and side data, referencing the same
payload (`av_packet_ref`). A failure raises the mapped error and returns no
packet.

```ocaml
val get_flags : 'media t -> flag list
val set_flags : 'media t -> flag list -> unit
```

`get_flags`: the flags of §3.2 set on the packet (E6). `set_flags` replaces
them with the given list; bits outside the table are cleared.

```ocaml
val get_size : 'media t -> int
val get_stream_index : 'media t -> int
val set_stream_index : 'media t -> int -> unit
```

The payload size in bytes, and the stream index.

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

Field access with the sentinels of §3.2. No time-base conversion happens
here.

```ocaml
val to_bytes : 'media t -> bytes
val content : 'media t -> string
```

Each returns a fresh copy of the payload.

```ocaml
val create : string -> 'media t
```

A new packet whose payload is a copy of the string. It has no timestamp, no
duration, no position, stream index 0 and no flag. A failure releases the
packet and raises.

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
§11.

```ocaml
val find_encoder_by_name : string -> encode t
val find_encoder : id -> encode t
val find_decoder_by_name : string -> decode t
val find_decoder : id -> decode t
```

FFmpeg's lookup by name or by identifier. When FFmpeg finds no codec, or one
that is not an audio codec, the encoder variants raise
``Error `Encoder_not_found`` and the decoder variants
``Error `Decoder_not_found``.

```ocaml
val get_supported_channel_layouts : _ t -> Avutil.Channel_layout.t list
val get_supported_sample_formats : _ t -> Avutil.Sample_format.t list
val get_supported_sample_rates : _ t -> int list
```

The values the codec declares it supports, in the codec's order
(`avcodec_get_supported_config`). The list holds exactly the entries FFmpeg
counts. A codec that declares none gives the empty list. Each channel layout
is an independent copy. A failure raises the mapped error.

```ocaml
val find_best_channel_layout :
  _ t -> Avutil.Channel_layout.t -> Avutil.Channel_layout.t
val find_best_sample_format :
  _ t -> Avutil.Sample_format.t -> Avutil.Sample_format.t
val find_best_sample_rate : _ t -> int -> int
```

§11.

```ocaml
val create_decoder : ?params:audio params -> decode t -> audio decoder
```

§4.9.

```ocaml
val sample_format : audio decoder -> Sample_format.t
```

The decoder's current sample format (E3).

```ocaml
val create_encoder :
  ?opts:opts -> channel_layout:Channel_layout.t -> sample_rate:int ->
  sample_format:Avutil.Sample_format.t -> time_base:Avutil.rational ->
  encode t -> audio encoder
```

§4.9.

```ocaml
val frame_size : audio encoder -> int
```

The number of samples per channel the encoder wants in each frame; 0 for an
encoder that accepts any.

```ocaml
val get_name : _ codec -> string
val get_description : _ codec -> string
val string_of_id : id -> string
val get_id : _ t -> id
```

- `get_name`: the codec's name, as `Avcodec.name`.
- `get_description`: the codec's long name (A3).
- `string_of_id`: FFmpeg's name of the identifier (`avcodec_get_name`).
- `get_id`: the codec's identifier. It raises a failure (E3) for a codec whose
  identifier is outside the audio family (§11).

```ocaml
val get_params_id : audio params -> id
val get_channel_layout : audio params -> Avutil.Channel_layout.t
val get_nb_channels : audio params -> int
val get_sample_format : audio params -> Avutil.Sample_format.t
val get_bit_rate : audio params -> int
val get_sample_rate : audio params -> int
```

| Getter               | Returns                           |
| -------------------- | --------------------------------- |
| `get_params_id`      | the codec identifier (E3)         |
| `get_channel_layout` | an independent copy of the layout |
| `get_nb_channels`    | the channel count of the layout   |
| `get_sample_format`  | the sample format (E3)            |
| `get_bit_rate`       | the bit rate                      |
| `get_sample_rate`    | the sample rate                   |

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

As in `Audio`, for video codecs and the video identifier family.

```ocaml
val get_supported_frame_rates : _ t -> Avutil.rational list
val get_supported_color_spaces : _ t -> Avutil.Color_space.t list
val get_supported_color_ranges : _ t -> Avutil.Color_range.t list
val get_supported_pixel_formats : _ t -> Avutil.Pixel_format.t list
```

As the audio lists. An element with no constructor is left out (E4).

```ocaml
val find_best_frame_rate : _ t -> Avutil.rational -> Avutil.rational
val find_best_pixel_format :
  ?hwaccel:bool -> _ t -> Avutil.Pixel_format.t -> Avutil.Pixel_format.t
```

§11.

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

| Getter                    | Returns                                               |
| ------------------------- | ----------------------------------------------------- |
| `get_params_id`           | the codec identifier (E3)                             |
| `get_width`, `get_height` | the picture size                                      |
| `get_sample_aspect_ratio` | the sample aspect ratio as stored, `0/1` when unknown |
| `get_pixel_format`        | the pixel format; `None` when unset                   |
| `get_pixel_aspect`        | the same ratio as an option: `None` when unknown      |
| `get_bit_rate`            | the bit rate                                          |

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

As in `Audio`, for subtitle codecs and the subtitle identifier family.
Subtitle encoding and decoding go through containers
([avformat.md](avformat.md) §4).

### 4.6 `Unknown`

```ocaml
type 'mode t = ([ `Data ], 'mode) codec
type id = Codec_id.unknown
val codec_ids : Codec_id.unknown list
val string_of_id : id -> string
val get_params_id : [ `Data ] params -> id
```

`codec_ids`: the generated list. `string_of_id`: FFmpeg's name of the
identifier. `get_params_id`: the codec identifier of the parameters (E3).

### 4.7 All codec identifiers

```ocaml
type id = Codec_id.codec_id
val string_of_id : id -> string
```

FFmpeg's name of the identifier.

### 4.8 `BitstreamFilter`

```ocaml
type filter = private { name : string; codecs : id list; options : Avutil.Options.t }
type 'a t
val filters : filter list
```

`filters`: every bitstream filter FFmpeg registers, in FFmpeg's order, built
at module load. `codecs` lists the identifiers the filter declares, in order,
empty when it accepts any (E4). `options` is the filter's private option
class, with no class when the filter has no option.

```ocaml
val init : ?opts:opts -> filter -> 'a params -> 'a t * 'a params
```

Creates and initialises an instance of the filter for input of the given
parameters, and returns it with a copy of its output parameters.

- `opts` follows [avutil.md](avutil.md) §9.1. The entries are applied to the
  filter with child search, so that the filter's private options, the ones
  `filter.options` lists, are reached.
- The input parameters value is not modified. The input time base keeps
  FFmpeg's default.
- A filter FFmpeg does not know raises ``Error `Bsf_not_found``.
- Any failure releases the instance and raises the mapped error.

```ocaml
val send_packet : 'a t -> 'a Packet.t -> unit
```

Gives the filter a reference of its own to the packet (A4). FFmpeg treats a
packet with no payload and no side data as the end of the stream. A refusal
raises the mapped error: ``Error `Eagain`` when output must be read first.

```ocaml
val send_eof : 'a t -> unit
```

Signals the end of the stream to the filter.

```ocaml
val receive_packet : 'a t -> 'a Packet.t
```

The next packet the filter has ready. It raises ``Error `Eagain`` when the
filter needs more input and ``Error `Eof`` when it is drained; both are part
of normal operation. A failure releases the packet it was receiving into.

The library provides no drain loop for filters.

### 4.9 Creating decoders and encoders

**Opening.** A codec is opened with an automatic thread count: libavcodec
would otherwise run it on a single thread. An explicit `threads` entry in
`opts` takes precedence.

**Decoder** (`Audio.create_decoder`, `Video.create_decoder`): allocates a
context for the codec, applies `params` when given, and opens it. `params` is
read, not retained. A decoder takes no option and decodes in software. Any
failure releases the context and raises.

**Audio encoder** (`Audio.create_encoder`): allocates a context for the
codec, sets on it the sample format, a copy of the channel layout, the sample
rate and the time base, then opens it with the caller's `opts`
([avutil.md](avutil.md) §9.1).

**Video encoder** (`Video.create_encoder`): allocates a context for the codec,
sets on it the pixel format, the width, the height, the time base, the frame
rate when given, and a reference to the hardware device context or the
hardware frame context when given, then opens it with the caller's `opts`.

For both encoders:

- every typed argument takes effect on the context or the creation fails
  (contract O2);
- an entry of `opts` that addresses the same setting as a typed argument is
  applied by FFmpeg after it, and prevails;
- any failure releases the context and raises, and the caller's table is
  untouched.

### 4.10 Decode, encode, flush

```ocaml
val decode : 'media decoder -> ('media frame -> unit) -> 'media Packet.t -> unit
val flush_decoder : 'media decoder -> ('media frame -> unit) -> unit
val encode : 'media encoder -> ('media Packet.t -> unit) -> 'media frame -> unit
val flush_encoder : 'media encoder -> ('media Packet.t -> unit) -> unit
```

Composite operations; §11 gives the algorithm. Their properties:

- `decode dec f pkt` gives the packet to the decoder and calls `f` on every
  frame that becomes available, in order, each as soon as it is received. A
  packet may produce no frame, or several. Producing none is not an error.
- `encode enc f frame` is the same in the other direction. The packets
  delivered carry timestamps in the encoder's time base.
- `flush_decoder dec f` and `flush_encoder enc f` signal the end of the stream
  and deliver every frame or packet the codec still holds.
- None of the four raises ``Error `Eagain``.
- The states of §2.4 decide what each does on a draining or drained codec.
- An exception raised by `f` propagates, whatever it is. §7.2.
- The caller's packet or frame is unchanged (A4).

**Hardware frames.** An encoder created with a hardware frame context uploads
each frame before encoding it: it takes a frame from the context's pool,
transfers the pixel data, and copies the frame's properties — its timestamps
among them — onto the uploaded frame. A failure releases the pool frame and
raises.

## 5. Errors

| Raised                                                  | By                                                                                                  |
| ------------------------------------------------------- | --------------------------------------------------------------------------------------------------- |
| `Error e`, `e` mapped from an FFmpeg code               | every operation that calls a fallible FFmpeg function                                               |
| ``Error `Encoder_not_found`` / `` `Decoder_not_found `` | `find_*`, when the codec is missing or of another media type                                        |
| ``Error `Bsf_not_found``                                | `BitstreamFilter.init`                                                                              |
| ``Error `Eof``                                          | `decode` and `encode` on a draining or drained codec; `BitstreamFilter.receive_packet` when drained |
| ``Error `Eagain``                                       | `BitstreamFilter.send_packet` and `receive_packet`                                                  |
| ``Error (`Failure msg)``                                | a value with no constructor (E3); a null object handed to a wrapping constructor                    |
| `Out_of_memory`                                         | a failed allocation (F3)                                                                            |

No operation of this library raises `Not_found`.

After any error a decoder, encoder, packet or filter is valid and in a state
of §2.

## 6. Blocking and concurrency

### 6.1 The runtime lock

Contract M1 covers: opening a codec, every send to and receive from a codec,
the hardware upload, and initialising, sending to and receiving from a
bitstream filter.

### 6.2 Guards

| Handle                    | Exclusive                                   | Shared                                               |
| ------------------------- | ------------------------------------------- | ---------------------------------------------------- |
| decoder, encoder          | each send and each receive step of §11      | `params`, `time_base`, `frame_size`, `sample_format` |
| bitstream filter instance | `send_packet`, `send_eof`, `receive_packet` |                                                      |

Packets have no guard (contract M8). The operations that change a packet are
its setters and `add_side_data`; every other operation that takes a packet,
in any library, only reads it.

Codecs, parameters and filter descriptors are immutable.

### 6.3 Global state

The codec lists, the filter list, `version` and `flag_qscale`: computed at
module load, immutable afterwards. The library registers no thread; FFmpeg's
codec worker threads never enter OCaml.

## 7. Callbacks

### 7.1 Functions FFmpeg calls

None.

### 7.2 The functions passed to `decode`, `encode` and the two flushes

Called by the binding between native steps (contract §7.2).

- The codec is not in use while the function runs: it may call any operation
  on it.
- When the function raises, the frames or packets not yet delivered stay in
  the codec. The next `decode`, `encode` or flush on that codec delivers them
  first (§11).

## 8. Data transfer

| Path                                              | Behaviour                                                              |
| ------------------------------------------------- | ---------------------------------------------------------------------- |
| `Packet.create`                                   | the string is copied into a new payload                                |
| `Packet.content`, `Packet.to_bytes`               | fresh copy                                                             |
| `Packet.dup`                                      | payload shared by reference count; properties and side data copied     |
| side data, write and read                         | copied                                                                 |
| packet to decoder, packet to bitstream filter     | FFmpeg references the payload; the caller's packet is unchanged        |
| packet from encoder or filter                     | the packet value owns what FFmpeg produced; no copy                    |
| frame to encoder                                  | FFmpeg references the frame's buffers; the caller's frame is unchanged |
| frame to an encoder with a hardware frame context | pixel data uploaded into a pool frame that is released after the send  |
| frame from decoder                                | the frame value owns what FFmpeg produced; no copy                     |
| codec parameters, channel layouts                 | always deep-copied, in both directions                                 |
| hardware contexts                                 | shared by a reference the codec context owns                           |
| names, descriptions, MIME types, profile names    | copied                                                                 |

No bigarray, stride or plane handling happens in this library.

## 9. Options

| Operation              | Consumer of the entries                                                             |
| ---------------------- | ----------------------------------------------------------------------------------- |
| `Audio.create_encoder` | the codec open: the codec's private options first, then the generic context options |
| `Video.create_encoder` | the same                                                                            |
| `BitstreamFilter.init` | the filter context and its private options                                          |

All three follow [avutil.md](avutil.md) §9.1. Decoders take no option. No
option getter or setter is offered on a decoder, encoder or filter instance.

## 10. Version-dependent behaviour

None.

## 11. Composite operations

**Codec lists.** `Audio.encoders`, `Audio.decoders` and the `Video` and
`Subtitle` equivalents hold, in FFmpeg's iteration order, every registered
codec such that:

- its media type is the module's;
- it is an encoder, respectively a decoder;
- its identifier has a constructor in the module's family.

`get_id` therefore succeeds on every listed codec. FFmpeg types a few codecs
as audio or video while their identifier belongs to the unknown family; they
are in no list, the `find_*_by_name` functions find them, and `get_id` raises
a failure on them. Codecs of the unknown family are in no list.

**Best-value selection.**

| Function                   | Rule                                                                                                                                                                                          |
| -------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `find_best_channel_layout` | the default when the codec lists none, or lists one equal to it (`Channel_layout.compare`); else the first listed                                                                             |
| `find_best_sample_format`  | the default when the codec lists none or lists it; else the first listed                                                                                                                      |
| `find_best_sample_rate`    | the same rule                                                                                                                                                                                 |
| `find_best_frame_rate`     | the same rule; two rates are equal when they are equal as fractions                                                                                                                           |
| `find_best_pixel_format`   | the default when the codec lists none or lists it; else the first listed format that is not a hardware format, or the first listed format when `~hwaccel:true`; the default when none remains |

A hardware format is one whose descriptor carries the hardware-acceleration
flag. `hwaccel` defaults to `false`.

**Decode.** `decode dec f pkt`:

1. In the draining or drained state, raise ``Error `Eof``.
2. Deliver: receive frames one at a time, calling `f` on each, until the
   decoder has none ready. This returns the frames an earlier, interrupted
   call left behind.
3. Send the packet.
4. Deliver again.

An exception raised by `f` in step 2 leaves the packet unsent; in step 4 the
packet was accepted.

**Flush.** `flush_decoder dec f`:

1. In the drained state, return.
2. In the open state: deliver as above, then signal the end of the stream;
   the state becomes draining.
3. Receive frames one at a time, calling `f` on each, until the decoder
   reports the end of the stream; the state becomes drained.

`encode` and `flush_encoder` are the same with frames in and packets out.

A receive that has nothing to return releases what it allocated to receive
into.
