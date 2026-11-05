# Variants

Part A. A **variant** is one entry of the output's stream list: a name, an
encoder, and the properties the playlists announce for it. This file owns
how a variant is described by the script and how its properties are
derived. How it is segmented, written and announced is owned by
[segmenting.md](segmenting.md), [files.md](files.md) and
[playlists.md](playlists.md).

## 1. Variant fields

A variant is `(name, format)`. The format value may carry these optional
fields, written `%encoder(...).{field = value, ...}`:

| Field         | Type          | Effect                                                                                                                         |
| ------------- | ------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| `bandwidth`   | `int`         | The announced bandwidth, in bits per second.                                                                                   |
| `codecs`      | `string`      | The announced codec string (RFC 6381).                                                                                         |
| `extname`     | `string`      | The media segment extension.                                                                                                   |
| `video_size`  | `(int * int)` | The announced resolution, width then height.                                                                                   |
| `id3`         | `bool`        | ID3 on or off ([metadata.md §2](metadata.md#2-id3)).                                                                           |
| `id3_version` | `int`         | ID3v2 version; default 3.                                                                                                      |
| `replay_id3`  | `bool`        | Default `true` ([metadata.md §2](metadata.md#2-id3)).                                                                          |
| `extra_tags`  | `[string]`    | Playlist tags ([metadata.md §3](metadata.md#3-inserted-tags)) written into this variant's media playlist header, each trimmed. |

The name is the media playlist's base name (`<name>.m3u8`), the
`stream_name` of segment names, and the key of the variant's persisted
state. Names MUST be non-empty and unique within an output.

## 2. Building a variant

At construction:

1. The encoder factory for the format is looked up; none: `Unsupported
format`.
2. The extension is derived (§3); none is a construction error.
3. A first encoder is created and its `init` called
   ([encoder-contract.md §3](encoder-contract.md#3-init)); its answer is
   the variant's ID3 mode. A rejection stops the script.
4. The variant's container MUST be a segment format of RFC 8216 §3:
   MPEG-TS, fragmented MP4, packed audio (ADTS, MP3, AC-3, E-AC-3) or
   WebVTT. Any other is rejected, naming the variant: "Transport of other
   media file formats is not defined" (§3.1). By encoder: `%mp3` and
   `%shine` (MP3), `%fdkaac` with ADTS transport, and `%ffmpeg` with the
   `mpegts`, fragmented `mp4`, `adts`, `mp3`, `ac3`, `eac3` or `webvtt`
   muxer. Every other encoder is rejected.

At the start of every run each variant gets a new encoder, built the same
way ([encoder-contract.md §2](encoder-contract.md#2-calls-and-order)).

## 3. Derived properties

Each property takes the first source that answers:

| Property   | 1. variant field | 2. encoder                     | 3. format                         | 4. measured                            |
| ---------- | ---------------- | ------------------------------ | --------------------------------- | -------------------------------------- |
| bandwidth  | `bandwidth`      | `bitrate()` × (1 + headroom)   | nominal bit rate × (1 + headroom) | peak segment bit rate × (1 + headroom) |
| codecs     | `codecs`         | listed segments' codec strings | the format's RFC 6381 string      | none, with a warning                   |
| extension  | `extname`        | —                              | the format's file extension       | construction error                     |
| video size | `video_size`     | `video_size()`                 | the format's declared size        | none                                   |

- `headroom`: recommended 10%. Reason: `BANDWIDTH` is a peak segment bit
  rate (RFC 8216 §4.3.4.2), and encoders and formats give a nominal one.
- **Measured** bandwidth: the peak segment bit rate of RFC 8216 §4.1 over
  the variant's listed segments: the largest bit rate (bytes × 8 /
  duration) of any run of consecutive segments lasting between 0.5 and 1.5
  times `T`, or of the one segment while there is a single one. Container
  overhead counts; HTTP does not.
- **Codecs** from the encoder: each segment records `codec_attrs()` when it
  closes; the variant announces every format found in its listed segments,
  each once, in first-seen order. If any listed segment recorded none, none.
  Reason: `CODECS` MUST list every format present (RFC 8216 §4.3.4.2), and a
  restart can change the codec while older segments are still listed.
- An extension of `mp4` becomes `m4s` for media segments; the init segment
  keeps `mp4` ([files.md §6](files.md#6-init-segments)).

Properties are evaluated when read, from the encoder's current answers once
it has returned its init segment, and from sources 1, 3 and 4 before that.
Nothing read before the init segment is kept.

When the measured bandwidth exceeds the announced one, a warning names the
variant, once per run. Reason: a player trusting a low `BANDWIDTH` picks a
variant it cannot sustain, and Apple's authoring specification (§1.29)
fails live content whose peak exceeds 125% of it.

### 3.1 Format tables

Nominal bit rate, bits per second: `%mp3` CBR `n` kbps → `n × 1000`, ABR →
its mean × 1000, VBR → none; `%shine` and `%fdkaac` → configured kbps × 1000. Every other format: none.

RFC 6381 strings: `%mp3`, `%shine` → `mp4a.40.34`. `%fdkaac` by object
type: MPEG-4 AAC-LC `mp4a.40.2`, HE-AAC `mp4a.40.5`, HE-AACv2 `mp4a.40.29`,
AAC-LD `mp4a.40.23`, AAC-ELD `mp4a.40.39`; every MPEG-2 profile `mp4a.67`.
Every other format: none from the format.

Extensions: `%mp3` and `%shine` `mp3`, `%fdkaac` `aac`, `%ffmpeg` by its
muxer ([ffmpeg encoder-format](../../../optionals/ffmpeg/spec/encoder-format.md)).

Declared video size: `%ffmpeg` → by its format; others: none.
