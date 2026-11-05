# Variants

Part A. As built. A **variant** is one entry of the output's stream list: a
name, an encoder, and the properties the playlists announce for it. This file
owns how a variant is described by the script and how its properties are
derived. How it is segmented, written and announced is owned by
[segmenting.md](segmenting.md), [files.md](files.md) and
[playlists.md](playlists.md).

## 1. Variant fields

A variant is `(name, format)`. The format value may carry these optional
fields, written `%encoder(...).{field = value, ...}`:

| Field         | Type          | Effect                                                                 |
| ------------- | ------------- | ---------------------------------------------------------------------- |
| `bandwidth`   | `int`         | Overrides the announced bandwidth, in bits per second.                 |
| `codecs`      | `string`      | Overrides the announced codec string (RFC 6381).                       |
| `extname`     | `string`      | Overrides the media segment extension.                                 |
| `video_size`  | `(int * int)` | Overrides the announced resolution, width then height.                 |
| `id3`         | `bool`        | Asks for ID3 metadata on or off ([metadata.md §2](metadata.md#2-id3)). |
| `id3_version` | `int`         | ID3v2 version; default 3.                                              |
| `replay_id3`  | `bool`        | Default `true` ([metadata.md §2](metadata.md#2-id3)).                  |
| `extra_tags`  | `[string]`    | Tags written into this variant's media playlist header, each trimmed.  |

The name is used verbatim as the media playlist's base name
(`<name>.m3u8`) and as `stream_name` in segment names.

## 2. Building a variant

1. The encoder factory for the format is looked up; none: `Unsupported
format`.
2. An encoder is created with the HLS flag set, the variant's name and empty
   initial metadata ([encoder-contract.md §2](encoder-contract.md#2-calls-and-order)).
3. The extension is derived (§3). A missing one is an error now.
4. The encoder's `init` is called with `id3` if given and `id3_version` if
   given. Its result is the variant's `id3_enabled`
   ([encoder-contract.md §3](encoder-contract.md#3-init)). An encoder may
   reject the request here, which stops the script.
5. Bandwidth, codecs and video size are set up to be computed on first use
   (§3).

The same steps run again, from the same description, at every `stop`
([operator.md §3.2](operator.md#32-stop)).

## 3. Derived properties

Each property takes the first source that answers:

| Property   | 1. variant field | 2. encoder, once running                     | 3. format                            | 4. otherwise               |
| ---------- | ---------------- | -------------------------------------------- | ------------------------------------ | -------------------------- |
| bandwidth  | `bandwidth`      | `bitrate()` plus 10%, integer division by 10 | the format's nominal bit rate, as is | error (below)              |
| codecs     | `codecs`         | `codec_attrs()`                              | the format's RFC 6381 string (table) | `""` with a level-3 log    |
| extension  | `extname`        | —                                            | the format's file extension          | construction error (below) |
| video size | `video_size`     | `video_size()`                               | the format's declared size           | none                       |

After derivation, an extension of `mp4` becomes `m4s`. The init segment of
an `m4s` variant uses `mp4` ([files.md §6](files.md#6-init-segments)).

Bandwidth, codecs and video size are computed once, the first time they are
read, and kept until the variant is rebuilt. They are read when the main
playlist is written (after the first segment close,
[playlists.md §3](playlists.md#3-main-playlist)) and by `streams()`. A read
before the encoder has returned its init segment caches the answer of
sources 3 and 4 (finding F-VAR-2).

**Bandwidth error**, raised at the first read, so at the first segment
close, not at construction: `Bandwidth for stream "<name>" cannot be
inferred from codec, please specify it with: %encoder(...).{bandwidth =
<number>, ...}`. It goes through the error path of
[operator.md §6](operator.md#6-errors-and-reopening) and repeats on every
retry (checked: `%wav`).

**Codecs log**: `Stream <name>: codecs cannot be inferred, CODECS is omitted
from the main playlist. Specify them with %encoder(...).{codecs = "...",
...}.`

**Extension error**, at construction: `File extension for stream "<name>"
cannot be inferred from codec, please specify it with:
%encoder(...).{extname = "...", ...}`.

### 3.1 Format tables

Nominal bit rate, bits per second: `%mp3` CBR `n` kbps → `n × 1000`, ABR →
its mean × 1000, VBR → none; `%shine` and `%fdkaac` → configured kbps × 1000. Every other format: none.

RFC 6381 strings: `%mp3`, `%shine` → `mp4a.40.34`. `%fdkaac` by object
type: MPEG-4 AAC-LC `mp4a.40.2`, HE-AAC `mp4a.40.5`, HE-AACv2 `mp4a.40.29`,
AAC-LD `mp4a.40.23`, AAC-ELD `mp4a.40.39`; every MPEG-2 profile `mp4a.67`.
Every other format: none from the format (the `%ffmpeg` encoder answers
through `codec_attrs`, [encoder-ffmpeg.md §6](encoder-ffmpeg.md#6-properties)).

Extensions: `%wav` `wav`, `%avi` `avi`, `%ogg` `ogg`, `%mp3` and `%shine`
`mp3`, `%flac` `flac`, `%fdkaac` `aac`, `%ffmpeg` by its muxer
([ffmpeg encoder-format](../../../optionals/ffmpeg/spec/encoder-format.md));
every other format: none.

Declared video size: `%ogg` with Theora → its width and height; `%ffmpeg` →
by its format; every other format: none.
