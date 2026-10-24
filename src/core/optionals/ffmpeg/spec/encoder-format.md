# The `%ffmpeg` encoder format

This file owns the script syntax of `%ffmpeg(...)`: every parameter, the track
forms, how each argument maps onto the format record, the defaults, the
rendering back to a string, the content type the format implies, the file
extension and MIME type, and the errors raised while reading it. The syntax is
a frozen compatibility surface: every form a working script uses keeps its
meaning. It relies on the generic encoder-term machinery of the language (an
encoder term is a name and a list of anonymous, labelled and nested encoder
arguments) and on the content kinds of [content.md](content.md). What happens
once the format is instantiated is in [encoder.md](encoder.md); the inline
encoders that reuse this record are in [inline-codecs.md](inline-codecs.md).

## 1. Shape of a format value

`%ffmpeg(...)` evaluates to an encoder format holding one record:

| Field         | Type                                            | Default   | Set by                                                                         |
| ------------- | ----------------------------------------------- | --------- | ------------------------------------------------------------------------------ |
| `format`      | short muxer name or none                        | none      | `format=` (§3)                                                                 |
| `mime_type`   | string or none                                  | none      | never set by the syntax                                                        |
| `output`      | `Stream` or `Url of string`                     | `Stream`  | outputs only ([encoder.md §9](encoder.md#9-outputs-with-a-url-or-file-target)) |
| `streams`     | ordered list of (field, track mode)             | empty     | nested `%name(...)` arguments (§4)                                             |
| `interleaved` | `Default`, `True` or `False`                    | `Default` | `interleaved=` (§3)                                                            |
| `metadata`    | metadata list, in written order                 | empty     | `metadata=` (§3)                                                               |
| `opts`        | container options, key → string/int/int64/float | empty     | every other top-level labelled argument (§3)                                   |

`streams` keeps the order of the nested arguments in the script. That order is
the order of the streams in the container
([encoder.md §5](encoder.md#5-encoding-a-frame)). A field MUST appear at most
once: a format that names a field twice is rejected, at type checking and at
run time, with an error of kind `encoder` naming the field. Reason: one frame
field feeds one stream, so a second track on the same field has no data of its
own and would leave a stream in the container that is never fed.

A track mode is one of:

- `Drop`: the field is not muxed.
- `Copy of copy_opt`: packets are remuxed as they come; `copy_opt` is
  `Wait_for_keyframe` or `Ignore_keyframe`.
- `Encode of encoded_stream`: the content is encoded. `encoded_stream` holds
  `mode` (`Internal` or `Raw`), `codec` (string or none), `options` (audio,
  video or subtitle options, §5–§7) and `opts` (codec options, key → value).

Option values are one of `String s`, `Int i`, `Int64 i`, `Float f`. The syntax
produces `String`, `Int` and `Float`; `Int64` appears only through
`q`/`qscale` (§8).

### 1.1 Conformance

- `%ffmpeg(%audio(codec="aac"), %audio.copy)` is rejected with an error naming
  `audio`, both by `liquidsoap --check` and at run time.
- The streams of an output appear in the container in the order of the
  `%name(...)` arguments, for every mix of modes.

## 2. Track names

A nested argument `%name(...)` or `%name.mode(...)` is a track. The name is
split on `.`:

| Written          | Field | Mode                     |
| ---------------- | ----- | ------------------------ |
| `%f`             | `f`   | encode, internal (§5–§7) |
| `%f.raw`         | `f`   | encode, raw              |
| `%f.copy`        | `f`   | copy                     |
| `%f.drop`        | `f`   | drop                     |
| `%f.<other>`     | —     | error                    |
| `%f.<mode>.<x>…` | —     | error                    |

The errors are of kind `encoder`, positioned at the track, and name the
written suffix. The field is the frame field of that name; any name registers
a new field (`%dolby_audio`, `%audio_2`, `%sub`).

### 2.1 Media type of an encoded track

An `Internal` or `Raw` track needs a media type: audio, video or subtitle. It
is read, first match wins, from:

1. an anonymous argument `audio_content`, `video_content` or
   `subtitle_content`, in that order of precedence;
2. the field name containing `subtitle`, then `audio`, then `video`
   (substring match anywhere in the field name);
3. the `codec` argument when it is a literal string: the codec is looked up as
   an audio, then a video, then a subtitle encoder of the linked FFmpeg.

The type checker and the runtime apply this one rule. The type checker sees
only literal strings (`codec="aac"`); a computed codec gives no answer there.
When no rule answers:

- at type-checking time the track gets an unconstrained track type (§10);
- at run time an error of kind `encoder` is raised, positioned at the `codec`
  argument when it has a position, that lists the three conventions. Example
  message:

```
Unable to find a track media content type. Please use one of the available convention:
- Use `"audio"`, `"video"` or `"subtitle"` in the track name, e.g. `%dolby_audio`
- Add `audio_content`, `video_content` or `subtitle_content` to the track parameters, e.g. `%track(audio_content)`
- Use a static codec string, e.g. `%track(codec="libmp3lame")`
```

A copy or drop track has no media type.

## 3. Top-level arguments

| Argument                     | Value                         | Effect                                            |
| ---------------------------- | ----------------------------- | ------------------------------------------------- |
| `format="none"`              | string                        | `format` = none                                   |
| `format=s`                   | string                        | `format` = `s` (a muxer short name, e.g. `"mp4"`) |
| `metadata=[(k, v), …]`       | list of string pairs          | `metadata` = the pairs, in written order          |
| `interleaved=true` / `false` | bool                          | `True` / `False`                                  |
| `interleaved="default"`      | string                        | `Default`                                         |
| `interleaved=<other>`        | any other value               | error                                             |
| `k=s` (any other key)        | string                        | `opts[k] = String s`                              |
| `k=i`                        | int                           | `opts[k] = Int i`                                 |
| `k=f`                        | float                         | `opts[k] = Float f`                               |
| anything else                | any other value, or anonymous | error                                             |

Errors are of kind `encoder`, positioned at the argument. `metadata` with a
list whose elements are not string pairs is an error. A later occurrence of a
container option key replaces an earlier one. Within `metadata`, a key written
twice keeps its last value, as the container's metadata does
([avformat.md §4.4](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output)).

## 4. Track arguments: common rules

Every track's arguments are read left to right. For an encoded track:

- `codec=v` sets `codec` to the string form of `v` (§9). It is not an option.
- `q=v` or `qscale=v` sets the global-quality pair (§8).
- the media-type markers `audio_content`, `video_content` and
  `subtitle_content` (anonymous) are consumed. They are never codec options.
- media-specific keys are consumed by §5–§7.
- every other labelled argument becomes a codec option: `String`, `Int` or
  `Float` by its value; any other value is an error of kind `encoder` at the
  value's position.
- any other anonymous argument is an error of kind `encoder`.

For a copy track the arguments are:

| Arguments                         | Result                                       |
| --------------------------------- | -------------------------------------------- |
| none                              | `Copy Wait_for_keyframe`                     |
| one anonymous `wait_for_keyframe` | `Copy Wait_for_keyframe`                     |
| one anonymous `ignore_keyframe`   | `Copy Ignore_keyframe`                       |
| anything else                     | error naming the first argument not accepted |

A drop track accepts no arguments: any argument is an error. Reason: an
argument on a dropped track has no effect, and a silent no-op hides a typo in
the mode.

## 5. Audio tracks

Defaults: `pcm_kind` = the float PCM kind, `channels` = 2, `channel_layout` =
none, `samplerate` = the global audio rate (`settings.frame.audio.samplerate`,
read when first needed and kept for this format value), `sample_format` =
none. On a raw track (`%audio.raw`), a channel count or sample rate the
format does not write is taken from the first frame instead
([encoder-internal.md §2.1](encoder-internal.md#21-audio)), and the printed
form (§8) leaves it out.

| Argument                              | Effect                                                                |
| ------------------------------------- | --------------------------------------------------------------------- |
| anonymous `pcm`, `pcm_s16`, `pcm_f32` | sets `pcm_kind` (float, s16, f32); the last one written wins          |
| `channel_layout=v`                    | sets `channel_layout` to the named layout and `channels` to its count |
| `channels=v`, `ac=v`                  | sets `channels` (§9 integer, at least 1) and clears `channel_layout`  |
| `samplerate=v`, `ar=v`                | sets `samplerate` (§9 integer, at least 1)                            |
| `sample_format=v`                     | sets `sample_format` (string)                                         |

**Channel rule.** Of `channel_layout`, `channels` and `ac`, the last one
written decides the channel count, and `channel_layout` names the layout only
when it is that last one. The same rule holds at type checking (§10.1).
Without a named layout the stream uses FFmpeg's default layout for the count
([encoder-internal.md §2.1](encoder-internal.md#21-audio)). `channel_layout`
is not passed to the codec as an option: the stream's layout is a typed
parameter of the stream.

`channel_layout` written as a float (`channel_layout=5.1`) is read as the
string with one decimal (`"5.1"`). A layout name FFmpeg does not know is an
error of kind `encoder` at the value's position.

## 6. Video tracks

Defaults: `framerate` = global video rate, `width`/`height` = global video
dimensions (each read when first needed and kept for this format value),
`pixel_format` = none, `alpha` = none, `hwaccel` = `auto`, `hwaccel_device` =
none, `hwaccel_pixel_format` = none.

| Argument                                                           | Effect                                                                                                 |
| ------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------ |
| `framerate=v`, `r=v`                                               | `framerate` (§9 integer, at least 1)                                                                   |
| `width=v`, `height=v`                                              | `width` / `height` (§9 integer, at least 1)                                                            |
| `pixel_format="guess"`                                             | `pixel_format` none, `alpha` none                                                                      |
| `pixel_format=v`                                                   | `pixel_format` = string of `v`; `alpha` = whether FFmpeg's descriptor of that format has an alpha flag |
| `hwaccel=` `"auto"`, `"none"`, `"internal"`, `"device"`, `"frame"` | `hwaccel`                                                                                              |
| `hwaccel=<other>`                                                  | error                                                                                                  |
| `hwaccel_device="none"`                                            | none                                                                                                   |
| `hwaccel_device=v`                                                 | string of `v`                                                                                          |
| `hwaccel_pixel_format="guess"`                                     | none                                                                                                   |
| `hwaccel_pixel_format=v`                                           | string of `v`                                                                                          |

A `pixel_format` or `hwaccel_pixel_format` name FFmpeg does not know is an
error of kind `encoder` at the value's position. How `hwaccel*` and
`pixel_format` are used is in
[encoder-internal.md §2](encoder-internal.md#2-stream-creation).

## 7. Subtitle tracks

Default `text_to_ass` is `fun (i, text) -> "<i>,0,Default,,0,0,0,,<text>"`
(the ASS dialogue fields after the layer: read order, layer 0, style
`Default`, empty name, zero margins, empty effect, then the text).

`text_to_ass=f` with a script function of type `(int, string) -> string`
replaces it; it is applied when a text subtitle is encoded
([encoder-internal.md §4.7](encoder-internal.md#47-subtitle)). Every other
argument follows §4.

## 8. `q` and `qscale`

`q=v` (or `qscale=v`) on any encoded track, with `v` read as a float (§9):

1. `flags` becomes: an existing `Int i` → `Int (i lor QSCALE)`; `Int64 i` →
   `Int64 (i lor QSCALE)`; `String s` → `String (s ^ "+qscale")`; absent →
   `Int QSCALE`. `QSCALE` is FFmpeg's `AV_CODEC_FLAG_QSCALE` (2).
2. `global_quality` = `Float (FF_QP2LAMBDA × v)`, `FF_QP2LAMBDA` = 118.

This is how the FFmpeg command line implements `-q`. A `flags` argument
written after `q` replaces the combined value. The encoder adds its own flags
to `flags` in the same combining way
([encoder.md §3](encoder.md#3-building-an-instance) step 1).

## 9. Value conversions

| Conversion | Int      | Float                | String                                                     | Other              |
| ---------- | -------- | -------------------- | ---------------------------------------------------------- | ------------------ |
| integer    | itself   | itself when integral | FFmpeg expression evaluation (`"2*640"`), must be integral | `integer expected` |
| float      | as float | itself               | FFmpeg expression evaluation                               | `float expected`   |
| string     | `%i`     | `%f` (six decimals)  | itself                                                     | `string expected`  |

Every failure, including a string that is not a valid expression or not
integral, is an error of kind `encoder` at the value's position. Without
FFmpeg linked, a string is read as a decimal literal.

## 10. Content type implied by a format

The content type of a format is computed by **one** function of the format,
used both while type checking (from the term) and when an output instantiates
the format (from the record). The two MUST agree: the type a script is checked
against is the type the encoder is given. The type checker differs only in
seeing literal values alone (§2.1, §10.1).

| Track                      | Field type                                              |
| -------------------------- | ------------------------------------------------------- |
| `.drop`                    | an unconstrained track variable                         |
| `.copy`                    | `ffmpeg.copy`                                           |
| `.raw`, audio              | `ffmpeg.audio.raw`                                      |
| `.raw`, video              | `ffmpeg.video.raw`                                      |
| internal audio             | `pcm`/`pcm_s16`/`pcm_f32` with the channel layout of §5 |
| internal video             | video, with alpha when `alpha` is true                  |
| subtitle (internal or raw) | `subtitles`                                             |
| no media type (§2.1)       | an unconstrained track variable                         |

### 10.1 Literal values at type-checking time

- **Channels.** §5's channel rule, applied to the written arguments. The
  winning argument MUST be a literal: an integer for `channels`/`ac`, a string
  or a float for `channel_layout`. A non-literal winner is an error of kind
  `encoder` at its position. Example message:
  `Invalid value <term> for channels parameter. Only static numbers are allowed.`
  With no channel argument, 2.
- **Layout names.** Known through FFmpeg when it is linked, otherwise from the
  table below. Without FFmpeg, a name not in the table gives an unconstrained
  track variable.
- **Alpha.** `pixel_format` as a literal string whose FFmpeg descriptor has
  alpha gives an alpha video type. Without FFmpeg, the video type has no
  alpha.

| Layout name | Channels |
| ----------- | -------- |
| mono        | 1        |
| stereo      | 2        |
| 2.1         | 3        |
| quad        | 4        |
| 5.0         | 5        |
| 5.1         | 6        |
| 7.1         | 8        |

A process without FFmpeg uses this table, cannot infer a media type from a
codec name, and never instantiates a format.

Examples (`liquidsoap --check -i`):

```
%ffmpeg(format="mp4", %audio(codec="aac"), %video(codec="libx264"))
  : format(audio=pcm(stereo), video=yuv420p)
%ffmpeg(%audio.copy, %video.raw(codec="libx264"), %audio_2.raw(codec="aac"))
  : format(audio=ffmpeg.copy, audio_2=ffmpeg.audio.raw, video=ffmpeg.video.raw)
%ffmpeg(%sub(subtitle_content, codec="ass"), %audio.drop, %mytrack(codec="libopus", ac=1))
  : format(audio='A, mytrack=pcm(mono), sub=subtitles) where 'A is a track
%ffmpeg(%audio(pcm_s16, codec="pcm_s16le", channels=1))
  : format(audio=pcm_s16(mono))
```

### 10.2 Conformance

- `%audio(codec="aac", channel_layout="stereo", channels=1)` types as
  `pcm(mono)` and encodes one channel;
  `%audio(codec="aac", channels=1, channel_layout="stereo")` types as `pcm(stereo)` and encodes two.
- `%video(codec="libvpx-vp9", pixel_format="yuva420p")` types with alpha under
  `--check`, and the encoder receives alpha video.
- For every format of the test matrix, the type printed by `--check` equals
  the type the output instantiates.

## 11. Rendering (`to_string`)

A format prints as `%ffmpeg(<items>)`, items joined by `,`. The printed form
MUST parse back to an equal format, with one exception: a non-default
`text_to_ass` function has no printed form and is omitted. The fields `output`
and `mime_type` are not syntax and are not printed. Items, in order:

1. `format="<f>"` when set.
2. `metadata=[("<k>","<v>"),…]` when non-empty, in stored order.
3. `interleaved="default"`, `interleaved=true` or `interleaved=false`.
4. The tracks, in written order.
5. The container options as `k=v`, in a stable order.

Tracks:

| Track    | Rendering                                                                                                                                                                       |
| -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| drop     | `%f.drop`                                                                                                                                                                       |
| copy     | `%f.copy(wait_for_keyframe)` or `%f.copy(ignore_keyframe)`                                                                                                                      |
| raw      | `%f.raw(…)`, contents as for internal                                                                                                                                           |
| audio    | `%f(…)`: content marker, PCM marker, `codec`, `channel_layout` when named else `channels`, `samplerate`, `sample_format` when set, then the codec options                       |
| video    | `%f(…)`: content marker, `codec`, `width`, `height`, `framerate`, `pixel_format` when set, `hwaccel`, `hwaccel_device`, `hwaccel_pixel_format` when set, then the codec options |
| subtitle | `%f(…)`: content marker, `codec`, then the codec options                                                                                                                        |

The content marker (`audio_content`, `video_content`, `subtitle_content`) is
printed when the field name does not give the media type (§2.1 rule 2). The
PCM marker (`pcm_s16`, `pcm_f32`) is printed when the kind is not the float
default. Strings are printed as script string literals (`"auto"` and `"none"`
included); ints and int64 in decimal; floats as script float literals.
Rendering evaluates the default rates and sizes (§5, §6).

Example:

```
%ffmpeg(format="mp4", %audio(pcm_s16, codec="aac", b="128k"), %video(codec="libx264", preset="fast"), metadata=[("title","x")])
→ %ffmpeg(format="mp4",metadata=[("title","x")],interleaved="default",%audio(pcm_s16,codec="aac",channels=2,samplerate=44100,b="128k"),%video(codec="libx264",width=1280,height=720,framerate=25,hwaccel="auto",hwaccel_device="none",preset="fast"))
```

### 11.1 Conformance

- For every format of the test matrix (without a custom `text_to_ass`),
  printing, parsing and printing again gives the same string, and the parsed
  format has the same content type.

## 12. File extension and MIME type

Outputs ask the format for an extension and a MIME type. Only `format` (and,
for `adts`, the single track's codec) decides:

| `format`                                              | Extension | MIME                       |
| ----------------------------------------------------- | --------- | -------------------------- |
| `ogg`                                                 | `ogg`     | `application/ogg`          |
| `opus`                                                | `opus`    | `application/ogg`          |
| `mp3`                                                 | `mp3`     | `audio/mpeg`               |
| `matroska`                                            | `mkv`     | `video/x-matroska`         |
| `mpegts`                                              | `ts`      | `application/octet-stream` |
| `ac3`                                                 | `ac3`     | `audio/ac3`                |
| `eac3`                                                | `eac3`    | `audio/eac3`               |
| `adts`, exactly one track, encoded with `codec="aac"` | `aac`     | `audio/aac`                |
| `adts`, otherwise                                     | `adts`    | `application/octet-stream` |
| `mp4`                                                 | `mp4`     | `video/mp4`                |
| `webm`                                                | `webm`    | `video/webm`               |
| `wav`                                                 | `wav`     | `audio/wav`                |
| other                                                 | none      | `application/octet-stream` |

Icecast and Harbor use their own table instead
([io.md §12](io.md#12-other-outputs-special-casing-ffmpeg)).

The format gives no RFC 6381 codec string: codec strings come from the
streams, with a per-stream fallback
([encoder.md §8.6](encoder.md#86-properties)).

`video_size` of the format: the width and height of the single internal or
raw video track; none with zero or several.

## 13. Errors summary

All errors are of kind `encoder`, positioned at the offending argument when it
has a position. Message texts are examples, not requirements. Every error of
this file is a configuration error ([faults.md §2](faults.md#2-classes)):
raised at script evaluation (type check) or when the output that holds the
format starts, before any frame is encoded, and the script fails.

| Condition                                             | When                                          |
| ----------------------------------------------------- | --------------------------------------------- |
| unknown or extra mode suffix (§2)                     | type check and run                            |
| field named twice (§1)                                | type check and run                            |
| no media type for an encoded track (§2.1)             | run                                           |
| non-literal winning channel argument (§10.1)          | type check                                    |
| bad copy or drop argument (§4)                        | type check and run                            |
| bad top-level argument, bad `interleaved` (§3)        | run                                           |
| bad track option value, bad `hwaccel` (§4, §6)        | run                                           |
| unknown layout or pixel format name (§5, §6)          | run; type check for layouts                   |
| bad integer/float/string, non-positive count (§5, §9) | run                                           |
| unknown option, unknown codec, missing codec          | [encoder.md §4](encoder.md#4-option-handling) |
