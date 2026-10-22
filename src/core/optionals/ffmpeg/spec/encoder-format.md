# The `%ffmpeg` encoder format

This file owns the script syntax of `%ffmpeg(...)`: every parameter, the track
forms, how each argument maps onto the format record, the defaults, the
rendering back to a string, the content type the format implies, and the errors
raised while reading it. It relies on the generic encoder-term machinery of the
language (an encoder term is a name and a list of anonymous, labelled and nested
encoder arguments) and on the content kinds defined in
[content.md](content.md). What happens once the format is instantiated is in
[encoder.md](encoder.md); the inline encoders that reuse this record are in
[inline-codecs.md](inline-codecs.md).

## 1. Shape of a format value

`%ffmpeg(...)` evaluates to an encoder format holding one record:

| Field         | Type                                            | Default   | Set by                                                            |
| ------------- | ----------------------------------------------- | --------- | ----------------------------------------------------------------- |
| `format`      | short muxer name or none                        | none      | `format=` (§3)                                                    |
| `mime_type`   | string or none                                  | none      | never set by the syntax                                           |
| `output`      | `Stream` or `Url of string`                     | `Stream`  | outputs only ([encoder.md §3](encoder.md#3-building-an-instance)) |
| `streams`     | ordered list of (field, track mode)             | empty     | nested `%name(...)` arguments (§4)                                |
| `interleaved` | `Default`, `True` or `False`                    | `Default` | `interleaved=` (§3)                                               |
| `metadata`    | metadata list                                   | empty     | `metadata=` (§3)                                                  |
| `opts`        | container options, key → string/int/int64/float | empty     | every other top-level labelled argument (§3)                      |

`streams` keeps the order of the nested arguments in the script. A field named
twice appears twice in the list.

A track mode is one of:

- `Drop` — the field is not muxed.
- `Copy of copy_opt` — packets are remuxed as they come; `copy_opt` is
  `Wait_for_keyframe` or `Ignore_keyframe`.
- `Encode of encoded_stream` — the content is encoded. `encoded_stream` holds
  `mode` (`Internal` or `Raw`), `codec` (string or none), `options` (audio,
  video or subtitle options, §5–§7) and `opts` (codec options, key → value).

Option values are one of `String s`, `Int i`, `Int64 i`, `Float f`. The syntax
produces `String`, `Int` and `Float`; `Int64` appears only through
`q`/`qscale` (§8).

## 2. Track names

A nested argument `%name(...)` or `%name.mode(...)` is a track. The name is
split on `.`:

| Written      | Field | Mode                                                                     |
| ------------ | ----- | ------------------------------------------------------------------------ |
| `%f`         | `f`   | encode, internal (§5–§7)                                                 |
| `%f.raw`     | `f`   | encode, raw                                                              |
| `%f.copy`    | `f`   | copy                                                                     |
| `%f.drop`    | `f`   | drop                                                                     |
| `%f.<other>` | —     | failure `invalid content field` (an OCaml `Failure`, not a script error) |

Only the first two components count: `%audio.copy.x` is a copy track. The field
is the frame field of that name; any name registers a new field
(`%dolby_audio`, `%audio_2`, `%sub`).

### 2.1 Media type of an encoded track

An `Internal` or `Raw` track needs a media type: audio, video or subtitle. It
is read, first match wins, from:

1. an anonymous argument `audio_content`, `video_content` or
   `subtitle_content`, in that order of precedence;
2. the name containing `subtitle`, then `audio`, then `video` (substring
   match anywhere in the whole written name, mode suffix included);
3. the `codec` argument when it is a literal string: the codec is looked up as
   an audio, then a video, then a subtitle encoder of the linked FFmpeg.

The type checker and the runtime both apply this rule. The type checker sees
only literal strings (`codec="aac"`); a computed codec gives no answer there.
When no rule answers:

- at type-checking time the track gets an unconstrained track type (§10);
- at run time the error below is raised (kind `encoder`, positioned at the
  `codec` argument when it has a position):

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
| `metadata=[(k, v), …]`       | list of string pairs          | `metadata` = the pairs                            |
| `metadata=[]`                | empty list                    | `metadata` empty                                  |
| `interleaved=true` / `false` | bool                          | `True` / `False`                                  |
| `interleaved="default"`      | string                        | `Default`                                         |
| `k=s` (any other key)        | string                        | `opts[k] = String s`                              |
| `k=i`                        | int                           | `opts[k] = Int i`                                 |
| `k=f`                        | float                         | `opts[k] = Float f`                               |
| anything else                | any other value, or anonymous | generic encoder parameter error                   |

`metadata` with a list whose elements are not string pairs raises the generic
parameter error. A later occurrence of a key replaces an earlier one. The
metadata pairs are stored in reverse of their written order; duplicate keys keep
whichever the metadata container keeps (see
[content.md](content.md) for the metadata container).

`interleaved="other"` (a string other than `"default"`) falls into the generic
`k=s` row and becomes a container option named `interleaved`, which is then
reported unrecognized when the encoder opens ([encoder.md §4](encoder.md#4-option-handling)).

## 4. Track arguments: common rules

Every track's arguments are read left to right. For an encoded track:

- `codec=v` sets `codec` to the string form of `v` (§9). It is not an option.
- `q=v` or `qscale=v` sets the global-quality pair (§8).
- the media-type markers `audio_content` and `video_content` (anonymous) are
  consumed. `subtitle_content` is not ([findings](findings/encoder.md)).
- media-specific keys are consumed by §5–§7.
- every other labelled argument becomes a codec option: `String`, `Int` or
  `Float` by its value; any other value raises `unexpected option` (kind
  `encoder`, at the value's position).

For a copy track (§2) the arguments are:

| Arguments                         | Result                                                                |
| --------------------------------- | --------------------------------------------------------------------- |
| none                              | `Copy Wait_for_keyframe`                                              |
| one anonymous `wait_for_keyframe` | `Copy Wait_for_keyframe`                                              |
| one anonymous `ignore_keyframe`   | `Copy Ignore_keyframe`                                                |
| one anonymous other string        | `Invalid value for copy encoder parameter: <value>`                   |
| one labelled, or two or more      | generic parameter error naming the single argument, or the second one |

A drop track ignores its arguments.

## 5. Audio tracks

Defaults: `pcm_kind` = the float PCM kind, `channels` = 2, `samplerate` = the
global audio rate (`settings.frame.audio.samplerate`, read when first needed),
`sample_format` = none.

| Argument                              | Effect                                                                                                             |
| ------------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| anonymous `pcm`, `pcm_s16`, `pcm_f32` | sets `pcm_kind` (float, s16, f32); the last one written wins; not an option                                        |
| `channel_layout=v`                    | sets `channels` to the channel count FFmpeg gives for the layout, **and** keeps `channel_layout` as a codec option |
| `channels=v`, `ac=v`                  | sets `channels` (§9 integer)                                                                                       |
| `samplerate=v`, `ar=v`                | sets `samplerate` (§9 integer)                                                                                     |
| `sample_format=v`                     | sets `sample_format` (string)                                                                                      |

`channel_layout` written as a float (`channel_layout=5.1`) is formatted with
one decimal (`"5.1"`). A layout name FFmpeg does not know raises FFmpeg's
error at encoder-construction time.

Arguments apply in order, so at run time the last of `channel_layout`,
`channels`, `ac` wins. The type checker uses a different rule (§10.1).

## 6. Video tracks

Defaults: `framerate` = global video rate, `width`/`height` = global video
dimensions (each read when first needed), `pixel_format` = none, `alpha` = none, `hwaccel` =
`auto`, `hwaccel_device` = none, `hwaccel_pixel_format` = none.

| Argument                                                           | Effect                                                                                                                                        |
| ------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------- |
| `framerate=v`, `r=v`                                               | `framerate` (integer, §9)                                                                                                                     |
| `width=v`, `height=v`                                              | `width` / `height` (integer)                                                                                                                  |
| `pixel_format="guess"`                                             | `pixel_format` none, `alpha` none                                                                                                             |
| `pixel_format=v`                                                   | `pixel_format` = string of `v`; `alpha` = whether FFmpeg's descriptor of that format has an alpha flag, none if FFmpeg does not know the name |
| `hwaccel=` `"auto"`, `"none"`, `"internal"`, `"device"`, `"frame"` | `hwaccel`                                                                                                                                     |
| `hwaccel=<other string>`                                           | becomes a codec option named `hwaccel` (reported unrecognized later)                                                                          |
| `hwaccel_device="none"`                                            | none                                                                                                                                          |
| `hwaccel_device=v`                                                 | string of `v`                                                                                                                                 |
| `hwaccel_pixel_format="guess"`                                     | none                                                                                                                                          |
| `hwaccel_pixel_format=v`                                           | string of `v`                                                                                                                                 |

How `hwaccel*` and `pixel_format` are used is in
[encoder-internal.md §2](encoder-internal.md#2-stream-creation).

## 7. Subtitle tracks

Default `text_to_ass` is `fun (i, text) -> "<i>,0,Default,,0,0,0,,<text>"`
(the ASS dialogue fields after the layer: read order, layer 0, style
`Default`, empty name, zero margins, empty effect, then the text).

`text_to_ass=f` with a script function of type `(int, string) -> string`
replaces it; it is applied when a text subtitle is encoded
([encoder-internal.md §4](encoder-internal.md#4-conversion)). Every other argument follows
§4.

## 8. `q` and `qscale`

`q=v` (or `qscale=v`) on any encoded track, with `v` read as a float (§9):

1. `flags` becomes: an existing `Int i` → `Int (i lor QSCALE)`; `Int64 i` →
   `Int64 (i lor QSCALE)`; `String s` → `String (s ^ "+qscale")`; absent →
   `Int QSCALE`. `QSCALE` is FFmpeg's `AV_CODEC_FLAG_QSCALE` (2).
2. `global_quality` = `Float (FF_QP2LAMBDA × v)`, `FF_QP2LAMBDA` = 118.

This is how the FFmpeg command line implements `-q`. `q=2` renders as
`flags=2,global_quality=236.` (from the implementation). A `flags` argument
written after `q` replaces the combined value.

## 9. Value conversions

| Conversion | Int      | Float                | String                                                                           | Other              |
| ---------- | -------- | -------------------- | -------------------------------------------------------------------------------- | ------------------ |
| integer    | itself   | itself when integral | FFmpeg expression evaluation (`"2*640"`), must be integral; else `int_of_string` | `integer expected` |
| float      | as float | itself               | FFmpeg expression evaluation                                                     | `float expected`   |
| string     | `%i`     | `%f` (six decimals)  | itself                                                                           | `string expected`  |

Errors are kind `encoder`, at the value's position. A string that is neither a
valid expression nor an integer raises OCaml's `Failure "int_of_string"`.

## 10. Content type implied by a format

The content type is computed twice: from the term while type checking, and
from the built record when an output instantiates the format. Both give one
frame field per track.

### 10.1 At type-checking time (from the term)

| Track                      | Field type                                             |
| -------------------------- | ------------------------------------------------------ |
| `.drop`                    | an unconstrained track variable                        |
| `.copy`                    | `ffmpeg.copy`                                          |
| `.raw`, audio              | `ffmpeg.audio.raw`                                     |
| `.raw`, video              | `ffmpeg.video.raw`                                     |
| internal audio             | `pcm`/`pcm_s16`/`pcm_f32` with the channel count below |
| internal video             | the default video format (`yuv420p`)                   |
| subtitle (internal or raw) | `subtitles`                                            |
| no media type (§2.1)       | an unconstrained track variable                        |

Channel count at type-checking time: `channel_layout` when its value is a
literal string, or a float printed with one decimal, and the layout is known
(through FFmpeg when it is linked, otherwise from the table below); otherwise
`channels`, then `ac`, which must be literal integers (error
`Invalid value <term> for channels parameter. Only static numbers are allowed.`,
or the same with `ac`); otherwise 2. **`channel_layout` wins whatever the
order**; at run time the last one written wins (§5) ([findings](findings/encoder.md)).

| Layout name | Channels |
| ----------- | -------- |
| mono        | 1        |
| stereo      | 2        |
| 2.1         | 3        |
| quad        | 4        |
| 5.0         | 5        |
| 5.1         | 6        |
| 7.1         | 8        |

A process without FFmpeg uses this table and cannot infer a media type from a
codec name.

Examples (from the implementation, `liquidsoap --check -i`):

```
%ffmpeg(format="mp4", %audio(codec="aac"), %video(codec="libx264"))
  : format(audio=pcm(stereo), video=yuv420p)
%ffmpeg(%audio.copy, %video.raw(codec="libx264"), %audio_2.raw(codec="aac"))
  : format(audio=ffmpeg.copy, audio_2=ffmpeg.audio.raw, video=ffmpeg.video.raw)
%ffmpeg(%audio(pcm_f32, codec="flac", channel_layout="5.1"), %video(codec="libx264", pixel_format="yuva420p"))
  : format(audio=pcm_f32(5.1), video=yuv420p)
%ffmpeg(%sub(subtitle_content, codec="ass"), %audio.drop, %mytrack(codec="libopus", ac=1))
  : format(audio='A, mytrack=pcm(mono), sub=subtitles) where 'A is a track
%ffmpeg(%audio(pcm_s16, codec="pcm_s16le", channels=1))
  : format(audio=pcm_s16(mono))
```

### 10.2 At instantiation time (from the record)

| Track                 | Field type                                                                                    |
| --------------------- | --------------------------------------------------------------------------------------------- |
| drop                  | unconstrained track variable                                                                  |
| copy                  | `ffmpeg.copy`                                                                                 |
| raw audio / raw video | `ffmpeg.audio.raw` / `ffmpeg.video.raw`                                                       |
| internal audio        | PCM of `pcm_kind` with the default channel layout for `channels` (`channels > 0` is asserted) |
| internal video        | video with width and height unset and `alpha` = the record's `alpha` (unset when none)        |
| subtitle              | `subtitles`                                                                                   |

The two differ for `alpha`: `pixel_format="yuva420p"` gives an alpha video
format only at instantiation.

## 11. Rendering (`to_string`)

A format prints as `%ffmpeg(<items>)`, items joined by `,`, in this order:

1. `format="<f>"` when set (OCaml `%S` quoting).
2. `metadata=[<k>=<v>,…]` when non-empty, keys and values quoted as script
   strings.
3. `interleaved="default"`, `interleaved=true` or `interleaved=false`, always.
4. The tracks, **last written first**.
5. The container options, when any, as `k=v,…`.

Tracks:

| Track    | Rendering                                                                                                                                          |
| -------- | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| drop     | `%f.drop`                                                                                                                                          |
| copy     | `%f.copy(wait_for_keyframe)` or `%f.copy(ignore_keyframe)`                                                                                         |
| raw      | `%f.raw(…)`, else as internal                                                                                                                      |
| video    | `%f(<video_content,>opts)`, with `codec`, `framerate`, `width`, `height`, `hwaccel=<name>`, `hwaccel_device=none` or `="<d>"` added to the options |
| audio    | `%f(<audio_content,>opts)`, with `codec`, `channels`, `samplerate` added                                                                           |
| subtitle | `%f(<subtitle_content,>opts)`, with `codec` added                                                                                                  |

The `<x>_content,` prefix is written when the written name does not contain
`video`/`audio`/`subtitle` (regex match on the rendered name). Option values:
strings with `%S`, ints and int64 in decimal, floats with OCaml's
`string_of_float` (`236.`), bare words (`hwaccel=auto`, `none`) unquoted.
Option order does not follow the written order (examples below).
Rendering evaluates the default rates and sizes (§5, §6).

Not rendered: `pcm_kind`, `sample_format`, `pixel_format`, `alpha`,
`hwaccel_pixel_format`, `text_to_ass`, `mime_type`, `output`.

Examples (from the implementation, `print(...)`):

```
%ffmpeg(format="mp4", %audio(codec="aac", b="128k", channels=2), %video(codec="libx264", preset="fast"))
→ %ffmpeg(format="mp4",interleaved="default",%video(width=1280,codec="libx264",hwaccel=auto,framerate=25,hwaccel_device=none,preset="fast",height=720),%audio(codec="aac",samplerate=44100,b="128k",channels=2))

%ffmpeg(format="mpegts", %audio.copy, %video.copy(ignore_keyframe), metadata=[("title","x")])
→ %ffmpeg(format="mpegts",metadata=["title"="x"],interleaved="default",%video.copy(ignore_keyframe),%audio.copy(wait_for_keyframe))

%ffmpeg(%audio.raw(codec="libmp3lame", q=2), %video.drop, interleaved=false)
→ %ffmpeg(interleaved=false,%video.drop,%audio.raw(codec="libmp3lame",samplerate=44100,channels=2,flags=2,global_quality=236.))

%ffmpeg(format="matroska", %subtitles(codec="ass"), %dolby_audio(codec="ac3", channel_layout=5.1))
→ %ffmpeg(format="matroska",interleaved="default",%dolby_audio(channel_layout=5.1,codec="ac3",samplerate=44100,channels=6),%subtitles(codec="ass"))

%ffmpeg(%foo(codec="aac"), %bar.copy, %audio(pcm_s16, codec="flac"))
→ %ffmpeg(interleaved="default",%audio(codec="flac",samplerate=44100,channels=2),%bar.copy(wait_for_keyframe),%foo(audio_content,codec="aac",samplerate=44100,channels=2))
```

Metadata renders as `metadata=["title"="x"]`, which parses back to another
value than the written one, and the PCM kind (`pcm_s16`) is not rendered.

## 12. File extension and MIME type

Outputs ask the format for an extension and a MIME type. Only `format` (and,
for `adts`, the single track's codec) decides:

| `format`                                              | Extension          | MIME                       |
| ----------------------------------------------------- | ------------------ | -------------------------- |
| `ogg`                                                 | `ogg`              | `application/ogg`          |
| `opus`                                                | `opus`             | `application/ogg`          |
| `mp3`                                                 | `mp3`              | `audio/mpeg`               |
| `matroska`                                            | `mkv`              | `video/x-matroska`         |
| `mpegts`                                              | `ts`               | `application/octet-stream` |
| `ac3`                                                 | `ac3`              | `audio/ac3`                |
| `eac3`                                                | `eac3`             | `audio/eac3`               |
| `adts`, exactly one track, encoded with `codec="aac"` | `aac`              | `audio/aac`                |
| `adts`, otherwise                                     | `adts`             | `application/octet-stream` |
| `mp4`                                                 | `mp4`              | `video/mp4`                |
| `wav`                                                 | `wav`              | `audio/wav`                |
| other                                                 | none (`Not_found`) | `application/octet-stream` |

Icecast and Harbor use their own table instead ([io.md §12](io.md#12-other-outputs-special-casing-ffmpeg)).

The RFC 6381 codec string used by HLS falls back to this format-level table
when no stream reports one: `format="libmp3lame"` → `mp4a.40.34`,
`format="aac"` → `mp4a.40.2`, otherwise none. `libmp3lame` and `aac` are
codec names compared with the muxer name ([findings](findings/encoder.md)).

`video_size` of the format: the width and height of the single internal or
raw video track, forced; none with zero or several.

## 13. Errors summary

| Condition                                          | When               | Error                                                              |
| -------------------------------------------------- | ------------------ | ------------------------------------------------------------------ |
| unknown mode suffix                                | type check and run | `Failure "invalid content field"`                                  |
| no media type for an encoded track                 | run                | kind `encoder`, message of §2.1                                    |
| non-literal `channels`/`ac` (when no known layout) | type check         | kind `encoder`, `Invalid value … Only static numbers are allowed.` |
| bad copy argument                                  | run                | `Invalid value for copy encoder parameter: …` or generic           |
| bad top-level argument                             | run                | generic encoder parameter error                                    |
| bad track option value                             | run                | `unexpected option`                                                |
| bad integer/float/string                           | run                | `integer expected` / `float expected` / `string expected`          |
| unknown option, unknown codec, missing codec       | encoder creation   | [encoder.md §4](encoder.md#4-option-handling)                      |
