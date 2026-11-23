# Encoder formats and the `%name(...)` language (as-built)

What a format value is, how a script writes one, how it is typed, built and
printed, and the helpers every caller reads from it. Each encoder's options
(names, defaults, ranges, error messages) are its implementation file's §1;
this file owns the machinery they share and the table of where each lives.

Relies on: the seam ([encoder.md](encoder.md)) for what a format is used for.

## 1. Syntax

- A format expression is `%` followed by a name of letters, digits, `.` and
  `_` (`%mp3`, `%mp3.vbr`, `%audio.none`), optionally followed by a
  parenthesised, comma-separated parameter list. `%name` and `%name()` are
  the same.
- A parameter is one of:
  - `label = expression` (labelled); the label is an identifier or a string
    literal;
  - `word` or `"string"` (anonymous): a bare identifier or a string literal,
    never evaluated as a variable;
  - `%subname(...)` (a nested format expression, "sub-encoder").
- String literals used as labels or anonymous parameters are rendered at
  parse time (interpolation included).
- Labelled values are ordinary expressions, evaluated in the script's
  environment when the format expression is evaluated.
- Preprocessor: `%ifencoder %name ... [%else ...] %endif` and `%ifnencoder`
  keep the first branch when `%name` (with no parameters) builds a format
  that some stream plug-in answers (`%ifnencoder`: when it does not). The test
  runs before type checking. Any exception while building it, an unknown name
  included, counts as "not available" for both forms. It checks the kind's
  plug-in only: `%ifencoder %vorbis` holds whenever the Ogg plug-in exists,
  whether or not the Vorbis encoder is compiled in.

## 2. The format kinds

A format value is one of ten kinds. The script names map onto them:

| Script name(s)                             | Kind                    | Language file (builder)                         | Format type file                             | Stream plug-in (§2.1 of the seam)          | Implementation owner                                         |
| ------------------------------------------ | ----------------------- | ----------------------------------------------- | -------------------------------------------- | ------------------------------------------ | ------------------------------------------------------------ |
| `%wav`                                     | WAV                     | `lang_wav.ml`                                   | `wav_format.ml`                              | `wav_encoder.ml`                           | simple-encoders                                              |
| `%avi`                                     | AVI                     | `lang_avi.ml`                                   | `avi_format.ml`                              | `avi_encoder.ml`                           | simple-encoders                                              |
| `%ndi`                                     | NDI                     | `lang_ndi.ml`                                   | `ndi_format.ml`                              | `ndi_encoder.ml`                           | simple-encoders                                              |
| `%external`                                | External                | `lang_external_encoder.ml`                      | `external_encoder_format.ml`                 | `external_encoder.ml`                      | simple-encoders                                              |
| `%mp3`, `%mp3.cbr`, `%mp3.abr`, `%mp3.vbr` | MP3                     | `lang_mp3.ml`                                   | `mp3_format.ml`                              | `optionals/lame`                           | native-codecs                                                |
| `%shine`, `%mp3.fxp`                       | Shine                   | `lang_shine.ml`                                 | `shine_format.ml`                            | `optionals/shine`                          | native-codecs                                                |
| `%fdkaac`                                  | FDK-AAC                 | `lang_fdkaac.ml`                                | `fdkaac_format.ml`                           | `optionals/fdkaac`                         | native-codecs                                                |
| `%flac`                                    | FLAC                    | `lang_flac.ml`                                  | `flac_format.ml`                             | `optionals/flac`                           | native-codecs                                                |
| `%ogg(...)`                                | Ogg                     | `lang_ogg.ml`                                   | `ogg_format.ml`                              | `optionals/ogg`                            | ogg                                                          |
| `%vorbis`, `%vorbis.cbr`, `%vorbis.abr`    | Ogg (one Vorbis stream) | `lang_vorbis.ml`                                | `vorbis_format.ml`                           | `optionals/ogg` + `optionals/vorbis`       | ogg                                                          |
| `%opus`                                    | Ogg (one Opus stream)   | `lang_opus.ml`                                  | `opus_format.ml`                             | `optionals/ogg` + `optionals/opus`         | ogg                                                          |
| `%speex`                                   | Ogg (one Speex stream)  | `lang_speex.ml`                                 | `speex_format.ml`                            | `optionals/ogg` + `optionals/speex`        | ogg                                                          |
| `%theora`                                  | Ogg (one Theora stream) | `lang_theora.ml`                                | `theora_format.ml`                           | `optionals/ogg` + `optionals/theora`       | ogg                                                          |
| `%ogg(%flac(...))`                         | Ogg (one FLAC stream)   | `lang_flac.ml`                                  | `flac_format.ml`                             | `optionals/ogg` + `optionals/ogg_flac`     | ogg                                                          |
| `%ffmpeg(...)`                             | FFmpeg                  | `optionals/ffmpeg/lib/encoder/format_parser.ml` | `optionals/ffmpeg/formats/encoder_format.ml` | `optionals/ffmpeg/lib/encoder/instance.ml` | [ffmpeg tree](../../optionals/ffmpeg/spec/encoder-format.md) |

Paths without a directory are in `src/core/encoder/`.

- `%ogg(...)` takes sub-encoders: the **first** of `%vorbis`, `%vorbis.cbr`,
  `%vorbis.abr`, `%opus`, `%speex`, `%flac` is its audio stream, the first
  `%theora` its video stream. Every other parameter (more audio streams, other
  sub-encoders, labelled or anonymous parameters) is ignored silently.
- `%flac` alone is the native FLAC kind; inside `%ogg` it is an Ogg FLAC
  stream with the same options.
- `%ndi` takes only the sub-encoders `%audio`, `%audio.none`, `%video`,
  `%video.none`, each without parameters; both default to present.

## 3. Format helpers

Functions of the format value alone, used by callers without an instance:

| Helper                                              | Answers                                                                                                                                                                                       | When unknown                                                                              |
| --------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------- |
| `extension`                                         | WAV `wav`, AVI `avi`, Ogg `ogg`, MP3 `mp3`, Shine `mp3`, FLAC `flac`, FDK-AAC `aac`, FFmpeg from its muxer                                                                                    | raises `Not_found` (NDI, External, FFmpeg without a known extension)                      |
| `mime`                                              | WAV `audio/wav`, AVI `video/avi`, Ogg `application/ogg`, MP3 and Shine `audio/mpeg`, FLAC `audio/flex`, FDK-AAC `audio/aac`, FFmpeg from its muxer                                            | `application/octet-stream` (NDI, External)                                                |
| `bitrate` (estimate, bit/s)                         | MP3 CBR: rate×1000; MP3 ABR: mean rate×1000; Shine and FDK-AAC: `bitrate`×1000 (FDK-AAC also in VBR mode)                                                                                     | raises `Not_found` (MP3 VBR, every other kind)                                            |
| `iso_base_file_media_file_format` (RFC 6381 codecs) | MP3, Shine `mp4a.40.34`; FDK-AAC by object type: MPEG-4 AAC-LC `mp4a.40.2`, HE-AAC `mp4a.40.5`, HE-AACv2 `mp4a.40.29`, AAC-LD `mp4a.40.23`, AAC-ELD `mp4a.40.39`, every MPEG-2 type `mp4a.67` | raises `Not_found`                                                                        |
| `video_size`                                        | Ogg with Theora: (width, height); FFmpeg: its video stream's size                                                                                                                             | none                                                                                      |
| `type_of_format`                                    | the frame type of the value (§6)                                                                                                                                                              | NDI with neither audio nor video: assertion failure (unreachable: rejected at evaluation) |
| `string_of_format`                                  | the script form, every resolved option printed (§7)                                                                                                                                           | —                                                                                         |

- Script functions: `encoder.content_type(f)` returns `mime(f)` and
  `encoder.extension(f)` returns `extension(f)`; each returns `""` on any
  exception.
- The `mp4a.40.34` value for MP3 carries a code comment noting `mp4a.69` and
  `mp3` are also seen; the MPEG-2 HE-AAC values carry "TODO: check this".

## 4. Evaluation: building the format value

- The parser produces a format term (name, parameters). At evaluation,
  labelled parameter values are evaluated (sub-encoder parameters
  recursively), then the name's builder is called with the evaluated
  parameters. The builder folds over the parameters **in order**, starting
  from the kind's defaults: a later parameter overrides an earlier one.
- A builder rejects a parameter it does not know through one shared helper,
  giving `Runtime_error` kind `encoder` with:
  - unknown anonymous word: "Unknown encoder parameter: `<word>`", no position;
  - unknown label or a known label with a value of the wrong type: "unknown
    parameter name (`<label>`) or invalid parameter value (`<value>`)", at the
    value's position;
  - unexpected sub-encoder: "unexpected subencoder", no position.
- Option-specific validation raises the same error kind with its own message
  (each implementation file lists them).
- After building, evaluation calls `get_factory` on the result. No plug-in
  for the kind (or no builder for the name) gives `Runtime_error` kind
  `encoder` "unsupported format" at the expression's position. Script error
  code 14 (uncaught runtime error).
- The value is a "custom" language value named `encoder`. It has no methods.
  It cannot be converted to JSON ("Encoders cannot be represented as json",
  kind `json`). Equality and ordering compare the underlying structures; see
  [findings](findings/seam.md) for formats this fails on.

## 5. Deferred values

- Options whose defaults come from the frame settings (audio sample rate,
  video width and height) are stored unevaluated and forced on first use, so
  that a script can set `settings.frame.*` after writing a format
  (issue #933, cited by code comments). Explicit values given in the script
  are stored already evaluated.
- Validation that needs such a value is deferred with it: MP3's sample rate
  check runs when the value is forced (by `string_of_format`, by instance
  creation, by the HLS helpers), not at evaluation.
- Channel counts are never deferred: the type needs them (§6). Defaults that
  would read the frame's channel count use a fixed 2 instead (code comments
  cite #933), except NDI, whose type reads the frame's channel count.

## 6. Typing

- A format expression has type `format(T)` where `T` is a frame type with no
  methods and the content fields of the stream the encoder takes (invariant
  parameter). Observed (`--check` type errors): `%mp3(mono)` :
  `format(audio=pcm(mono))`; `%avi` and `%ndi` :
  `format(audio=pcm(stereo), video=yuv420p)`; `%theora` :
  `format(video=yuv420p)`. The video field is the default video content
  format; the type does not depend on the format's width or height.
- The type is computed **from the term**, before evaluation and without
  building anything, by a per-name typer registered separately from the
  builder. A name with no typer is a type error "unsupported format:
  `<name>`" (`Runtime_error` kind `encoder`, error 14, at type checking: it
  fails `--check`).
- Typers, as registered:
  - plain PCM audio, channel count from the parameters (§6.1): `fdkaac`,
    `flac`, `mp3`, `mp3.abr`, `mp3.cbr`, `mp3.fxp`, `mp3.vbr`, `opus`, `shine`,
    `speex`, `vorbis`, `vorbis.abr`, `vorbis.cbr`, `wav`;
  - `avi`: PCM audio with channels from the parameters, plus video;
  - `theora`: video only;
  - `external`: PCM audio, plus video when `video=true`;
  - `ndi`: audio (frame channel count) and/or video per its sub-encoders;
    neither raises "Invalid %%ndi encoder parameter!";
  - `ogg`: audio channels from the first audio sub-encoder (0 when none),
    video when any `%theora` is present; video only when 0 channels and
    video; otherwise PCM audio with those channels;
  - `ffmpeg`: its own typer ([ffmpeg tree](../../optionals/ffmpeg/spec/encoder-format.md)).

### 6.1 Channel count from parameters (shared rule)

- First, any anonymous `pcm_s16` or `pcm_f32` (any case) is rejected: "`<word>`
  audio is only supported by the %ffmpeg encoder." (no position).
- Then the **first** parameter among these decides; default 2:
  anonymous `mono` → 1, anonymous `stereo` → 2 (any case);
  `stereo=true|false` → 2|1; `mono=true|false` → 1|2; `channels=<n>` → n.
- `stereo`, `mono` and `channels` must be literal constants; anything else
  is a type error "Invalid value `<term>` for <stereo|mono|channels> mode.
  Only static ..." at the value's position.
- The count must have a channel layout (1, 2, ... as supported by the audio
  layer); otherwise type checking aborts with an uncaught
  `Audio_layout.Unsupported` (error -1, no position).

### 6.2 Type of a built value

- A second, independent computation gives the type of an evaluated format
  value from its fields (channels, presence of video). It is used by the
  evaluation-time type check, which compares a value against the static type
  where a value crosses a typed boundary.
- The typer (§6) reads the first deciding parameter; the builder (§4) keeps
  the last. When they disagree, the script passes `--check` and fails at run
  time with a type error with no position. Observed:
  `%wav(stereo=true, mono=true)` prints as channels=1 and, given to
  `output.file`, fails "this value has type source(audio=pcm(stereo)) but it
  should be a subtype of source(audio=pcm(mono))".
- The typers and builders also accept different parameter sets (findings):
  the typer accepts `channels`, `stereo`, `mono` for every PCM name while some
  builders reject some of them.

## 7. Printing

- `string_of_format` renders the script form with every resolved option,
  forcing deferred values. Each kind's format file owns its rendering. Shapes:
  - `%wav(samplerate=44100,channels=2,samplesize=16,header=true)` (plus
    `,duration=<float>` when set);
  - `%mp3(stereo,bitrate=128,samplerate=44100,id3v2=none)`; ABR and VBR print
    `%mp3.abr(...)` / `%mp3.vbr(...)` with their constraints;
  - Ogg prints `%ogg(<audio>,<video>)` from its streams; an Ogg format with
    no audio prints `%ogg()` even when it has a video stream;
  - `%speex(...)` prints `mode=widebande` for wideband;
  - External prints an empty field (`,,`) when no restart condition is set.
- It is what `print(format)` and `string(format)` show.

## 8. Files

| Concern                                           | Where                                                                  |
| ------------------------------------------------- | ---------------------------------------------------------------------- |
| Lexing `%name`, `%ifencoder`                      | `src/lang/parser/lexer.ml`, `parser.mly`                               |
| `%ifencoder` decision                             | `src/lang/reducer/term_reducer.ml`                                     |
| Typing hook, typers, channel rule                 | `src/core/encoder/types/encoder_types.ml`                              |
| Evaluation hook, builder registry, generic errors | `src/core/encoder/lang_encoder.ml`                                     |
| Hook wiring                                       | `src/core/lang/hooks_implementations.ml`                               |
| Kinds, helpers, value type                        | `src/core/encoder/encoder.ml`                                          |
| Script helpers                                    | `src/core/builtins/builtins_sys.ml`                                    |
| Metadata export settings                          | `src/core/stream/metadata_settings.ml` (`settings.encoder.metadata.*`) |
