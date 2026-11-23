# Formats and the `%name(...)` language

Part A. What a format value is, how a script writes one, how it is typed and
built from one parameter specification, when each part of it is validated,
how availability is decided, and the helpers every output reads from it.
Each writer's own options (names, defaults, ranges, effects, errors) are its
implementation file's §1 ([encoder.md §10](encoder.md#10-implementation-files));
this file owns what they share.

It relies on: the seam's vocabulary, declarations and checks
([encoder.md](encoder.md)); the fault classes
([faults.md](../../../spec/faults.md)); principle 13, errors found at
evaluation ([principles](../../../spec/principles.md)).

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
  - `%subname(...)`, a nested format expression (a sub-encoder).
- String literals used as labels or anonymous parameters are rendered when
  the script is parsed, interpolation included.
- Labelled values are ordinary expressions, evaluated in the script's
  environment when the format expression is evaluated.
- `%ifencoder %name ... [%else ...] %endif` keeps its first branch when
  `%name` is available (§8), and `%ifnencoder` when it is not.

## 2. Names and kinds

| Script names                                                                       | Kind                     | Implementation file                                                                                        |
| ---------------------------------------------------------------------------------- | ------------------------ | ---------------------------------------------------------------------------------------------------------- |
| `%wav`                                                                             | WAV                      | [wav.md](wav.md)                                                                                           |
| `%avi`                                                                             | AVI                      | [avi.md](avi.md)                                                                                           |
| `%external`                                                                        | External                 | [external.md](external.md)                                                                                 |
| `%mp3`, `%mp3.cbr`, `%mp3.abr`, `%mp3.vbr`                                         | MP3                      | [mp3.md](mp3.md)                                                                                           |
| `%shine`, `%mp3.fxp`                                                               | Shine                    | [shine.md](shine.md)                                                                                       |
| `%fdkaac`                                                                          | FDK-AAC                  | [fdkaac.md](fdkaac.md)                                                                                     |
| `%flac`                                                                            | FLAC                     | [flac.md](flac.md)                                                                                         |
| `%ogg(...)`                                                                        | Ogg                      | [ogg.md](ogg.md)                                                                                           |
| `%vorbis`, `%vorbis.cbr`, `%vorbis.abr`, `%opus`, `%speex`, `%theora` (deprecated) | Ogg with that one stream | [ogg.md](ogg.md) and the stream's file ([ogg-streams.md](ogg-streams.md))                                  |
| `%ffmpeg(...)`                                                                     | FFmpeg                   | [ffmpeg.md](ffmpeg.md); its language is [encoder-format.md](../../optionals/ffmpeg/spec/encoder-format.md) |
| `%ndi`                                                                             | NDI, not a writer        | [ndi.md](ndi.md)                                                                                           |

- `%ogg(...)` takes at most one audio sub-encoder (`%vorbis`, `%vorbis.cbr`,
  `%vorbis.abr`, `%opus`, `%speex`, `%flac`) and at most one `%theora`.
  Any other parameter, including a second audio or video sub-encoder, is
  rejected. `%flac` inside `%ogg` is an Ogg FLAC stream with the options of
  `%flac`; alone, it is the native FLAC kind.
- `%ndi` takes only the sub-encoders `%audio`, `%audio.none`, `%video`,
  `%video.none`, without parameters except the channel parameters of §4 on
  `%audio` (`%audio(channels=n)`). It is accepted by `output.ndi` only; any
  other output refuses it at construction
  ([encoder.md §1](encoder.md#1-registry)).
- A **deprecated** name or parameter is accepted, and evaluating it logs one
  warning at `important` that names it and its replacement:

  | Deprecated         | Effect                                                       | Replacement                          |
  | ------------------ | ------------------------------------------------------------ | ------------------------------------ |
  | `%theora`          | as documented ([ogg-theora.md](ogg-theora.md))               | `%ffmpeg` with a current video codec |
  | `%wav(duration=…)` | none on the bytes ([wav.md §1](wav.md#1-format-and-options)) | nothing: remove it                   |

## 3. The parameter specification

Each name has **one parameter specification**, owned by its implementation
file's §1. It lists every parameter the name accepts, and for each:

- its form: labelled, anonymous word, or sub-encoder;
- the type of its value, and its range or its allowed values;
- its default: a constant, or a deferred value (§6);
- whether it decides the format's type (a channel count, the presence of a
  track); such a parameter takes only a literal value;
- its effect on the configuration, and how it is printed (§10);
- the checks that involve it and the error each gives (§11).

The type (§4) and the value (§5) are both derived from this one
specification: they accept exactly the same parameters, and give each the
same meaning. When a parameter is given more than once, or several
parameters set the same property, the last one wins, in both readings.

**Explicit and default.** A format value records, for every property of
its configuration, whether the script set it explicitly (a parameter
written in the expression, a sub-encoder's own parameter included) or left
it to its default. A default is the writer's to adapt to its destination;
an explicit setting is never adapted, and is refused when it contradicts the
destination ([encoder.md §5.3](encoder.md#53-the-check)). Each
specification marks the properties a writer adapts (preambles, header
forms, container options that depend on seekability). For `%ffmpeg`, an
option the script does not write is a default even when the muxer has a
value for it (the mp3 muxer's ID3v2 version, its encoder-information
frame).

Every accepted parameter has its documented effect on the bytes, or on the
configuration's declarations ([encoder.md §5.2](encoder.md#52-what-a-configuration-declares)).
A parameter that has neither is not accepted, except a deprecated one (§2),
whose warning says it does nothing. Reason: an option that parses and does
nothing is a promise the output silently breaks.

`streamable=true` on `%ffmpeg` and on `%external` is the one parameter whose
effect is a declaration only: the configuration is declared streamable, on
the script's word ([encoder.md §5.3](encoder.md#53-the-check)). It is never
inferred, and never set by an output.

## 4. Typing

- A format expression has type `format(T)`, where `T` is a frame type with
  no methods and the content fields of the stream the format encodes
  (invariant). Examples: `%mp3(mono)` : `format(audio=pcm(mono))`; `%avi` :
  `format(audio=pcm(stereo), video=yuv420p)`; `%theora` :
  `format(video=yuv420p)`.
- `T` is computed from the parameter specification before evaluation,
  without building anything. Every value the expression builds has that
  type; no second computation of a value's type exists to disagree with
  it.
- **Channel count.** The specification of each audio format names the
  parameters that decide its channel count among: anonymous `mono` (1),
  anonymous `stereo` (2), `mono=true|false` (1|2), `stereo=true|false`
  (2|1), `channels=<n>`. The last one given decides. Their values are
  literals; a computed value is a type error at its position. Without any,
  the count is 2, unless the implementation file states another default
  with its reason. Channel counts are not deferred: the type needs them
  before any setting the script writes is evaluated.
- A channel count with no supported channel layout is a type error at the
  position of the value that set it.
- An anonymous `pcm_s16` or `pcm_f32`, in any case, is a type error that
  says this sample format is only offered by `%ffmpeg`.
- **Tracks.** Which tracks a format has is decided by type-deciding
  parameters only (a `%theora` sub-encoder, `%external`'s `video=true|false`,
  `%ndi`'s `%video` and `%video.none`). No parameter adds or removes a track
  in the value without doing so in the type.
- The video field has the default video content type; a format's width and
  height are not in its type.
- Two formats with different contents have no common type: a list of them,
  or a choice between them, is a type error. Reason: a format's type fixes a
  data layout, and a supertype would let an output take a source whose
  tracks the encoder drops.

## 5. Building

- At evaluation, labelled values are evaluated (sub-encoders recursively),
  then the specification folds the parameters in order over the name's
  defaults.
- The result is the format value: immutable, comparable (§10), printable
  (§10). An output never edits a format value it is given, and never builds
  a modified copy of it to steer a writer: what depends on the destination
  is the writer's choice ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles)).
  Reason: one format value commonly feeds several outputs.
- After building, the value is looked up in the registry
  ([encoder.md §1](encoder.md#1-registry)); a format no writer answers is an
  error at the expression's position (§8).

## 6. Deferred values

- A default read from a frame setting (the audio sample rate, the video
  size, the video frame rate) is deferred: the format holds the setting,
  not its value. A value the script writes explicitly is not deferred.
  Reason: a script may set `settings.frame.*` after writing a format, and
  the stream must declare the settings in force.
- Deferred values are resolved once the script has been evaluated, when the
  clocks start, before any writer is built. Reading a format value
  (printing, comparing, a helper) never resolves them and never fixes a
  setting.
- A check that needs a deferred value runs when it is resolved: an invalid
  value is a configuration error at start-up, before any output streams,
  naming the parameter, the setting it read and the format's position.

## 7. When a format is validated

Every check runs at the earliest phase at which its inputs exist. A check
that does not depend on settings, on the build's libraries or on the
destination fails at type checking when its values are literals, and at
evaluation otherwise.

| Check                                                                                                    | Phase                                                                                                                                                                                        | Reported by `--check` |
| -------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------- |
| unknown name; a component missing from the build (§8)                                                    | type checking                                                                                                                                                                                | yes                   |
| type-deciding parameters (§4)                                                                            | type checking                                                                                                                                                                                | yes                   |
| every other parameter whose value is a literal: known, of its type, in range, compatible with the others | type checking                                                                                                                                                                                | yes                   |
| parameters whose values are computed                                                                     | evaluation of the format expression                                                                                                                                                          | no                    |
| what the build's library accepts (a profile or mode a library build lacks)                               | evaluation, by asking the library without side effect where it offers such a question; otherwise the writer's creation, as a hard configuration error ([encoder.md §8](encoder.md#8-faults)) | no                    |
| the format against the output's declarations ([encoder.md §5.3](encoder.md#53-the-check))                | construction of the output, at evaluation                                                                                                                                                    | no                    |
| deferred values (§6)                                                                                     | start-up, before any writer is built                                                                                                                                                         | no                    |
| capabilities confirmed at open ([encoder.md §5.4](encoder.md#54-at-open))                                | the sink's open, before the writer is built                                                                                                                                                  | no                    |
| anything that depends on the data or on a peer                                                           | while streaming: a fault ([encoder.md §8](encoder.md#8-faults))                                                                                                                              | no                    |

- No check is silent: a configuration a library or a process refuses is
  reported as an error, never as an empty or missing output.
- Degraded but valid output is not an error
  ([encoder.md §5.3](encoder.md#53-the-check)).

## 8. Availability and `%ifencoder`

- A name is **available** when every component its format needs with its
  defaults is in the build: the library of its codec, the Ogg muxer and the
  stream codec of an Ogg name (`%vorbis` needs the Ogg muxer and the Vorbis
  library), FFmpeg for `%ffmpeg`. Components a format names in its
  parameters (an FFmpeg codec or muxer) are available when the build has
  them; the FFmpeg tree states when that is checked.
- A format that uses a component the build lacks is a configuration error
  that names the missing component and the format's position: at type
  checking for a component its name implies, at the phase of the parameter
  (§7) for a component a parameter names.
- `%ifencoder %name` decides from availability alone, before type checking:
  it builds nothing and validates nothing. A name the language does not know
  counts as not available. No other failure is read as "not available".

## 9. Helpers

Functions of the format value alone, read without a writer and without
resolving deferred values. An unknown answer is "none", never a failure.

| Helper         | Answers                                                                                                                             |
| -------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| `extension`    | The file extension (table below); none for `%external`, `%ndi`, and an `%ffmpeg` format whose muxer has none.                       |
| `mime`         | The MIME type (table below); none for `%external` and `%ndi`.                                                                       |
| `bitrate`      | The nominal bits per second when the configuration fixes it, else none. Each implementation file's §10 states which options fix it. |
| `codecs`       | The RFC 6381 codecs string of every stream, or none; never a partial list. Values in each implementation file's §10.                |
| `video_size`   | `(width, height)` when the format has video, else none.                                                                             |
| `content_type` | The type of §4.                                                                                                                     |
| declarations   | [encoder.md §5.2](encoder.md#52-what-a-configuration-declares).                                                                     |

| Kind    | `extension`                                                                            | `mime`            |
| ------- | -------------------------------------------------------------------------------------- | ----------------- |
| WAV     | `wav`                                                                                  | `audio/wav`       |
| AVI     | `avi`                                                                                  | `video/avi`       |
| MP3     | `mp3`                                                                                  | `audio/mpeg`      |
| Shine   | `mp3`                                                                                  | `audio/mpeg`      |
| FDK-AAC | `aac`; `loas` for the LOAS transport                                                   | `audio/aac`       |
| FLAC    | `flac`                                                                                 | `audio/flac`      |
| Ogg     | `opus` when its only stream is Opus, else `ogg`                                        | `application/ogg` |
| FFmpeg  | from its muxer, per [encoder-format.md](../../optionals/ffmpeg/spec/encoder-format.md) | the same          |

- `application/ogg` is kept for every Ogg format: servers pick their Ogg
  parser from it.
- The script functions `encoder.content_type(f)` and `encoder.extension(f)`
  return `mime` and `extension`, and `""` for none.

## 10. Equality, printing, serialisation

- `==` and the ordering on format values compare their parameter sets after
  defaults, and whether each adaptable property was set explicitly; a
  deferred default is equal to the same deferred default. They
  never fail, and their answer does not depend on whether deferred values
  were resolved.
- Printing gives the script form with every option of the configuration,
  except the defaults a writer adapts to its destination, which are left
  out so that they stay defaults; an unresolved deferred value prints the
  setting's value at the time of printing, without resolving it. Evaluating
  a printed format, under the same settings, gives a value that builds the
  same configuration, with the same properties explicit.
- A format value has no JSON form: converting one is an error of kind
  `json`.

## 11. Errors

- Every rejection of a format is a runtime error of kind `encoder` (or a
  type error at type checking) that names the parameter, the name of the
  format and, for a value out of range, the value and what is accepted.
- It carries the position of the parameter's value, or of the format
  expression when the parameter has no value (an anonymous word, a
  sub-encoder). No rejection is without a position, and none escapes as an
  internal exception.
- A component missing from the build is its own error, naming the component
  (§8).

## 12. Conformance

- For every name and a generated corpus of parameter lists, the type
  `--check` reports equals the content of the value built, and a parameter
  rejected at one phase is rejected at the same phase whichever way it is
  combined.
- `%wav(stereo=true, mono=true)` types and builds mono.
- `print(%ogg())` and every invalid channel count give an error with a
  position, never an internal exception.
- `%mp3(samplerate=1234)` fails `--check`; with the rate computed, it fails
  at evaluation; with the default rate and a frame setting the codec cannot
  take, it fails at start-up before any output streams.
- With a build that lacks a stream codec, `%ifencoder` of its name takes the
  else branch, and a format using it fails type checking naming the
  library.
- `%mp3 == %mp3` is true and never fails; printing then evaluating every
  format of a corpus gives a value that builds the same configuration.
- Setting the frame's sample rate after writing `%wav`, then starting: the
  WAV header declares the setting's rate.
- `encoder.content_type(%flac)` is `audio/flac`.
- `%mp3` given to a live output writes no ID3v2 tag and is accepted;
  `%mp3(id3v2=true)` given to the same output is refused at evaluation,
  naming the parameter. `print(%mp3)` shows no `id3v2` option, and
  `print(%mp3(id3v2=true))` shows it.
- A parameter documented with an effect, set to two values in two runs,
  gives bytes that differ as documented.
