# Cross-cutting encoder tests (as-built)

The tests that protect the format language and the seam as a whole. Tests of
one encoder's bytes belong to its implementation file (§14); tests of a
caller to its caller file. Test names are traces.

## 1. Format typing: channel count is static and part of the type

- Traces: `tests/language/encoders.liq` (sections "wav mono/stereo in same
  list", "mp3/wav mismatch in same list", "non-constant mp3
  stereo|mono|channels", "non-constant ffmpeg channels|ac"),
  `tests/language/type_errors.liq` section "CONTENT KIND" and "FORMATS".
- Invariant: two formats with different channel counts have incompatible
  types; a list of them does not type. A channel count given by a
  non-literal expression is rejected before the script runs.
  `stereo`, `mono`, `stereo=false`, `mono=false` are accepted by every audio
  name that takes them (`%external`, `%flac`, `%opus`, `%shine`, `%vorbis`,
  `%mp3`).
- Binding: rejection (the test passes on any error); acceptance of the four
  spellings.
- Incidental: the error message and code; that the test uses an `eval` of a
  string to trigger type checking at run time.
- Not covered: a script mixing a typer-first and builder-last spelling
  (`%wav(stereo=true, mono=true)`); unsupported channel counts (`channels=3`).

## 2. Format typing: content kinds the encoder cannot take

- Traces: `type_errors.liq` "CONTENT KIND" (`%vorbis(stereo)` with a mono
  source, with a video source; the same source to stereo and mono outputs;
  `%wav` with video generators) and "TRACKS" (`%mp3` with an audio+video
  source rejected; with the video dropped accepted; HLS variants mixing
  audio-only and video formats rejected in either order).
- Invariant: a format's type fixes the fields of the source it encodes; a
  source with an extra or missing field, or another channel count, is a type
  error.
- Binding: which scripts type and which do not.
- Incidental: wording.

## 3. pcm_s16 / pcm_f32 only for `%ffmpeg`

- Trace: `tests/core/encoder_pcm_kind_test.ml` (OCaml unit test, calls the
  type checker on a format term directly).
- Invariant: an anonymous `pcm_s16` or `pcm_f32` in a non-FFmpeg format, at
  top level or inside `%ogg(...)`, is rejected at type checking with an
  error of kind `encoder`; `%mp3(mono)` types.
- Binding: the error kind `encoder` and that the rejection happens during
  type checking, without evaluation.
- Harness need: the type checker callable on a single term with the encoder
  typers registered.
- Related: `tests/regression/unified-pcm-types.liq` (a `pcm_s16` source into
  `%ffmpeg(%audio(codec="aac"))` types and runs; its pass is the first track
  event, so it proves only that the script starts).

## 4. Option validation at evaluation

- Trace: `encoders.liq` "invalid wav samplesize".
- Invariant: an invalid option value raises a catchable error of kind
  `encoder` when the format expression is evaluated.
- Binding: kind `encoder` and message "invalid sample size" (the test
  compares both).
- Gap: a missed raise is not detected (the `try` has no `test.fail()` after
  the expression).

## 5. Evaluation-time type check of format values

- Trace: `tests/regression/eval-check-demeth.liq` (a `%mp3(mono)` output on
  a rebuilt source starts).
- Invariant: the type computed from a built format value agrees with the
  static type for a plain case.
- Binding: the output starts (`on_start` passes the test).
- Incidental: the compressor in the source.

## 6. Syntax snapshots

- Traces: `tests/snapshots/cases/encoder.liq` with its expected output, and
  `tests/snapshots/cases_analysis/encoder.liq`, `ffmpeg_encoder.liq`.
- Invariant: `%mp3(bitrate = 128, stereo)` parses to an encoder term with
  a labelled and an anonymous parameter, prints back as
  `%mp3(bitrate=128, stereo)` and types as `format(audio=pcm(stereo))`; the
  analysis tool types a script using a variable as a labelled value.
- Binding: the parsed tree, printed term and type.
- Incidental: the hash line; the value section, which records
  "Failure: Encoders are not implemented!" because the snapshot harness runs
  the language without the encoder hooks.

## 7. `%ifencoder`

- No test exercises `%ifencoder` / `%ifnencoder` (checked by `git grep`
  over `tests/`).

## 8. Doubles a new harness needs

- A type checker callable on a term with the per-name typers installed.
- An evaluator with the builders and plug-ins installed and a way to
  register a plug-in that answers nothing (to test "unsupported format" and
  `%ifencoder` without a missing library).
- A recording fake instance (records the sequence of calls and their
  arguments, returns scripted bytes) to test callers against the seam.
- A clock-free way to create an instance and force its deferred values, to
  test creation-time failures.
