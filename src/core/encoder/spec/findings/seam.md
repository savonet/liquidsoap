# Findings: encoder seam and format language

Kept out of the as-built text. Each: status (**checked**: what was run, with
the binary built from 02fa52985; **read only**), traces, scenario.

## Defects

### D1. Typer and builder disagree on the channel count (first vs last wins) — checked

- Traces: `src/core/encoder/types/encoder_types.ml:65-99` (`List.find_map`:
  first deciding parameter), `src/core/encoder/lang_wav.ml:37-60` and every
  builder (`List.fold_left`: last parameter wins).
- Scenario: `output.file(%wav(stereo=true, mono=true), path, sine())`.
  `--check` passes (exit 0). At run: `print` shows
  `%wav(...,channels=1,...)`, then "Unknown position: Error 5: this value has
  type source(audio=pcm(stereo), _) but it should be a subtype of
  source(audio=pcm(mono), _)". Same with `%vorbis(channels=1, stereo)`
  (typed mono, built stereo).
- Effect: a statically accepted script fails at run time with no position.

### D2. Typer accepts parameters the builder rejects, and the reverse — checked

- `%mp3(channels=1)`: the typer reads `channels` (shared rule,
  `encoder_types.ml:88`), the MP3 builder has no `channels`
  (`lang_mp3.ml:44-86`): `--check` passes; evaluation fails "unknown
  parameter name (channels) or invalid parameter value (1)".
- `%avi(mono)`: typed mono; builder rejects "Unknown encoder parameter:
  mono" (`lang_avi.ml:38-49` knows only `channels`, `samplerate`, `width`,
  `height`).
- `%external(process="cat", video=false)`: the typer accepts only a literal
  `true` (`encoder_types.ml:156-169`: `Some {term = `Bool true}`then any
other`Some`raises) so`video=false`is a type error "Invalid value false
for value mode. Only`true`or`false is allowed." (the message says
  `false` is allowed, and has an unbalanced backquote and "value mode" for
  "video mode").

### D3. `%ogg()` and `%ogg(<non-Ogg sub-encoder>)` crash the type checker — checked

- Traces: `encoder_types.ml:191-212` (no audio sub-encoder → 0 channels, no
  video → `audio_type 0`), `Audio_layout.layout_of_channels` raises.
- Scenario: `print(%ogg())` or `print(%ogg(%mp3))` (also `%wav(channels=3)`,
  `%mp3(channels=3)`): "Unknown position: Error -1: Exception raised:
  Audio_layout.Unsupported" with an OCaml backtrace, at `--check`.
- Expected by the rest of the code: an `encoder` error with a position.

### D4. Structural equality on formats raises — checked

- Trace: `src/core/encoder/lang_encoder.ml:37` (`compare = Stdlib.compare`)
  over records holding `Lazy.Mutexed.t` closures.
- Scenario: `print(%mp3 == %mp3)` → "Error 14: Uncaught runtime error: type:
  invalid, message: "compare: functional value"". `%wav == %wav` → `true`
  (shared default object, physically equal);
  `%mp3(bitrate=128) == %mp3(bitrate=192)` → `false` (differs before
  reaching a closure). Result depends on field order and on whether values
  were forced.

### D5. Ogg format with video only prints as `%ogg()` — checked

- Trace: `src/core/encoder/ogg_format.ml` `to_string`: `match audio with Some
e -> ... :: l | None -> []` drops the video part.
- Scenario: `print(%theora)` and `print(%ogg(%theora))` print `%ogg()`.

### D6. Error 12 ("Unsupported encoder ... missing an optional dependency") is unreachable — read only + checked

- Traces: `src/lang/runtime/typechecking.ml:205-211` maps `Not_found` to
  `Unsupported_encoder`; `encoder_types.ml:115-117` never raises it (unknown
  name → `Runtime_error` "unsupported format: <name>"); every name has a
  typer whether or not its library is compiled in.
- Checked: `print(%foo)` → "Error 14 ... unsupported format: foo" at
  `--check`. The Windows hint in `runtime.ml:213-217` is never shown. A
  format whose library is missing (e.g. `%mp3` without LAME) types and fails
  at evaluation with the bare "unsupported format" (read only: every library
  is present in this build).
- `Lang_error.Encoder_error` (`lang_error.ml:25`, handled as error 8 in
  `runtime.ml:198`) is never raised anywhere.

### D7. Literal `%%` in error messages — checked

- Traces: `encoder_types.ml:189`, `lang_ndi.ml:37,42`: `raise_error` takes a
  plain string, so `"%%ndi"` prints both percent signs.
- Scenario: `print(%ndi(%audio.none, %video.none))` → message "Invalid %%ndi
  encoder parameter!".

### D8. MIME type of native FLAC is `audio/flex` — checked

- Trace: `src/core/encoder/encoder.ml:135`.
- Scenario: `encoder.content_type(%flac)` → `audio/flex`. Used by
  `output.icecast`/`output.harbor` Content-Type (through the callers' use of
  `mime`; see caller files) and by the HLS content type helper.

### D9. Speex wideband prints as `widebande` — checked

- Trace: `src/core/encoder/speex_format.ml` `string_of_mode`.
- Scenario: `print(%speex(mode="wideband"))` → `...mode=widebande...`; the
  printed form does not parse back to the same format.

### D10. Ogg sub-encoder missing is not detected at evaluation — read only

- Traces: `lang_encoder.ml:65-70` checks only `get_factory`, which the Ogg
  plug-in answers for any Ogg format (`ogg_encoder.ml:153-156`); the stream
  encoder lookup is at instance creation (`ogg_encoder.ml:79-95`).
- Scenario: a build with libogg but without libvorbis: `%vorbis` types,
  evaluates and passes `%ifencoder %vorbis`; the output fails when it starts
  ("Could not find any vorbis encoder."). With Theora missing,
  `Option.get` raises `Invalid_argument "option is None"` instead
  (`ogg_encoder.ml:89`).
- Contradicts the previous normative rule "a format no plug-in answers is a
  configuration error at evaluation".

## Contradictions with the previous normative `encoder.md`

### C1. `header` incomplete for native FLAC and External — read only

- Rule: "It MUST be complete for every format the encoder produces, the
  container's identification headers included."
- Native FLAC: `header` returns empty (`optionals/flac/flac_encoder.ml:66-68`,
  comment "Flac encoder do not support header for now. It will probably never
  do..") while its stream starts with `fLaC` + STREAMINFO (+ comments): a
  harbor or SRT client joining after the start receives frames with no
  stream header.
- External: `header` empty (`external_encoder.ml:164`) although the encoder
  itself writes a WAV or AVI header to the process, whose output may start
  with its own header.

### C2. `encode_metadata` silently ignored by most encoders — read only

- Rule: "`encode_metadata(metadata)`: in-stream metadata, effective in later
  output."
- WAV, AVI, MP3 (LAME), Shine, FDK-AAC, native FLAC: `fun _ -> ()`; External
  without `restart_on_metadata`: nothing; FFmpeg with a non-chaining muxer
  after its header: `debug` log only. Nothing tells the caller that the
  metadata was not carried.

### C3. "An instance is stopped exactly once" not enforced — read only

- `protect` does not track state (`encoder.ml:207-240`). FFmpeg's `stop` is
  idempotent; LAME's calls `Lame.encode_flush` again; Ogg's ends streams
  again; External's waits on a condition that may never be signalled a second
  time (`external_encoder.ml:155-160`: `Condition.wait` with no predicate
  loop).

### C4. Zero-length pieces — read only

- Rule: "Callers hand an instance zero-length pieces as well as full ones."
- The generic encoded output skips them (`src/core/outputs/output.ml:339-356`:
  `if start < stop then f start stop`). Whether every encoder accepts them is
  thus untested by the main path.

### C5. Serialisation bypassed by the FFmpeg test producer — read only

- `optionals/ffmpeg/test/copy_producer.ml:438-440` uses an unprotected
  instance. Single-threaded, so harmless; the rule says "whoever makes them".

## Asymmetries

- A1. `header()` timing: WAV and AVI answer empty before the first `encode`
  and the header after it (their first `encode` embeds it); Ogg and FFmpeg
  answer their header pages / captured header independent of what was
  returned. A caller that calls `header()` before any `encode` gets nothing
  from WAV/AVI and the header from Ogg (`wav_encoder.ml:76-79`,
  `avi_encoder.ml:113-116`).
- A2. `bitrate()` is known only to FFmpeg; MP3 CBR, Shine, FDK-AAC know their
  rate at format level (`Encoder.bitrate`) but their instances answer none.
  `output.icecast` therefore reads two different sources for the same
  information depending on the kind (`icecast2.ml:110-130`).
- A3. Format-level `bitrate` raises `Invalid_argument` (not `Not_found`) for
  an ABR MP3 with no mean bitrate (`mp3_format.ml:84-88`, `Option.get`);
  callers catch only `Not_found`. Unreachable today: the ABR builder defaults
  the mean to 128 (`lang_mp3.ml:229`).
- A4. FDK-AAC's format-level bitrate is `bitrate×1000` even in VBR mode,
  where `bitrate` is not used by the encoder.
- A5. Initial metadata: LAME, native FLAC, Ogg, FFmpeg use it; WAV, AVI,
  Shine, FDK-AAC, External ignore it. The HLS packed-audio factory passes
  empty metadata, so an `%mp3(id3v2=true)` HLS variant starts its first
  segment with an ID3v2 tag of no frames before the packed-audio ID3 tag
  (`hls_packed.ml:46-49`, `liquidsoap_lame.ml:89-97`; read only, the HLS
  tree owns the segment bytes).
- A6. FFmpeg (and External) reach their destination around the seam; every
  other encoder returns bytes. For FFmpeg the destination travels inside the
  format value, rewritten by `output.file` (`pipe_output.ml:458-475`) and
  `output.url` (`url_output.ml:76-87`); the encoder then opens, appends to
  (`instance.ml:131-136`, `O_APPEND`) and closes the file itself.

## Gaps

- G1. Validation is split across three times with no rule: static typing
  (channels, pcm kind), evaluation (most options), instance creation
  (deferred values such as MP3 sample rate, Ogg stream availability).
  Checked: `%mp3(samplerate=1234)` passes `--check` and evaluation
  (`print("evaluated")` runs) and fails when `output.file` starts: "Clock
  failed in source output.file: Lang.Runtime_error { kind: "encoder", msg:
  "invalid samplerate value", ... }" (level 1).
- G2. Typer and builder are two independent parsers of the same parameters
  (`encoder_types.ml` vs `lang_*.ml`); nothing checks they agree (D1, D2).
- G3. Errors without a position: unknown anonymous parameter, unexpected
  sub-encoder (`lang_encoder.ml:49,55`), the NDI checks, the typer crash (D3).
- G4. `%ifencoder` swallows every exception (`term_reducer.ml:63-69`) and has
  no test; it relies on plugs being complete when the reducer runs
  (`Plug.Incomplete` would read as "absent"). Checked: `%ifencoder %mp3`
  takes the first branch and `%ifencoder %foo` the else branch.
- G5. `%ogg(...)` ignores extra audio streams and any non-Ogg parameter
  silently. Checked: `print(%ogg(%vorbis, %opus))` → `%ogg(%vorbis(...))`.
- G6. The seam has no way to say "this instance writes to the destination
  itself"; callers learn it from the format kind.

## To verify

- V1. OCaml 5 `Mutex.lock` on a mutex already held by the calling thread
  raises `Sys_error` (error-checking mutex) rather than deadlocking; relevant
  to an `encode_into` consumer calling back into its instance. From memory.
- V2. `mp4a.40.34` as the RFC 6381 codecs string for MP3 in HLS (code
  comment lists `mp4a.69` and `mp3` as alternatives); `mp4a.67` for every
  MPEG-2 AAC profile ("TODO: check this" in code). From memory: `mp4a.40.34`
  is MPEG-1/2 Layer 3 as an MPEG-4 audio object type; `mp4a.6B`/`mp4a.69` are
  the MPEG-1/MPEG-2 audio object type indications.
