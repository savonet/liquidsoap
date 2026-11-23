# Findings: native codecs (`%mp3`, `%shine`, `%fdkaac`, `%flac`)

Kept out of the as-built text. Measurements: 66 150 stereo samples at
44.1 kHz (0.5 s silence + 1 s sine) through `output.file`, decoded with
FFmpeg 9.0.2 to raw samples; onset = first |x| > 0.02. Scripts and outputs:
`/private/tmp/claude-501/-Users-toots-sources-savonet-liquidsoap/7003a131-9f64-45cc-a903-0c65dccca755/scratchpad/spec-encoding/native-codecs/`.

## Defects

1. **`%fdkaac` never emits its delay tail at stop.** Checked (file decode,
   plus a C reproduction of the binding's FDK calls). `ocaml_fdkaac_flush`
   (`src/modules/synced/fdkaac/src/fdkaac_stubs.c:230-264`) leaves
   `outBuf.numBufs = 0`, so every flush call returns 0 bytes until EOF. Lost
   per stop: 2048 samples (LC), 7106 (HE-AAC v2, the default) plus the
   zero-padded remainder. Scenario: a 1.5 s file in default `%fdkaac` holds
   60 478 of 66 150 input samples; the last 0.13 s of every recording, and
   of every HLS run, is cut. Contradicts [draining.md §2](../../../source/spec/draining.md#2-the-rule).
2. **`%mp3` drops its last 1..F input samples at stop.** Checked.
   `liquidsoap_lame.ml:101` loops `while length > frame_size` (strict) and
   `stop` (`:117`) only flushes LAME; the Generator remainder is discarded.
   486 samples lost in the measurement; a remainder of exactly `F` is also
   lost. Contradicts draining.md §2 and §4 (partial final unit).
3. **`%shine` drops its remainder and its delay line at stop.** Checked.
   Same strict loop (`liquidsoap_shine.ml:34`); `shine_flush` only flushes
   bits. 1543 samples (35 ms) lost per stop in the measurement. Contradicts
   draining.md §2/§4.
4. **Resamplers are never drained** (`%shine`, `%fdkaac`, `%flac`). Checked
   for `%flac(samplerate=48000)`: 71 979 samples out of 72 000. Contradicts
   draining.md §2 (a resampler is a stage).
5. **`%flac` header is not in `header`.** Checked: a listener joining a
   shared `output.harbor(%flac)` after 3 s gets `ff f8 …` frames with no
   `fLaC`/STREAMINFO. The previous normative encoder.md §3 required `header`
   to be complete "the container's identification headers included".
   `flac_encoder.ml:45-47` returns `Strings.empty` by design (comment).
6. **`%flac` STREAMINFO is incomplete in every file.** Checked
   (`metaflac`): total samples 0, MD5 zero, frame sizes 0. The encoder
   passes no seek/tell (`flac_encoder.ml:26`) although the binding supports
   them (`flac.ml:220-236` uses them for its file API). Scenario:
   `output.file(%flac, "x.flac")` → ffprobe shows no duration; seeking and
   integrity checks are unavailable.
7. **`%flac` initialisation failure is silent.** Checked:
   `%flac(samplerate=1000000)` produces a 0-byte file, exit 0, no log.
   `flac_stubs.c:795` discards `FLAC__stream_encoder_init_stream`'s status;
   process/finish statuses are discarded too (`:851-865`).
8. **`%flac` MIME is `audio/flex`** (`encoder.ml`, `mime`). Read only. Sent
   as Content-Type by streaming outputs that use the format's MIME.
9. **`id3v2` with an empty initial metadata writes an empty tag; and almost
   every caller passes empty metadata.** Checked: `output.file` with
   `%mp3(id3v2=true)` and a titled source writes `49 44 33 03 00 00 00 00 00
00`; `%flac` gets 0 comments. `pipe_output.ml:61` `current_metadata` is
   never assigned since 2ba7ba739 (#2924); icecast, SRT and HLS pass
   `Export.empty`. Only the harbor dedicated encoder fills the tag
   (checked). Before #3064 the tag waited for the first non-empty metadata.
   Owned by the callers; listed here because it decides these encoders'
   bytes.
10. **`id3v2=n` is not range checked and `n=2` is malformed.** Checked for
    the version byte (`49 44 33 07 …` for `id3v2=7`); read only for the v2
    frame layout: version byte 2 with 4-byte ids and 32-bit sizes
    (`metadataID3v2.ml:540-567`) is not a valid ID3v2.2 tag.
11. **HE-AAC v2 channel check raises a bare `Failure`.** Checked: error 9,
    no position, not caught by `--check` (`lang_fdkaac.ml:104-106`).
12. **`stop` errors are swallowed by `output.file`.** Read only.
    `pipe_output.ml:91` `try enc.Encoder.stop () with _ -> Strings.empty`
    and `icecast2.ml:669`: a failing stop loses its bytes with no log.
    Owned by the callers.

## Asymmetries

1. Validation time differs per encoder for the same kind of option. `%mp3`
   and `%fdkaac` reject bad bit rate lists / modes at evaluation; `%shine`
   validates nothing until the first `encode` (`Shine.Invalid_configuration`,
   no message, checked); `%mp3` sample rates are checked at instance
   creation even when literal (checked: `%mp3(samplerate=12345)` fails in
   the streaming thread with the format position). `%flac` rejects only
   compression and bit depth; libFLAC rejects the rest silently (Defect 7).
2. Tail behaviour differs: LAME's flush drains LAME (only the Liquidsoap
   queue is lost); shine's flush drains nothing; FDK's flush is broken;
   libFLAC drains fully. Measured table in each file's §9.
3. The bit rate estimate exists for `%fdkaac` VBR (`bitrate × 1000`, the
   unused CBR field) but not for `%mp3.vbr` (`Not_found`).
4. Error reporting at creation: `%fdkaac` maps library rejections to
   `encoder` runtime errors with a position; `%shine`, `%flac` and LAME raise
   their binding exceptions; LAME also prints to stderr.
5. Silent clamping vs errors: LAME clamps (`samplerate=22050,bitrate=320`
   → 160 kbps, checked) and FDK clamps (`bitrate=1000` → ~132 kbps,
   checked); shine refuses the same kind of combination.
6. `%fdkaac` extension/MIME are `aac`/`audio/aac` for every transport,
   including LOAS, LATM, ADIF and raw.
7. ID3v2 at the start of `%mp3` but no in-band metadata anywhere, while
   Ogg restarts a link; neither `%shine` nor `%fdkaac` (ADTS) can carry the
   initial metadata at all.

## Gaps

1. No encoder delay or padding is signalled by any of the four (no LAME
   Info frame, no shine equivalent, no gapless metadata for ADTS, FLAC has
   none but needs none). Decoders keep LAME's 576 and shine's ≈528 samples
   (checked: onsets +1105 and +1056 including the 529-sample decoder
   delay), and AAC's 2048 / 7106 (checked).
2. `%fdkaac` raw and LATM-out-of-band streams are undecodable: the
   AudioSpecificConfig is never exposed (no `header`, no side channel).
   ADIF's header is only at byte 0. Nothing rejects these transports for
   streaming outputs. Checked: ffprobe misreads raw/ADIF/LATM files.
3. `%mp3` VBR/ABR files carry no TOC or frame count; duration is estimated
   from the first frame. Read only for the consequence on long files (To
   verify below).
4. NaN samples: LAME maps them to 0; shine, mm's `s16_clip` (fdkaac) and
   `sample_from_double` (flac) convert NaN to an integer, undefined in C.
   Read only.
5. Unchecked allocations in bindings: `lame_stubs.c:260-261` (`malloc` of
   input buffers), `fdkaac_stubs.c:120-123` (`free` instead of
   `aacEncClose`). Read only.
6. `LIQ_BUFLEN` changes `%fdkaac`'s chunking process-wide with no
   documentation or reason in the code. Read only.
7. `%mp3(channels=n)` passes the typer and fails in the builder; the
   language test that expects a non-literal `channels` to be rejected passes
   for that reason, not the one it names. Read only.
8. Native encoder handles are freed only by the GC finaliser, after `stop`;
   a stopped instance keeps its library state alive until collection. Read
   only.

## To verify

1. libFLAC behaviour when the seek callback returns UNSUPPORTED at finish
   (skips the STREAMINFO rewrite rather than failing): consistent with the
   observed file and the return status ignored, but stated from memory.
2. ID3v2 frame flags `0xC000`: in ID3v2.3 these bits mean "discard the frame
   if the tag / file is altered"; in ID3v2.4 the status byte bit 0x80 is
   unused. From memory of the ID3 specs.
3. FFmpeg's MP3 decoder delay of 529 samples and that it applies no skip
   without an Info frame (consistent with the measured +1105 = 576 + 529).
4. LAME's frame size of 576 for 8–12 kHz (MPEG-2.5) — measured only at
   22 050 Hz.
5. `mp4a.40.34` for MP3 and `mp4a.67` for the MPEG-2 HE profiles as HLS
   `CODECS` values; the code itself marks the MPEG-2 HE ones "TODO".
6. That players use the first frame's bit rate to estimate the duration of
   a VBR MP3 without a Xing header.
