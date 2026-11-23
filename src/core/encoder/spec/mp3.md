# `%mp3` encoder via LAME (as built)

What the LAME-backed MP3 encoder does today: a raw MPEG-1/2 Layer III
elementary stream, optionally preceded by one ID3v2 tag built from the
initial metadata. No Xing/LAME Info frame is written. This file owns the
`%mp3`, `%mp3.cbr`, `%mp3.abr` and `%mp3.vbr` options, the LAME calls, the
bytes, and the encoder's assumptions about their destination. The shared
seam (registry, factories, per-instance lock, call contract) is
[encoder.md](encoder.md); the `%name(...)` language, deferred values and
typing are [formats.md](formats.md); draining is
[draining.md](../../source/spec/draining.md). Measurements below were made
against LAME 4.0 (Homebrew) and decoded with FFmpeg 9.0.2.

## 1. Format and options

Four names build the same format kind. `%mp3` and `%mp3.cbr` start from the
CBR defaults; `%mp3.abr` from the ABR defaults; `%mp3.vbr` from the VBR
defaults. Parameters are folded in order, last one wins
([formats.md §4](formats.md#4-evaluation-building-the-format-value)).

Shared parameters (all four names):

| Parameter                     | Value                                     | Default                            | Effect / validation                                                                                                                                                                        |
| ----------------------------- | ----------------------------------------- | ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `stereo` / `mono` (anonymous) | —                                         |                                    | stereo true / false                                                                                                                                                                        |
| `stereo=b`, `mono=b`          | static bool                               | stereo                             | 2 or 1 channels                                                                                                                                                                            |
| `stereo_mode=s`               | `"default"`, `"stereo"`, `"joint_stereo"` | `joint_stereo`                     | other strings: "invalid stereo mode" at the value                                                                                                                                          |
| `internal_quality=q`          | int 0..9                                  | 2                                  | LAME algorithm quality; out of range: "internal quality must be a value between 0 and 9"                                                                                                   |
| `samplerate=n`                | int                                       | `frame.audio.samplerate`, deferred | output rate; must be one of 8000, 11025, 12000, 16000, 22050, 24000, 32000, 44100, 48000, else "invalid samplerate value" — checked only when the value is forced, also for a literal (§2) |
| `id3v2=true`                  | bool                                      | none                               | ID3v2 version 3                                                                                                                                                                            |
| `id3v2=false`, `id3v2="none"` |                                           |                                    | no tag                                                                                                                                                                                     |
| `id3v2=n`                     | int                                       |                                    | ID3v2 version byte `n`, not range checked (§10.2)                                                                                                                                          |

`channels=n` is accepted by the typer but rejected by the builder ("unknown
parameter name (channels) ..."); see [formats.md §6.2](formats.md#62-type-of-a-built-value).

Bit rate parameters. Every bit rate value must be one of 8, 16, 24, 32, 40,
48, 56, 64, 80, 96, 112, 128, 144, 160, 192, 224, 256, 320 (kbps), else
"invalid bitrate value" at the value. The list mixes the MPEG-1 and MPEG-2
Layer III tables; it is not checked against the chosen sample rate (§2).

| Name               | Parameters                                                                                                           | Defaults                                |
| ------------------ | -------------------------------------------------------------------------------------------------------------------- | --------------------------------------- |
| `%mp3`, `%mp3.cbr` | `bitrate`                                                                                                            | 128                                     |
| `%mp3.abr`         | `bitrate` (mean), `min_bitrate`, `max_bitrate`, `hard_min=b`                                                         | mean 128, hard_min false, min/max unset |
| `%mp3.vbr`         | `quality=q` (0..9, else "quality should be in [0..9]"), `bitrate` (mean), `min_bitrate`, `max_bitrate`, `hard_min=b` | quality 4, everything else unset        |

`quality` on `%mp3.abr` is rejected as an unknown parameter. No consistency
check between min, mean and max is made: `%mp3.vbr(min_bitrate=320,
max_bitrate=8)` is accepted and produces 32 kbps frames only (checked).

Type: `format(audio=pcm(stereo))` or `pcm(mono)` per the channel count
([formats.md §6](formats.md#6-typing)).

Format helpers:

- extension `mp3`, MIME `audio/mpeg`;
- RFC 6381 codec string `mp4a.40.34` (a code comment notes `mp4a.69` and
  `mp3` are also seen);
- bit rate estimate (bits per second): CBR `bitrate × 1000`; ABR `mean ×
1000`; VBR: none (`Not_found`);
- printed as `%mp3(<stereo|mono>,bitrate=N,samplerate=N,id3v2=<none|N>)`, or
  `%mp3.abr(...)` / `%mp3.vbr(...)` with `quality=`, `bitrate=`,
  `min_bitrate=`, `max_bitrate=`, `hard_min=` for the fields that are set.
  `stereo_mode` and `internal_quality` are not printed.

## 2. Factory and instance creation

The factory ignores the output name and the `pos`; it uses the initial
metadata only for the ID3v2 tag. Creation does, in order:

1. Create a LAME context. Input rate = the frame's audio rate (forced at
   creation). Input channels = 2 or 1.
2. Algorithm quality = `internal_quality`.
3. Channel mode: mono when one channel; otherwise per `stereo_mode`
   (`default` leaves LAME's choice, `stereo` = plain stereo, `joint_stereo`).
4. Bit rate control:
   - CBR: set the constant bit rate in kbps. LAME's VBR mode is left at its
     default (off).
   - ABR: VBR mode = ABR; then, for each field that is set, in this order:
     hard_min, VBR quality, mean, min, max.
   - VBR: VBR mode = `vbr_mtrh` (LAME's default VBR algorithm); then the same
     sequence.
5. Output rate = `samplerate` (forced now; an invalid deferred or literal
   sample rate raises the `encoder` runtime error here, at the format's
   position, from the streaming thread).
6. Disable the VBR info tag (Xing/LAME Info frame).
7. Initialise LAME parameters. Failure raises the binding's
   `Init_params_failed` and prints `init_params error: <n>` on stderr.
8. Read LAME's frame size `F` (samples per MPEG frame at the output rate:
   1152 for 32–48 kHz, 576 for 16–24 kHz and 8–12 kHz).
9. If `id3v2` is set, render one ID3v2 tag from the initial metadata
   (§10.2) and put it in the pending-output buffer: it is returned by the
   first `encode`.

LAME corrects impossible settings itself, silently: `%mp3(samplerate=22050,
bitrate=320)` produces 160 kbps MPEG-2 frames (checked). No error and no log.

## 3. `encode`

- Input: the audio field of the piece, whatever its length (callers skip
  zero-length pieces; a zero-length piece adds nothing). The data is
  floating point in [−1, 1]; LAME resamples internally from the frame rate to
  the output rate, so no resampler runs in Liquidsoap.
- The audio is appended to a sample queue. While the queue holds **more
  than** `F` samples (strictly), `F` samples are taken and given to LAME
  (left, right; a mono stream passes its one channel as both). `F` is the
  frame size at the output rate, used as a count of input samples; it is a
  chunk size, not an MPEG frame boundary when LAME resamples.
- Samples are clipped to [−1, 1] (NaN → 0) and scaled by 32768 before LAME.
- Every byte LAME returns is appended to the pending-output buffer; the call
  returns and empties that buffer. LAME returns whole MPEG frames once its
  look-ahead is filled, so the first calls return nothing (or only the ID3v2
  tag) and output lags input.
- After any call, between 1 and `F` input samples stay queued in
  Liquidsoap (a queue of exactly `F` is not encoded), plus LAME's own
  look-ahead.

## 4. `encode_into`

Nothing. Not provided.

## 5. `encode_metadata`

Nothing: metadata is ignored. In-stream titles are the caller's (ICY,
[harbor](../../outputs/spec/harbor/icy.md)).

## 6. `header`

Always empty. The ID3v2 tag, when configured, is not a header: it is in the
bytes of the first `encode`.

## 7. `restart_offset`

Always none.

## 8. `bitrate`

Always none. The format's estimate (§1) is what callers announce.

## 9. `stop`

- Calls LAME's flush once and returns its bytes. LAME pads its own remaining
  samples and look-ahead with silence and emits the frames that hold them.
- The samples still queued in Liquidsoap (1 to `F`) are **not** given to
  LAME and are lost.
- Any ID3v2 tag not yet returned (no `encode` call happened) is lost too:
  `stop` returns only the flush bytes.
- No Xing/Info frame, no ID3v1 tag, no seek-back. The encoder delay and the
  padding are not signalled anywhere in the stream.
- Measured: 66 150 stereo samples at 44.1 kHz in (1.5 s: 0.5 s silence then a
  sine) = 57 × 1152 + 486. The decode holds 58 frames (66 816 samples); the
  sine starts 1105 samples late (LAME's 576-sample encoder delay plus the
  decoder's 529) and its last 486 input samples are missing. At
  `samplerate=22050` (F = 576): 59 frames, the same 1105-sample offset at the
  output rate, 486 input samples missing. ABR and VBR: same frame count and
  offset.

The seam's draining rule is [draining.md](../../source/spec/draining.md);
how this encoder departs from it is in the findings.

## 10. Bytes produced

### 10.1 Layout

```
[ID3v2 tag]                          only when id3v2 is set; first encode
MPEG audio frame × n                 MPEG-1 (32–48 kHz) or MPEG-2/2.5 Layer III
```

- No Xing/LAME Info frame, no ID3v1, no APE tag, no trailer.
- Each frame: 4-byte header `FF FB|F3|E3 ...` (sync, version, Layer III, no
  CRC), bit rate index, sample rate index, padding bit, channel mode, mode
  extension, copyright 0, original 1, emphasis 0. LAME fills unused bits of
  early frames with its version string and `0x55` bytes.
- CBR: every frame has the configured bit rate (padding bit toggles to keep
  the average). ABR/VBR: per-frame bit rate varies; nothing records the
  average or the frame count, so a reader's duration estimate comes from
  the first frame's rate.
- The MPEG bit reservoir is used: a frame's main data may begin in earlier
  frames.

Worked example (`%mp3`, stereo, 44.1 kHz, no ID3v2), first bytes:

```
00000000: fffb 9064 000f f000 0069 0000 0008 0000  ...d.....i......
00000010: 0d20 0000 0100 0001 a400 0000 2000 0034  . .......... ..4
00000020: 8000 0004 4c41 4d45 342e 3055 5555 5555  ....LAME4.0UUUUU
```

`FF FB 90 64`: MPEG-1 Layer III, no CRC, 128 kbps (index 9), 44.1 kHz,
no padding, joint stereo, mode extension MS on, original. 1.5 s gives 58
frames, 24 241 bytes, all at 128 kbps (checked by parsing every header).

### 10.2 ID3v2 tag

Built once, from the initial metadata given to the factory (after the
seam's export filter, [encoder.md](encoder.md)):

```
"ID3" | version (1 byte = n) | 0x00 | flags 0x00 | size (4 bytes, 7 bits each)
frame × k
```

- No extended header, no padding, no footer. The size counts the frames.
- One frame per metadata entry whose key maps to an ID3 frame:
  `comment`→COMM, `album`→TALB, `bpm`→TBPM, `composer`→TCOM,
  `content`→TCON, `copyright`→TCOP, `date`→TDAT, `encoder`→TENC,
  `title`→TIT2, `language`→TLAN, `length`→TLEN, `performer`→TOPE,
  `artist`→TPE1, `band`→TPE2, `publisher`→TPUB, `tracknumber`→TRCK,
  `year`→TYER, `url`→WXXX, then any key that is itself a frame id. Other
  keys are dropped. Frames come in the reverse order of the metadata list.
- Frame: 4-byte id; size as a 32-bit big-endian integer when `n < 4`, as a
  4×7-bit synchsafe integer when `n ≥ 4`; 2 flag bytes: `0xC000` (bits for
  "tag alter" and "file alter") for most frames, `0x8000` for AENC, ETCO,
  EQUA, MLLT, POSS, SYLT, SYTC, RVAD, TENC, TLEN, TSIZ; then the data.
- Text data: encoding byte then text. `n < 4`: encoding 1, UTF-16 with BOM;
  `n ≥ 4`: encoding 3, UTF-8. Binary frames carry the value as is.
- An empty metadata gives the 10-byte tag `49 44 33 nn 00 00 00 00 00 00`.
- `id3v2=2` writes version byte 2 with the 2.3 frame layout above (4-byte
  ids, 32-bit sizes); `id3v2=7` writes version byte 7 (checked, empty tag).

Worked example (harbor dedicated encoder, metadata title "Hé", artist "A";
version 3, then version 4):

```
4944 3303 0000 0000 0020 5449 5432 0000 0007 c000 01ff fe48 00e9 00
5450 4531 0000 0005 c000 01ff fe41 00 | fffb 9064 ...

4944 3304 0000 0000 001a 5449 5432 0000 0004 c000 0348 c3a9
5450 4531 0000 0002 c000 0341 | fffb 9064 ...
```

What reaches the tag in practice depends on the caller: `output.file`,
`output.icecast`, `output.srt`, the shared harbor encoder and HLS pass an
empty initial metadata, so the tag is the empty 10-byte tag; only the
harbor dedicated encoder passes the last metadata (§13, findings).

## 11. Concurrency and failures

- Calls are serialised by the seam's per-instance lock. The streaming thread
  of the output's clock calls `encode` and `stop`; LAME runs with the
  OCaml runtime lock released.
- Failures:
  - creation: invalid sample rate (`encoder` runtime error at the format
    position); `Init_params_failed` (plus a stderr line);
  - `encode`/`stop`: LAME error codes become `Init_params_not_called` (−3),
    `Psychoacoustic_problem` (−4) or `Unknown_error n`. Nothing is logged by
    the encoder; the caller sees the exception.
  - Out of memory for the 147 456-byte output buffer raises `Out_of_memory`.
- The LAME context is released by the garbage collector (finaliser), not by
  `stop`.

## 12. Segmenting (HLS) face

Driven through the packed-audio adapter
([encoder-packed-audio.md](../../outputs/hls/spec/encoder-packed-audio.md)),
one instance per variant for the whole run, created with empty metadata.
What this encoder adds there:

- split points fall at chunk starts, but the bytes `encode` returns lag the
  input by LAME's look-ahead and the queue (up to `F` + look-ahead
  samples), and frames use the bit reservoir across the split;
- with `id3v2` set, the empty 10-byte ID3v2 tag is the first thing in the
  first segment's media bytes, after the adapter's timestamp tag (read only);
- VBR gives no bit rate estimate.

## 13. Where the bytes go

- The encoder never does I/O. Every byte is returned by `encode` or `stop`.
- It assumes a **single, unseekable stream read from byte 0**: the Info
  frame is disabled precisely so nothing needs rewriting, and the ID3v2 tag
  is emitted once, in the first `encode` bytes, not as a header.
- A late joiner (harbor shared encoder) gets frames from its join point,
  without the ID3v2 tag (checked: a listener joining after 3 s gets `FF FB
90 64 ...` first). MPEG frames resynchronise on their own; the first
  frames may reference bit-reservoir data the joiner never got.
- It assumes one destination per instance and a fresh instance per file or
  connection (the callers re-create it on reopen and reconnect).
- A file written by it has no Info frame: decoders do not know the encoder
  delay (576 samples) nor the end padding, VBR files have no TOC, and
  gapless trimming is impossible.

## 14. Tests

| Test                                                                 | Invariant                                                                                                           | Binding                                | Incidental                                                       |
| -------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------- | -------------------------------------- | ---------------------------------------------------------------- |
| `tests/media` generated encoder tests (`%mp3(mono)`, `%mp3(stereo)`) | 10 s of sine through `output.file` yields a file ffprobe reads as audio with duration ≥ 10 s or at least one packet | an audio stream is found and non-empty | the 10 s, the sine                                               |
| `tests/media` decoding test (`%mp3(stereo)`)                         | a file this encoder wrote decodes back                                                                              | decodable                              | content                                                          |
| `tests/language/encoders.liq`                                        | non-literal `stereo=`/`mono=`/`channels=` is rejected                                                               | rejection                              | the error message; `channels=` is rejected by the builder anyway |
| `tests/snapshots` encoder cases                                      | the script form prints and parses back                                                                              | the printed text                       |                                                                  |
| `tests/streams/hls_mp3_vbr_bandwidth.liq`                            | a VBR variant announces measured peak bandwidth                                                                     | owned by the HLS tree                  |                                                                  |
| harbor, icecast, file tests using `%mp3`                             | use `%mp3` as a convenient stream format                                                                            | nothing about this encoder             |                                                                  |

Nothing tests: the bytes, the encoder delay, the tail loss at stop, the
ID3v2 tag bytes, the ABR/VBR mappings, LAME's silent clamping. A harness
needs: a finite source of known sample count, a decoder that reports
sample counts without trimming, an ID3v2 parser, and an MPEG header walker.
