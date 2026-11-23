# `%shine` fixed-point MP3 encoder (as built)

What the shine-backed MP3 encoder does today: a raw MPEG Layer III
elementary stream, constant bit rate, nothing else (no tag, no Info frame).
This file owns `%shine` and its alias `%mp3.fxp`, the shine calls, the bytes
and the encoder's assumptions about their destination. The shared seam is
[encoder.md](encoder.md); the `%name(...)` language and typing are
[formats.md](formats.md); draining is
[draining.md](../../source/spec/draining.md). Measured with libshine 3.x
(Homebrew, dylib version 4.1.0), decoded with FFmpeg 9.0.2.

## 1. Format and options

Syntax `%shine(...)` or `%mp3.fxp(...)`. Parameters in order, last wins:

| Parameter                     | Value       | Default                            | Validation at evaluation                                                                                               |
| ----------------------------- | ----------- | ---------------------------------- | ---------------------------------------------------------------------------------------------------------------------- |
| `stereo` / `mono` (anonymous) | —           |                                    | none                                                                                                                   |
| `stereo=b`, `mono=b`          | static bool | 2 channels                         | none                                                                                                                   |
| `channels=n`                  | static int  | 2                                  | none (type level: 1, 2 or 6 have a layout, [formats.md §6.1](formats.md#61-channel-count-from-parameters-shared-rule)) |
| `samplerate=n`                | int         | `frame.audio.samplerate`, deferred | none                                                                                                                   |
| `bitrate=n`                   | int, kbps   | 128                                | none                                                                                                                   |

Every option is validated only when an instance is created (§2). The
default channel count is a fixed 2 so the frame's channel count is not
forced early (code comment cites #933).

Format helpers: extension `mp3`; MIME `audio/mpeg`; codec string
`mp4a.40.34`; bit rate estimate `bitrate × 1000`; printed
`%shine(channels=N,samplerate=N,bitrate=N)` (always `%shine`, also for
`%mp3.fxp`).

## 2. Factory and instance creation

Ignores the name, `pos` and the initial metadata. Creation:

1. Forces the sample rate.
2. Asks shine whether (sample rate, bit rate) is a valid MPEG Layer III
   pair; if not, raises `Shine.Invalid_configuration` (no message, no log
   from the encoder). Checked: `%shine(samplerate=12345)` and
   `%shine(bitrate=7)` both fail this way at the first `encode` of
   `output.file`, in the streaming thread.
3. Channels outside 1..2 raise `Shine.Invalid_channels` (so `channels=6`,
   which types, fails here).
4. Initialises shine with MPEG defaults, the channel count, rate and bit
   rate; channel mode MONO for one channel, JOINT_STEREO otherwise. A null
   encoder raises `Out_of_memory`.
5. Reads `P` = samples per pass (1152 for MPEG-1 rates, 576 for MPEG-2/2.5).
6. Creates a sample-rate converter from the frame rate to the format's rate
   (pass-through copy when equal).

## 3. `encode`

- Input: the piece's audio, from 0 to its position. It is resampled by the
  converter (ratio `samplerate / frame rate`) and appended to a sample
  queue.
- While the queue holds **more than** `P` samples, `P` samples are taken
  and encoded as one pass. Samples are clipped to [−1, 1] and converted to
  16-bit by `s × 32767` truncated (−1 → −32768; NaN is not handled).
- Each pass returns the bytes shine has completed; the call returns their
  concatenation. One pass gives one MPEG frame, delayed by shine's internal
  analysis delay.
- After any call, 1 to `P` samples stay queued.

## 4. `encode_into`

Nothing.

## 5. `encode_metadata`

Nothing: ignored.

## 6. `header`

Always empty.

## 7. `restart_offset`

Always none.

## 8. `bitrate`

Always none.

## 9. `stop`

- Calls shine's flush once and returns its bytes: the bits shine has
  buffered for an incomplete frame. It does not encode any further audio.
- Lost at stop: the 1..`P` samples queued in Liquidsoap, the samples held in
  shine's analysis delay line (≈528 samples), and the converter's held
  samples when resampling.
- Measured (66 150 stereo samples at 44.1 kHz, 1.5 s, sine starting at
  0.5 s): 57 frames, 65 664 decoded samples; the sine starts 1056 samples
  late in the decode (encoder delay ≈528 plus decoder 529) and the decode
  ends while the sine still plays: it holds 64 607 of the 66 150 input
  samples. The last 1543 are lost: 486 never encoded, the rest in the two
  delay lines with no frame after them.
- With `samplerate=48000` from a 44.1 kHz frame: 62 frames (71 424
  samples) for 72 000 resampled input samples.

## 10. Bytes produced

```
MPEG audio frame × n      Layer III, CBR, no CRC
```

- No tag, no Info frame, no trailer. Frame header: sync, version per rate,
  Layer III, no CRC, the bit rate index, the rate index, padding bit as
  needed, mode MONO or JOINT_STEREO (mode extension 0), copyright 0,
  original 1, emphasis 0.

Worked example (`%shine`, stereo, 44.1 kHz):

```
00000000: fffb 9244 0000 0bf0 0069 0000 0000 0000  ...D.....i......
00000010: 0d20 0000 0000 0001 a400 0000 0000 0034  . .............4
00000020: 8000 0000 ffff ffff ffff ffff ffff ffff  ................
```

`FF FB 92 44`: MPEG-1 Layer III, 128 kbps, 44.1 kHz, padding 1, joint
stereo, mode extension 00, original. 1.5 s gives 57 frames, 23 824 bytes,
all 128 kbps (checked by walking every header).

## 11. Concurrency and failures

- Serialised by the seam's lock; called from the output's streaming thread.
  shine runs with the OCaml runtime lock released.
- Failures: `Invalid_configuration`, `Invalid_channels` at creation;
  `Invalid_buffer_size` if a pass is not exactly `P` samples (cannot happen
  through `encode`). No log from the encoder.
- The shine handle is released by the garbage collector.

## 12. Segmenting (HLS) face

Packed audio, as `%mp3`
([encoder-packed-audio.md](../../outputs/hls/spec/encoder-packed-audio.md));
one instance per variant, empty metadata. Bytes lag split points by the
queue and the delay line.

## 13. Where the bytes go

- No I/O; bytes are returned. A single unseekable stream read from byte 0
  is assumed, but nothing at the start is needed to decode: a late joiner
  decodes from any frame (bit reservoir aside).
- Nothing in the stream records the encoder delay or the end loss.

## 14. Tests

| Test                                                                             | Invariant                                                                       | Binding                                 | Incidental     |
| -------------------------------------------------------------------------------- | ------------------------------------------------------------------------------- | --------------------------------------- | -------------- |
| `tests/media` generated encoder tests `%shine(channels=1)`, `%shine(channels=2)` | 10 s of sine yields an audio file ffprobe reads with duration ≥ 10 s or packets | an audio stream exists and is non-empty | the sine, 10 s |

Nothing tests the bytes, validation (all deferred to creation), the delay or
the tail loss. A harness needs a finite source of known length and a
sample-counting decoder.
