# `%fdkaac` AAC encoder (as built)

What the FDK-AAC encoder does today: AAC access units wrapped in the
transport chosen by `transmux` (ADTS by default), nothing else. This file
owns `%fdkaac`, the FDK calls, the bytes and the encoder's assumptions about
their destination. The shared seam is [encoder.md](encoder.md); the
`%name(...)` language and typing are [formats.md](formats.md); draining is
[draining.md](../../source/spec/draining.md). Measured with libfdk-aac 2.0.3
(Homebrew), decoded with FFmpeg 9.0.2; library behaviour was also
reproduced by a C program issuing the same FDK calls as the binding.

## 1. Format and options

Syntax `%fdkaac(...)`. Parameters in order, last wins:

| Parameter                                                       | Value                                                                                                                                 | Default                            | Validation at evaluation                                     |
| --------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------- | ------------------------------------------------------------ |
| `aot=s`                                                         | `mpeg4_aac_lc`, `mpeg4_he_aac`, `mpeg4_he_aac_v2`, `mpeg4_aac_ld`, `mpeg4_aac_eld`, `mpeg2_aac_lc`, `mpeg2_he_aac`, `mpeg2_he_aac_v2` | `mpeg4_he_aac_v2`                  | other: "invalid aot value"                                   |
| `bitrate=n`                                                     | int, kbps                                                                                                                             | 64                                 | none                                                         |
| `vbr=n`                                                         | 1..5                                                                                                                                  | unset (constant)                   | other: "invalid vbr mode. Possible values: 1, 2, 3, 4, 5"    |
| `bandwidth=n`, `bandwidth="auto"`                               | Hz, or auto (any case)                                                                                                                | auto                               | none                                                         |
| `afterburner=b`                                                 | bool                                                                                                                                  | false                              | none                                                         |
| `sbr_mode=b`                                                    | bool                                                                                                                                  | false                              | none (only used with `mpeg4_aac_eld`)                        |
| `transmux=s`                                                    | `raw`, `adif`, `adts`, `latm`, `latm_out_of_band`, `loas`                                                                             | `adts`                             | other: "invalid transmux value"                              |
| `samplerate=n`                                                  | 8000, 11025, 12000, 16000, 22050, 24000, 32000, 44100, 48000, 64000, 88200, 96000                                                     | `frame.audio.samplerate`, deferred | "invalid samplerate value. Possible values: ..." when forced |
| `stereo`/`mono` (anonymous), `stereo=b`, `mono=b`, `channels=n` | channel count                                                                                                                         | 2                                  | none                                                         |

After the fold: an HE-AAC v2 profile (`mpeg4_he_aac_v2` or
`mpeg2_he_aac_v2`) with a channel count other than 2 raises a plain
`Failure "HE-AAC v2 is only available with 2 channels."` — not an encoder
runtime error: the script aborts with error 9 and no position (checked
with `ignore(%fdkaac(channels=1))`). `--check` does not see it.

`vbr` and `bitrate` coexist: the last `vbr` makes the mode variable and the
`bitrate` field is then unused by the encoder but still used by the
estimate.

Format helpers:

- extension `aac` and MIME `audio/aac` for every transport (also LOAS,
  LATM, ADIF, raw);
- RFC 6381 codec string: `mp4a.40.2` (MPEG-4 LC), `mp4a.40.5` (HE),
  `mp4a.40.29` (HE v2), `mp4a.40.23` (LD), `mp4a.40.39` (ELD), `mp4a.67` for
  every MPEG-2 profile (HE and HE v2 marked "TODO: check this" in code);
- bit rate estimate `bitrate × 1000`, also when `vbr` is set;
- printed `%fdkaac(afterburner=B,aot="..",bitrate=N|vbr=N,channels=N,
samplerate=N,sbr_mode=B,transmux="..")`; `bandwidth` is not printed.

## 2. Factory and instance creation

Ignores the name and the initial metadata; uses `pos` for errors.

1. Open an FDK encoder for the channel count; channel mode 1→mono,
   2→stereo, 3→1/2, 4→1/2/1, 5→1/2/2, 6→1/2/2/1; channel order WAV.
   Any other count raises `Fdkaac.Encoder.Unsupported_parameter`.
2. Set, in this order: AOT (codes 2, 5, 29, 23, 39, 129, 132, 156),
   bandwidth (0 for auto), sample rate (forced now; an invalid value raises
   the deferred `encoder` error), transport (0 raw, 1 ADIF, 2 ADTS, 6 LATM,
   7 LATM out of band, 10 LOAS), afterburner, then SBR mode only for
   `mpeg4_aac_eld`, then either the VBR mode `n` or the bit rate in bits per
   second. Granule length is never set.
3. A parameter the library rejects raises an `encoder` runtime error at the
   format position: "Invalid configuration: <param>" or "Unsupported
   parameter: <param>"; any other library error is re-raised as is.
4. Create a sample-rate converter, frame rate → format rate.

Combinations are only checked by FDK at its own initialisation, which runs
at the first `encode` (the binding initialises the encoder when it first
asks for its frame length): e.g. `aot="mpeg4_aac_eld"` with ADTS raises
`Error 3` ("Transport library initialization error") at the first `encode`
(checked). Impossible bit rates are clamped silently: `bitrate=1000` gives
768-byte HE-AAC v2 frames, about 132 kbps (checked).

## 3. `encode`

Two buffering stages, then FDK's own.

1. The piece's audio (0 to its position) is resampled and converted to
   interleaved signed 16-bit little-endian (clip to [−1, 1], `s × 32767`
   truncated, −1 → −32768) and appended to a byte buffer.
2. While that buffer holds at least `B` bytes, the first `B` bytes are
   given to the binding. `B` = the `LIQ_BUFLEN` environment variable, read
   once at startup, default 1024 (256 stereo samples). No reason is given
   in the code.
3. The binding appends them to its own buffer and calls FDK once per full
   chunk of `2 × channels × frame_length` bytes, where `frame_length` is
   FDK's input frame length (measured: 1024 for LC, 2048 for HE v2; LD and
   ELD not measured).
4. FDK returns at most one transport frame per call (none while it fills
   its delay). The call returns the concatenation.

## 4. `encode_into`

Nothing.

## 5. `encode_metadata`

Nothing: ignored.

## 6. `header`

Always empty. For ADIF, LATM out of band and raw, the configuration a
decoder needs is not available from `header` (ADIF puts its header in the
first `encode` bytes; raw and out-of-band LATM emit none at all).

## 7. `restart_offset`

Always none.

## 8. `bitrate`

Always none.

## 9. `stop`

1. The bytes left in the Liquidsoap buffer (< `B`) are given to the binding.
2. The binding encodes every full chunk, then pads the remaining partial
   chunk with zero bytes to a full chunk and encodes it.
3. It then calls FDK's flush (input count −1) until FDK reports end of
   stream or "unsupported parameter", concatenating what each call returns.

What happens: the flush call gives FDK no output buffer, so every flush
call returns 0 bytes until end of stream. FDK's delay (2048 samples for LC,
7106 for HE v2 at 44.1 kHz stereo 64 kbps) is never emitted. Reproduced in C
with the binding's exact calls: LC gives 65 frames, then two flush calls of
0 bytes, then EOF; with an output buffer the same calls give 186 and 185
bytes (two more frames).

Measured (66 150 stereo samples at 44.1 kHz, 1.5 s, sine from 0.5 s):

| Profile             | Frames    | Decoded samples | Onset delay | Input present in decode |
| ------------------- | --------- | --------------- | ----------- | ----------------------- |
| HE-AAC v2 (default) | 33 × 2048 | 67 584          | 7105        | 60 478 (5672 lost)      |
| LC                  | 65 × 1024 | 66 560          | 2048        | 64 512 (1638 lost)      |

The zero padding of the last chunk is not signalled. The resampler's held
samples are not drained either.

## 10. Bytes produced

```
ADTS (default):   [ADTS header 7 bytes | raw_data_block] × n
LOAS:             [AudioSyncStream 0x2B7 sync | AudioMuxElement] × n
LATM:             AudioMuxElement × n, configuration in band, no sync word
LATM out of band: AudioMuxElement × n, no configuration anywhere
ADIF:             "ADIF" header once, then raw_data_block × n
raw:              raw_data_block × n, no framing, no configuration
```

ADTS header as written (protection absent, no CRC, one raw data block per
frame): MPEG id bit 0 for MPEG-4 AOTs, 1 for MPEG-2; profile field 1 (LC)
for every profile; sampling index and channel configuration of the **AAC
core**. HE-AAC and HE-AAC v2 are signalled implicitly: the header advertises
the core (half rate; mono core for v2) and the decoder discovers SBR and PS
in the payload. Buffer fullness carries FDK's value in CBR and `0x7FF` in
VBR.

Worked examples (first header of each file):

```
HE-AAC v2, 44.1 kHz stereo, 64 kbps:  ff f1 5c 40 2e 61 8c
  MPEG-4, no CRC, LC, sf index 7 (22050), channel config 1, 371-372 bytes/frame
LC, 44.1 kHz stereo, 64 kbps:         ff f1 50 80 17 22 a0
  MPEG-4, no CRC, LC, sf index 4 (44100), channel config 2, 185-186 bytes/frame
MPEG-2 LC:                            ff f9 50 80 17 22 a0   (id bit 1)
LC vbr=3:                             ff f1 50 80 01 ff fc   (fullness 0x7FF)
LOAS:                                 56 e0 b6 20 ...
ADIF:                                 41 44 49 46 00 1f 40 00 ...  ("ADIF")
raw:                                  21 11 45 00 14 50 01 46 ...
```

FFmpeg reads the ADTS and LOAS files; it misidentifies the raw, ADIF and
in-band LATM files (profile "Main", rate 0) (checked).

## 11. Concurrency and failures

- Serialised by the seam's lock; called from the output's streaming thread.
  FDK runs with the OCaml runtime lock released; each call copies its input
  to a C buffer and writes into a 20 480-byte stack buffer.
- Failures, none logged by the encoder:
  - creation: `Unsupported_parameter` (channel count), `encoder` runtime
    errors (§2), the deferred sample-rate error;
  - first `encode`: FDK initialisation errors `Error 0..4` (general, AAC,
    SBR, transport, metadata);
  - `encode`: `Error 5` (encoding), `Invalid_data`, `Unknown n`.
- The FDK handle is released by the garbage collector.

## 12. Segmenting (HLS) face

Packed audio
([encoder-packed-audio.md](../../outputs/hls/spec/encoder-packed-audio.md))
only for `transmux="adts"`; any other transport has no HLS plug-in and the
HLS output rejects it. One instance per variant, empty metadata. Each ADTS
frame is self-contained; segment bytes lag split points by FDK's delay and
the two buffers.

## 13. Where the bytes go

- No I/O; bytes are returned. One unseekable stream per instance is
  assumed.
- ADTS and LOAS: a late joiner decodes from any frame boundary; nothing at
  the start is needed.
- ADIF: the header is only in the first bytes; a late joiner cannot decode.
- Raw and LATM out of band: no consumer can decode without the
  AudioSpecificConfig, which the encoder never exposes (no `header`, no
  side channel). Nothing in the seam can carry it to a container.
- Nothing records the encoder delay or the padding: the files are not
  gapless.

## 14. Tests

| Test                                                                                                  | Invariant                                                                       | Binding                           | Incidental |
| ----------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------- | --------------------------------- | ---------- |
| `tests/media` generated encoder tests `%fdkaac(aot="mpeg4_aac_lc",channels=1)`, `%fdkaac(channels=2)` | 10 s of sine yields an audio file ffprobe reads with duration ≥ 10 s or packets | an audio stream exists, non-empty | sine, 10 s |
| HLS tests using `%fdkaac` variants                                                                    | owned by the HLS tree                                                           |                                   |            |

Nothing tests: the tail (the lost delay would be caught by a sample count),
the transports other than ADTS, the HE v2 channel check, FDK clamping. A
harness needs a finite source, a sample-counting decoder that does not trim
priming, and an ADTS/LOAS walker.
