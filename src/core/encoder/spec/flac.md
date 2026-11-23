# `%flac` native FLAC encoder (as built)

What the native FLAC encoder does today: a FLAC stream (`fLaC`, STREAMINFO,
VORBIS_COMMENT, frames) written by libFLAC's stream encoder with no seek
callback, so STREAMINFO is never completed. This file owns `%flac` (native
container), the libFLAC calls, the bytes and the encoder's assumptions about
their destination. `%ogg(%flac)` shares the option parser (§1) but is
[ogg.md](ogg.md)'s. The shared seam is [encoder.md](encoder.md); the
`%name(...)` language and typing are [formats.md](formats.md); draining is
[draining.md](../../source/spec/draining.md). Measured with libFLAC 1.5.0.

## 1. Format and options

Syntax `%flac(...)`. Parameters in order, last wins (the same parser builds
the Ogg/FLAC stream format):

| Parameter                                                       | Value         | Default                            | Validation at evaluation                              |
| --------------------------------------------------------------- | ------------- | ---------------------------------- | ----------------------------------------------------- |
| `stereo`/`mono` (anonymous), `stereo=b`, `mono=b`, `channels=n` | channel count | 2 (fixed, #933)                    | none (type level: 1, 2, 6 have a layout)              |
| `samplerate=n`                                                  | int           | `frame.audio.samplerate`, deferred | none                                                  |
| `compression=n`                                                 | 0..8          | 5                                  | "invalid compression value"                           |
| `bits_per_sample=n`                                             | 8, 16, 24, 32 | 16                                 | "invalid bits_per_sample value"                       |
| `bytes_per_page=n`                                              | int           | unset                              | none; stored, unused by the native encoder (Ogg only) |

Format helpers: extension `flac`; MIME `audio/flex` (sic); no codec string;
no bit rate estimate (`Not_found`); printed
`%flac(channels=N,bits_per_sample=N,samplerate=N,compression=N)`.

## 2. Factory and instance creation

Ignores the name and `pos`; uses the initial metadata as Vorbis comments.

1. Comments = the initial metadata (export form, see
   [encoder.md](encoder.md)) as a list of (key, value).
2. A sample-rate converter, frame rate → format rate (forced now).
3. libFLAC stream encoder: channels, bits per sample, sample rate,
   compression level; no total-samples estimate. A channel count ≤ 0 raises
   `Flac.Encoder.Invalid_data`.
4. One VORBIS_COMMENT metadata block with every comment appended in order.
   A key or value libFLAC rejects raises `Flac.Encoder.Invalid_metadata`.
   The vendor string is libFLAC's own.
5. Initialise the stream with a write callback, a seek callback and a tell
   callback. Seek and tell answer "unsupported" (none were given). The write
   callback appends a copy of each libFLAC write to a byte buffer. The
   status of initialisation is **not checked**.
6. libFLAC writes `fLaC`, STREAMINFO and VORBIS_COMMENT during
   initialisation: they sit in the buffer until the first `encode`.

Invalid parameters libFLAC refuses (e.g. `samplerate=1000000`) make
initialisation fail silently: every later call writes nothing, no error, no
log; the output file is 0 bytes (checked).

## 3. `encode`

- The piece's audio (0 to its position) is resampled and given to libFLAC
  in one call, as integers: clip to [−1, 1], then `x × M` truncated, with
  `M` = 127, 32 767, 8 388 607 or 2 147 483 647 for 8, 16, 24, 32 bits (NaN
  not handled).
- libFLAC buffers samples up to its block size (4096 at the default
  compression level) and writes each completed frame through the callback.
- The call returns and empties the byte buffer: the first call returns the
  stream header plus any completed frames.
- Return codes of libFLAC's process call are ignored.

## 4. `encode_into`

Nothing.

## 5. `encode_metadata`

Nothing: ignored. Comments are only the initial metadata.

## 6. `header`

Always empty (code comment: "Flac encoder do not support header for now. It
will probably never do.."). The stream header is in the first `encode`
bytes instead.

## 7. `restart_offset`

Always none.

## 8. `bitrate`

Always none.

## 9. `stop`

- Calls libFLAC's finish: it encodes the remaining partial block as a
  shorter last frame and writes it, then returns the buffer.
- libFLAC would then seek back to rewrite STREAMINFO (total samples, MD5,
  min/max frame size); the seek callback answers "unsupported", so it does
  not. Nothing else is written.
- No input sample is lost at the frame rate: 66 150 samples in, 66 150
  decoded (checked). When resampling, the converter's held samples are not
  drained: 72 000 expected at 48 kHz, 71 979 decoded (21 lost, checked).

## 10. Bytes produced

```
"fLaC"
STREAMINFO block      (header 0x00, length 34)
VORBIS_COMMENT block  (header 0x84: last block, type 4)
FLAC frame × n        fixed block size, last one shorter
```

STREAMINFO as written: min and max block size = the encoder's block size;
min and max frame size 0 (unknown); sample rate, channels, bits per
sample; **total samples 0 (unknown)**; **MD5 all zero**. No PADDING,
SEEKTABLE or other block.

Worked example (`%flac`, stereo, 44.1 kHz, 16-bit, no metadata):

```
00000000: 664c 6143 0000 0022 1000 1000 0000 0000  fLaC..."........
00000010: 0000 0ac4 42f0 0000 0000 0000 0000 0000  ....B...........
00000020: 0000 0000 0000 0000 0000 8400 0028 2000  .............( .
00000030: 0000 7265 6665 7265 6e63 6520 6c69 6246  ..reference libF
00000040: 4c41 4320 312e 352e 3020 3230 3235 3032  LAC 1.5.0 202502
00000050: 3131 0000 0000 fff8 c918 00c2 0000 0000  11..............
```

- `00 00 00 22`: STREAMINFO, not last, 34 bytes. `10 00 10 00`: block size
  4096/4096. `00 00 00 00 00 00`: frame sizes unknown. `0a c4 42 f0 00 00 00
00`: 44 100 Hz, 2 channels, 16 bits, 0 total samples. 16 zero bytes: MD5.
- `84 00 00 28`: VORBIS_COMMENT, last block, 40 bytes; vendor length 32
  (little-endian), vendor string, comment count 0.
- `ff f8 c9 18`: first frame, fixed block size, block size code 4096, rate
  code 44.1 kHz, left/side, 16-bit.

`metaflac --list` reports `total samples: 0`, `MD5 signature: 000…0`,
`minimum framesize: 0 bytes`; ffprobe reports no duration (checked).

With initial metadata (harbor dedicated encoder, title "Hé", artist "A"),
the block holds `artist=A` and `title=Hé` (checked).

## 11. Concurrency and failures

- Serialised by the seam's lock; called from the output's streaming thread.
  libFLAC runs with the OCaml runtime lock released; its write callback
  re-acquires it, on the same thread, to append to the buffer.
- Failures: `Invalid_data`, `Invalid_metadata`, `Out_of_memory` at creation
  or in `encode`; every libFLAC status (init, process, finish) is ignored, so
  an encoder libFLAC put in an error state produces no bytes, silently.
- The libFLAC encoder is released by a garbage-collector finaliser.

## 12. Segmenting (HLS) face

Nothing. No HLS plug-in accepts `%flac`.

## 13. Where the bytes go

- No I/O by the encoder; bytes are returned. The library could seek and
  rewrite through callbacks, and the binding supports them (its own file
  API passes seek and tell), but this encoder passes none: it assumes an
  unseekable stream even when the destination is a regular file.
- The stream needs its first bytes (`fLaC` + STREAMINFO) to be decoded as
  FLAC, and they are only in the first `encode` result, not in `header`. A
  late joiner of a shared harbor stream starts at a frame (`ff f8 …`)
  without them (checked: a listener joining after 3 s). Its frames still
  carry rate and bit depth in their headers.
- Files written by `output.file` keep the incomplete STREAMINFO: no
  duration, no MD5 check, no seeking help.
- One stream per instance; the callers re-create the instance per file and
  per connection, which re-emits the header.

## 14. Tests

| Test                                                                 | Invariant                                                                       | Binding                                                                                    | Incidental       |
| -------------------------------------------------------------------- | ------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------ | ---------------- |
| `tests/media` generated encoder tests `%flac(stereo)`, `%flac(mono)` | 10 s of sine yields an audio file ffprobe reads with duration ≥ 10 s or packets | an audio stream exists, non-empty (duration is absent, so the packet count is what passes) | sine, 10 s       |
| `tests/media` decoding test `%flac(mono)`                            | a file this encoder wrote decodes back                                          | decodable                                                                                  | content          |
| `tests/regression/GH3316.liq`                                        | each file of a reopening `output.file(%flac)` is FLAC                           | ffprobe codec name `flac` for 3 files                                                      | timing of reopen |

Nothing tests STREAMINFO, the comments, the header-in-first-`encode`
placement, or silent initialisation failure. A harness needs a STREAMINFO
parser and a finite source.
