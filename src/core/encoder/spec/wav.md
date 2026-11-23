# `%wav` encoder (as built)

What the native WAV encoder does today: a 44-byte RIFF/WAVE header followed
by interleaved little-endian PCM. It owns its options, its bytes and its
assumptions about where they go. The shared seam (registry, factories,
the per-instance lock, the call contract) is [encoder.md](encoder.md).
Draining is [draining.md](../../source/spec/draining.md).

The same header writer is used by two other parts of the code, which this
file is the owner of as far as the bytes go:
[`%external`](external.md) (the header it writes to its process's stdin) and
the `pipe` operator (the header it writes to its child's stdin).

## 1. Format and options

Syntax: `%wav(...)`. Parameters, in any order, last one wins:

| Parameter            | Value                      | Default                                                      | Effect                                     |
| -------------------- | -------------------------- | ------------------------------------------------------------ | ------------------------------------------ |
| `stereo` (anonymous) | —                          |                                                              | channels = 2                               |
| `mono` (anonymous)   | —                          |                                                              | channels = 1                               |
| `stereo=b`           | static bool                |                                                              | channels = 2 if b, else 1                  |
| `mono=b`             | static bool                |                                                              | channels = 1 if b, else 2                  |
| `channels=n`         | static int                 | 2                                                            | channels = n (no range check)              |
| `samplerate=n`       | int                        | `frame.audio.samplerate` (read lazily, at instance creation) | output sample rate                         |
| `samplesize=n`       | int, one of 8, 16, 24, 32  | 16                                                           | bits per sample                            |
| `header=b`           | bool                       | true                                                         | emit the 44-byte header                    |
| `duration=d`         | float (an int is rejected) | none                                                         | only changes the header's size fields (§6) |

Errors (all raised while the script is evaluated, kind `encoder`):

- `samplesize` not in {8,16,24,32}: "invalid sample size", at the value's position.
- any other labelled parameter or a wrong value type (e.g. `duration=2`):
  "unknown parameter name (duration) or invalid parameter value (2)".
- any other anonymous parameter: "Unknown encoder parameter: foo".
- `stereo=`/`mono=`/`channels=` given a non-literal: "Invalid value … Only
  static `true` or `false` are allowed." / "… Only static numbers are allowed."
  (raised by the script-level type computation, before the value exists).
- `pcm_s16`/`pcm_f32` anonymous: "pcm_s16 audio is only supported by the
  %ffmpeg encoder."

Type: a source with exactly one `audio` track of PCM with _channels_
channels, no other track. The channel count is the literal from the script
(default 2, not `frame.audio.channels`: the default is hard-coded so that
typing does not force the frame channel setting, issue #933). Giving `%wav`
a source with video is a type error.

Printed as `%wav(samplerate=44100,channels=2,samplesize=16,header=true)`,
with `,duration=%f` appended when set.

Extension `wav`; MIME `audio/wav` (Icecast/Harbor content-type
`audio/wav` via the same mapping); no format-level bitrate estimate (the
format-level query raises "not found"); Icecast info reports samplerate and
channels, no bitrate, no quality.

## 2. Factory and instance creation

The factory ignores the output name and the initial metadata. Creating an
instance:

- forces the sample rate (the format's, or the frame audio rate);
- creates a sample-rate converter for _channels_ channels (stateful, one per
  instance), used with ratio `samplerate / frame.audio.samplerate`;
- computes the header once (§6), from the format only;
- sets a "header pending" flag to the value of `header`.

Nothing fails here.

## 3. `encode`

Input: one frame. Only its audio track is read, from offset 0 for the
frame's audio length; other tracks, metadata and track marks are ignored.

1. The audio is passed through the converter (identity when the rates are
   equal; otherwise it may return fewer or more samples than given and keeps
   state across calls).
2. Each sample is converted and interleaved channel by channel, little
   endian: 8 bits unsigned (`U8`, silence 0x80), 16/24/32 bits signed
   (`S16LE`, `S24LE` packed in 3 bytes, `S32LE`). Float samples are
   clipped to [-1, 1] by the conversion (see [findings](findings/simple-encoders.md#to-verify)).
3. If "header pending", the result is the header followed by the samples,
   and the flag is cleared; otherwise the samples alone.

A zero-length frame yields zero sample bytes, but still carries the header
if it was pending (and clears the flag). Bytes out per call:
`samples_out × channels × samplesize/8`, plus 44 once.

An unsupported sample size raises at this point ("Unsupported sample size",
kind `encoder`); validation at §1 makes it unreachable.

## 4. `encode_into`

Nothing.

## 5. `encode_metadata`

Nothing: metadata is accepted and dropped. WAV output never carries
metadata (no `LIST/INFO`, no ID3).

## 6. `header`

The header is computed once per instance:

```
offset size  value
0      4     "RIFF"
4      4     RIFF size, LE  (see below)
8      4     "WAVE"
12     4     "fmt "
16     4     16                      (fmt chunk length, LE)
20     2     1                       (PCM)
22     2     channels
24     4     samplerate
28     4     channels*samplerate*samplesize/8   (bytes per second)
32     2     channels*samplesize/8               (block align)
34     2     samplesize
36     4     "data"
40     4     data size, LE  (see below)
```

Size fields:

- Without `duration`: RIFF size bytes `FF FF FF EF` (0xEFFFFFFF) and data
  size bytes `DB FF FF EF` (0xEFFFFFDB = 0xEFFFFFFF − 36). These are literal
  constants, "the maximum possible values" per the code comment; neither the
  code nor the history gives why 0xEF rather than 0xFF in the top byte.
- With `duration=d`: data size `v = floor(d × channels × samplerate ×
samplesize / 8)` and RIFF size `v + 36`. Each is written with its bit 31
  cleared (values ≥ 2³¹ are silently reduced modulo 2³¹). `v` is not rounded
  to the block alignment.

`header()` returns:

- the empty string when `header=false`;
- the empty string before the first `encode` of the instance (that `encode`
  carries the header itself);
- the 44-byte header, unchanged, after it (same sizes: a joiner never gets a
  header describing the remaining length).

## 7. `restart_offset`

Always "none". The encoder never starts a new logical stream.

## 8. `bitrate`

Always "none" (even though the rate is constant and known).

## 9. `stop`

Returns no bytes. Nothing is drained: samples the rate converter holds
(non-zero only when `samplerate` differs from the frame rate) are dropped.
The header's sizes are never corrected; the encoder has no way to rewrite
bytes it already returned.

## 10. Bytes produced

The whole output of an instance is: `header (44 bytes, if header=true)` then
the concatenated sample bytes of every `encode`, in order, nothing at the
end. The header is in the first `encode`'s bytes, whatever that frame's length.

Worked examples (real binary at 02fa52985, `sine(440.)`, 44.1 kHz frames of
0.02 s, run ~0.56 s, `output.file`):

`%wav` (stereo, 16 bits, no duration), 98 828 bytes:

```
00000000: 5249 4646 ffff ffef 5741 5645 666d 7420  RIFF....WAVEfmt
00000010: 1000 0000 0100 0200 44ac 0000 10b1 0200  ........D.......
00000020: 0400 1000 6461 7461 dbff ffef 0000 0000  ....data........
```

`%wav(duration=1.)`, same audio, same length: the size fields become
`34 b1 02 00` (176 436) and `10 b1 02 00` (176 400) although only 98 784
data bytes follow. `ffmpeg` reports "Packet corrupt" and decodes the 0.56 s
that exist.

`%wav(samplesize=24, duration=0.5)`, 148 220 bytes: `fmt` gives
`98 09 04 00` (264 600 B/s), block 6, 24 bits; data size `cc 04 02 00`
(132 300). The file holds 0.56 s; `ffmpeg` decodes exactly 0.5 s and
ignores the rest: data past the declared length is silently lost to
readers.

`%wav(mono, samplesize=8, samplerate=8000)`: `01 00` channels, `40 1f 00 00`
rate and byte rate, block 1, 8 bits; samples start `84 a8 d0 eb`.

`%wav(header=false)`: raw `s16le` interleaved from byte 0:
`0000 0000 0408 0408 0110 0110 …` (L, R, L, R…).

## 11. Concurrency and failures

No thread of its own, no lock of its own beyond the seam's per-instance lock
([encoder.md](encoder.md)). Every call is pure computation on the caller's
thread. No call fails at run time in practice.

## 12. Segmenting (HLS) face

Nothing. No HLS factory is registered for `%wav`; the HLS output rejects
it at evaluation ("transport of other media file formats is not defined",
[HLS tree](../../outputs/hls/spec/encoder-contract.md)). The same holds for
`%avi`, `%ndi` and `%external`.

## 13. Where the bytes go

- **The encoder assumes it cannot seek back.** It never rewrites its header;
  lengths are either placeholders (0xEFFFFFFF/0xEFFFFFDB) or a promise taken
  from `duration`, which nothing enforces: the encoder does not stop, pad or
  cut at that duration. A file shorter than `duration` has a header that
  overstates it; a file longer has a tail that readers drop (§10). The only
  encoder in the tree that can finalise a header is `%ffmpeg`, because
  `output.file` hands it the file name and lets it write the file itself.
- **One stream, starting at the first encode.** The header is emitted once
  per instance, in the first `encode`'s bytes. Whether each file of
  `output.file` gets its own header therefore depends on whether the caller
  creates a new instance per file (the caller's documents). An instance's
  bytes appended to an existing file (`append=true`) put a second header
  mid-file.
- **Late joiners** (harbor) get `header()`, i.e. the same 44 bytes with the
  same placeholder/duration sizes, then PCM from wherever the stream is: the
  header is valid for them because PCM has no framing beyond block alignment
  and every `encode` returns whole sample blocks.
- The encoder never does I/O; it only returns bytes.

## 14. Tests

- **Bad options are rejected at evaluation with kind `encoder`.**
  (`tests/language/encoders.liq`, invalid `samplesize`.) Binding: the
  error kind and that it is raised before any output runs. Incidental: the
  message wording (it is asserted literally there).
- **The channel count is part of the type.** Two `%wav` with different
  channels cannot be in one list; `%wav` fed a video-only source is a type
  error (`tests/language/encoders.liq`, `tests/language/type_errors.liq`).
  Binding: rejection at type checking. Incidental: which formats are mixed.
- **Output decodes with the requested channels and rate.**
  (`tests/media/mono.liq`, `stereo.liq`, the `%wav(...)` fixtures of
  `tests/media/test_media_formats.ml`.) Binding: an external decoder reads
  the file with the declared channels and sample rate. Incidental: ffprobe,
  48 kHz, the source file.
- `%wav` is the default cheap encoder of many output tests (harbor,
  icecast, `output.file` reopen); those assert the caller's behaviour, not
  the bytes. A harness for those needs only "some encoder whose first
  output carries a header and whose header() is stable".
- **Untested:** the header bytes; the size fields with and without
  `duration`; `header()` before vs after the first encode; `header=false`;
  8/24/32-bit layouts; resampled output length. A new harness needs a
  byte-level check of §6 and §10 and a frame double with a known PCM ramp.
