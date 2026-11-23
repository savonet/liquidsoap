# `%flac`: native FLAC

Part A. The writer of `%flac` alone: a native FLAC stream (`fLaC`, metadata
blocks, frames) encoded by libFLAC. `%flac` inside `%ogg(...)` is an Ogg
FLAC stream, owned by [ogg-flac.md](ogg-flac.md); the two share the options
of §1. It answers [encoder.md §10](encoder.md#10-implementation-files)
under the same numbers.

Native FLAC is a file format with a valid streaming form: it is accepted on
every output, shared mounts included. `%ogg(%flac)` is the usual form for
streaming FLAC to players and servers, which know Ogg pages better than
native FLAC frames.

It relies on: the contract ([encoder.md](encoder.md)); format values
([formats.md](formats.md)); sinks ([sink.md](sink.md)); draining
([draining.md](../../source/spec/draining.md)); the fault classes
([faults.md](../../../spec/faults.md)).

## 1. Format and options

| Parameter                                                     | Value                                                                                                  | Default                          | Effect                                                                          |
| ------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------ | -------------------------------- | ------------------------------------------------------------------------------- |
| `mono`, `stereo` (anonymous); `mono=`, `stereo=`, `channels=` | a channel count with a layout, 1 to 8, literal                                                         | 2                                | the stream's channels                                                           |
| `samplerate`                                                  | int, 1 to 655 350 Hz                                                                                   | the frame's audio rate, deferred | the stream's rate                                                               |
| `compression`                                                 | int 0..8                                                                                               | 5                                | the library's compression level                                                 |
| `bits_per_sample`                                             | 8, 16, 24, 32                                                                                          | 16                               | the stream's sample size                                                        |
| `bytes_per_page`                                              | refused on native `%flac`; accepted inside `%ogg` ([ogg-flac.md §1](ogg-flac.md#1-format-and-options)) | —                                | native FLAC has no pages: the error says to use `%ogg(%flac(bytes_per_page=…))` |

Checks, per [formats.md §7](formats.md#7-when-a-format-is-validated):

| Check                                                                                                                     | Phase                                     |
| ------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------- |
| channel count                                                                                                             | type checking                             |
| literal rate, compression, sample size; `bytes_per_page` on native FLAC                                                   | type checking                             |
| the same, computed                                                                                                        | evaluation                                |
| `bits_per_sample=32` against the library's version (32-bit encoding needs libFLAC 1.4 or later), read without side effect | evaluation                                |
| a deferred rate                                                                                                           | start-up                                  |
| the configuration against the library's own initialisation                                                                | creation; a refusal is a hard error (§15) |

A 32-bit stream lies outside FLAC's streamable subset; the writer encodes
it with the subset off and logs that once at `info`. Every other
configuration stays within the subset.

Helpers: `extension` `flac`; `mime` `audio/flac`; `bitrate` none (lossless);
`codecs` none (RFC 6381 names FLAC only inside an ISO base media file).

## 2. Declarations

| Declaration        | Value                                                                    |
| ------------------ | ------------------------------------------------------------------------ |
| streamable         | yes: every frame is an access point, the header is the entry header (§5) |
| needs seekable     | no                                                                       |
| can continue       | yes: frames only (§6.3)                                                  |
| in-band metadata   | no                                                                       |
| segmentable        | no                                                                       |
| needs no byte sink | no                                                                       |
| accepts delegated  | no                                                                       |

Adapted defaults: the header's form (§6) and the tags (§7). Native FLAC has
no preamble, and no explicit parameter is refused against capabilities.

## 3. Creation

- Inputs: the configuration, the confirmed capabilities, the initial
  metadata (the comments, §7), the output's name and the format's position.
- The library is configured with the channel count, sample size, rate and
  compression level, and initialised before the writer is returned. Its
  initialisation status is checked: a refusal is a hard configuration error
  naming the output, the library's status and the format's position. No
  configuration the library refuses yields an empty destination.
- The form is chosen once, from the capabilities (§6).
- The stream's header, produced by the library at initialisation, is held
  and written with the first piece's bytes. Creation writes nothing into the
  sink.

## 4. Input

- The piece's audio is converted from the frame's rate to the stream's by
  the writer's rate converter, a stage of
  [draining.md §1](../../source/spec/draining.md#1-stages).
- **Integer conversion**: samples become integers of `bits_per_sample` as
  [encoder.md §3.3](encoder.md#33-frames-in) states; Ogg FLAC does the
  same.
- The library is handed every sample at once; it holds less than one block
  between calls. No result depends on the piece boundaries
  ([encoder.md §3.3](encoder.md#33-frames-in)): the decoded result is
  bit-exact whatever the cut.

## 5. Bytes and markers

```
"fLaC"
STREAMINFO           block size, frame sizes, rate, channels, sample size, total samples, MD5
VORBIS_COMMENT       ENCODER=, and the initial metadata as tags (§7)
[PICTURE]            the cover, on a file sink only (§7)
[SEEKTABLE]          optional, file form only (§6.1)
FLAC frame × n       fixed block size, the last one shorter
```

- The header is `fLaC` and every metadata block, the last one flagged
  last. `stream_start(header_length, t₀)` is written with it, before its
  first byte.
- `access_point(t)` before every frame, at its frame start code; `t` is the media
  time of the frame's first sample. Frames are found from their own start
  code and header, never from the way the library hands its output over.
- A reader starting at an access point is sent the header first (the entry
  header): with it, every frame decodes alone.
- Frame numbers count from the start of the stream the frames belong to
  (§6.3 for an appended stream).

## 6. Header forms and preambles

### 6.1 File form

On a `seekable`, `fresh` sink:

1. the header is written in the streaming form (§6.2);
2. a SEEKTABLE block MAY be reserved, with placeholder points;
3. at a clean end, the writer seeks back and rewrites STREAMINFO with the
   total sample count, the minimum and maximum frame sizes and the MD5
   signature of the unencoded samples, and fills a reserved SEEKTABLE.

A file whose writer was abandoned, or whose process died, keeps a valid
streaming-form header.

### 6.2 Streaming form

On every other fresh sink: STREAMINFO with a total sample count of 0, frame
sizes of 0 and an MD5 of zeroes, the values FLAC defines as unknown. It is
the stream's header and the entry header of its access points.

### 6.3 Continuing

On a `continuing` sink, the writer writes frames only. It takes the
existing stream to be of its own rate, channel count and sample size.

- Its frames carry frame numbers that continue the existing stream's, so
  that the file is one stream whose numbering a reader can seek by. The
  writer learns the existing stream's sample count from the existing
  STREAMINFO, or, where that count is 0, from the last frame's header.
  It reads them at creation from the existing stream's first and last bytes,
  which the sink hands over when it opens
  ([sink.md §2](sink.md#2-operations)); where the last frame lies beyond
  them, the numbering starts from the STREAMINFO count, and where that is
  0 too, the writer writes the stream's frames numbered from 0, logged at
  `info` with the file's name.
- At a clean end, the sink being seekable, the writer SHOULD rewrite the
  existing STREAMINFO: the total sample count becomes the sum where the
  existing count was known, and stays 0 otherwise; the frame and block size
  bounds cover both streams where they were known; the MD5 signature becomes
  zeroes (unknown), since the writer did not see the existing samples.
- A STREAMINFO of another rate, channel count or sample size is left
  unchanged, logged at `severe` with the file's name.

## 7. Metadata

- The VORBIS_COMMENT block, with the library's vendor string, carries the
  `ENCODER=` entry on every sink
  ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles), item 8),
  then, as tags under item 7, the initial metadata
  ([encoder.md §2.2](encoder.md#22-creation)): on a file sink, one
  `key=value` entry per exported entry, in order, and a cover as a
  PICTURE block (type 3, front cover); on any other sink, nothing more,
  `%flac` having no parameter that asks for tags. A key the comment syntax
  cannot hold (an `=` or a byte outside 0x20–0x7D) is dropped with a
  warning; the writer does not fail for it.
- The writer offers no in-band metadata: native FLAC cannot start a new
  stream inside a stream.

## 8. Delay and padding

- Delay 0 and padding 0: FLAC is lossless and its last frame is short.
- The file form signals the exact total sample count (§6.1).
- Each file of a split has its own writer and header; the concatenated
  decode of the files equals the input sample for sample.

## 9. End

`end(clean)`, upstream first:

1. the rate converter is told the input ended, and what it returns is
   encoded;
2. the library is finished: it encodes the last partial block as a shorter
   frame, and its bytes are written;
3. on a `seekable` sink, STREAMINFO (and a reserved SEEKTABLE) is rewritten
   (§6.1), or the existing STREAMINFO updated on a continuing sink (§6.3);
4. `end` is written.

No input sample is lost, at the frame's rate or at another.

A writer ended before any piece writes nothing, header included.
`end(abandoned)` releases the library and the converter and writes nothing
more. The library's encoder is released exactly once, at `end`.

## 10. Properties

| Property  | Value | Known         |
| --------- | ----- | ------------- |
| `bitrate` | none  | —             |
| `delay`   | 0     | from creation |
| `padding` | 0     | from creation |

## 11. Live latency

The library holds one block (4 096 samples at the default level, about
93 ms at 44 100 Hz): a FLAC frame is its codec unit, not counted against
`L` ([encoder.md §3.9](encoder.md#39-live-latency-bound)). Every frame is
written when it completes, digital silence included.

## 12. Segmenting face

Nothing.

## 13. Delegated destination

Nothing.

## 14. Concurrency

No work of its own. The library hands its output to the writer during the
writer's own call, on the same thread; that output is written into the sink
before the call returns
([encoder.md §3.2](encoder.md#32-order-of-a-calls-output)). One library
encoder per writer; writers of several outputs run on several domains
without coordination.

## 15. Faults

| Fault                                                           | Class               | When                                |
| --------------------------------------------------------------- | ------------------- | ----------------------------------- |
| a value out of range; `bytes_per_page` on native FLAC           | configuration error | type checking or evaluation         |
| 32 bits with a library that cannot encode them                  | configuration error | evaluation                          |
| a deferred rate out of range                                    | configuration error | start-up                            |
| the library refuses its initialisation                          | configuration error | creation; hard error                |
| the library reports an error while encoding, or memory runs out | output fault        | the writer becomes `failed`         |
| finishing, or rewriting STREAMINFO, fails at a clean end        | not escalated       | logged at `severe`; the end goes on |

Every library status is checked; none is ignored.

## 16. Tests

Harness: a STREAMINFO parser, a reference decoder with MD5 verification, a
frame walker, a finite source of a known sample count.

1. A file written to a seekable fresh sink: STREAMINFO's total equals the
   input count, the MD5 verifies, the frame sizes bound the walker's; the
   decode is bit-exact against 16-bit input, at 44 100 Hz and at 48 000 Hz
   from a 44 100 Hz frame (the converter drained: 72 000 samples for 1.5 s).
2. The same through a FIFO and a live output: total 0, MD5 zeroes; the
   stream decodes.
3. A writer abandoned after 1 s: the file decodes with its streaming-form
   header.
4. A shared harbor reader starting after 3 s receives the header, then
   frames from a frame boundary, and decodes.
5. Initial metadata `title="Hé", artist="A"` and a cover, to `output.file`:
   the VORBIS_COMMENT block holds `ENCODER=` and both entries, and a
   PICTURE block the cover; with none, the block holds `ENCODER=` alone.
   The same to a shared harbor mount: `ENCODER=` alone, no PICTURE.
6. One minute appended to a one-minute file: one header, frame numbers
   continuous, total covering both minutes, MD5 zeroes; the decode is two
   minutes.
7. `%flac(samplerate=1000000)` and `%flac(bytes_per_page=500)` fail
   `--check`; a configuration the library refuses fails at creation with
   the library's status, never as an empty file.
8. `encoder.content_type(%flac)` is `audio/flac`.
9. Pieces of random lengths and offsets give a decode bit-exact with one
   piece's.
