# Ogg FLAC stream encoder

Answers [ogg-streams.md](ogg-streams.md) for FLAC in Ogg (libFLAC's own Ogg
encoder through the vendored binding). Container behaviour is
[ogg.md](ogg.md). The native `%flac` encoder is another tree's.

Unlike the other codecs, libFLAC forms the Ogg pages of this logical stream
itself, with its own libogg stream state and the serial the muxer chose. The
muxer's libogg stream for that serial never receives a packet; it is only
used for its serial and, at the end, marked ended so the muxer considers the
track finished.

## 1. Format and options

Only as `%ogg(%flac(...))` (`%flac` alone is native FLAC).

| parameter         | values            | default          | error                              |
| ----------------- | ----------------- | ---------------- | ---------------------------------- |
| `channels`        | int               | 2                | none here (layout check at typing) |
| `stereo`, `mono`  | anonymous or bool |                  |                                    |
| `samplerate`      | int               | frame audio rate | none                               |
| `compression`     | 0..8              | 5                | `invalid compression value`        |
| `bits_per_sample` | 8, 16, 24, 32     | 16               | `invalid bits_per_sample value`    |
| `bytes_per_page`  | int               | none             | no effect                          |

Printed form: `%flac(channels=2,bits_per_sample=16,samplerate=44100,compression=5)`.

## 2. Instance creation

Per instance: channels, rate (forced), resampler.

Per link, on the first callback: a libFLAC stream encoder with channels, bits
per sample, rate, compression level, total samples unknown, the link's
comments, the Ogg serial set to the logical stream's, initialised for Ogg
output with a write callback only (no seek, no tell). Every page libFLAC
writes during initialisation is kept as the link's **first pages**. The
result of the initialisation is not checked.

## 3. Identification header (BOS page)

The first of the first pages: the Ogg FLAC mapping packet (51 bytes) alone,
79-byte page:

| offset | size | value                           |
| ------ | ---- | ------------------------------- |
| 0      | 1    | 0x7F                            |
| 1      | 4    | `FLAC`                          |
| 5      | 2    | mapping version 1.0             |
| 7      | 2    | number of header packets, BE: 1 |
| 9      | 4    | `fLaC`                          |
| 13     | 4+34 | STREAMINFO metadata block       |

STREAMINFO as written (example, 44.1 kHz stereo 16 bits): block size
4096/4096, frame sizes 0/0 (unknown), rate 44100, 2 channels, 16 bits, total
samples **0**, MD5 all zero. It is never rewritten (§10).

## 4. Skeleton fisbone

2 header packets (code comment "2 for now"), granule rate = rate / 1,
preroll 2 (code comment "for flac its 2 ???"), granule shift 0,
`Content-type: audio/x-flac\r\n`.

## 5. Secondary headers

The remaining first pages, as libFLAC wrote them: the VORBIS_COMMENT
metadata block (with the last-block flag, header byte 0x84), vendor
`reference libFLAC 1.5.0 20250211` with the installed library, then the
comments. 72-byte page with no comment.

## 6. Comments

Every metadata pair as `key=value`, in list order. No `ENCODER` tag.

## 7. Data input and conversion

Each block is copied and handed to libFLAC, which converts samples to
integers of the configured width (clipping to [-1, 1] in the binding),
encodes blocks of 4096 samples and writes pages as they fill. The pages
written during the call are collected and handed to the muxer's queue for
this stream, in order. libFLAC holds less than one block between calls.

## 8. Packets, granule positions and pages

libFLAC's: one packet per FLAC frame, page granulepos = samples at the end
of the page at the codec rate. Page boundaries are libFLAC's (4 frames per
page, about 7.5 KB, for 16-bit stereo at compression 5 in the example).

## 9. Page end time

granulepos / rate.

## 10. End of stream

1. If no block was given in the link, one silent sample per channel, with the
   **frame's** channel count, is processed first.
2. libFLAC is finished: the partial block is encoded as a short last frame,
   and libFLAC writes its last page with the EOS flag. With no seek callback,
   STREAMINFO keeps 0 total samples and a zero MD5.
3. The muxer's (unused) libogg stream for this serial is marked ended
   directly (a C stub in the Liquidsoap tree sets its EOS field), so that the
   muxer's "stream has seen EOS" test passes.
4. The pages collected are returned.

Nothing is lost: the final granulepos equals the samples given (48 510 and
66 150 in the two links of the example, 2.6 s in all).

## 11. Page sizing

Not applicable: libogg's threshold in libFLAC applies, the muxer's never
does. `bytes_per_page` is ignored as for every codec.

## 12. Failures

- channel count ≤ 0: binding exception at link start (not reachable: typing);
- libFLAC initialisation failure: unchecked; with no first page the
  registration raises the muxer's invalid-data error;
- the empty-link fallback with a channel count different from the codec's:
  a libFLAC error, from `encode_metadata` or `stop` (read only).

## 13. Concurrency

libFLAC init, process and finish run with the OCaml runtime lock released;
the write callback reacquires it to hand each page to OCaml. The list of
pages written is protected by a mutex of its own, although every write
happens on the calling thread.

## 14. Tests

`tests/media` (mono, stereo: decodable), `tests/harbor/harbor_output_join_points.liq`
(`flac.ogg`: links), `tests/regression/fallible_ogg.liq` (to a harbor input).
