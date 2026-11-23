# Ogg Speex stream encoder

Answers [ogg-streams.md](ogg-streams.md) for Speex (libspeex through the
vendored binding, which also builds the Speex headers and forms the pages).
Container behaviour is [ogg.md](ogg.md).

## 1. Format and options

`%speex(...)`, also `%ogg(%speex(...))`.

| parameter           | values                                                     | default          | error                                 |
| ------------------- | ---------------------------------------------------------- | ---------------- | ------------------------------------- |
| `stereo`, `mono`    | anonymous or bool                                          | stereo           |                                       |
| `samplerate`        | int                                                        | frame audio rate | none                                  |
| `quality`           | int 0..10 (constant quality)                               | 7                | `Speex quality should be in 0..10`    |
| `vbr`               | int (VBR with that quality)                                | off              | none                                  |
| `abr`               | int (average bitrate)                                      | off              | none                                  |
| `mode`              | `"narrowband"`, `"wideband"`, `"ultra-wideband"`, any case | narrowband       | others: generic parameter error       |
| `frames_per_packet` | int                                                        | 1                | none                                  |
| `complexity`        | int 1..10                                                  | unset            | `Speex complexity should be in 1..10` |
| `dtx`, `vad`        | bool                                                       | false            |                                       |
| `bytes_per_page`    | int                                                        | none             | no effect                             |

`quality`, `vbr` and `abr` are one setting: the last one given wins. The
defaults pair the narrowband mode (designed for 8 kHz) with the frame audio
rate (44.1 kHz by default).

Printed form:
`%speex(stereo,quality=7,samplerate=44100,mode=narrowband,frames_per_packet=1,dtx=false,vad=false)`;
VBR prints `vbr,quality=<q>`, ABR `abr,bitrate=<b>`, the wideband mode prints
`widebande` (checked), and `mode` is unquoted: the printed form does not
parse back.

## 2. Instance creation

Per instance: channel count (2 or 1), rate (forced), resampler.

Per link: a Speex header record (frames per packet, mode, VBR flag set only
for `vbr`, channels, rate); a libspeex encoder for the mode and frames per
packet; then the controls in order: `vbr` → `SPEEX_SET_VBR 1`,
`SPEEX_SET_VBR_QUALITY q`; `abr` → `SPEEX_SET_ABR 1`, `SPEEX_SET_BITRATE b`;
`quality` → `SPEEX_SET_QUALITY q`; then complexity (if set), DTX (if on), VAD
(if on), `SPEEX_SET_SAMPLING_RATE rate`. The codec frame size is read back
from the encoder (160, 320 or 640 samples for narrow, wide, ultra-wide band).
The two header packets are built with the link's comments. A carry-over
buffer of samples starts empty.

## 3. Identification header (BOS page)

The 80-byte Speex header alone on a flushed page (108 bytes). Example
(defaults, stereo, 44.1 kHz), offsets in the packet:

| offset | size | field                  | example             |
| ------ | ---- | ---------------------- | ------------------- |
| 0      | 8    | `Speex   `             |                     |
| 8      | 20   | version string         | `1.2.1`, NUL padded |
| 28     | 4    | version id             | 1                   |
| 32     | 4    | header size            | 80                  |
| 36     | 4    | rate                   | 44100               |
| 40     | 4    | mode                   | 0 (narrowband)      |
| 44     | 4    | mode bitstream version | 4                   |
| 48     | 4    | channels               | 2                   |
| 52     | 4    | bitrate                | -1                  |
| 56     | 4    | frame size             | 160                 |
| 60     | 4    | VBR                    | 0                   |
| 64     | 4    | frames per packet      | 1                   |
| 68     | 4    | extra headers          | 0                   |
| 72     | 8    | reserved               | 0                   |

All little-endian.

## 4. Skeleton fisbone

2 + extra headers header packets, granule rate = header rate / 1, preroll 3,
granule shift 0, `Content-type: audio/speex\r\n`.

## 5. Secondary headers

The comment packet, flushed: vendor `ocaml-speex by the savonet team
(http://savonet.sf.net/)` (56 bytes), then the comments (Vorbis-comment
layout, LE lengths). 109-byte page for the default `title=Unknown`.

## 6. Comments

`title` first: the metadata's `title`; else the basename of its `uri` with
the last extension removed; else `Unknown`. Then every other metadata pair as
`key=value` in list order.

## 7. Data input and conversion

Each block is copied and appended to the carry-over. Samples are converted to
16-bit integers by `x × 32767` truncated toward zero and clamped to
[-32768, 32767] (the code comment: the float API's range is distrusted, "so
using the int API"). Whole codec frames are fed to the binding, which encodes
frame after frame (stereo: `speex_encode_stereo_int` then `speex_encode_int`
on the interleaved frame), writes a packet every `frames_per_packet` frames,
and pages the stream out after each packet until libogg yields a page; that
page is handed to the muxer's queue for this stream. A frame is fed only when
strictly more than a whole frame remains, so the carry-over keeps between 1
and `frame_size` samples (a whole frame stays when the samples divide
exactly). Packets that do not fill a page stay in the libogg stream for the
next call.

## 8. Packets, granule positions and pages

Packet number 2 + (frames / frames per packet); granulepos = frames encoded ×
frame size at the codec rate, no look-ahead accounting. A packet is written with at
most `frame_size` bytes (the binding's limit to `speex_bits_write`). Pages: libogg's default threshold,
formed inside the binding (103 packets of 40 bytes per 4250-byte page in the
example).

## 9. Page end time

granulepos / rate.

## 10. End of stream

No token input for an empty link. The stream is terminated by the Ogg
binding's generic terminator (as for Opus, [ogg-opus.md §10](ogg-opus.md#10-end-of-stream)):
pending pages flushed, then a 27-byte EOS page without segments,
granulepos = last + 1 (example: last data page 66080, EOS page 66081). Lost:
the carry-over (1 to `frame_size` samples), frames encoded into an
incomplete packet when `frames_per_packet` > 1, and the resampler's held
samples. The binding's own Speex end-of-stream call is not used.

## 11. Page sizing

libogg default threshold; `bytes_per_page` ignored.

## 12. Failures

From `encode`: binding exceptions for a stream already ended or a frame of
the wrong length (neither reachable). libspeex controls do not report errors.
No validation of `vbr`, `abr`, `frames_per_packet` or `samplerate` values.

## 13. Concurrency

The libspeex encode calls run with the OCaml runtime lock released; the
frame feed is an OCaml callback called from C. No global state.

## 14. Tests

None. No test uses `%speex`.
