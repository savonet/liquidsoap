# Ogg Opus stream encoder

Answers [ogg-streams.md](ogg-streams.md) for Opus (libopus through the
vendored binding, which also builds the Ogg Opus headers, RFC 7845).
Container behaviour is [ogg.md](ogg.md).

## 1. Format and options

`%opus(...)`, also `%ogg(%opus(...))`.

| parameter         | values                                                                              | default                 | error                                                                 |
| ----------------- | ----------------------------------------------------------------------------------- | ----------------------- | --------------------------------------------------------------------- |
| `application`     | `"voip"`, `"audio"`, `"restricted_lowdelay"`                                        | unset (`audio` is used) | other strings: generic parameter error                                |
| `complexity`      | int 0..10                                                                           | unset                   | `Opus complexity should be in 0..10`                                  |
| `max_bandwidth`   | `"narrow_band"`, `"medium_band"`, `"wide_band"`, `"super_wide_band"`, `"full_band"` | unset                   |                                                                       |
| `frame_size`      | float, one of 2.5, 5., 10., 20., 40., 60. (ms)                                      | 20.                     | `Opus frame size should be one of 2.5, 5., 10., 20., 40. or 60.`      |
| `samplerate`      | 8000, 12000, 16000, 24000, 48000                                                    | 48000                   | `Opus samplerate should be one of 8000, 12000, 16000, 24000 or 48000` |
| `bitrate`         | int kbps (×1000 must be in 500..512000), `"auto"`, `"max"`                          | `"auto"`                | `Opus bitrate should be in 5..512`                                    |
| `channels`        | 1 or 2; also `stereo`, `mono`                                                       | 2                       | `only mono and stereo streams are supported for now`                  |
| `vbr`             | `"none"` (CBR), `"constrained"`, `"unconstrained"`                                  | `"constrained"`         |                                                                       |
| `signal`          | `"voice"`, `"music"`                                                                | unset                   | `"auto"` is not accepted                                              |
| `dtx`             | bool                                                                                | false                   |                                                                       |
| `phase_inversion` | bool                                                                                | true                    |                                                                       |
| `bytes_per_page`  | int                                                                                 | none                    | no effect                                                             |

Note: `bitrate=5` gives 5000 b/s, so the accepted integer range is 1..512
(`bitrate=1` is 1000 ≥ 500), not 5..512 as the message says.

Printed form (checked):
`%opus(vbr="constrained",bitrate="auto",channels=2,complexity="3",signal="voice",samplerate=48000,frame_size=20.00,dtx=false,phase_inversion=true)`;
`complexity` prints as a quoted string, which does not parse back.

## 2. Instance creation

Per instance: channel count, codec rate, resampler (frame rate → codec rate;
with the default 48000 and a 44.1 kHz frame, every block is resampled).

Per link: a sample FIFO for the link, and, on the first callback, a libopus
encoder `opus_encoder_create(rate, channels, application)` followed by these
controls in order: bitrate (`auto`, `max` or the value), VBR off for
`vbr="none"` else VBR on and VBR constraint on/off, complexity (if set), max
bandwidth (if set), signal (if set), DTX, phase inversion disabled =
not `phase_inversion`. The two header packets are built at the same time.

Codec frame size in samples: ⌊frame_size × rate / 1000⌋ (960 for 20 ms at
48 kHz).

## 3. Identification header (BOS page)

The `OpusHead` packet, 19 bytes, alone on a flushed page (47 bytes):

| offset | size | value                                           |
| ------ | ---- | ----------------------------------------------- |
| 0      | 8    | `OpusHead`                                      |
| 8      | 1    | version 1                                       |
| 9      | 1    | channel count                                   |
| 10     | 2    | pre-skip, LE: **3840**, always                  |
| 12     | 4    | input sample rate, LE: the codec rate parameter |
| 16     | 2    | output gain, LE: 0                              |
| 18     | 1    | channel mapping family 0                        |

The pre-skip is the binding's default (3840 samples at 48 kHz, 80 ms), not
the encoder's look-ahead, which libopus reports (312 samples at 48 kHz in the
`audio` application) and which is never queried.

Example (2 channels, 48 kHz):
`4f707573 48656164 01 02 000f 80bb0000 0000 00`.

## 4. Skeleton fisbone

None: Opus answers no fisbone. With `%ogg(%opus,%theora)` the skeleton
describes the Theora stream only.

## 5. Secondary headers

The `OpusTags` packet, flushed (one page unless the comments exceed a page):

| field         | encoding                                                        |
| ------------- | --------------------------------------------------------------- |
| magic         | `OpusTags`                                                      |
| vendor length | 32-bit LE                                                       |
| vendor        | `ocaml-opus by the Savonet Team.` (31 bytes)                    |
| comment count | 32-bit, host byte order (little-endian on every supported host) |
| each comment  | 32-bit LE length, then `key=value`                              |

75-byte page with no comment. In the run of
[ogg.md §10](ogg.md#worked-examples) made with `%ogg(%opus)`, the second
link's packet carries `artist=x` then `title=second`.

## 6. Comments

Every metadata pair as `key=value`, in the metadata's list order. No
`ENCODER` tag, no vendor other than the binding's.

## 7. Data input and conversion

Each block (at the codec rate) is appended to the link's FIFO. While the FIFO
holds at least one codec frame, one frame is taken and encoded into one Opus
packet (libopus float API, samples clipped to [-1, 1], at most 4000 bytes per
packet). A packet of 1 byte or less (what DTX produces for silence) is not
put in the stream and does not advance the granulepos. Samples short of a
frame stay in the FIFO until the next block.

## 8. Packets, granule positions and pages

Packet numbers start at 2. The granulepos of a packet is the total 48 kHz
sample count of the packets put so far, including it:
`n × frame_size × (48000 / rate)`, so the first audio packet has granulepos
960 (20 ms) and no pre-skip is added. With DTX, dropped packets make the
granulepos fall behind real time. Pages: libogg's default threshold (about
4 KB; 16–17 packets at the default bitrate on a sine, e.g. pages ending at
15360, 31680, 48000).

## 9. Page end time

granulepos / 48000, whatever the codec rate.

## 10. End of stream

1. If no block was given in the link, one silent sample, with the **frame's**
   channel count, is appended to the FIFO; being shorter than a frame it
   encodes nothing.
2. Then the stream is terminated by the Ogg binding's generic terminator, not
   by the Opus binding's own end-of-stream call: every pending page is
   flushed, then an empty packet with the EOS flag and granulepos = last
   packet's granulepos **+ 1** is put and paged out, and that page is rewritten
   to have **no segment at all** (27 bytes: header only, segment count 0,
   checksum recomputed). Its granulepos header field keeps last + 1.
3. The samples left in the FIFO (less than one frame) are dropped, as are
   the resampler's held samples.

End and start trimming as built:

- the decoder skips 3840 samples at the start of each link, of which only
  the encoder's look-ahead is delay: 3528 samples (73.5 ms) of real audio
  per link are lost at playback (checked: two links of 1.08 s and 1.5 s
  encoded, 2.42 s decoded by FFmpeg);
- the final granulepos is one sample beyond the last packet's end and the
  encoder's look-ahead is not flushed, so the last ≤ 1 frame + 312 samples of
  input are never heard.

Example (the same `%ogg(%opus)` run, link 1):
`#6 off=13953 len=27 gp=51841 flags=E nseg=0`.

## 11. Page sizing

libogg default threshold; `bytes_per_page` ignored (checked: 500 gives the
same 4269-byte pages).

## 12. Failures

- libopus errors at creation or on a control: binding exception (`check`),
  from the first callback of a link, i.e. registration (from `encode` or
  `encode_metadata`);
- libopus encode error: binding exception, from `encode`;
- the empty-stream fallback with a frame channel count different from the
  codec's: a content error from the FIFO, from `encode_metadata` or `stop`
  (read only).

## 13. Concurrency

`opus_encode_float` runs with the OCaml runtime lock released. The
`OpusHead` packet is written into one static 19-byte buffer shared by every
encoder of the process, then copied; creating two Opus encoders at the same
time in two domains races on it ([findings](findings/ogg.md)).

## 14. Tests

`tests/media` (mono, stereo: decodable), `tests/harbor/harbor_output_join_points.liq`
(`opus.ogg`: links), `tests/harbor/icecast_server_ogg_late_listener.liq`.
Pre-skip, trimming and DTX are untested.
