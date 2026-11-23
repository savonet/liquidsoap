# Ogg Speex stream encoder

Part A. Answers [ogg-streams.md](ogg-streams.md) for Speex, under the same
numbers. Container rules are [ogg.md](ogg.md). Claims about the Speex Ogg
mapping and its reference encoder marked "(from memory)" are stated from
memory.

## 1. Format and options

`%speex(...)`, also `%ogg(%speex(...))`.

| Parameter           | Values                                                     | Default                                           |
| ------------------- | ---------------------------------------------------------- | ------------------------------------------------- |
| `stereo`, `mono`    | anonymous or bool                                          | stereo                                            |
| `mode`              | `"narrowband"`, `"wideband"`, `"ultra-wideband"`, any case | from `samplerate` when it is set, else narrowband |
| `samplerate`        | 8000, 16000, 32000                                         | the mode's rate                                   |
| `quality`           | int 0..10: constant quality                                | 7                                                 |
| `vbr`               | int 0..10: variable bit rate at that quality               | unset                                             |
| `abr`               | int, bits per second: average bit rate                     | unset                                             |
| `frames_per_packet` | int 1..10                                                  | 1                                                 |
| `complexity`        | int 1..10                                                  | the library's                                     |
| `dtx`, `vad`        | bool                                                       | false                                             |
| `bytes_per_page`    | [ogg-streams.md §1](ogg-streams.md#1-format-and-options)   | 4096                                              |

- Each mode has its rate: narrowband 8000, wideband 16000, ultra-wideband 32000. `mode` and `samplerate` both set and not matching are refused,
  naming both. Reason: a Speex mode is designed for one rate; any other
  rate is encoded with the wrong band split.
- `quality`, `vbr` and `abr` set one rate control: the last given wins.
- Printed form: every set option, modes quoted (`mode="wideband"`), the rate
  control as written (`quality=7`, `vbr=5` or `abr=16000`).

Migration: the default rate becomes the mode's (8000 for the default
narrowband) instead of the frame's audio rate; the output is resampled to it.
`%speex(samplerate=44100)` is refused; use `samplerate=32000` or a mode.

## 2. Configuration and library checks

Codec rate: `samplerate`; codec frame: the frame size libspeex reports for
the mode (from memory: 160, 320, 640 samples). Every value is checked
statically.

## 3. Creation

Per link: a libspeex encoder for the mode, then the rate control, complexity,
DTX, VAD and the sampling rate; the encoder's look-ahead is queried (§8).

## 4. Header packets

1. The Speex header the library builds for the mode, rate, channels, VBR
   flag and frames per packet, 80 bytes, with no extra header.
2. The comment packet ([ogg-streams.md §5](ogg-streams.md#5-comments)),
   vendor string the libspeex version.

## 5. Comments

As [ogg-streams.md §5](ogg-streams.md#5-comments), `ENCODER=` included.
No title is derived from the output's name or address.

## 6. Input

Samples are converted to 16-bit integers by the media library's existing
conversion ([ogg-streams.md §6](ogg-streams.md#6-input)).
Stereo is encoded with the library's stereo coding. Whole frames are encoded;
the remainder waits.

## 7. Packets

One packet per `frames_per_packet` frames. Granule position (Speex reference
encoder convention, from memory): the codec samples of every packet so far,
this one included, minus the encoder's look-ahead. End time: granule position
/ rate.

## 8. Delay and padding

- Delay: the look-ahead, signalled by the granule positions of §7 (from
  memory: readers trim the start by the difference between the first page's
  samples and its granule position).
- Padding: the final granule position equals the link's codec sample count.
- `delay` the look-ahead, `padding` coded samples minus look-ahead minus input
  samples, at the codec rate.

## 9. End

Silence is appended until the frames cover the link's samples plus the
look-ahead and fill the last packet; they are encoded. The last packet
carries the final granule position.

## 10. Properties

`bitrate`: `abr` from creation; otherwise none.

## 11. Page constraints

Nothing.

## 12. Concurrency

libspeex keeps no state shared between encoders.

## 13. Faults

A frame libspeex fails to encode: item fault.

## 14. Tests

- Each mode at its rate, mono and stereo, from a 44.1 kHz frame: decodable,
  decoded length equals input length to within one sample.
- `frames_per_packet=4` with a link length that is not a multiple of four
  frames: nothing lost.
- `%speex(mode="wideband", samplerate=8000)` fails `--check`.
