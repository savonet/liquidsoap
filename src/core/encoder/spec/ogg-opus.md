# Ogg Opus stream encoder

Part A. Answers [ogg-streams.md](ogg-streams.md) for Opus, under the same
numbers. Container rules are [ogg.md](ogg.md). The writer builds the Ogg
Opus header packets itself, per RFC 7845; claims about it marked
"(from memory)" are stated from memory.

## 1. Format and options

`%opus(...)`, also `%ogg(%opus(...))`.

| Parameter         | Values                                                                              | Default         |
| ----------------- | ----------------------------------------------------------------------------------- | --------------- |
| `application`     | `"voip"`, `"audio"`, `"restricted_lowdelay"`                                        | `"audio"`       |
| `complexity`      | int 0..10                                                                           | the library's   |
| `max_bandwidth`   | `"narrow_band"`, `"medium_band"`, `"wide_band"`, `"super_wide_band"`, `"full_band"` | the library's   |
| `frame_size`      | ms, one of 2.5, 5, 10, 20, 40, 60                                                   | 20              |
| `samplerate`      | 8000, 12000, 16000, 24000, 48000                                                    | 48000           |
| `bitrate`         | int kbps in 1..512, `"auto"`, `"max"`                                               | `"auto"`        |
| channels          | 1 or 2                                                                              | 2               |
| `vbr`             | `"none"`, `"constrained"`, `"unconstrained"`                                        | `"constrained"` |
| `signal`          | `"voice"`, `"music"`                                                                | the library's   |
| `dtx`             | bool                                                                                | false           |
| `phase_inversion` | bool                                                                                | true            |
| `bytes_per_page`  | [ogg-streams.md §1](ogg-streams.md#1-format-and-options)                            | 4096            |

- Each refusal names the parameter and the accepted values; the `bitrate`
  message states 1..512.
- Channels beyond 2 are a type error: the writer uses channel mapping family
  0 only (RFC 7845, from memory).
- Printed form: every set option, numbers unquoted, e.g.
  `%opus(application="audio",vbr="constrained",bitrate="auto",channels=2,complexity=3,samplerate=48000,frame_size=20.,dtx=false,phase_inversion=true)`;
  options at the library's default are omitted.

## 2. Configuration and library checks

- Codec rate: `samplerate`. Codec frame: `frame_size × samplerate / 1000`
  samples. Every value is checked statically; libopus accepts every
  combination of §1.

## 3. Creation

Per link: a libopus encoder for the rate, channels and application, then
the controls bitrate, VBR and its constraint, complexity, maximum bandwidth,
signal, DTX and phase inversion, as set. The encoder's look-ahead is then
queried from the library (§8).

## 4. Header packets

1. `OpusHead` (RFC 7845 §5.1, from memory), 19 bytes, all fields
   little-endian: magic `OpusHead`, version 1, channel count, pre-skip
   (16 bits, 48 kHz samples, §8), input sample rate (32 bits: the frame's
   audio rate, the rate of the samples the writer received), output gain 0,
   channel mapping family 0.
2. `OpusTags` (RFC 7845 §5.2, from memory): magic `OpusTags`, vendor string
   (libopus's version string), comment count and comments
   ([ogg-streams.md §5](ogg-streams.md#5-comments)), lengths and count 32-bit
   little-endian.

## 5. Comments

As [ogg-streams.md §5](ogg-streams.md#5-comments), in `OpusTags`.

## 6. Input

Floats at the codec rate, stereo in L R order, clipped to [-1, 1]. Whole
codec frames are encoded; the remainder waits for the next share.

## 7. Packets

- One packet per codec frame, every packet libopus produces, those of one or
  two bytes that DTX produces included (valid Opus packets, RFC 6716, from
  memory).
- Granule position (RFC 7845 §4, from memory): the count of 48 kHz samples
  of every packet so far, this one included, pre-skip counted; at a codec
  rate below 48 kHz, codec samples × 48000 / rate.
- Times: end time = (granule position − pre-skip) / 48000.

## 8. Delay and padding

- Delay: pre-skip = the encoder's look-ahead as libopus reports it,
  converted to 48 kHz samples. Never a constant. Reason: RFC 7845 has the
  decoder discard exactly pre-skip samples (from memory); any larger value
  discards real audio at the start of every link. Pre-skip is not the
  decoder's seek pre-roll.
- Padding: the final granule position is pre-skip plus the link's codec
  sample count in 48 kHz samples; the decoder trims the rest of the last
  packets (RFC 7845 end trimming, from memory).
- `delay`: the look-ahead at the codec rate. `padding`: coded samples minus
  look-ahead minus input samples, at the codec rate, known at the end.

## 9. End

The remainder and the look-ahead are drained: silence is appended until the
encoded frames cover the link's samples plus the look-ahead, and those frames
are encoded. The last packet carries the final granule position of §8, which
never exceeds the samples the packets decode to. The final page therefore
holds at least one packet, and its granule position is above the previous
page's.

## 10. Properties

`bitrate`: an integer `bitrate` × 1000 from creation; `"auto"` and `"max"`:
none.

## 11. Page constraints

Nothing beyond [ogg-streams.md §11](ogg-streams.md#11-page-constraints).

## 12. Concurrency

The header packets are built in per-encoder memory; no buffer or table is
shared between encoders.

## 13. Faults

- libopus refusing creation or a control for a configuration of §1: a
  configuration error (none is expected).
- An encode error on one frame: item fault.

## 14. Tests

- A click at input sample `n`, after silence: decoded at sample `n` to within
  one sample, for every `application` and every `samplerate`, in the first
  link and after ten metadata changes.
- Decoded length equals input length to within one sample per link.
- Pre-skip equals the library's look-ahead at 48 kHz.
- `dtx=true` on 10 s of silence then a click: granule positions advance with
  media time and the click is at its input position.
- Two encoders created at once on two domains produce correct headers.
