# Ogg Vorbis stream encoder

Part A. Answers [ogg-streams.md](ogg-streams.md) for Vorbis, under the same
numbers. Container rules are [ogg.md](ogg.md). Mapping claims marked
"(from memory)" refer to the Vorbis I specification.

## 1. Format and options

Three names; each alone is `%ogg(<it>)`.

| Name          | Mode                                               | Parameters                                                                                                         |
| ------------- | -------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| `%vorbis`     | variable bit rate by quality                       | `quality`: float (an int is read as a float) in [-0.2, 1], default 0.3                                             |
| `%vorbis.cbr` | constant bit rate                                  | `bitrate`: kbps, positive, default 128                                                                             |
| `%vorbis.abr` | managed: average with optional minimum and maximum | `bitrate`, `min_bitrate`, `max_bitrate`: kbps, positive, each unset by default; when none is set, `bitrate` is 128 |

Common: the channel parameters, 1 to 8 channels, default 2; `samplerate`,
default the frame's audio rate (deferred); `bytes_per_page`
([ogg-streams.md §1](ogg-streams.md#1-format-and-options)).

- `%vorbis.abr` with `min_bitrate` above `bitrate` or `bitrate` above
  `max_bitrate` is refused at the phase of the values.
- Printed forms: `%vorbis(quality=0.3,channels=2,samplerate=44100)`,
  `%vorbis.cbr(bitrate=128,channels=2,samplerate=44100)`,
  `%vorbis.abr(bitrate=128,channels=2,samplerate=44100)` with each set ABR
  rate listed and unset ones omitted.

Migration: `%vorbis.abr` with none of its rates, which failed when the output
started, encodes at an average of 128 kbps.

## 2. Configuration and library checks

- Codec rate: `samplerate`. Channels: the type's count.
- Whether libvorbis accepts the mode, rates and channels together is asked
  of the library with a setup it then discards, without encoding: at
  evaluation when the rate is explicit, at start-up when it is the deferred
  default. A refusal is a configuration error naming the parameters.

## 3. Creation

Per link: a libvorbis encoder set up in the configured mode (quality; or
minimum, nominal and maximum rates in bits per second, all three equal for
`%vorbis.cbr`, an unset one left unset), with the link's comments.

## 4. Header packets

The three Vorbis headers libvorbis produces at creation: identification,
comment, setup (from memory: 30-byte identification packet starting
`\x01vorbis`).

## 5. Comments

The comment header libvorbis builds from the link's comments
([ogg-streams.md §5](ogg-streams.md#5-comments)); vendor string libvorbis's.

## 6. Input

Float samples go to libvorbis as given, in the Vorbis channel order for the
count (from memory: 3 L C R; 4 FL FR RL RR; 5 FL C FR RL RR; 6 FL C FR RL RR
LFE; 7 FL C FR SL SR RC LFE; 8 FL C FR SL SR RL RR LFE). No empty input is
ever handed to libvorbis except to end the stream (§9): libvorbis reads it
as end of input.

## 7. Packets

The packets libvorbis produces, with its granule positions: the sample
count at the codec rate at the end of each packet (from memory).

## 8. Delay and padding

Vorbis has no encoder delay to signal: decoded samples are aligned with the
input. Padding is signalled by the final granule position, equal to the
link's codec sample count (from memory: Vorbis I readers trim the last
packet to it). `delay` is 0; `padding` the difference between the samples
the last packets decode to and the final granule position.

## 9. End

Signals end of input to libvorbis, which flushes its look-ahead and produces
the last packets, the last carrying the exact final granule position. The
stream always received at least one sample
([ogg-streams.md §6](ogg-streams.md#6-input)).

## 10. Properties

`bitrate`: `%vorbis.cbr` its `bitrate`, `%vorbis.abr` its `bitrate` (average,
128 when no rate is set), in bits per second; `%vorbis` none.

## 11. Page constraints

Nothing.

## 12. Concurrency

libvorbis keeps no state shared between encoders.

## 13. Faults

- Setup refused: configuration error, at evaluation or start-up (§2), never
  first at a later link.
- Analysis error from libvorbis: output fault.

## 14. Tests

- Every mode with 1, 2 and 6 channels decodes; channel order checked with a
  tone per channel.
- `%vorbis.abr` with no rate encodes at an average near 128 kbps.
- A combination libvorbis refuses (`%vorbis.cbr(bitrate=8)` at 48 kHz
  stereo) fails before the output starts, naming the parameter.
- A frame rate different from `samplerate`: the decoded length equals the
  input length at the codec rate to within one sample.
