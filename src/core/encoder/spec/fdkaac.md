# `%fdkaac`: AAC through FDK-AAC

Part A. The writer of `%fdkaac`: one AAC stream encoded by libfdk-aac, in a
self-delimiting byte-stream transport (ADTS, LOAS or ADIF). It answers
[encoder.md §10](encoder.md#10-implementation-files) under the same
numbers.

It relies on: the contract ([encoder.md](encoder.md)); format values
([formats.md](formats.md)); sinks ([sink.md](sink.md)); draining
([draining.md](../../source/spec/draining.md)); the fault classes
([faults.md](../../../spec/faults.md)); the integer conversion of
[encoder.md §3.3](encoder.md#33-frames-in).

## 1. Format and options

### 1.1 Parameters

| Parameter                                                     | Value                                                                                                                                 | Default                           | Effect                                                             |
| ------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------- | ------------------------------------------------------------------ |
| `mono`, `stereo` (anonymous); `mono=`, `stereo=`, `channels=` | a channel count with a layout and an AAC channel configuration: 1, 2 or 6; literal                                                    | 2                                 | the stream's channels; 6 is 5.1, taken in the frame's layout order |
| `aot`                                                         | `mpeg4_aac_lc`, `mpeg4_he_aac`, `mpeg4_he_aac_v2`, `mpeg4_aac_ld`, `mpeg4_aac_eld`, `mpeg2_aac_lc`, `mpeg2_he_aac`, `mpeg2_he_aac_v2` | `mpeg4_he_aac_v2`                 | the audio object type                                              |
| `bitrate`                                                     | kbps, positive                                                                                                                        | 64                                | constant bit rate                                                  |
| `vbr`                                                         | 1..5                                                                                                                                  | unset                             | variable bit rate of that mode instead                             |
| `bandwidth`                                                   | Hz, positive, or `"auto"` (any case)                                                                                                  | `"auto"`                          | the coded audio bandwidth                                          |
| `afterburner`                                                 | bool                                                                                                                                  | false                             | the library's slower, higher-quality search                        |
| `sbr_mode`                                                    | bool                                                                                                                                  | false                             | SBR in AAC-ELD; accepted only with `aot="mpeg4_aac_eld"`           |
| `transmux`                                                    | `"adts"`, `"loas"`, `"adif"`                                                                                                          | `"adts"`; `"loas"` for LD and ELD | the transport (§5)                                                 |
| `samplerate`                                                  | 8000, 11025, 12000, 16000, 22050, 24000, 32000, 44100, 48000, 64000, 88200, 96000                                                     | the frame's audio rate, deferred  | the stream's rate                                                  |

- `bitrate` and `vbr` are two rate controls: `bitrate` alone, or by
  default, gives constant bit rate; `vbr` gives variable bit rate. Both
  given explicitly are refused, naming both. Reason: a variable bit rate
  has no target rate, so one of the two would be silently ignored.
- `transmux` values `"raw"`, `"latm"` and `"latm_out_of_band"` are not
  accepted. Reason: none of them delimits its frames or carries its
  configuration in a byte stream, so no sink of this writer receives a
  decodable result; the error says to use `%ffmpeg` with a container (MP4,
  Matroska, MPEG-TS) for those uses.
- The default transport follows the object type: ADTS and ADIF can signal
  only the MPEG-2 and MPEG-4 LC family (with SBR and PS signalled
  implicitly), so LD and ELD default to LOAS. LD or ELD with an explicit
  `"adts"` or `"adif"` is rejected; an MPEG-2 object type with `"loas"` is
  rejected (LOAS carries MPEG-4 audio only).
- HE-AAC v2 takes exactly 2 channels.

### 1.2 When each check runs

Per [formats.md §7](formats.md#7-when-a-format-is-validated):

| Check                                                                                                                                                    | Phase                                     |
| -------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------- |
| channel count; HE-AAC v2 with a channel count other than 2                                                                                               | type checking                             |
| every literal value above, and their combinations (`sbr_mode` without ELD, object type against transport, `bitrate` with `vbr`)                          | type checking                             |
| the same, computed                                                                                                                                       | evaluation                                |
| the object type against the library build (a build without SBR or PS has no HE-AAC), from the library's capability information, read without side effect | evaluation                                |
| a deferred rate                                                                                                                                          | start-up                                  |
| the full configuration against the library's initialisation (object type × rate × channels × bit rate)                                                   | creation; a refusal is a hard error (§15) |
| the format against the output (§2)                                                                                                                       | construction of the output                |

### 1.3 Helpers

| Helper      | Value                                                                                                                                                                                                 |
| ----------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `extension` | `aac` for ADTS and ADIF; `loas` for LOAS                                                                                                                                                              |
| `mime`      | `audio/aac`                                                                                                                                                                                           |
| `bitrate`   | constant rate control: `bitrate × 1000`; `vbr`: none                                                                                                                                                  |
| `codecs`    | `mp4a.40.2` (MPEG-4 LC), `mp4a.40.5` (HE), `mp4a.40.29` (HE v2), `mp4a.40.23` (LD), `mp4a.40.39` (ELD); `mp4a.67` for every MPEG-2 object type, whose core is LC with SBR and PS signalled implicitly |

## 2. Declarations

| Declaration        | ADTS                                            | LOAS             | ADIF                                                                      |
| ------------------ | ----------------------------------------------- | ---------------- | ------------------------------------------------------------------------- |
| streamable         | yes                                             | yes              | no: frames have no start code, and the configuration is only at the start |
| needs seekable     | no                                              | no               | no                                                                        |
| can continue       | yes: frames only                                | yes: frames only | yes: raw data blocks only                                                 |
| in-band metadata   | no                                              | no               | no                                                                        |
| segmentable        | yes, through the HLS packed-audio encoder (§12) | no               | no                                                                        |
| needs no byte sink | no                                              | no               | no                                                                        |
| accepts delegated  | no                                              | no               | no                                                                        |

Explicit parameters refused against the declared capabilities, at the
output's construction: `transmux="adif"` for an output whose sinks are
`live` (a server relays a live stream to readers who start in its middle,
and ADIF cannot be read from there). The default transport is never ADIF,
so nothing is adapted.

## 3. Creation

- Inputs: the configuration, the confirmed capabilities, the output's name
  and the format's position. The initial metadata is not used.
- The library is opened for the channel count, configured with the object
  type, rate, transport, bandwidth, afterburner, SBR mode and rate control,
  with LOAS configured to carry its configuration in every frame, and
  initialised, all before the writer is returned. A parameter or a
  combination the library refuses is a hard configuration error naming the
  output, the parameter the library names, and the format's position.
- When the library sets a bit rate or a bandwidth other than the one
  configured (it bounds them by what the configuration can code), the writer
  logs both values once at `important`, and `bitrate` answers the library's
  (§10).
- The writer reads the frame length and the delay the library reports.
- Creation writes nothing into the sink.

## 4. Input

- The piece's audio is converted from the frame's rate to the stream's by
  the writer's rate converter, a stage of
  [draining.md §1](../../source/spec/draining.md#1-stages), then to signed
  16-bit integers as [encoder.md §3.3](encoder.md#33-frames-in) states,
  interleaved in the library's channel order.
- The library takes one frame length of samples per call (1 024 for LC, 2
  048 for HE and HE v2 at the stream's rate, 480 or 512 for LD and ELD). The
  writer holds fewer than one frame length between calls, in memory not
  set by any environment. No result depends on the piece boundaries
  ([encoder.md §3.3](encoder.md#33-frames-in)).

## 5. Bytes and markers

| Transport | Bytes                                                                  | Header          | Access points           |
| --------- | ---------------------------------------------------------------------- | --------------- | ----------------------- |
| ADTS      | (7-byte ADTS header, one raw data block) × n                           | empty           | before every ADTS frame |
| LOAS      | (start code `0x2B7`, length, AudioMuxElement with StreamMuxConfig) × n | empty           | before every LOAS frame |
| ADIF      | ADIF header, then raw data blocks                                      | the ADIF header | the stream start only   |

- `stream_start(header_length, t₀)` before the first byte, with the ADIF
  header's length for ADIF and 0 otherwise.
- `t` of an access point is the media time of the first input sample the
  frame decodes to: `t₀ + (k × frame length − delay) / rate`, never below
  `t₀`.
- ADTS header: no CRC, one raw data block per frame; the MPEG identifier
  bit is 0 for MPEG-4 object types and 1 for MPEG-2; the profile and
  sampling index are the AAC core's, SBR and PS being signalled implicitly
  in the payload; buffer fullness `0x7FF` in VBR.

## 6. Header forms and preambles

- ADTS and LOAS: the header is empty, and the file form, the streaming form
  and the continuing form are all the frames alone. No field records the
  stream's length.
- ADIF: the header is the same on every fresh sink; it records no length.
- On a `continuing` sink, the writer writes frames only (ADTS, LOAS) or raw
  data blocks only (ADIF, its own ADIF header omitted), taking the existing
  stream to be of its configuration. Nothing is updated at the end.
- No preamble.

## 7. Metadata

Nothing: the transports carry no metadata, and the writer offers no
in-band metadata.

## 8. Delay and padding

- **Delay**: the delay the library reports for its configuration (2 048
  samples for LC, about 7 100 for HE-AAC v2 at 44 100 Hz), plus the rate
  converter's, in samples at the stream's rate.
- **Padding**: `n × frame length − delay − input` at the stream's end.
- None of the transports can signal them
  ([encoder.md §3.7](encoder.md#37-delay-and-padding)); the properties
  give them (§10). Gapless splits are not offered.

## 9. End

`end(clean)`, upstream first:

1. the rate converter is told the input ended; what it returns joins the
   held samples;
2. the held samples are handed to the library as its last, short input;
3. the library is flushed, with room for its output, until it reports the
   end of its stream: every frame it returns, its look-ahead included, is
   written;
4. `end` is written.

No input sample is lost: decoding the stream and trimming the delay and
padding of §8 gives exactly the input sample count at the stream's rate.

A writer ended before any piece writes nothing. `end(abandoned)` releases
the library and the converter and writes nothing more. The library's
encoder is closed exactly once, at `end`.

## 10. Properties

| Property  | Value                                                                 | Known         |
| --------- | --------------------------------------------------------------------- | ------------- |
| `bitrate` | constant rate control: the bit rate the library set (§3); `vbr`: none | from creation |
| `delay`   | §8                                                                    | from creation |
| `padding` | §8                                                                    | at the end    |

Where the library bounded the bit rate, `bitrate` answers the library's
value and differs from the helper's, as
[encoder.md §3.8](encoder.md#38-bitrate) provides.

## 11. Live latency

The library's look-ahead and one frame length of samples, not counted
against `L` ([encoder.md §3.9](encoder.md#39-live-latency-bound)). Every
frame is written as the library returns it.

## 12. Segmenting face

ADTS: [encoder-packed-audio.md](../../outputs/hls/spec/encoder-packed-audio.md).
LOAS and ADIF are not segmentable.

## 13. Delegated destination

Nothing.

## 14. Concurrency

No work of its own. One library encoder per writer; writers of several
outputs run on several domains without coordination.

## 15. Faults

| Fault                                                                                                                                                                        | Class               | When                                |
| ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------- | ----------------------------------- |
| a value out of its set; HE-AAC v2 without 2 channels; `sbr_mode` without ELD; an object type the transport cannot carry; a removed transport; `bitrate` and `vbr` both given | configuration error | type checking or evaluation         |
| an object type the library build does not encode                                                                                                                             | configuration error | evaluation                          |
| a deferred rate out of the set                                                                                                                                               | configuration error | start-up                            |
| `transmux="adif"` for a live output; a non-streamable transport for an output serving readers mid-stream                                                                     | configuration error | construction of the output          |
| the library refuses its configuration or its initialisation                                                                                                                  | configuration error | creation; hard error                |
| the library fails to encode while streaming, or memory runs out                                                                                                              | output fault        | the writer becomes `failed`         |
| the flush fails at a clean end                                                                                                                                               | not escalated       | logged at `severe`; the end goes on |

Every error names the parameter or the library's reason, the output and the
format's position.

## 16. Tests

Harness: a finite source with a click at a known sample and a tone in its
last 100 ms, a decoder that reports raw sample counts, ADTS and LOAS frame
walkers.

1. For LC and HE-AAC v2, at 44 100 Hz and at 48 000 Hz from a 44 100 Hz
   frame: decoding and trimming `delay` and `padding` gives exactly the
   input sample count, the click at its input index to within one sample,
   and the final tone.
2. ADTS and LOAS files are read by a reference decoder with the configured
   profile, rate and channels; every frame of the walker is an access point
   marked by the writer.
3. A shared harbor reader of ADTS and of LOAS starting after 3 s decodes
   from its first byte; `%fdkaac(transmux="adif")` given to a shared harbor
   mount and to `output.icecast` is refused at evaluation.
4. `%fdkaac(channels=1)`, `%fdkaac(transmux="raw")`,
   `%fdkaac(sbr_mode=true)`, `%fdkaac(aot="mpeg4_aac_eld",
transmux="adts")` and `%fdkaac(aot="mpeg2_aac_lc", transmux="loas")`
   fail `--check` with a position; `%fdkaac(aot="mpeg4_aac_eld")` types and
   writes LOAS.
5. With a library build lacking HE-AAC, `%fdkaac` fails at evaluation
   naming the object type and the library.
6. `%fdkaac(bitrate=1000)`: the writer starts, logs the bit rate the
   library set, and `bitrate` answers it.
7. `%fdkaac(vbr=3, bitrate=64)` and `%fdkaac(bitrate=64, vbr=3)` fail
   `--check`, naming both; `%fdkaac(vbr=3)` encodes in VBR mode 3, with no
   `bitrate`.
8. Pieces of random lengths and offsets give the same decoded sample count
   as one piece.
9. One minute of ADTS appended to a one-minute ADTS file decodes as two
   minutes.
