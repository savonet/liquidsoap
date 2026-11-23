# Ogg FLAC stream encoder

Part A. Answers [ogg-streams.md](ogg-streams.md) for FLAC in Ogg, under the
same numbers. Container rules are [ogg.md](ogg.md); the native FLAC writer is
[flac.md](flac.md). Claims about the FLAC Ogg mapping marked "(from memory)"
are stated from memory.

The codec library encodes FLAC frames; the stream encoder takes them as
packets and the muxer forms every page. The library's own Ogg layer is not
used.

## 1. Format and options

`%ogg(%flac(...))`. The parameters and their checks are those of `%flac`
([flac.md §1](flac.md#1-format-and-options)), plus `bytes_per_page`
([ogg-streams.md §1](ogg-streams.md#1-format-and-options)), accepted only
inside `%ogg`. The printed form is `%flac(...)`'s, with `bytes_per_page`
when set.

## 2. Configuration and library checks

Those of [flac.md §2](flac.md#2-declarations) for the codec: codec rate
`samplerate`, channels, bits per sample, compression level, checked at the
same phases.

## 3. Creation

Per link: a FLAC encoder with the channels, bits per sample, rate,
compression level and the link's comments, total sample count unknown,
writing to the stream encoder with no seek. Its initialisation status is
checked: a refusal is a configuration error (§13), never an empty stream.

## 4. Header packets

The FLAC Ogg mapping (from memory):

1. the mapping packet: `0x7F`, `FLAC`, mapping version 1.0, the number of
   header packets that follow (16-bit big-endian, here 1), `fLaC`, then the
   STREAMINFO metadata block;
2. the VORBIS_COMMENT metadata block, flagged as the last metadata block
   ([ogg-streams.md §5](ogg-streams.md#5-comments)), vendor string the
   library's.

STREAMINFO is written with the values FLAC defines as unknown for what is
known only at the end: total samples 0, MD5 signature all zero, frame sizes 0. On a file sink the muxer SHOULD rewrite them at the link's end
([ogg.md §6](ogg.md#6-header-forms-and-preambles)); `finish` answers the
link's total sample count, frame size bounds and MD5 signature for it. On
every other sink they stay unknown: a link's length is carried by its
granule positions.

## 5. Comments

As [ogg-streams.md §5](ogg-streams.md#5-comments), in the VORBIS_COMMENT
block.

## 6. Input

Samples are converted to integers of the configured width
([encoder.md §3.3](encoder.md#33-frames-in)), in the FLAC channel order
for the count ([flac.md §4](flac.md#4-input)).

The library hands its output through a write callback. The stream encoder
forms packets from the content: each metadata block by its header and length,
each frame from its start to the next frame's start or the stream's end. It
never takes a callback call, or the size of a binding's buffer, as a packet
or page boundary. Reason: a library's write call is not a unit of the format;
a binding may split one write into several or merge several into one.

## 7. Packets

One packet per FLAC frame. Granule position: the number of samples through
the end of the frame, at the codec rate (from memory). End time: granule
position / rate.

## 8. Delay and padding

None: FLAC is lossless with no delay; the last frame is short.

## 9. End

The library is finished: the remaining samples are encoded as a short last
frame, which carries the final granule position, equal to the link's codec
sample count. The stream always received at least one sample
([ogg-streams.md §6](ogg-streams.md#6-input)).

## 10. Properties

`bitrate`: none. `delay`, `padding`: 0.

## 11. Page constraints

Nothing.

## 12. Concurrency

The library keeps no state shared between encoders.

## 13. Faults

- Initialisation refused for the configuration: configuration error,
  reported before any byte.
- A frame the library fails to encode: output fault.

## 14. Tests

- At the highest compression with a metadata change every second on varied
  content: every page has a valid CRC and the length its segment table
  states, every link ends with an EOS page, no call raises.
- The decoded result equals the input bit for bit.
- On a live sink, STREAMINFO of every link: total samples 0, MD5 zero. On a
  file sink: each link's total and MD5 verify (or, where the writer skipped
  the SHOULD, stay 0 and zero).
- `%flac(bytes_per_page=1000)` outside `%ogg` fails `--check`.
