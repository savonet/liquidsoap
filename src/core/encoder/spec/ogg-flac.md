# Ogg FLAC stream encoder

Part A. Answers [ogg-streams.md](ogg-streams.md) for FLAC in Ogg, under the
same numbers. Container rules are [ogg.md](ogg.md); the native FLAC writer is
[flac.md](flac.md). Claims about the FLAC Ogg mapping marked "(from memory)"
are stated from memory.

The codec library encodes FLAC frames and, through its own Ogg layer, forms
the stream's packets and pages at the page sizes it chooses. The muxer
takes those pages whole and interleaves them; it does not form Ogg FLAC
pages. This is the stated exception to pass-2 decision D14
([ogg.md §5.2](ogg.md#52-pages)).

## 1. Format and options

`%ogg(%flac(...))`. The parameters and their checks are those of `%flac`
([flac.md §1](flac.md#1-format-and-options)), plus `bytes_per_page`
([ogg-streams.md §1](ogg-streams.md#1-format-and-options)), accepted only
inside `%ogg`; it has no effect on this stream, whose page sizes are the
library's (§11). The printed form is `%flac(...)`'s, with `bytes_per_page`
when set.

## 2. Configuration and library checks

Those of [flac.md §2](flac.md#2-declarations) for the codec: codec rate
`samplerate`, channels, bits per sample, compression level, checked at the
same phases.

## 3. Creation

Per link: a FLAC encoder in its Ogg form with the channels, bits per
sample, rate, compression level, the link's comments and the serial number
the muxer drew for the stream ([ogg.md §5.5](ogg.md#55-serial-numbers)),
total sample count unknown, writing to the stream encoder with no seek. Its initialisation status is
checked: a refusal is a configuration error (§13), never an empty stream.

## 4. Header packets

The FLAC Ogg mapping, as the library's Ogg layer writes it (from memory):

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

The library hands its output through a write callback that states, per
call, how many samples the output carries: none for the header pages, a
non-zero count for output that carries a frame (one call per frame, from
memory; to verify against the library). The stream encoder takes its
boundaries from those calls, which the binding passes through unsplit and
unmerged with their sample counts: the calls with no sample are the link's
header pages, the others its data pages. It never parses frame or page
boundaries from the bytes. Reason: the library writes one unit per call by
construction, and parsing would need sync-code and CRC scanning.

## 7. Packets

One packet per FLAC frame, formed by the library. Granule position: the
number of samples through the end of the frame, at the codec rate (from
memory), as the library's pages carry it. End time of a page: its granule
position / rate.

## 8. Delay and padding

None: FLAC is lossless with no delay; the last frame is short.

## 9. End

The library is finished: the remaining samples are encoded as a short last
frame, which carries the final granule position, equal to the link's codec
sample count. The stream's last page MUST carry the EOS flag; where the
library's last page does not, the stream encoder sets it and has libogg
recompute that page's CRC (`ogg_page_checksum_set`). The stream always received at least one sample
([ogg-streams.md §6](ogg-streams.md#6-input)).

## 10. Properties

`bitrate`: none. `delay`, `padding`: 0.

## 11. Page constraints

The library's Ogg layer forms the pages and chooses their sizes; the
muxer's page sizing and live-latency closing
([ogg.md §5.3](ogg.md#53-page-sizing), [§11](ogg.md#11-live-latency)) do
not apply to this stream. Each page is written whole, its bytes unchanged
but for the EOS flag of §9 and the file-sink STREAMINFO rewrite of §4.

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
- Every page of the stream carries the serial the muxer drew; its last page
  carries EOS; the header pages are exactly the outputs with no sample.
