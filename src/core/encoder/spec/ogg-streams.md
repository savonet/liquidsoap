# Ogg stream encoders (contract)

Part A. The private seam between the Ogg muxer ([ogg.md](ogg.md)) and the
codecs that fill its logical streams. A stream encoder turns one stream's
share of each piece into packets; the muxer forms every page from them
([ogg.md §5](ogg.md#5-bytes-and-markers)). No stream encoder forms a page,
writes a byte into a sink or sees a marker. Nothing outside the Ogg writer
uses this seam.

One file per codec answers every section below under the same number,
"Nothing." where it does not apply: [Vorbis](ogg-vorbis.md),
[Opus](ogg-opus.md), [Speex](ogg-speex.md), [FLAC](ogg-flac.md),
[Theora](ogg-theora.md).

It relies on: the Ogg vocabulary and the muxer's rules ([ogg.md](ogg.md));
the parameter specification and validation phases
([formats.md](formats.md)); draining
([draining.md](../../source/spec/draining.md)). Mapping claims marked
"(from memory)" are stated from memory.

## Interface

A stream encoder is built for one link and one logical stream.

| Operation          | Given                         | Answers                                                    |
| ------------------ | ----------------------------- | ---------------------------------------------------------- |
| create             | configuration, comments       | the stream encoder, or a fault (§3, §13)                   |
| `headers`          | —                             | the header packets, identification packet first (§4)       |
| `encode(share)`    | the stream's share of a piece | zero or more data packets (§6, §7)                         |
| `finish`           | —                             | the final data packets, at least one (§9)                  |
| `delay`, `padding` | —                             | as [encoder.md §3.7](encoder.md#37-delay-and-padding) (§8) |

Each packet comes with: its bytes, its granule position, its start and end
times, and whether it must begin a page (§7, §11).

## 1. Format and options

Each codec's file owns its parameter specification
([formats.md §3](formats.md#3-the-parameter-specification)). Common to the
audio codecs:

- the channel parameters of [formats.md §4](formats.md#4-typing), literal
  values only; a count the codec or its mapping cannot carry is a type error
  at its position;
- `samplerate=<int>`: the codec rate, the rate of the samples the codec
  encodes. Its default is the codec file's, possibly deferred
  ([formats.md §6](formats.md#6-deferred-values)).

Common to every codec:

- `bytes_per_page=<int>`, in 1..65025, default 4096: the
  `page_target_size` of this logical stream's data pages
  ([ogg.md §5.3](ogg.md#53-page-sizing)). Out of range is refused at the
  phase of its value ([formats.md §7](formats.md#7-when-a-format-is-validated)).

Every accepted parameter has its effect on the bytes; each printed form
lists the configuration's options, omits unset ones, and parses back to an
equal value ([formats.md §10](formats.md#10-equality-printing-serialisation)).

## 2. Configuration and library checks

Each codec file states: the codec rate and channel count it derives, the
checks it runs at each phase, and which of them ask the codec library
without side effect at evaluation (and at start-up for deferred values)
rather than at creation ([formats.md §7](formats.md#7-when-a-format-is-validated)).
A combination the library refuses is never discovered first at a later link.

## 3. Creation

- Each link gets fresh stream encoders: new codec state, new rate converter,
  the link's comments. Nothing survives from one link to the next.
  Reason: a link decodes alone, so its codec must start from a state a
  decoder can start from.
- Creation runs before the link's first byte (at the writer's creation for
  the first link, at the accepted offer's piece for the next ones,
  [ogg.md §3](ogg.md#3-creation), [§7](ogg.md#7-metadata)). It produces every
  header packet (§4).
- A codec file states what it passes its library.

## 4. Header packets

- `headers` answers every header packet of the stream, complete, before any
  data packet exists: the identification packet first, then the others in
  the order the mapping defines. The muxer pages them
  ([ogg.md §5.1](ogg.md#51-link-layout)).
- A header field whose final value is known only at the end of the stream
  takes the value its mapping defines as unknown. Only the muxer revises
  one, Ogg FLAC's STREAMINFO on a file sink
  ([ogg.md §6](ogg.md#6-header-forms-and-preambles)); the stream encoder
  answers the final values at `finish` for it.
- A codec file states each header packet's layout and the fields it sets.

## 5. Comments

- Each stream of a link carries the comments the muxer gives it for that
  link (the initial metadata where [ogg.md §7](ogg.md#7-metadata) item 1
  says so, or the accepted offer) in its comment header, in the Vorbis
  comment layout its mapping uses (from memory: a vendor string, then a
  count of `key=value` UTF-8 strings, lengths 32-bit little-endian).
- The first comment is `ENCODER=` with the software's name and version
  ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles), item 8);
  then one comment per `(key, value)` pair, `key=value`, in the order of
  the metadata, an `encoder` key excepted. The stream encoder adds no other
  comment and derives none from other keys. A key that cannot be a comment field name (empty, containing `=`,
  or with a byte outside 0x20–0x7D, from memory) is not written, logged at
  `info` once per link with the key.
- The vendor string is the codec library's, or for a header the writer
  builds itself, its own identification.
- A link with no metadata carries a comment header with `ENCODER=` alone.

## 6. Input

- **Audio.** The share is the piece's samples, every channel, as floats
  nominally in [-1, 1]. Samples beyond [-1, 1] are clipped where the codec
  takes integers or bounded floats.
- **Channel order.** Channels are handed to the codec in its mapping's order
  for the channel count; the codec file states it.
- **Rate conversion.** When the frame's audio rate differs from the codec
  rate, a converter is a stage before the codec
  ([draining.md §1](../../source/spec/draining.md#1-stages)). Its own delay
  is compensated: the codec receives, over a link, the link's input
  resampled with no leading delay, the number of samples being the link's
  input count times the rate ratio, rounded to nearest, and at least one.
  Reason: the signalled delay is the codec's alone, and every link of at
  least one input sample hands the codec at least one sample, so no codec is
  ever ended empty.
- **Remainders.** Samples short of a codec unit wait for the next share;
  they are drained at the end (§9). No result depends on how the input is
  cut ([encoder.md §3.3](encoder.md#33-frames-in)).
- **Empty share.** A share with no sample (a video-only piece) or no image
  is not handed to the codec. Reason: some libraries read an empty input as
  end of input.
- **Video.** The share is the images of the piece, in order, each one image
  period. Images are adapted to the configured picture size and pixel format
  before the codec sees them ([encoder.md §3.3](encoder.md#33-frames-in)).

## 7. Packets

Each data packet the stream encoder answers carries:

- its bytes. A packet is never empty, except a data packet to which the
  codec's mapping gives a meaning (a Theora repeated frame);
- its granule position, as the mapping defines;
- its start and end times: the media time, from the link's start, of its
  first and last sample or image, net of the signalled delay. The muxer
  interleaves by them ([ogg.md §5.4](ogg.md#54-interleaving));
- whether it must begin a page (§11).

Every packet the codec produces is answered, in order, including the short
packets of discontinuous transmission: a packet the codec counts in the
stream's time is never dropped, so granule positions follow media time.

## 8. Delay and padding

The codec file states what it signals, from values the library reports
([ogg.md §8](ogg.md#8-delay-and-padding)), and how: a header field, granule
positions, or none. The decoded result of a link, trimmed as signalled,
holds exactly the link's input samples, sample `n` at position `n`.

## 9. End

`finish` drains, in order: the rate converter's tail, the samples short of a
codec unit (sent short where the codec accepts a short final unit, else
padded with silence and the padding signalled), the codec's look-ahead
([draining.md §2–§4](../../source/spec/draining.md#2-the-rule)). It answers
at least one data packet, the last of which carries the stream's final
granule position. Nothing given to the stream encoder is lost. A codec file
states the final granule position and, where the stream could have received
nothing, what it encodes.

## 10. Properties

The codec file states whether its configuration fixes a nominal bit rate
(the value then answers `bitrate` from creation and the format helper), and
the values of `delay` and `padding`.

## 11. Page constraints

The muxer's rules ([ogg.md §5](ogg.md#5-bytes-and-markers)) meet every
mapping's requirements (from memory: the identification packet alone on the
BOS page, the other header packets starting on a new page, the first data
packet starting on a new page). A codec file states any further
requirement, such as packets that must begin a page.

## 12. Concurrency

- A stream encoder is used by one writer's calls only
  ([ogg.md §14](ogg.md#14-concurrency)).
- A codec file states any state its library or binding shares across
  encoders of the process. Shared mutable state is reached under a lock, or
  the stream encoder does not use it.

## 13. Faults

The codec file lists what its library refuses and when, mapped to the rows
of [ogg.md §15](ogg.md#15-faults): refusal of the configuration
(configuration error), lack of a resource (output fault), one share refused
(item fault), an error that leaves the codec unusable (output fault).

## 14. Tests

The codec file names its specific checks; the common ones are
[ogg.md §16](ogg.md#16-tests).
