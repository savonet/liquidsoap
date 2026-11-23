# Ogg stream encoders (contract)

The seam between the Ogg muxer ([ogg.md](ogg.md)) and the codecs that fill
its logical streams. One file per codec answers every section below under the
same number: [Vorbis](ogg-vorbis.md), [Opus](ogg-opus.md),
[Speex](ogg-speex.md), [FLAC](ogg-flac.md), [Theora](ogg-theora.md). As
built, the seam is internal to the Ogg encoder; nothing else uses it.

A codec provides two levels of object:

- a **track maker**, applied once per `%ogg` instance to the codec's format,
  giving a _track_: a per-piece encode function and a per-link `reset`;
- a **stream encoder**, built by each `reset` for one link: a record of six
  callbacks the muxer calls on one logical stream.

## 1. Format and options

Each codec has its own `%codec(...)` syntax, parsed into a format value with
defaults, validated at parse time with a language error at the parameter's
position. Common to all audio codecs:

- `stereo`, `mono` as anonymous arguments, and `stereo=<bool>`,
  `mono=<bool>` labelled (`mono=false` means 2 channels);
- `samplerate=<int>` (Vorbis, Speex, FLAC default to the frame audio rate,
  evaluated lazily; Opus defaults to 48000);
- `bytes_per_page=<int>`: accepted by all five codecs, without effect
  ([ogg.md §10 _Page sizing_](ogg.md#page-sizing)).

Any other argument raises the generic "unknown parameter" error of the
encoder parser. A codec file lists its parameters, defaults, validation and
printed form.

## 2. Instance creation

The track maker computes, once per `%ogg` instance:

- for audio: the codec's channel count and target sample rate (forcing a lazy
  default), and a resampler from the frame audio rate to that rate;
- for video: nothing (Theora takes everything from its format at `reset`).

`reset(muxer, metadata)` runs at the start of every link: it builds a fresh
codec encoder (with the link's comment metadata), wraps it in a stream
encoder and registers it in the muxer, which creates the logical stream with
a new serial and immediately calls §3. No codec state survives from one link
to the next, except the resampler's.

## 3. Identification header (BOS page)

Callback `header_encoder(stream) → page`, called once at registration. It puts
the codec's identification header packet in the logical stream and returns
the page that carries it, alone, with the BOS flag and granulepos 0. The muxer
emits it at once.

## 4. Skeleton fisbone

Callback `fisbone_packet(stream) → packet?`, called once per link, only when
the instance uses a skeleton (audio and video). The packet is the codec's
`fisbone` (Ogg Skeleton 3.0) for the stream's serial: identifier
`fisbone\0`, message header offset 44, serial, number of header packets,
granule rate, start granule 0, preroll, granule shift, then the message
header `Content-type: <mime>\r\n`. A codec may answer none.

## 5. Secondary headers

Callback `stream_start(stream) → pages`, called once per link when the link
starts (lazily, at the first `encode` of the link). It puts the remaining
header packets (comments, setup) and returns every page they fill, flushed
(header pages), in page-sequence order.

## 6. Comments

Each link's comment header carries that link's metadata: the instance's
initial metadata for the first link, the `encode_metadata` argument for every
later one. Which metadata keys reach the encoder (the exported set, cover art
handling) is the generic seam's ([encoder.md](encoder.md)). Each codec states
the vendor string, the comment order and any key it adds or derives.

## 7. Data input and conversion

Callback `data_encoder(block, stream, add_page)`:

- audio: `block` is non-interleaved float PCM at the codec rate, one array per
  channel, with an offset and a length, values nominally in [-1, 1]; it can be
  of any length, zero included;
- video: `block` is an array of YUV 4:2:0 images at the frame size, with an
  offset and a length (always the whole array);
- the codec encodes the block, putting packets into `stream`; a codec that
  forms pages itself hands them to `add_page`, which queues them for the
  muxer in order.

The muxer then assembles further pages from `stream` (ogg.md §10
_Interleaving_). A codec states its internal buffering, conversions (sample
format, clipping), and how many samples it consumes per call.

## 8. Packets, granule positions and pages

Each codec states the granulepos it gives its packets, which pages it forms
itself, and how its page sizes come out (libogg's default threshold or its
own).

## 9. Page end time

Callback `end_of_page(page) → time?`: the media time, in seconds from the
start of the link, at which the page's last completed packet ends, derived
from the page granulepos; _unknown_ when the granulepos is negative (no packet
completes on the page). This is the only clock the muxer interleaves by.

## 10. End of stream

Callback `end_of_stream(stream) → pages`, called once when the link ends
(metadata change or stop), unless the libogg stream has already seen an EOS
packet. It delivers what the codec still holds, marks the end of the logical
stream, and returns pages to queue (it may also leave packets in the stream
for the muxer to assemble). A codec states:

- what it does when no data was encoded in the link (all codecs but Speex
  encode a token input so that the stream carries at least one data packet);
- the final granulepos and how it relates to the samples given (end
  trimming);
- what is lost.

## 11. Page sizing

`bytes_per_page` is passed to the muxer at registration as the fill threshold
for this logical stream. It has no effect (ogg.md §10). A codec states
whether its page sizes depend on libogg's default threshold at all.

## 12. Failures

Every exception from a callback propagates through the muxer and the
instance to the caller ([ogg.md §11](ogg.md#11-concurrency-and-failures)). A
codec lists what raises, from which call.

## 13. Concurrency

Callbacks run on the thread of the instance call that triggers them, under
the instance's serialisation. A codec states whether it releases the OCaml
runtime lock while encoding and whether it shares global state.

## 14. Tests

See [ogg.md §14](ogg.md#14-tests); a codec file names what is specific to it.
