# Ogg writer (`%ogg`)

Part A. The native Ogg container writer: its format value, its declarations,
the links it writes, how its muxer forms and orders pages, in-band metadata
as new links, delay and padding per link, its end, its latency bound and its
faults. It answers [encoder.md §10](encoder.md#10-implementation-files)
under the same numbers.

The codecs that fill its logical streams are stream encoders, private to
this writer: they submit packets into logical streams the muxer owns and
pages, Ogg FLAC excepted (§5.2)
([ogg-streams.md](ogg-streams.md); one file per codec:
[Vorbis](ogg-vorbis.md), [Opus](ogg-opus.md), [Speex](ogg-speex.md),
[FLAC](ogg-flac.md), [Theora](ogg-theora.md)).

It relies on: the seam's vocabulary, markers, header forms, metadata and
faults ([encoder.md](encoder.md)); format values and their phases
([formats.md](formats.md)); sinks ([sink.md](sink.md)); draining
([draining.md](../../source/spec/draining.md)); the fault classes
([faults.md](../../../spec/faults.md)).

Claims about RFC 3533 (Ogg), RFC 5334 (Ogg media types), RFC 7845 (Ogg Opus)
and the Vorbis, Theora, Speex and FLAC Ogg mappings marked "(from memory)"
are stated from memory; a reimplementation checks them against the
documents.

## Vocabulary

Terms of the Ogg format (RFC 3533, from memory), used only in the Ogg files.

| Term             | Meaning                                                                                                                                                                                                                                                                                                |
| ---------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Page             | The unit of an Ogg physical stream: a 27-byte header, a segment table of up to 255 lacing values, then a body whose length is their sum.                                                                                                                                                               |
| Logical stream   | The pages of one serial number within a link: one codec, one track of the format.                                                                                                                                                                                                                      |
| Link             | A group of logical streams that begin together (all their BOS pages first) and all end (EOS) before the next group begins. A link is one **stream** in the sense of [encoder.md](encoder.md#vocabulary): it decodes alone from its first byte. A physical stream of several links is a chained stream. |
| BOS, EOS         | The beginning-of-stream and end-of-stream flags of a page.                                                                                                                                                                                                                                             |
| Packet           | A codec's unit of data, carried in one or more lacing segments of one or more pages. Header packets precede data packets in each logical stream.                                                                                                                                                       |
| Granule position | The 64-bit position a page carries: the codec-defined position of the last packet that ends on the page, or −1 when none ends there.                                                                                                                                                                   |
| Header pages     | The pages that carry a link's header packets. Data pages carry the rest.                                                                                                                                                                                                                               |
| End time         | Of a packet: the media time, from its link's start, at which its content ends, net of the signalled delay ([ogg-streams.md §7](ogg-streams.md#7-packets)). Of a page: the end time of the last packet that ends on it, else that of the packet it carries part of.                                     |
| Stream encoder   | The codec part of one logical stream ([ogg-streams.md](ogg-streams.md)).                                                                                                                                                                                                                               |

## 1. Format and options

- `%ogg(s1, s2)` takes at most one audio stream (`%vorbis`, `%vorbis.cbr`,
  `%vorbis.abr`, `%opus`, `%speex`, `%flac`) and at most one `%theora`, in
  any order ([formats.md §2](formats.md#2-names-and-kinds)). It takes no
  other parameter. A second audio or video stream, any other sub-encoder and
  any labelled or anonymous parameter are refused at type checking, naming
  the parameter.
- `%ogg` with no stream (`%ogg`, `%ogg()`) is refused at type checking: an
  Ogg format needs at least one stream. The error carries the format's
  position.
- `%vorbis(...)`, `%vorbis.cbr(...)`, `%vorbis.abr(...)`, `%opus(...)`,
  `%speex(...)` and `%theora(...)` written alone are the format
  `%ogg(<that stream>)`. `%flac` alone is the native FLAC writer
  ([flac.md](flac.md)); Ogg FLAC is `%ogg(%flac(...))`.
- Each stream's parameters are its codec file's §1. `%theora` is
  deprecated ([formats.md §2](formats.md#2-names-and-kinds)): accepted,
  with a warning.
- **Type.** The audio stream decides the channel count
  ([formats.md §4](formats.md#4-typing)); `%theora` adds a video field.
  Audio only: `format(audio=pcm(n))`; video only: `format(video=…)`; both:
  `format(audio=pcm(n), video=…)`.
- **Printed form.** `%ogg(<audio>, <video>)`, each stream present printed in
  its own form, audio first; a format with one stream prints as
  `%ogg(<stream>)`. Every stream is printed, a video stream without audio
  included, and the printed form parses back to an equal value
  ([formats.md §10](formats.md#10-equality-printing-serialisation)).

Migration: `%ogg(%vorbis, %opus)`, `%ogg(%mp3)` and `%ogg(foo=1)`, accepted
with the extra parameter ignored, are refused; remove the extra parameter.
`%ogg()`, which failed type checking with an internal error, fails with a
positioned error.

## 2. Declarations

The same for every Ogg configuration:

| Declaration        | Value | Reason                                                                          |
| ------------------ | ----- | ------------------------------------------------------------------------------- |
| streamable         | yes   | Every link begins with its header pages; the writer marks access points (§5.6). |
| needs seekable     | no    | The writer seeks only to finalise Ogg FLAC on a file sink (§6).                 |
| can continue       | yes   | It continues an existing stream by starting a new link (§6).                    |
| in-band metadata   | yes   | A new link per accepted offer (§7).                                             |
| segmentable        | no    | No segmented output takes Ogg.                                                  |
| needs no byte sink | no    |                                                                                 |
| accepts delegated  | no    |                                                                                 |

- Ogg has one header form and no preamble (§6). Three things depend on the
  sink's confirmed capabilities: the tags of the first link (§7), the
  finalisation of Ogg FLAC on a file sink (§6), and the latency bound on a
  `live(L)` sink (§11).
- No parameter of an Ogg format contradicts a capability, so none is refused
  at the output's check ([encoder.md §5.3](encoder.md#53-the-check)).

## 3. Creation

1. Inputs used: the configuration, the sink's confirmed capabilities, the
   initial metadata, whether the output offers metadata in band (§7), the
   output's name for logs.
2. The writer builds the stream encoders of its first link at creation
   ([ogg-streams.md §3](ogg-streams.md#3-creation)), video first, then
   audio, and draws their serial numbers (§5.5). A library that refuses the
   configuration therefore does so before any byte exists
   ([encoder.md §2.2](encoder.md#22-creation)). Creation writes nothing into
   the sink.
3. The checks a stream's library can answer without side effect run at
   evaluation, and at start-up for deferred values; each codec file's §2
   lists them. What remains is checked here.
4. A library refusal that follows from the configuration and the build is a
   configuration error, a hard error naming the output, the stream, the
   parameter and the format's position; a failure for lack of a resource is
   an output fault (§15). A creation that fails part way releases every
   stream encoder it built.

## 4. Input

- `write` hands each logical stream its share of the piece: the audio stream
  the piece's samples of every channel of the format, the video stream the
  images whose positions fall in the piece. What each stream encoder does
  with them (rate conversion, sample format, image adaptation) is
  [ogg-streams.md §6](ogg-streams.md#6-input).
- A piece of a format with audio holds at least one sample
  ([encoder.md §3.3](encoder.md#33-frames-in)); a piece may hold no image.
- The decoded result never depends on the piece boundaries. Page boundaries
  may (§11), and they change no decoded sample or image.

## 5. Bytes and markers

### 5.1 Link layout

Every link has this layout, in byte order:

1. the BOS page of each logical stream, video first, then audio. Each BOS
   page carries that stream's identification header packet and nothing
   else. Reason: a reader that sniffs the first page identifies the file as
   video when it has video (Theora mapping, from memory);
2. the remaining header packets of each logical stream, in the same order,
   each stream's starting on a new page; the last header page of each
   stream is complete before any data page of the link;
3. data pages, interleaved (§5.4); the last page of each logical stream
   carries the EOS flag (§9).

Every BOS page of a link precedes every other page of it, and every header
page precedes every data page (RFC 3533 and the Vorbis and Theora mappings,
from memory). A link has exactly one logical stream per stream of the
format, and no other: no skeleton. Reason: no mapping needs a skeleton; it
complicates the header and the end of every link, and readers that cannot
identify it reject the file.

### 5.2 Pages

The muxer owns each logical stream's packet state and forms its pages:
stream encoders submit packets into it, and a stream encoder asks for a page
start by flushing the stream through the muxer
([ogg-streams.md](ogg-streams.md#interface)).

**Ogg FLAC exception.** The FLAC stream's pages are formed by the FLAC
library's own Ogg layer, at the page sizes it chooses; the muxer takes them
whole, gives the library the stream's serial number (§5.5), and interleaves
them with the other streams' pages (§5.4)
([ogg-flac.md §11](ogg-flac.md#11-page-constraints)). This is a stated
exception to pass-2 decision D14 ("stream encoders produce packets; the
muxer forms every page, Ogg FLAC included"). Reason: the library's Ogg layer
already writes the FLAC Ogg mapping; replacing it gains no observable
property. §5.3 and §11's page closing do not apply to that stream; every
other rule of §5 does.

- Page format per RFC 3533 (from memory): capture pattern `OggS`, version 0,
  header type flags (continued 0x01, BOS 0x02, EOS 0x04), granule position
  (signed 64-bit little-endian), serial number, page sequence number,
  CRC-32 (polynomial 0x04C11DB7, initial value 0, unreflected, over the
  whole page with the CRC field zero), segment count, lacing values, body.
- Page sequence numbers start at 0 on each logical stream's BOS page and
  increase by one per page of that stream.
- A page's granule position is that of the last packet that ends on it, as
  its stream encoder states ([ogg-streams.md §7](ogg-streams.md#7-packets));
  −1 when no packet ends on it. Header pages have granule position 0.
- No page is written without a segment. No packet is empty, except where
  its codec's mapping gives an empty data packet a meaning
  ([ogg-streams.md §7](ogg-streams.md#7-packets)).

### 5.3 Page sizing

| Parameter           | Recommended                                          | Protects                                                                                                       |
| ------------------- | ---------------------------------------------------- | -------------------------------------------------------------------------------------------------------------- |
| `page_target_size`  | the stream's `bytes_per_page`, by default 4096 bytes | Overhead per byte (larger pages) against the spacing of access points and of bytes on the wire (smaller pages) |
| `max_page_duration` | 1 s of media time                                    | A reader's wait for an access point, a file's seek granularity, the age of held bytes                          |

- The BOS page carries the identification packet alone. Other header packets
  are packed into as few pages as the page format allows.
- `page_target_size` is met as the Ogg reference library's fill-target page
  output meets it (from memory): a data page closes once its body holds at
  least `page_target_size` bytes and, unless the page is full, at least four
  packets, or once it holds 255 lacing values. Nothing tighter is required.
- A data page is also closed: once its packets span `max_page_duration` of
  media time; before a packet that would take it past 255 lacing values;
  before a packet its codec marks as a page start (a Theora key frame,
  [ogg-theora.md §11](ogg-theora.md#11-page-constraints)); and at the end of
  its logical stream (§9). A packet longer than one page can carry continues
  on the next page.
- Every data page therefore begins a packet, except after a packet longer
  than a page.
- On a `live(L)` sink, each logical stream's `max_page_duration` is
  `min(1 s, L)` (§11).

### 5.4 Interleaving

The muxer holds closed pages per logical stream and writes them in end-time
order:

1. A closed page is written when it has the least end time among the closed
   pages held, and every logical stream of the link that has not ended holds
   at least one closed page. Ties go to the stream whose BOS page came
   first. The muxer MAY meet this rule in the held-page form: at most one
   closed page held per logical stream; a page whose end time is not after
   that of the last page written is written at once; when every logical
   stream that has not ended holds a page, the one with the least end time
   is written. Both forms write the same order.
2. In a link with one logical stream, each page is written as soon as it is
   closed.
3. Pages of one logical stream are written in their order.
4. Across logical streams, pages are written in non-decreasing end time,
   except for a page written at once under the held-page form of rule 1: it
   ends no later than the last page written.

Reason: a reader decodes the streams of a link together, and pages ordered
by end time keep what it must buffer to a minimum. Each logical stream
closes a page at least every `max_page_duration` (§5.3), so the wait of
rule 1 is bounded even for a sparse stream (a still image).

### 5.5 Serial numbers

- Each logical stream of each link gets a serial number drawn uniformly at
  random from the 32-bit range, distinct from every serial the writer has
  used. Serials are unique within a physical stream (RFC 3533, from memory)
  as far as the writer can know.
- A writer cannot know the serials of a stream it continues (§6), nor those
  of the writer before it on a shared stream. Random draws make a collision
  improbable (about `k / 2^32` per serial for `k` serials in use), and that
  risk is accepted. A serial is never derived from a counter, a clock or a
  generator state other writers reach without a lock.

### 5.6 Markers

- **Stream start.** At the first piece of each link, the writer writes
  `stream_start(header_length, t)` then the link's header pages:
  `header_length` is their total length, `t` the media time of the link's
  first piece. Every stream encoder produces its header packets at its
  creation ([ogg-streams.md §4](ogg-streams.md#4-header-packets)), so the
  whole header precedes any data page and is the entry header of every
  access point of the link.
- **Access points.** Within a link, after its header pages:
  - a link without video: before every data page that begins a packet;
  - a link with video: before every data page of the video stream that
    begins with a key-frame packet.

  `t` is the start time of the first packet that begins on that page. A
  reader that starts there is sent the link's header pages, then the pages
  from the access point. It discards a continued fragment at the start of an
  audio stream, and treats each stream's page sequence gap as lost data
  (RFC 3533 readers resume after it, from memory).

- Each link starts with a key frame, and the Theora stream encoder makes one
  at least every `keyframe_frequency` images, so a link with video has an
  access point at least once per key-frame interval; a link without video
  has one at least once per `max_page_duration`
  ([encoder.md §3.4](encoder.md#34-bytes-and-markers-out), item 5).
- `end` follows the final page of the last link (§9).

## 6. Header forms and preambles

- Ogg has one header form on every sink: a link's header pages, written once
  at the link's start. A stream whose codec has a header field known only at
  the end writes it at the value its mapping defines as unknown
  ([ogg-flac.md §4](ogg-flac.md#4-header-packets)). Reason: duration and
  position are carried by granule positions, and on a sink read from the
  middle a header rewritten at the end would differ from the entry header
  readers were already sent.
- **Ogg FLAC on a file sink.** On a sink confirmed `seekable` and `fresh`,
  the writer SHOULD, at each link's end, seek back to that link's
  STREAMINFO and rewrite its total sample count, frame sizes and MD5
  signature, then the CRC of the page that carries it, as native FLAC does
  ([flac.md §6.1](flac.md#61-file-form)); the page keeps its length. It
  records each link's STREAMINFO offset while it writes. Reason: a FLAC
  reader trusts STREAMINFO for duration and integrity, and nobody reads a
  file sink from the middle. No other Ogg stream rewrites anything, and on
  every other sink nothing is rewritten.
- **Continuing sink.** The writer starts a new link after the existing
  stream, with its own header pages and new serials (§5.5), and updates
  nothing in the existing bytes. Links need not share codecs, so the result
  is a valid chained stream whatever Ogg configuration wrote the existing
  one, provided it ends at a link boundary. An existing stream that ends
  inside a link (its writer was abandoned) keeps that link truncated.
- No preamble.

## 7. Metadata

1. **Initial metadata.** The first link carries the writer's initial
   metadata ([encoder.md §2.2](encoder.md#22-creation)) in the comment
   header of each of its logical streams when the sink is a file sink, or
   when the output offers metadata in band, whatever the sink: Ogg's
   comment headers are its in-band channel
   ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles), item 7).
   Otherwise its comment headers carry `ENCODER=` alone (item 8). How
   metadata becomes comments is
   [ogg-streams.md §5](ogg-streams.md#5-comments).
2. **Offers.** `metadata(m)` writes nothing and records `m`. At the next
   `write`, before encoding the piece, the writer compares the latest
   recorded offer with the metadata of the current link. When the offer is
   empty or equal to it, nothing happens. Otherwise the writer ends the
   current link (§9, steps 1–3), builds new stream encoders with `m` as
   their comments and new serials, and the piece is the first of the new
   link (§5.6).
3. **Equality.** Two metadata are equal when they hold the same
   `(key, value)` pairs, in any order, keys compared without regard to ASCII
   case (comment field names are case-insensitive in the Vorbis comment
   format, from memory). An offer is empty when it holds no pair.
4. Several offers with no piece between them start at most one link, with
   the latest. An offer still recorded at `end` starts no link. A link is
   started only by a piece, so the writer never writes a link without data.
5. Each link decodes alone from its first byte and carries every stream of
   the format; its granule positions count from the link's start. Given a
   decoder that honours the signalled delay and padding (§8), the media of
   consecutive links follow each other with no gap and no overlap.

Whether an output offers metadata in band, and any script parameter choosing
it, is its caller file's §4 ([encoder.md §9](encoder.md#9-in-band-metadata)).

## 8. Delay and padding

Each link signals its own delay and padding, from values its codec reports,
never constants ([encoder.md §3.7](encoder.md#37-delay-and-padding)):

| Stream | Delay signalled                                                                                         | Padding signalled                |
| ------ | ------------------------------------------------------------------------------------------------------- | -------------------------------- |
| Opus   | pre-skip, the encoder's look-ahead ([ogg-opus.md §8](ogg-opus.md#8-delay-and-padding))                  | the last page's granule position |
| Speex  | granule positions net of the encoder's look-ahead ([ogg-speex.md §8](ogg-speex.md#8-delay-and-padding)) | the last page's granule position |
| Vorbis | none: the codec has none                                                                                | the last page's granule position |
| FLAC   | none                                                                                                    | none: the last frame is short    |
| Theora | none                                                                                                    | none                             |

A rate converter in front of a codec adds no delay
([ogg-streams.md §6](ogg-streams.md#6-input)). At every link start the values
are the new link's.

## 9. End

A link ends at an accepted offer (§7) and at `end(clean)`:

1. each stream encoder drains its stages in order: the rate converter, the
   samples short of a codec unit, the codec's look-ahead
   ([ogg-streams.md §9](ogg-streams.md#9-end)). Every stream encoder ends
   with at least one data packet: a video stream that received no image in
   the link encodes one ([ogg-theora.md §9](ogg-theora.md#9-end)); an audio
   stream always received at least one sample (§4);
2. the muxer puts the EOS flag and the stream's final granule position on
   the page that carries each logical stream's last packet. It never ends a
   stream with an empty packet or a page without a segment;
3. every page still held is written in the order of §5.4;
4. at `end(clean)`, the writer then writes `end`.

- Every stream encoder's finish submits at least one packet after the muxer
  last took a page from that stream; the muxer then flags the stream's end
  and flushes it, so that the EOS flag goes on the page that carries the
  last packet. The Ogg FLAC stream's last page is the library's, and MUST
  carry the EOS flag ([ogg-flac.md §9](ogg-flac.md#9-end)).
- A writer ended before its first piece writes nothing
  ([encoder.md §2.3](encoder.md#23-states)).
- `end(abandoned)` releases every stream encoder and every held page and
  packet without draining, and writes nothing more.
- A drain step that fails at a clean end is logged at `severe`, and the end
  goes on ([encoder.md §3.6](encoder.md#36-draining-and-final-bytes)): the
  stream whose drain failed ends with the packets it had, its last page
  flagged EOS.

## 10. Properties

- `bitrate`: from creation, the sum of the nominal bit rates of the streams
  when every stream's configuration fixes one (each codec file's §10); else
  none. Container overhead is not counted. The format helper gives the same
  answer ([formats.md §9](formats.md#9-helpers)).
- `delay`, `padding`: those of the current link's audio stream, in samples
  at its codec rate (§8); none for a video-only format. `delay` is known
  from creation, `padding` from the link's end.
- Helpers: `extension` is `opus` for a format whose only stream is Opus,
  else `ogg`; `mime` is `application/ogg` for every Ogg format
  ([formats.md §9](formats.md#9-helpers)). `codecs` lists the RFC
  5334 name of each stream (from memory: `theora`, `vorbis`, `opus`,
  `speex`, `flac`), video first.

## 11. Live latency

What holds encoded data inside the writer: the page each logical stream is
filling and the closed pages waiting for interleaving (§5.4). The codec's
look-ahead, one codec unit, is not counted
([encoder.md §3.9](encoder.md#39-live-latency-bound)).

On a sink declared `live(L)`, each logical stream's `max_page_duration` is
`min(1 s, L)` (§5.3); there is no separate rule forcing pages out. Every
stream then closes a page at least once per `L` of media time, so the wait
of §5.4 rule 1 is bounded by `L` plus one page, and a stream that produces
little (a still image, silence) still has its bytes written at least once
per `L` plus one page.

The Ogg FLAC stream is the exception (§5.2): its pages close when the FLAC
library's Ogg layer closes them, so what it holds is bounded by the
library's page size, not by `L`.

On a sink that is not live, §5.3 and §5.4 alone bound what is held.

## 12. Segmenting face

Nothing.

## 13. Delegated destination

Nothing.

## 14. Concurrency

- The writer has no work of its own: every stream encoder and the muxer run
  within the writer's calls ([encoder.md §7](encoder.md#7-concurrency)).
- Writers share no mutable state. The serial source (§5.5) is owned by each
  writer or safe for concurrent use. Codec libraries are used only through
  per-encoder state; state a library or its binding keeps for the whole
  process is reached under a lock or not at all
  ([ogg-streams.md §12](ogg-streams.md#12-concurrency)).
- A writer's bytes depend on no other writer, the random serials aside.

## 15. Faults

| Fault                                                                                                   | Class               | Outcome                                                                  |
| ------------------------------------------------------------------------------------------------------- | ------------------- | ------------------------------------------------------------------------ |
| a parameter refused at type checking or evaluation (§1, each codec file's §1 and §2)                    | configuration error | the script fails before it starts                                        |
| a deferred value a codec cannot take (a sample rate, a video size or rate)                              | configuration error | at start-up, naming the parameter, the setting and the format's position |
| a stream library refuses the configuration at creation                                                  | configuration error | hard error naming the output, the stream and the parameter               |
| a stream encoder cannot be built for lack of a resource, at creation or at a new link                   | output fault        | the output's restart policy                                              |
| a stream library refuses one piece's data                                                               | item fault          | that stream's share of the piece is dropped with a warning and counted   |
| `max_consecutive_faults` item faults in a row                                                           | output fault        | the writer is `failed`                                                   |
| a stream library reports an error that leaves its state unusable                                        | output fault        | the writer is `failed`                                                   |
| a drain step fails at a clean end                                                                       | not escalated       | logged at `severe` (§9)                                                  |
| the muxer's own invariants broken (a packet after its stream's end, a page of a stream not in the link) | internal error      | fails loudly; a defect                                                   |

Every fault names the writer (the output's name) and the stream's codec.

## 16. Tests

A harness drives a writer directly with pieces and offers at chosen
positions, and parses pages (header fields, lacing, CRC, packets). Binding:

- For every Ogg configuration, alone and with `%theora`: every page has a
  valid CRC and the length its segment table states; every link's BOS pages
  come first and its header pages before its data pages; the identification
  packet is alone on its BOS page; every logical stream has exactly one EOS
  page, which carries a packet; no page has zero segments; a reference
  demuxer reads the whole stream.
- An offer before the first piece; two offers with no piece between; an
  offer then `end`; `end` with no piece: no link without data, every link has
  one logical stream per format stream, no call raises, and a writer given no
  piece writes nothing.
- An offer equal to the current link's metadata (pairs reordered, key case
  changed) and an empty offer start no link.
- A metadata change every second for a minute, with each codec: an
  independent demuxer reports each link as a new stream with its metadata;
  the concatenated decode equals the input length to within one sample per
  link and has a click at input sample `n` at decoded sample `n`.
- A mono audio stream under a stereo frame, and a video stream that receives
  no image in a link: every link ends validly.
- Serials are distinct over every logical stream of a writer across 10 000
  links; writers created at once on several domains draw independently.
- `live(L)` with `L` = 1 s, 30 s of digital silence through `%ogg(%vorbis)`
  and of a still image with audio through `%ogg(%vorbis, %theora)`: no
  interval of media time between two written pages of a stream exceeds `L`
  plus one page and the look-ahead, and pages are in non-decreasing end
  time except as §5.4 rule 4 allows. Not live: every interval is at most
  `max_page_duration` plus one packet.
- `%ogg(%flac)`: every FLAC page the library formed is written whole and
  unchanged (CRC included, the file-sink STREAMINFO rewrite of §6 aside),
  carries the writer's serial for that stream, and the stream's last page
  carries EOS.
- A reader that starts at any marked access point, given the entry header,
  decodes; with `%theora`, the first video packet it decodes is a key frame.
- `bytes_per_page` set to two values gives data pages whose body sizes follow
  §5.3's fill-target rule (at least the target, or fewer than four packets
  only on a full page or a page closed by `max_page_duration` or the end).
- An append of one minute of `%ogg(%opus)` to a file holding one minute of
  `%ogg(%vorbis)`: two links, both decoded, two minutes in all.
- `%ogg(%flac)` to `output.file` with three metadata changes: each link's
  STREAMINFO holds its total sample count and an MD5 that verifies, and
  every page CRC is valid; to a shared harbor mount, every STREAMINFO keeps
  0 and zeroes.
- Comments: on a file, and on a live output offering metadata in band,
  the first link's comments carry the initial title and `ENCODER=`; on a
  live output with in-band metadata off, `ENCODER=` alone.
- `encoder.extension(%opus)` is `opus`, `encoder.extension(%vorbis)` is
  `ogg`.
- Printing then evaluating every Ogg format of a corpus gives an equal
  value; `print(%theora)` shows a format with video.
