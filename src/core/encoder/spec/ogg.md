# Ogg encoder (`%ogg`)

As-built description of the Ogg container encoder: the format value, the
instance the outputs drive, and the muxer that interleaves the pages of its
logical streams into one chained Ogg physical stream. The codecs that fill
the logical streams answer a seam of their own,
[ogg-streams.md](ogg-streams.md), with one file per codec
([Vorbis](ogg-vorbis.md), [Opus](ogg-opus.md), [Speex](ogg-speex.md),
[FLAC](ogg-flac.md), [Theora](ogg-theora.md)).

Relies on: the generic encoder seam ([encoder.md](encoder.md)) for the
registry, factories, serialisation of calls and the stream face; the outputs'
seam ([output.md](../../outputs/spec/output.md)) for how pieces and metadata
reach an encoder; [draining.md](../../source/spec/draining.md) for what an
encoder delivers at its end. OCaml specifics are in
[language-notes/ogg.md](language-notes/ogg.md); defects and gaps in
[findings/ogg.md](findings/ogg.md).

Vocabulary used here (Ogg terms as in RFC 3533):

- **page**: an Ogg page, a 27-byte header, a segment table of up to 255
  lacing values, then a body of the sum of these values.
- **logical stream** (also **track**): the pages of one serial number, one
  codec.
- **link**: a group of logical streams that begin together (all their BOS
  pages first) and all end (EOS) before the next group begins. A file of
  several links is a _chained_ Ogg stream.
- **header pages**: the pages that carry a codec's header packets (and the
  skeleton's), as opposed to **data pages**.
- **end time** of a page: the media time, in seconds from the start of its
  link, at which the last packet completed on the page ends, or _unknown_
  (see [ogg-streams.md §9](ogg-streams.md#9-page-end-time)).

## 1. Format and options

### Syntax

`%ogg(e1, e2, ...)` where each `ei` is itself an encoder expression. The
arguments are scanned for:

- the **first** audio codec among `%vorbis`, `%vorbis.cbr`, `%vorbis.abr`,
  `%opus`, `%speex`, `%flac`;
- the **first** video codec, `%theora`.

Every other argument is ignored without an error: a second audio codec, a
second `%theora`, a non-Ogg encoder such as `%mp3`, a labelled parameter. The
codec parameters are each codec's §1.

Each codec is also registered alone: `%vorbis(...)`, `%vorbis.cbr(...)`,
`%vorbis.abr(...)`, `%opus(...)`, `%speex(...)` and `%theora(...)` are the
same format value as `%ogg(<that codec>)`. `%flac(...)` alone is the native
FLAC encoder, not Ogg; Ogg FLAC is only reachable as `%ogg(%flac(...))`.

### The format value

A record of an optional audio codec format and an optional video codec format.
Both absent is accepted by the parser (`%ogg`, `%ogg()`, `%ogg(%mp3)`).

### Type (`type_of_format`)

The channel count is the audio codec's (`channels`, or 2/1 for Speex's
`stereo`/`mono`); 0 when there is no audio codec.

| audio channels | video | type                              |
| -------------- | ----- | --------------------------------- |
| 0              | yes   | video only                        |
| n > 0          | yes   | audio (n channels, PCM) and video |
| n              | no    | audio (n channels, PCM)           |

With no audio and no video the type is "audio with 0 channels", which the
channel-layout code rejects with an uncaught internal exception at type
checking (`Error -1: Exception raised: Audio_layout.Unsupported`). The same
happens for an audio codec whose channel count has no layout (Vorbis or FLAC
`channels=0`, `channels=7`); see [findings](findings/ogg.md).

### Printed form

`%ogg(<audio>,<video>)`, each codec printing itself (each codec's §1). When
there is no audio codec, the video codec is dropped from the printed form too:
`%theora` and `%ogg(%theora)` print `%ogg()`.

### Extension, MIME, bitrate estimate

- Extension: `ogg` (whatever the codecs).
- MIME: `application/ogg` (whatever the codecs).
- Bitrate estimate (the format-level one): none (not found).

## 2. Factory and instance creation

The `ogg` plug-in answers every Ogg format value. Its factory takes the
output's name and the initial metadata and builds an instance:

1. For the audio codec, if any: the codec's stream-encoder maker is looked up
   by codec name (`vorbis`, `opus`, `flac`, `speex`) in a table filled by
   each optional codec module when it is linked in. A missing codec fails the
   factory call with the language error `Could not find any <name> encoder.`
   at the format's position. This lookup happens each time the factory is
   called (instance creation), not when the script is evaluated.
2. For the video codec, if any: the Theora maker, set by the Theora module
   when linked in. When it is not linked in, the factory raises an internal
   "option is None" exception.
3. Each maker is applied to the codec format, giving one **track** per codec:
   an encode function (per piece), a `reset` function (per link: creates a
   fresh codec encoder and registers it as a logical stream in the muxer,
   returning its serial), and a current serial, initially none. What a maker
   computes once (per instance) and what `reset` computes (per link) is in
   [ogg-streams.md §2](ogg-streams.md#2-instance-creation).
4. The track list is `[video; audio]`: video first when both exist. This
   order is the registration order, hence the order of the BOS pages and of
   per-piece encoding.
5. A **skeleton** logical stream is used when there are two tracks (audio and
   video), never with one. The source comment says "for now".
6. The muxer is created (§10 _Muxer state_). With a skeleton, creating it
   immediately emits the skeleton's BOS page (the `fishead`) into both the
   output buffer and the header buffer.

The initial metadata is kept and used as the comment metadata of the **first
link** of the instance (and of any link started by an `encode` after a
`stop`). Nothing else is computed from it.

Settings read: the frame's audio rate and video rate, and the frame's channel
count for the "empty stream" fallbacks of some codecs (codec files §10).

## 3. `encode`

Input: one piece (a frame or a slice of one) of the type of §1.

1. **Lazy start.** If the muxer is not streaming (it is at the start of the
   instance, after a metadata change, or after `stop`):
   - every track that has no serial yet is `reset` with the **initial
     metadata**, registering its logical stream and emitting its BOS page;
   - the muxer starts its streams (§10 _Starting a link_): skeleton
     `fisbone`s, every codec's secondary header pages, skeleton EOS.

   The reason given in the code: "We do a lazy start, to avoid empty streams
   at beginning."

2. Each track, in track-list order (video, then audio), encodes the piece:
   - audio: the piece's PCM, from position 0 to the piece's length, is
     resampled from the frame audio rate to the codec rate (a plain copy when
     the rates are equal; the resampler is created once per instance and
     keeps its state across links), then handed to the codec's stream encoder
     as one block; zero-length blocks are handed over too;
   - video: every image of the piece is rendered (composited canvas to a
     YUV420 image) and the list handed to the codec as one block; a piece
     with no image hands nothing over.

   The codec turns the block into packets in its logical stream and possibly
   whole pages ([ogg-streams.md §7–§8](ogg-streams.md#7-data-input-and-conversion));
   then the muxer collects that track's pages (§10 _Interleaving_).

3. The instance returns **every byte the muxer has accumulated since the
   previous return**, and records where in them a new link starts (§7). These
   bytes can include pages produced by an earlier `encode_metadata`.

Internal buffering, per piece:

- inside each codec (encoder look-ahead, partial frames, partial packets;
  codec files §7);
- inside each logical stream, packets not yet forming a page (libogg's
  default page threshold of about 4096 body bytes or 255 segments; see §10);
- inside the muxer, one held page per track (the interleaving hold, §10).

So a call returns zero or more whole pages, never a partial page. Most calls
return nothing: with the default Vorbis settings and a sine, 1 to 2 s
pass between two returned pages; with a still image as video, the
audio is held until the video stream completes a page, about 10 s at 25 fps
(§10, checked).

## 4. `encode_into`

Nothing. The instance offers no `encode_into`.

## 5. `encode_metadata`

In-stream metadata ends the current link and begins a new one, whose codec
comment headers carry the new metadata:

1. Every track's serial is cleared.
2. The muxer ends the link (§10 _Ending a link_): every logical stream is
   ended and drained, in order of end time, then the skeleton (if any)
   is flushed; the header buffer is emptied; the muxer is back to "ended".
3. Every track is `reset` with the new metadata: a fresh codec encoder per
   track, a new logical stream with a new random serial, its BOS page emitted.
   Being in the "ended" state, the muxer marks the new link's start at the
   current end of its output buffer and, with a skeleton, emits a new
   `fishead` first.

The secondary header pages of the new link are only emitted by the next
`encode` (lazy start). The call returns nothing: the EOS pages of the old link
and the BOS pages of the new one wait in the output buffer for the next
`encode` or `stop`.

Every metadata change makes a new link, whatever its content (an empty or
identical metadata too). How the metadata becomes comments (which keys, which
order, vendor strings) is each codec's §6.

Calling `encode_metadata` before the first `encode` ends an empty link: the
muxer starts zero streams and ends them. Without a skeleton this emits
nothing. With a skeleton it emits a skeleton-only link (the `fishead` page and
the skeleton EOS page, 120 bytes) before the real first link; this happens
whenever the first piece carries metadata at its position 0, e.g. any
`%ogg(%vorbis,%theora)` file output of a playlist (checked, §10 example 3).

## 6. `header`

Returns a copy of the **header buffer**: every header page emitted since the
last end of a link (or since creation), in emission order. It is emptied when
a link ends (by `encode_metadata` or `stop`).

What it holds depends on when it is called:

| moment                                            | content                                                                 |
| ------------------------------------------------- | ----------------------------------------------------------------------- |
| before the first `encode`                         | nothing, or the skeleton `fishead` page                                 |
| after the first `encode`                          | all header pages of the current link (complete)                         |
| after `encode_metadata`, before the next `encode` | the new link's BOS pages (and `fishead`): secondary headers are missing |
| after `stop`                                      | nothing                                                                 |

The header pages are also part of the bytes `encode` returns: a reader of the
whole output gets them once in place; `header` is for a reader joining later.

## 7. `restart_offset`

The muxer tracks the offset, in its output buffer, of the first page of the
latest link ("link start"):

- at creation the link start is offset 0;
- when a link is registered while the muxer is "ended" (after
  `encode_metadata` or `stop`), the link start is set to the current length
  of the output buffer, i.e. just after the previous link's EOS pages; with a
  skeleton, the new `fishead` is the page at that offset;
- each time the instance returns bytes (`encode`, `stop`), the link start
  pending, if any, becomes the answer of `restart_offset` and is cleared.

So `restart_offset ()` after an `encode` or `stop` is the offset, in the
bytes just returned, of the first BOS page of a link that begins in them;
none when they begin no link. The first output of an instance reports 0. When
two links begin within one return (two metadata changes with no `encode`
between), the later one is reported. After the leading skeleton-only link of
§5, the offset reported skips it (120 with the example of §10).

## 8. `bitrate`

Always none. The instance never knows a bitrate. (The formats themselves carry
nominal values that some outputs use for their announcements; that is the
outputs' business.)

## 9. `stop`

1. Every track's serial is cleared.
2. The muxer ends the link (§10 _Ending a link_): each codec's end-of-stream
   procedure (codec files §10), then all pending pages of every logical stream
   in end-time order, then the skeleton.
3. All bytes accumulated since the last return are returned.

What is lost, per codec (codec files §10): Opus and Speex drop the samples of
their last incomplete codec frame (up to one frame, 2.5 to 60 ms for Opus,
up to 160 or 320 or 640 samples for Speex); samples held by the resampler when
the rates differ are not drained. Vorbis, FLAC and Theora deliver everything
they were given.

The instance can be used after `stop`: a later `encode` starts a new link with
the initial metadata. No output does this.

A link that ends with no data encoded in a track makes that codec encode a
token input first so that its stream is not empty (codec files §10): one
silent sample (Vorbis, Opus, FLAC), one black frame (Theora, which fails; see
[findings](findings/ogg.md)), nothing (Speex).

## 10. Bytes produced

### Muxer state

- an identifier (the output's name), for logs;
- an output buffer and a header buffer (sequences of pages);
- the pending link start (§7) and the one last handed over;
- a **position**: the end time of the last page released by the
  interleaving, in seconds from the link start; 0 at creation and after each
  link end;
- the table of live logical streams, by serial; for each: its libogg stream
  state, its codec callbacks, a queue of pages the codec produced itself, and
  at most one **held page** with its end time;
- a state: _beginning_ (tracks can be registered), _streaming_ (data can be
  encoded), _ended_ (tracks can be registered, which begins a new link);
- an optional skeleton logical stream.

### Serial numbers

Every logical stream (codec or skeleton) gets a serial drawn uniformly from
`[0, 0x3FFFFFFF)` by a process-wide pseudo-random generator seeded from the
system at start. Serials are never checked for uniqueness, neither within a
link nor across the links of an instance. The source comment: "As per
specifications, we need a random injective sequence of nativeint. This might
not be assumed here, but chances are very low."

### Registering a track (BOS)

Allowed in _beginning_ and _ended_; in _streaming_ it logs at `info`
`<id>: Invalid new track: ogg stream already started..` and raises a usage
error. In _ended_: logs `<id>: Starting new sequentialized ogg stream.` at
`info`, sets the link start (§7), emits a fresh skeleton `fishead` when a
skeleton is used, and goes to _beginning_.

Then: a new serial and logical stream; the codec's identification header page
([ogg-streams.md §3](ogg-streams.md#3-identification-header-bos-page)) is
emitted at once, into the output buffer and the header buffer.

### Starting a link

Logged at `info`: `<id>: Starting <n> track(s)`. In order:

1. with a skeleton: each track's `fisbone` packet, if the codec has one, is
   put in the skeleton stream (iteration order of the track table), then the
   skeleton stream is flushed into pages (header pages);
2. each track's secondary header pages
   ([ogg-streams.md §5](ogg-streams.md#5-secondary-headers)), track after
   track in iteration order of the track table (not registration order: the
   two links of example 2 below have them in opposite orders);
3. with a skeleton: the skeleton EOS packet (empty, granulepos 0) is put and
   flushed as one page (header page);
4. state _streaming_.

All of these pages go to the output buffer and the header buffer. Every BOS
page of the link therefore precedes every secondary header page, and every
header page precedes every data page, as RFC 3533 and the skeleton
specification ask.

### Interleaving

After each codec call for a track `src`, the muxer runs this loop:

1. If **every** live track has a held page: release the held page with the
   least end time (on ties, the last one met in table iteration), set the
   position to its end time, clear that hold.
2. If there is only one live track, or `src` has no held page: take the next
   page of `src` (first from its queue of codec-made pages, else from libogg's
   page assembly of its stream with the default threshold; see _Page
   sizing_). If there is none, stop. If its end time is known and later than
   the position, hold it and stop. Otherwise (end time not later, or unknown)
   release it and go back to 1.
3. Afterwards, if `src` is finished (its libogg stream has seen its EOS
   packet, it holds no page, its queue is empty, and no further page can be
   assembled; probing for one queues it if there is one), it is removed from
   the table.

Released pages are appended to the output buffer (not the header buffer).

Consequences:

- Pages of one stream keep their order. Between streams, pages are released
  in non-decreasing end-time order, except pages with an unknown end time,
  which are released as soon as they are taken.
- With one track, the latest completed page is held until the next codec call
  for that track: output lags one `encode` call behind page completion.
- With two tracks, a track's pages are held until the other track also holds
  a page. A sparse stream holds the dense one back for as long as it takes to
  complete a page. With a still image at 25 fps and Theora, the video stream
  completes a page every 255 frames (≈10.2 s); the audio pages wait for it
  (checked: an `%ogg(%vorbis,%theora)` file output with `flush=true` stayed at
  its 7811 header bytes from 2 s to 10 s of a 12 s run, while the same audio
  alone grew by a 4 KB page every 1–2 s).

### Ending a link

1. With a skeleton: any pages still pending in the skeleton stream are
   flushed to the output buffer.
2. While any track is live, for each live track: if its stream has not seen
   its EOS packet, log `<id>: Setting end of track <serial in hex>.` at `info`,
   call the codec's end-of-stream procedure, queue the pages it returns; then
   run the interleaving loop for it. Ending is called on a muxer still in
   _beginning_ too: it then logs `<id>: Stream finished without calling
streams_start !` and starts the link first.
3. If the muxer is not _streaming_ (no track was ever registered), it starts a
   link with zero tracks (with a skeleton, this emits the skeleton EOS page).
4. Log `<id>: Every ogg logical tracks have ended: setting end of stream.` at
   `info`; empty the header buffer; position 0; state _ended_.

Every logical stream ends with a page carrying the EOS flag: libvorbis and
libFLAC set it on their last data page; Opus, Speex and Theora end with an
extra empty page (codec files §10).

### Page sizing

The muxer and the codecs ask libogg for pages in two ways:

- **flush** (header pages, and draining at end): one page per call, as many
  packets as fit; libogg puts the first packet of a stream alone on its BOS
  page;
- **page-out** (data pages): a page only when about 4096 body bytes or 255
  lacing values have accumulated, or the stream has seen its EOS packet.

Each codec takes a `bytes_per_page` parameter meant to set libogg's fill
threshold. It has no effect: the binding only honours it when compiled with a
flag that no build rule defines, so the default threshold always applies
(checked: `%opus(bytes_per_page=500)` gives the same 4269-byte pages as
`%opus`). Ogg FLAC does not use libogg's page assembly at all
([ogg-flac.md §8](ogg-flac.md#8-packets-granule-positions-and-pages)).

### Layout of the output

```
link 1:
  [fishead BOS]                              skeleton only (audio+video)
  BOS page of each track, registration order (video, then audio)
  [fisbone page(s)]                          skeleton only
  secondary header pages, track after track (table order)
  [skeleton EOS page]                        skeleton only
  data pages, interleaved by end time, ending with each track's EOS page
link 2 (after a metadata change): same, new serials, new comments
...
```

### Worked examples

Produced by the binary of this tree (`02fa52985`), `output.file`, a 2.6 s
sine with a metadata change (`title=second`, `artist=x`) at about 1 s. Dumped
with a page parser; `gp` is the page granulepos, flags B/E are BOS/EOS.

**Example 1: `%ogg(%vorbis)`** (15 385 bytes)

```
#0 off=0     len=58   S0 seq=0 gp=0      B  pkts=[30]       \x01vorbis (identification)
#1 off=58    len=4006 S0 seq=1 gp=0      -  pkts=[130,3832] \x03vorbis (comment) + setup
#2 off=4064  len=3061 S0 seq=2 gp=45864  E  48 segments, all data of link 1
#3 off=7125  len=58   S1 seq=0 gp=0      B  pkts=[30]
#4 off=7183  len=4034 S1 seq=1 gp=0      -  pkts=[158,3832] comment now has title, artist
#5 off=11217 len=4168 S1 seq=2 gp=68796  E
```

By §3 and §10, the first `encode` returns pages #0–#1, page #2 comes out with
the bytes of the `encode` after the metadata change; `restart_offset`
reports 7125 with the bytes that contain page #3. Link 1 comment packet:
vendor `Xiph.Org libVorbis I 20200704 (Reducing Environment)`, one comment
`ENCODER=Liquidsoap/2.5.0+git@02fa52985 (Unix; OCaml 5.5.2)`. Link 2:
`title=second`, `artist=x`, then the same `ENCODER`.

**Example 2: `%ogg(%vorbis,%theora)`**, 320×240, still video (23 082 bytes)

```
#0  off=0     len=92   S0 skel   gp=0     B  fishead (64 bytes)
#1  off=92    len=70   S1 theora gp=0     B  \x80theora
#2  off=162   len=58   S2 vorbis gp=0     B  \x01vorbis
#3  off=220   len=189  S0 skel   gp=0     -  two fisbones (80 bytes each)
#4  off=409   len=3368 S1 theora gp=0     -  \x81theora + setup
#5  off=3777  len=4006 S2 vorbis gp=0     -  \x03vorbis + setup
#6  off=7783  len=28   S0 skel   gp=0     E  one empty packet
#7  off=7811  len=3061 S2 vorbis gp=45864 E
#8  off=10872 len=66   S1 theora gp=89    -  26 packets (1 keyframe, 25 empty)
#9  off=10938 len=27   S1 theora gp=90    E  no segment
#10 off=10965 ...      S3 skel            B  link 2: fishead, theora BOS, vorbis BOS,
                                             fisbones, then vorbis secondary headers
                                             BEFORE theora's (table order), skeleton EOS
```

Vorbis page #7 (end 1.04 s) is released before Theora page #8 (end
(1+25+1)/25 = 1.08 s). The installed ffprobe reports durations of the first link only for such a
file and the skeleton stream as codec `unknown`; on example 3 it logs
`Changing stream parameters in multistream ogg is not implemented`.

**Example 3: leading skeleton-only link.** Same format, a source whose first
piece carries metadata at position 0 (an mp3 with a `title` tag):

```
#0 off=0   len=92 S0 skel gp=0 B  fishead
#1 off=92  len=28 S0 skel gp=0 E  skeleton EOS: an empty link
#2 off=120 len=92 S1 skel gp=0 B  the real first link starts here
...
```

The log shows `Starting 0 track(s)` followed by `Every ogg logical tracks
have ended`.

The per-codec header layouts and granulepos are in the codec files.

## 11. Concurrency and failures

### Concurrency

The instance has no lock of its own; the generic serialisation of calls
([encoder.md](encoder.md)) applies. The muxer, the tracks and the codec
encoders are owned by one instance and touched only from its calls. Two
things are process-wide: the serial generator (shared by all instances,
unsynchronised) and the codec tables filled at module initialisation. Some
codec calls run with the OCaml runtime lock released (codec files §13).

### Failures

Nothing is caught in the instance or the muxer: every exception propagates to
the caller of the call that raised it, and the instance is then in an
unspecified state (no rollback). Sources of exceptions:

| raise                                               | when                                                                                                                               |
| --------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| usage error (logged at `info` first)                | registering in _streaming_; encoding in _ended_ (`<id>: Cannot encode: ogg stream finished..`): not reachable through the instance |
| invalid data                                        | a piece kind not matching the track kind (not reachable: types)                                                                    |
| not found                                           | encoding for a serial no longer in the table (not reachable)                                                                       |
| codec errors                                        | each codec's §12                                                                                                                   |
| language error `Could not find any <name> encoder.` | factory call, codec module missing                                                                                                 |

Logs: only the `info` lines quoted in §10 (logger `ogg.muxer`). Nothing is
logged at `severe` or above.

## 12. Segmenting (HLS) face

Nothing. No HLS plug-in answers an Ogg format; `output.file.hls` and
`output.harbor.hls` cannot use `%ogg`.

## 13. Where the bytes go

The instance never does I/O. It returns bytes, and these assumptions about
their destination are built in:

1. **One append-only byte stream.** Bytes are produced in final order, never
   revised; the muxer never seeks back (unlike native FLAC or WAV, which would
   rewrite a header). Ogg FLAC's STREAMINFO is therefore left with 0 total
   samples and a zero MD5 ([ogg-flac.md §10](ogg-flac.md#10-end-of-stream)).
2. **Readers start at a link start.** Pages are only meaningful to a reader
   that saw their link's header pages. The instance supports two ways to start
   a late reader: `header()` (the current link's header pages, to send before
   the stream continues from "now") and `restart_offset()` (an offset in the
   latest returned bytes where a reader can start from scratch). Neither says
   at which offset of the bytes returned so far the current link began, so a
   caller wanting "start at the current link" keeps that offset itself
   (the harbor and SRT outputs do).
3. **`header()` is not self-sufficient after a metadata change**: between an
   `encode_metadata` and the next `encode` it lacks the secondary headers,
   and the bytes the next `encode` returns start with the _previous_ link's
   EOS pages, which a reader given `header()` cannot place.
4. **Page boundaries are the only safe cut points.** Every return ends on a
   page boundary, so a caller can cut between returns; it cannot cut inside a
   return except at `restart_offset`, as it does not know where the pages
   inside a return start.
5. **No timing.** The bytes carry no wall-clock information; granulepos
   restart at 0 in every link. A caller that needs to pace or time-stamp them
   (SRT, harbor bursts) measures data, not pages, and the interleaving hold
   (§10) means the bytes of a time interval can come out much later than the
   interval was encoded.
6. **One physical stream per instance**, shared by all readers: the instance
   knows nothing of readers, and every reader gets the same serials.
7. **Restart is in-band.** A metadata change is expressed by ending the link
   in the byte stream, not by a new destination: a file output gets a chained
   file, a network output a chained stream. The encoder cannot be told that
   the destination changed (a reconnect, a new file); outputs get a fresh
   stream by creating a new instance.

## 14. Tests

| invariant                                                                                                                                                                                                                                                                           | binding                                                                       | incidental                                  | trace                                               |
| ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------- | ------------------------------------------- | --------------------------------------------------- |
| An Ogg file of 10 s of audio decodes as one audio stream, for Vorbis, FLAC and Opus, mono and stereo.                                                                                                                                                                               | a decoder finds an audio stream with packets (or duration ≥ 10 s)             | ffprobe; the duration fallback; the sine    | `tests/media` `encoder_audio_only` × `%ogg(...)`    |
| A late network reader of a chained stream receives whole consecutive pages from its first byte; the first page is a BOS page; every non-BOS page belongs to the link begun last; a link changes at least twice in 3 s with metadata every second; the same after an output restart. | page integrity, BOS at offset 0, serial continuity, at least two link changes | the 1 s metadata period, 3 s window, harbor | `tests/harbor/harbor_output_join_points.liq`        |
| A single-link stream joined with a short burst starts with the codec identification header.                                                                                                                                                                                         | bytes 28..34 are `\x01vorbis` (first page is the Vorbis BOS page)             | burst size 4096                             | same                                                |
| Stopping an output delivers the EOS pages: the reader's bytes are whole pages and the last page has the EOS flag.                                                                                                                                                                   | last page flag                                                                | harbor                                      | `tests/harbor/harbor_output_final_bytes.liq`        |
| A late joiner of an SRT Ogg output decodes, including right after a metadata change.                                                                                                                                                                                                | decodable by a Vorbis decoder                                                 | SRT, 5 s                                    | `tests/srt/srt-late-joiner.liq`                     |
| An Ogg/Opus mount joined late gets the headers first.                                                                                                                                                                                                                               | decodable start                                                               | icecast server burst                        | `tests/harbor/icecast_server_ogg_late_listener.liq` |
| Ogg FLAC sent to a harbor input is decodable from its start.                                                                                                                                                                                                                        | the input starts                                                              | icecast/harbor pair                         | `tests/regression/fallible_ogg.liq`                 |
| Vorbis channel count is part of the type.                                                                                                                                                                                                                                           | type errors for mismatched channels                                           |                                             | `tests/language/type_errors.liq`                    |
| Theora file output runs.                                                                                                                                                                                                                                                            | (skipped: "theora encoder/decoder is deprecated")                             |                                             | `tests/regression/GH4116.liq`                       |

Not covered by any test: byte layout of headers, granulepos values, end
trimming, skeleton, Speex, Theora, `header()` contents after a metadata
change, the interleaving order, `bytes_per_page`.

A new harness needs: a page parser (header fields, lacing, packet
boundaries, CRC); a way to drive an instance directly with pieces and
metadata at chosen positions (including metadata before the first piece and
two metadata with no piece between); a decoder that honours pre-skip and
end-of-stream granulepos to measure trimming; a clock-free run so the
interleaving hold is observed per call rather than in wall time.
