# Playlists

Part A. This file owns the bytes of the media playlists and of the main
playlist, the numbering they announce, and when each is written. Writing a
file is [files.md §3](files.md#3-writing-a-file); which segments exist on
disk is [files.md §5](files.md#5-retention).

## 1. Common form

- UTF-8, no byte-order mark. Every line ends with CR LF, the last included.
- **Version**: every media playlist announces `EXT-X-VERSION:6`, and the
  main playlist writer receives 6. Reason: from version 6 the target
  duration bounds the _rounded_ segment duration, which the cut rule relies
  on ([segmenting.md §4](segmenting.md#4-the-cut-rule)); version 5 and
  earlier read it as an unrounded maximum (second edition, §8). Version 6
  also covers `EXT-X-MAP` and decimal `EXTINF` (RFC 8216 §7), so no
  playlist announces more than it needs (§6.2.1).
- Playlists are live: no `EXT-X-PLAYLIST-TYPE`, which a live playlist MUST
  NOT carry (RFC 8216 §6.2.2), and no `EXT-X-ENDLIST`. RFC 8216 §6.2.1
  requires `EXT-X-ENDLIST` once a published playlist holds the final
  segment; the output never ends a presentation it keeps on disk: a stop
  either keeps it resumable or removes its files
  ([files.md §7](files.md#7-cleanup-at-a-final-stop)).
- Every URI follows §4.

## 2. Media playlist

One per variant, named `<variant name>.m3u8`, in the output directory.

**When.** Rewritten after each segment close
([files.md §4](files.md#4-closing-a-segment)) and after a restore. First
written when the variant's first segment closes.

**Window.** The listed segments, oldest first. Each close appends the new
segment; then the oldest is removed while the window holds more than
`segments` segments and its duration without the oldest is still at least
three times `T`. The window's start only ever moves forward. Reason: RFC
8216 §6.2.2 removes segments in order only, and forbids a live playlist
shorter than three target durations. A `segments` below 6 is accepted with
a warning at construction: Apple's authoring specification (§8.11) requires
six segments in a live playlist.

**Bytes**, in order:

```
#EXTM3U
#EXT-X-TARGETDURATION:<T>
#EXT-X-VERSION:6
#EXT-X-INDEPENDENT-SEGMENTS                     when every listed segment qualifies
#EXT-X-MEDIA-SEQUENCE:<media sequence>
#EXT-X-DISCONTINUITY-SEQUENCE:<discontinuity sequence>
<each variant extra tag, one per line>
then for each segment, i from 0:
#EXT-X-DISCONTINUITY                            if i > 0 and the segment is discontinuous
#EXT-X-MAP:URI="<init URI>"                     if (i = 0 or the init differs from the previous segment's) and there is one
<each tag inserted for this segment, one per line>
#EXTINF:<length in seconds, 3 decimals>,
<segment URI>
```

- `T`: [segmenting.md §1](segmenting.md#1-time-the-grid-and-the-target-duration).
- `EXT-X-INDEPENDENT-SEGMENTS`: when the encoder's
  `independent_segments()` is true and no listed segment starts at a
  forced split ([encoder-contract.md §6](encoder-contract.md#6-properties)).
  Reason: the tag applies to every listed segment (RFC 8216 §4.3.5.1).
- An empty window writes media sequence 0 and discontinuity sequence 0.
- The init segment changes only at a restart, which is discontinuous
  ([segmenting.md §8](segmenting.md#8-container-restart)), so `EXT-X-MAP`
  follows every discontinuity that changed it, and applies to every later
  segment until the next one.
- `EXTINF` has no title. Its value rounded to the nearest integer is at most
  `T` ([segmenting.md §4](segmenting.md#4-the-cut-rule)).
- Inserted tags precede the `EXTINF` of the segment they belong to: a tag
  applies to the next segment URI.

Example (`segment_duration=1.`, `segments=3`, an fMP4 variant `m` after a
restart from persisted state):

```
#EXTM3U
#EXT-X-TARGETDURATION:1
#EXT-X-VERSION:6
#EXT-X-INDEPENDENT-SEGMENTS
#EXT-X-MEDIA-SEQUENCE:6
#EXT-X-DISCONTINUITY-SEQUENCE:0
#EXT-X-MAP:URI="m_0.mp4"
#EXTINF:1.000,
m_7.m4s
#EXT-X-DISCONTINUITY
#EXT-X-MAP:URI="m_1.mp4"
#EXTINF:1.000,
m_8.m4s
#EXTINF:1.000,
m_9.m4s
```

### 2.1 Media sequence

Each variant numbers its published segments with **position numbers**,
consecutive integers from 1, given at publish: a dropped segment takes
none. The counter continues across runs of one process
([operator.md §3](operator.md#3-lifecycle)) and across processes with
persistence.

The media sequence is the first listed segment's position number minus
one. Since numbers have no gaps, never go back, and segments leave the
window in order, a segment keeps its media sequence number while it is
listed (RFC 8216 §6.2.2).

The **discontinuity sequence** follows the variant's discontinuity counter,
which counts published discontinuous segments: each one increments it at
publish, and every segment records the value after its publish. The
playlist announces the first listed segment's value, so that segment's own
discontinuity, if any, is counted there, and it carries no
`EXT-X-DISCONTINUITY`. The counter continues like position numbers, and
discontinuities are output-wide
([segmenting.md §7](segmenting.md#7-output-wide-cuts)).

What is not promised: a new process without persistence starts again at
position 1. Its previous playlists were removed at the final stop; after a
crash they stay and are overwritten with lower numbers. Players already
playing then see the media sequence go back. Persistence is the way to keep
numbering across processes.

## 3. Main playlist

Named by `playlist`, in the output directory.

**When.** Due at the start of every run. Written as soon as every variant
has a listed segment, so every media playlist it names exists. Rewritten
whenever a value it was written with changes
([variants.md §3](variants.md#3-derived-properties)). Not rewritten
otherwise.

**Content.** The operator calls `main_playlist_writer` with `extra_tags`,
`prefix`, `version`, and one string per variant: the variant name, with
methods `bandwidth`, `codecs` and, when known, `video_size`. A `null`
writer or a `null` result: no main playlist, and an existing one is left as
it is. A string: written as is.

### 3.1 `hls.playlist.main`

`hls.playlist.main(~extra_tags=[], ~prefix="", ~version=7, streams)`:

1. `prefix` is joined to each URI by §4.
2. The lines are `#EXTM3U`, `#EXT-X-VERSION:<version>`, each extra tag, then
   for each stream:
   `#EXT-X-STREAM-INF:BANDWIDTH=<b>[,CODECS="<codecs>"][,RESOLUTION=<w>x<h>]`
   and the URI of `<name>.m3u8`. `CODECS` is left out when the string is
   empty; `RESOLUTION` when there is no video size.
3. Lines are joined with CR LF and the result ends with CR LF.

Example, as the output calls it:

```
#EXTM3U
#EXT-X-VERSION:6
#EXT-X-STREAM-INF:BANDWIDTH=123,CODECS="mp4a.40.2"
prefix/stream_a.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=456,CODECS="avc1.64001f,mp4a.40.2",RESOLUTION=1280x720
prefix/stream_b.m3u8
```

Every variant is an `EXT-X-STREAM-INF`, audio-only ones included (RFC 8216
allows it; Apple forbids audio-only variants on tvOS, §9.20).

## 4. URIs and prefix

A file's **URI** is its name relative to the output directory, directories
included, each path segment percent-encoded where RFC 3986 does not allow
the character in a path. A non-empty `prefix` is followed by `/` unless it
ends with one, then the URI. The same rule holds for segment URIs, init
URIs and the main playlist's media playlist URIs.

A quoted URI (`EXT-X-MAP`) is written between double quotes with no escape:
HLS quoted strings have none. Percent-encoding keeps `"`, CR, LF and other
control characters out of every URI, and `{`, so no URI holds the `{$`
that the second edition of the protocol reads as a variable reference.
