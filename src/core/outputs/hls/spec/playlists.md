# Playlists

Part A. As built. This file owns the bytes of the media playlists and of the
main playlist, and when each is written. Writing a file is
[files.md §3](files.md#3-writing-a-file); the segment list a media playlist
reads is [files.md §5](files.md#5-retention).

## 1. Common form

Every line ends with CR LF (`\r\n`), including the last. No playlist ever
carries `#EXT-X-ENDLIST`, `#EXT-X-PLAYLIST-TYPE`, `#EXT-X-PROGRAM-DATE-TIME`
or `#EXT-X-START`, also after `stop`.

## 2. Media playlist

One per variant, named `<variant name>.m3u8`, in the output directory.

**When.** Rewritten after each segment close
([files.md §4](files.md#4-closing-a-segment)), except during `stop`. So it
is first written when the variant's first segment closes, and at `stop` it
keeps its last content.

**Window.** From the variant's list of closed segments that have a file
name, the newest `segments` (the parameter), oldest first.

**Bytes**, in order:

```
#EXTM3U
#EXT-X-TARGETDURATION:<T>
#EXT-X-VERSION:<version>
#EXT-X-INDEPENDENT-SEGMENTS                     only if the encoder says so
#EXT-X-MEDIA-SEQUENCE:<first segment's position − 1, or 0 if empty>
#EXT-X-DISCONTINUITY-SEQUENCE:<first segment's discontinuity count, or 0>
<each variant extra tag, one per line>
then for each segment, i from 0:
#EXT-X-DISCONTINUITY                            if i > 0 and the segment is discontinuous
#EXT-X-MAP:URI=<quoted prefix + basename(init)> if (i = 0 or discontinuous) and it has an init segment
#EXTINF:<length in seconds, 3 decimals>,
<each tag inserted for this segment, one per line>
<prefix + basename(segment file)>
```

- `T`: [segmenting.md §1](segmenting.md#1-segment-duration-and-the-grid).
- `version`: [operator.md §2](operator.md#2-construction) step 7; the same
  for every variant of the output.
- `EXT-X-INDEPENDENT-SEGMENTS`: the encoder's `independent_segments()` at
  the time of writing ([encoder-contract.md §6](encoder-contract.md#6-properties)).
- A segment's **discontinuity count** is the variant's count when the
  segment opened, plus one if the segment is discontinuous. The first
  segment's `EXT-X-DISCONTINUITY` is not written; its count includes it, so
  the sequence counts every discontinuity tag that has left the window.
- The `EXT-X-MAP` URI is quoted with the language's string quoting (double
  quotes, backslash escapes for special characters).
- `EXTINF` has no title after the comma.
- Inserted tags sit between `EXTINF` and the URI
  ([metadata.md §3](metadata.md#3-inserted-tags)).

Example, taken from a run (`segment_duration=1.`, `segments=3`, an MPEG-TS
AAC variant `a` beside an fMP4 variant, so version 7):

```
#EXTM3U
#EXT-X-TARGETDURATION:1
#EXT-X-VERSION:7
#EXT-X-INDEPENDENT-SEGMENTS
#EXT-X-MEDIA-SEQUENCE:4
#EXT-X-DISCONTINUITY-SEQUENCE:0
#EXTINF:1.000,
a_5.ts
#EXTINF:1.000,
a_6.ts
#EXTINF:1.000,
a_7.ts
```

The fMP4 variant of the same output after a resume from persisted state
([persistence.md](persistence.md)):

```
#EXTM3U
#EXT-X-TARGETDURATION:1
#EXT-X-VERSION:7
#EXT-X-INDEPENDENT-SEGMENTS
#EXT-X-MEDIA-SEQUENCE:8
#EXT-X-DISCONTINUITY-SEQUENCE:1
#EXT-X-MAP:URI="m_7.mp4"
#EXTINF:1.000,
m_9.m4s
#EXTINF:1.000,
m_10.m4s
#EXTINF:1.000,
m_11.m4s
```

### 2.1 Media sequence

The media sequence of the window is the first segment's position number
minus one. A segment's number in the RFC sense is therefore its position
number minus one only while position numbers have no gap and do not go
back. They have gaps after an aborted segment and after a resume, and go
back after a `stop` without persistence
([segmenting.md §8](segmenting.md#8-position-numbers); findings F-PL-1,
F-LIFE-1).

## 3. Main playlist

Named by `playlist` (default `stream.m3u8`), in the output directory.

**When.** After the first segment close of any variant, at most once per
run: once written, or once the writer has declined, it is not written again
until a `stop` without persistence has deleted it
([files.md §7](files.md#7-cleanup-on-stop)). With persistence, a `stop`
and `start` in the same process do not rewrite it; a new process writes it
again after its first segment close.

**Content.** The operator calls `main_playlist_writer` with
`extra_tags`, `prefix`, `version`, and one string per variant: the variant
name, with methods `bandwidth`, `codecs` and, when known, `video_size`
([variants.md §3](variants.md#3-derived-properties)). `null` writer or a
`null` result: no main playlist, logged at level 5. A string: written as is.

### 3.1 `hls.playlist.main`

`hls.playlist.main(~extra_tags=[], ~prefix="", ~version=7, streams)`:

1. A non-empty `prefix` without a trailing `/` gets one.
2. The lines are `#EXTM3U`, `#EXT-X-VERSION:<version>`, each extra tag, then
   for each stream:
   `#EXT-X-STREAM-INF:BANDWIDTH=<b>[,CODECS="<codecs>"][,RESOLUTION=<w>x<h>]`
   and `<prefix><name>.m3u8`. `CODECS` is left out when the string is
   empty; `RESOLUTION` when there is no video size.
3. Lines are joined with CR LF and the result ends with CR LF.

Example (from the test suite):

```
#EXTM3U
#EXT-X-VERSION:54
foo
bar
#EXT-X-STREAM-INF:BANDWIDTH=123,CODECS="codecs_a"
prefix/stream_a.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=456,CODECS="codecs_b",RESOLUTION=12x34
prefix/steam_b.m3u8
```

Every variant is a `STREAM-INF`, audio-only ones included; there is no
`EXT-X-MEDIA` grouping, no `AVERAGE-BANDWIDTH`, `FRAME-RATE` or
`EXT-X-INDEPENDENT-SEGMENTS`.

## 4. URIs and prefix

Segment and init URIs in media playlists are `prefix` followed by the file's
base name: the prefix is concatenated as is, and a name with directories
loses them in the playlist. In the main playlist, `hls.playlist.main` adds
the missing `/` to the prefix (finding F-PL-3).
