# Conformance

The checks an implementation of this spec is run against, by principle,
grouped by the document each protects. Each names what is **binding**;
file names, log wording, timing and the harness are incidental unless said
otherwise. Every entry of [known-complexity.md](known-complexity.md) is a
check here, cited by its number.

## Doubles a harness needs

- A source that can be switched off and on while running, so a fallible
  output goes idle and starts again.
- An unpaced clock, so segments are produced faster than real time.
- A recorder of file-change events, run synchronously, that can act inside
  the event.
- A reader for the produced HLS: playlist parser, segment probe (streams,
  first packet of each stream, timestamps), ID3 parser.
- A file system that can fail on demand: an output directory made
  unwritable or replaced by a file, then restored; a temporary directory on
  another file system.
- A metadata injector on the source.
- Persisted state carried from one process run to the next, and a state
  file that can be corrupted or truncated.
- A poller that reads every published playlist and checks that each URI it
  names exists, at every file-change event.
- Content with a fixed offset between key frames and the grid: copied H.264
  with B-frames, a 2 s GOP, plus AAC-LC and HE-AAC.

## 1. Operator

- Each construction error of [operator.md §2](operator.md#2-construction) —
  unwritable directories, `segment_duration` under one frame, `segments`
  0, negative overhead, empty or duplicate variant names, a multi-line
  `extra_tags` entry, `id3` against the encoder — stops the script before streaming, naming the argument.
- A missing bandwidth never fails: an `%mp3` VBR variant without
  `bandwidth` announces a measured one (KC9).
- `%wav`, `%ogg`, `%flac` and `%ffmpeg` WebM or Matroska variants are
  rejected at construction, naming the variant.
- A strict restore of an invalid state file makes the output fail without
  calling `reopen_on_error`.
- Sleep while the source is down (output idle), without persistence,
  leaves no file the output wrote; `clock.stop()` then `clock.start()` with
  persistence restores from `persist_at`.
- A tag inserted through a `streams()` entry kept from before a stop
  reaches the next run.
- A file-system error mid-stream: the output resumes after
  `reopen_on_error`'s delay; the first segment after recovery is
  discontinuous in every variant and starts with a key packet of every
  stream; no temporary file remains; other outputs never stop. A `stop`
  during the outage completes (KC24).
- A stop where one variant's flush fails still flushes and closes the
  others.
- A `segment_name` returning `../x` makes the output fail without retry.
- A synchronous callback that raises on a segment's `created` event: the
  segment is listed, numbering has no gap.

## 2. Segmenting

- **Field scenario** (KC3, KC4): three copied H.264 variants with
  B-frames, AAC-LC and HE-AAC audio, a 2 s GOP, 6 s segments, and an
  audio-only variant. Binding: every `EXTINF` is 6 s within one video frame,
  except the first segment of a run; no long/short alternation; segment _n_
  starts at the same media time in every variant within one video frame;
  media sequence and segment counts are equal across variants. Repeat with
  key frames a few frames before each boundary: same result.
- Grid without drift (KC2): an AAC MPEG-TS variant with 2 s segments, ten
  segments sum to 20 s within one frame; HE-AAC and AAC-LC variants of one
  source, after fifty segments, start segment _n_ within one codec frame.
- Bound (KC1): re-encoded video with key frames every 4 s and 1 s segments
  starts every segment on a key frame; the same video copied keeps every
  rounded `EXTINF` within `T`, logs each forced cut, and announces no
  `EXT-X-INDEPENDENT-SEGMENTS`.
- A segment is never empty and never of length 0 (KC5); an inserted tag or
  discontinuity of a dropped or aborted segment appears on the next one.
- An MPEG-TS segment starts with a PAT and a PMT.
- Restart (KC22): two copied files whose H.264 extradata differ into an
  fMP4 variant beside an encoded audio-only variant: the segment after the
  switch is discontinuous in both, starting within one codec frame of the
  switch, the fMP4 one preceded by the new `EXT-X-MAP` and starting with a
  key packet; discontinuity sequences are
  equal across variants; timestamps keep increasing; boundaries after the
  switch are on the grid.

## 3. Playlists

- At every file-change event, every URI in every published playlist names
  an existing file (KC7, KC10), including init segments and names with
  sub-directories.
- Media sequence: across an aborted segment, a stop and start without
  persistence, and a resume with persistence, every segment keeps its
  media sequence number while listed, and the number never goes back
  (KC8, KC16).
- Discontinuities (KC8): stop and restart a three-variant output with
  persistence: each variant's next segment is discontinuous; after the
  window has scrolled past it every discontinuity sequence is 1; the first
  listed segment carries no `EXT-X-DISCONTINUITY`; a run without restart
  shows none.
- Window: never under three target durations, its start never moves back,
  also when short segments follow long ones; `EXT-X-VERSION` is 6.
- Main playlist (KC9, KC11): written once every variant has a segment;
  exists after each of five stop/start cycles; rewritten when a restart
  changes codecs; a writer returning `null` leaves no file and no error.
  The same H.264 variant in MPEG-TS and fMP4 gives the same full codec
  string; an unknown one gives no `CODECS` and a warning.
- `hls.playlist.main` bytes for extra tags, prefixes with and without `/`,
  `CODECS` and `RESOLUTION` present and absent. Binding: every byte.
- Inserted tags (KC20): a tag inserted once appears exactly once, before
  the `EXTINF` of the first segment opened after it, in that variant only,
  in insertion order; a tag pending at stop appears after restart; a tag
  with a line break is rejected.

## 4. Encoder seam

- Packed audio (KC18): every segment starts with an ID3 tag whose `PRIV`
  timestamp equals the segment's start in 90 kHz units, also with 0.0232 s
  frames; the bytes of [encoder-packed-audio.md §7](encoder-packed-audio.md#7-insert_id3)
  are reproduced exactly.
- MPEG-TS timed ID3 (KC18): a title change appears as a timed ID3 packet at
  the media time of the change; in-stream tag times strictly increase, also
  when a cut lands before the change; with replay, every segment carries the
  current title from its start; with `id3=false`, none.
- Cuts on metadata (KC6): with only free-split variants (`%mp3`, `%ffmpeg`
  AAC in MPEG-TS, `%fdkaac`), a new title starts a new segment within one
  codec frame of its media time in every variant, counts stay equal, and
  the grid after it is unchanged; with a copied video variant and the default, no
  cut; the same metadata sent ten times causes no cut; a single fMP4 variant
  with no ID3 is not cut on metadata; with only free-split variants, an
  inserted tag starts a new segment at the next chunk.
- Init segments (KC21): non-empty; the first media segment holds the first
  frame; deleted once no retained segment refers to them.
- Large covers in metadata keep memory flat over an hour (KC19).

## 5. Files

- Retention (KC7): with `segments=3` and `segments_overhead=1`, a segment
  stays on disk at least its length plus the window duration after leaving
  the playlist, and is deleted once neither allowance of
  [files.md §5](files.md#5-retention) holds.
- Modes: under umask 022, files are `0644` and directories `0755` with the
  defaults.
- Temporary directory on another file system (KC12): every published file
  is complete; one warning at construction.
- Events (KC13): per file, one `created`, any `updated`, at most one
  `deleted`, with the same path; none for temporary or state files.
- Cleanup: a final stop without persistence leaves no file the output
  wrote, and deletes no other file, decoys in the working directory
  included (KC11).
- A stop and start without persistence (a fallible source off for 2 s)
  keeps every file; the next segment is discontinuous in every variant and
  numbering continues (KC17).
- A segment deleted by hand mid-stream is logged and streaming continues;
  removing the whole directory before stop lets stop complete (KC15).
- Sub-directories in `segment_name` created mid-stream (KC14).

## 6. Persistence

- Round trip (KC16): media and discontinuity sequences and the window
  continue across a process restart; the next segment is discontinuous.
- A truncated or corrupt state file: a fresh start with a warning, nothing
  half applied; with `strict_persist`, `start` fails.
- A crash during the save leaves the previous or the new state, complete.
- A process killed mid-stream, then restarted with persistence, continues
  numbering from its last published segment.
- A state naming a segment deleted by hand restores the newer segments,
  drops it and the older ones, and keeps the counters.

## 7. Serving

- Every file a playlist names is served with the content type of
  [serving.md §4.1](serving.md#41-content-types), init segments included
  (KC25).
- `..%2F`, `%2e%2e/`, a literal `../`, a NUL byte, temporary files and the
  state file give 404; a key file placed in the directory is served.
- A 404 carries the configured headers; a configured `Cache-Control`
  replaces the generated one.
- After a restore, restored segments and rewritten playlists are served.
- Playlists and segments carry the cache headers of
  [serving.md §4.2](serving.md#42-caching).
