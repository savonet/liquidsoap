# The test suite

As built. What today's tests check, by principle, grouped by the subsystem
each protects. Each entry names what is **binding**; unless said otherwise,
file names, log wording, timing and the harness are incidental. Test names
are given only as a trace back to the source. Entries marked KC are
situations of [known-complexity.md](known-complexity.md).

## Doubles a harness needs

- A source that can be switched off and on while running, so a fallible
  output goes idle and starts again.
- A clock that runs unpaced, so segments are produced faster than real
  time.
- A recorder of file-change events, run synchronously, that can act inside
  the event (switch the source, read the file, end the test).
- A reader for the produced HLS: an HLS input that decodes the playlist and
  reports metadata, and a probe that lists the streams of a playlist.
- A file system that can fail on demand: the output directory replaced by a
  file, then restored; directories made unwritable.
- A metadata injector on the source.
- Persisted state carried from one process run to the next.

## 1. Operator and construction

- An unwritable output directory, `temp_dir` or `persist_at` directory is
  rejected before streaming. Binding: the script fails, and the message
  names which directory. (GH5261)
- An output with `persist_at` in a directory that does not exist yet, on a
  source that ends, stops cleanly and runs its `on_stop`. Binding: the stop
  completes. (GH2926)
- A file-system error while streaming does not end the process; once
  `reopen_on_error` has repaired the cause, a new segment is created.
  Binding: a second segment creation after the error. (GH5259)

## 2. Segmenting

- Ten consecutive segments of an AAC MPEG-TS variant with 2 s segments
  total 20 s within one frame. Binding: the sum of the segment durations
  the naming function receives. Incidental: individual durations. Guards
  the grid against drift (KC2). (GH5319)
- A video variant whose frame duration is larger than usual (0.1 s) and
  whose key frames come every 12 frames produces files. Binding: one file
  event. (hls_large_frame)
- Forced cuts, split points and restarts are checked by the FFmpeg layer's
  conformance suite ([encoder.md §8.8](../../../optionals/ffmpeg/spec/encoder.md#88-conformance)).

## 3. Playlists

- `hls.playlist.main` produces exact bytes for: extra tags, a prefix with
  and without a trailing `/`, no prefix, `CODECS`, `RESOLUTION` present and
  absent. Binding: every byte, CR LF included. (hls_main_playlist)
- Variant `extra_tags` reach media playlists of an ADTS and an fMP4
  variant; main `extra_tags` reach the main playlist after trimming;
  `insert_tag` on one variant reaches that variant's playlist. Binding:
  presence of the tag text. Incidental: position. Not checked: that the
  inserted tag is absent from the other variant. (hls_custom_tags)
- Codec strings of H.264 variants in MPEG-TS and fMP4 are the same full
  string (`avc1.64001f`), as given to the main playlist writer. Binding:
  the strings. (hls_global_headers)
- Discontinuities after a source off/on cycle with persistence: exact
  playlist bytes at three points. **Skipped**: the test is disabled and its
  expected bytes predate the current grid. Its intent: the media sequence
  advances across the gap, the next segment is marked
  `EXT-X-DISCONTINUITY`, and the discontinuity sequence counts it once it
  leaves the window. (hls_discontinuity)

## 4. Encoder seam

- Seven variants (ADTS, MPEG-TS with ID3v2.4, MPEG-TS with `id3=false`,
  `%shine`, `%mp3`, `%fdkaac`, fMP4) run side by side. Read back through
  the HLS input: the MPEG-TS-with-ID3 variant reports the injected title and
  album; the variant with `id3=false` reports none. Binding: those two
  outcomes. The fMP4 check asserts that **no** metadata is read back, so it
  fails the day the reader supports fMP4 ID3. The packed-audio variants are
  built but not read back. (hls_id3v2)
- The first MPEG-TS segment's bytes contain the title text of its timed ID3,
  not an empty tag. Binding: the text in the segment bytes. (GH5084)
- `%ffmpeg` fMP4 from encoded raw content, from an encoded file, and from a
  copied file; three MPEG-TS variants each taking one audio track and
  dropping the others. Binding: the playlist probes as the expected codecs,
  layout, sample format and rate (copy: an audio and a video stream).
  Incidental: segment count, names. (ffmpeg_raw_hls, test_ffmpeg_hls_mp4,
  test_ffmpeg_hls_copy_mp4, test_ffmpeg_drop_tracks)

## 5. Persistence

- A run with copied fMP4 and MPEG-TS variants saves its state at shutdown;
  a second process, with `strict_persist`, restores it from a copy of the
  directory and produces a playlist that probes as H.264 and AAC. Binding:
  the restore succeeds strictly and the playlist decodes. Not checked: the
  continuity of media and discontinuity sequences, the reuse of old segments.
  (test_ffmpeg_distributed_hls_setup, test_ffmpeg_distributed_hls)

## 6. Not covered

No test checks: retention counts and which files are deleted, cleanup at
`stop`, file modes, the bytes of the persisted state, media-sequence
continuity across an error, a stop/start or a resume, the `deleted` event
paths, the packed-audio ID3 bytes, the serving wrapper, or a stop and start
without persistence. Each is where a finding sits ([findings.md](findings.md)).
