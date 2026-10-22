# Findings: FFmpeg decoder

Line numbers refer to `src/core/optionals/ffmpeg/decoder/` unless stated.

## Defects

### Seek target ignores the container start time

What: the seek target is `position () + x`, with `position ()` derived from
`container_duration − remaining` (ffmpeg_decoder.ml:755-759). Before any frame
is decoded `remaining = container_duration`, so the target is the cue-in in
seconds from 0, compared against absolute stream PTS (ffmpeg_decoder.ml:427).
For a file whose start time is not 0 the cue-in is short by the start time.
Why: wrong cue-in on MPEG-TS and similar files, silently.
Checked: a 3.004 s MPEG-TS file with start time 1.4 s and `liq_cue_in=2.0`
plays 2.40 s instead of about 1.0 s.

### Remaining time is short by the container start time

What: `set_remaining` subtracts the absolute end PTS from the container
duration (ffmpeg_decoder.ml:733-748). It goes negative before the real end.
Why: crossfades and `on_end`-style logic fire early by the start time.
Checked: same MPEG-TS file reports `remaining = 1.584` at 0.02 s, and 0.584 at
1.02 s, with about 2 s left.

### A seek target is never cleared: later timestamp resets are discarded

What: `target_position` stays set after the target is reached
(ffmpeg_decoder.ml:427-432). Any item whose PTS later falls below it goes to
the discard buffer.
Why: chained Ogg (and any stream whose timestamps restart) loses everything
after the restart once a cue-in was applied.
Checked: two 3 s Vorbis files concatenated, decoded by ffmpeg with
`liq_cue_in=1.0`, play 1.99 s instead of about 5 s; without cue-in, 6.01 s.

### Copy chunk lengths drift: each DTS step is truncated separately

What: `Ffmpeg_utils.Duration.push` converts each timestamp difference to
ticks with integer division (base/ffmpeg_utils.ml:480-482), so the fractional
tick of every step is lost and the error accumulates.
Why: copied content runs shorter than its timestamps, about 0.03% for AAC in
Matroska; over an hour this is about a second against other tracks and the
remaining-time estimate.
Checked: a 600.023 s Matroska AAC file decoded as `ffmpeg.copy` plays
599.834 s. The same helper is used by the raw decoders, the encoder and the
filters ([common.md, Duration helper](../common.md); the effect on encoder
output belongs to [timestamps.md](../timestamps.md)).

### Raw decoders drop the last frame at end of stream

What: the raw decoder's `` `Flush `` arm is a no-op (ffmpeg_raw_decoder.ml:34);
`Duration.flush` is never called, so the held group (at least the last frame)
is lost. The copy decoder does flush (ffmpeg_copy_decoder.ml:60-63).
Why: every raw track loses its last frame; for long-frame codecs this is
audible.
Read only.

**Verification** (verification pass): Confirmed by run: a 2 s AAC file written to wav gives 88064 samples through the raw path and 88200 through the internal path; the 136 missing samples are the last frame.

### Per-stream metadata is never emitted for files

What: in `tracked_stream` the condition `(not !is_first) ||
decode_first_metadata` is false while `is_first` is true, and `is_first` only
becomes false inside the branch (ffmpeg_decoder.ml:527-534). With
`decode_first_metadata = false` (files) the branch never runs.
Why: the intent reads as "skip the first metadata", the effect is "skip all".
Container-level changes still go through (§10.1 of decoder.md), which is
what carried the title change of a chained Ogg in the check below.
Read only (a chained Ogg showed `title=two` at 3.0 s, so its source could not
be isolated without instrumentation; see To verify).

### Stream decoder seek ignores `whence`

What: the custom seek given to `open_input_stream` is `fun len _ -> fn len`
(ffmpeg_decoder.ml:818-822): `SEEK_CUR` and `SEEK_END` are treated as absolute
offsets.
Why: a demuxer seeking relative to the current position or the end lands at
the wrong byte.
Read only.

### Subtitle frame PTS converted with the stream time base

What: subtitle frames carry their PTS in `1/1000000`
([avformat.md](../../../../../modules/synced/ffmpeg/spec/avformat.md),
`read_input`), but `update_position` converts every PTS with the stream's
time base (ffmpeg_decoder.ml:351-361). For a Matroska subtitle stream
(1/1000) the stream `pts` is 1000 times too large.
Why: after a seek, subtitle frames before the target are not pre-rolled
away; the interleaving window compares them with other streams' positions
wrongly.
Read only.

### Second seek before any decoded frame jumps past the end

What: a seek sets `remaining` to none (ffmpeg_decoder.ml:782); until the next
audio or video item `get_remaining` is −1 and `position ()` is
`ticks(D) + 1`, so a further seek targets past the end.
Why: two seeks in a row (cue-in then a script seek) misbehave.
Read only.

### Normal end of file logs at level 2

What: `End_of_file` reaches `Decoder.mk_decoder.fread`, which logs every
exception with `Utils.log_exception` (severe) as `Decoding "<file>" ended:
Ffmpeg_decoder.End_of_file.` plus the backtrace at level 4.
Why: every track end looks like an error in the logs.
Checked: seen on every run.

### Matroska `DURATION` tag breaks `request.duration`

What: the metadata resolver returns the stream tag `DURATION`
(`00:00:03.023000000`); lowercased to `duration`, `Request.duration` reads it
first and `float_of_string` fails, so the result is `null`.
Why: `request.duration` returns `null` for most Matroska files.
Checked: `request.duration.ffmpeg("av.mkv")` is `null`; with
`resolve_metadata=false` it is `3.023`.

## Asymmetries

### Field numbering differs between probe and decoder

What: the probe numbers fields over readable streams only
(base/ffmpeg_stream_description.ml:174, 192, 213), the decoder over all
streams (ffmpeg_decoder.ml:539-546). A stream whose parameters raise shifts
the probe's fields but not the decoder's.
Why: the probed type and the decoded fields disagree for such files.
Read only.

### Data fields numbered after subtitles in the description

What: `collect_data_streams` uses `data_n (count + subtitle_count)`
(base/ffmpeg_stream_description.ml:231-232).
Why: the first data stream of a file with one subtitle stream is described
as `data_2`.
Read only.

### Image extension tests differ in case handling

What: `get_file_type` and the generic test lowercase the extension
(`Utils.get_ext_opt`); `create_decoder` (ffmpeg_decoder.ml:800-808) and
`check_image` (ffmpeg_image_decoder.ml:47-52) use `Filename.extension`, which
keeps the case.
Why: `IMG.PNG` is rejected by the ffmpeg image decoder and, as raw video, gets
no `loop`/`framerate` options.
Checked: `IMG.PNG` falls to the `sdl` image decoder; `img.png` is taken by
`ffmpeg`.

### Remaining options checked only at decoder creation

What: the probe, the duration resolver and the metadata resolver never call
`check_opts`.
Why: a file with a bad `ffmpeg_options` passes the probe and resolves a
duration, then fails at play time.
Checked: `annotate:ffmpeg_options="foo=1,f=mp3":a.mp3` probes, then fails with
`Unrecognized options: foo=1`; `request.duration.ffmpeg` with `foo=1` gives a
duration.

### Unknown forced format is ignored

What: `Av.Format.find_input_format` returns `None` for an unknown name and the
code passes it as "no format" (ffmpeg_decoder.ml:178).
Why: a typo in `f=` silently probes.
Checked: `f=nosuchformat` gives `3.0` for an mp3.

### Error message names MIME types for metadata options

What: a parse error in `ffmpeg_options` raises `Invalid mime type
arguments!` (ffmpeg_decoder.ml:190).
Read only.

### Interleaving drops instead of aligning

What: the change that introduced the interleaving buffer (#3616) described
releasing data within ±30 ms of the last stream seen; the flush now keeps
everything within `max_interleave_duration` (5 s) of it
(ffmpeg_decoder.ml:397-399). Pending items are sorted by raw `ts` across
streams with different time bases (ffmpeg_decoder.ml:395).
Why: up to 5 s of initial desynchronisation remains; order across streams
follows raw numbers.
Read only.

### Internal audio layout comparison

What: the initial input layout is the codec's layout; each frame is compared
with the **default** layout for its channel count
(ffmpeg_internal_decoder.ml:84-116).
Why: a file with a non-default layout logs `Frame format change detected!`
and rebuilds the resampler on its first frame, then uses the default layout.
Read only.

### Duplicate keys in `file.metadata.ffmpeg`

What: the resolver returns stream tags then container tags; the request path
keeps the first value, `file.metadata.ffmpeg` (via `from_list`) the last.
Checked: `encoder` is `Lavc… aac` in `file.metadata`, `Lavf63.7.100` in
`file.metadata.ffmpeg`.

### Image MIME test uses the full MIME string

What: the image test for streams compares the full MIME type
(ffmpeg_decoder.ml:830), while selection uses the part before `;`.
Read only.

## Gaps

### No preferred-codec settings for subtitles

What: `conf_codecs` is built from audio and video decoders only
(ffmpeg_decoder_common.ml:45-83); `configure_subtitle_stream` never finds a
key.
Read only.

### Pending interleave items are lost at end of stream

What: `mk_eof` flushes decoders but not the pending list. A selected
non-sparse stream that never yields a timestamped item keeps everything
pending until the end, and nothing is output.
Read only.

### Internal audio resampler is not drained

What: `` `Flush `` is a no-op in `mk_audio_decoder`
(ffmpeg_internal_decoder.ml:95); samples held by swresample are lost.
Read only.

### Seek failure leaves a target set

What: `target_position` is set before `Av.seek`; when the seek raises, the
mapping is not rebuilt and the target stays (ffmpeg_decoder.ml:307-315).
Read only.

### Rebuild after seek is outside the lock

What: `Atomic.set decoder_ref (prepare_decoder …)` runs after
`mutable_lock` returns (ffmpeg_decoder.ml:780-781); a concurrent `decode`
can read with the old mapping and the new target.
Read only.

### Subtitle positions absolute, advance positions relative

What: subtitle timelines place items at absolute PTS, while `advance` gives
positions relative to each stream's first timestamp
(ffmpeg_decoder.ml:364, ffmpeg_internal_decoder.ml:186).
Why: on a file with a non-zero start time, subtitles are placed late by the
start time relative to the advance-driven gaps.
Read only.

### Duplicate default MIME entry

What: `application/octet-stream` appears twice in the default list
(ffmpeg_decoder_conf.ml:37-38).
Read only.

## To verify

- Where the chained-Ogg title change comes from (container metadata vs.
  stream metadata): instrument `tracked_stream` and `mk_decoder` metadata
  emission.
- Whether `Avutil.Error \`Invalid_data`can repeat at the same position and
make`decode` loop forever.
- The effect of the `Duration` truncation on encoder output timestamps
  ([timestamps.md](../timestamps.md)).
- Whether bitmap subtitle rectangles positioned in the subtitle's own
  coordinate space land correctly when the video size differs from it.
