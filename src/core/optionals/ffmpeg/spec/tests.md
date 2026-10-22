# Test suite of the FFmpeg layer

This file describes, by principle, what the existing test suite pins about the FFmpeg layer: for each subsystem, the invariants the tests assert, which part of each assertion is binding and which is incidental, the failure modes the tests simulate, and the fixtures and doubles a new harness needs to assert the same things. It relies on the behaviour described in the subsystem files it links to ([content.md](content.md), [encoder-format.md](encoder-format.md), [encoder.md](encoder.md), [encoder-copy.md](encoder-copy.md), [timestamps.md](timestamps.md), [decoder.md](decoder.md), [image-decoder.md](image-decoder.md), [inline-codecs.md](inline-codecs.md), [filters.md](filters.md), [bitstream-filters.md](bitstream-filters.md), [side-data.md](side-data.md), [converters.md](converters.md), [io.md](io.md)) and restates none of it. Test names appear only in the trailing "Trace:" list of each group. Findings on the tests: [findings/tests.md](findings/tests.md).

## 1. Harness model

### 1.1 Outcome protocol

A test is one script run as a separate process. Its outcome is its exit status:

| Outcome | How the script reaches it                                                                                               | Exit status    | Reported as                                  |
| ------- | ----------------------------------------------------------------------------------------------------------------------- | -------------- | -------------------------------------------- |
| pass    | an explicit "pass" call, which prints a line and requests a clean shutdown; later pass calls are ignored                | 0              | success                                      |
| fail    | an explicit "fail" call, an equality or near-equality assertion that does not hold, or any uncaught error in any thread | 1              | failure, with the captured stdout and stderr |
| skip    | an explicit "skip" call                                                                                                 | 123            | success, with a warning annotation           |
| timeout | no exit within 10 minutes                                                                                               | process killed | failure                                      |
| crash   | the process dies on a signal                                                                                            | —              | failure, naming the signal                   |

An assertion that fails inside a callback raises an error and also schedules "fail" on a separate thread, so an assertion in a callback whose errors are swallowed still fails the test.

A test passes only by calling "pass". Reaching the end of input, closing a file or stopping a source does nothing by itself; each test calls "pass" from the event it is waiting for (file closed, output stopped, track started, metadata seen, segment named, file created, timer fired).

### 1.2 Oracles

Almost every media assertion goes through an independent prober run on the produced file: the stream list as JSON, optionally with per-stream packet counts. The fields read are `codec_type`, `codec_name`, `channels`, `channel_layout`, `sample_rate`, `sample_fmt`, `pix_fmt`, `width`, `height`, `r_frame_rate`, `duration` (stream or container), the `DURATION` tag (Matroska, `H:M:S.frac`), `nb_read_packets` and the rotation entries of `side_data_list`. A separate transcoder extracts subtitle streams to SRT or ASS text for exact comparison, extracts a still frame, and computes SSIM against a reference image.

The prober's output is parsed against a declared shape. A missing required field makes the parse raise, which fails the test.

### 1.3 Clocks and pacing

Most media tests run their sources on a clock with no synchronisation (CPU-paced), so a 10 s encode takes a fraction of that. Tests of live behaviour (HLS segment timing, buffer underrun, SRT, harbor) run on real time with a wall-clock deadline that calls "fail".

### 1.4 The generated encoder and decoder matrix

The media suite is generated from one list of encoder configurations, each a literal encoder expression. A configuration's escaped form is also the name of the file it produces: `%`→`@`, `"`→`'`, `(`→`[`, `)`→`]`, so `%ffmpeg(format="mp4",%video(codec="libx264")).mp4` produces `@ffmpeg[format='mp4',@video[codec='libx264']].mp4`.

1. Each configuration is substituted into one of seven templates by the shape of its tracks: audio only, video only, audio+video, multitrack (two audio, two video), audio+subtitles, video+subtitles, audio+video+subtitles.
2. The template builds the input from generators: a sine for audio, a test pattern for video, the 5-cue subtitle fixture for subtitles; 10 s, or 16 s when subtitles are present; played once.
3. The template writes the file and, on close, probes it with packet counting.
4. The pass condition, per declared track kind (every stream for multitrack): the stream exists, and its duration is at least 10 s **or** it holds at least one packet.

The 33 configurations:

| Shape                 | Count | Configurations                                                                                                                                                                                                      |
| --------------------- | ----- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| audio only            | 20    | fdkaac (mono LC, stereo), shine (mono, stereo), flac, wav, mp3 (mono, stereo each), ogg with vorbis/flac/opus (mono, stereo each), `%ffmpeg` mp4 aac: default, `samplerate="48k"`, `pcm_s16` input, `pcm_f32` input |
| video only            | 1     | mp4 libx264                                                                                                                                                                                                         |
| audio+video           | 3     | mp4 aac mono + libx264; aac stereo + libx264; aac stereo + libx264 `r=12`                                                                                                                                           |
| multitrack            | 1     | mp4 `audio` aac stereo, `audio_2` aac mono, `video` and `video_2` libx264                                                                                                                                           |
| audio+subtitles       | 2     | matroska aac + subrip; aac + ass                                                                                                                                                                                    |
| video+subtitles       | 3     | matroska libx264 + subrip; libx264 `b="500k"` + ass; webm libvpx + webvtt                                                                                                                                           |
| audio+video+subtitles | 3     | matroska aac + libx264 + subrip; aac `b="128k"` + libx264 + ass; mp4 aac + libx264 + mov_text                                                                                                                       |

The produced files are then the inputs of the decoder tests:

| Decoder test family            | Input files                                              |
| ------------------------------ | -------------------------------------------------------- |
| audio decoding (3 tests)       | `%mp3(stereo)`, `%flac(mono)` outputs                    |
| video decoding (2 tests)       | mp4 aac stereo + libx264 output                          |
| audio+video decoding (9 tests) | mp4 aac stereo + libx264, mp4 aac mono + libx264 outputs |

A further 12 standalone media tests and about 25 hand-wired media tests run outside the matrix. Every generated decoder and standalone test depends on every matrix file, as does the media suite itself, so one failing encoder configuration blocks all of them.

### 1.5 Suites

| Suite               | Contents                                                                 | Runs in CI                                                               |
| ------------------- | ------------------------------------------------------------------------ | ------------------------------------------------------------------------ |
| core unit tests     | OCaml executables asserting on content values, typing, encoder factories | yes, except four that no build rule runs ([findings](findings/tests.md)) |
| type snapshots      | inferred types of short scripts diffed against expected text             | yes                                                                      |
| language            | typing and encoder-expression acceptance and rejection                   | yes                                                                      |
| regression, streams | one script per issue or behaviour                                        | yes                                                                      |
| media               | the matrix of §1.4 and the hand-wired media tests                        | yes, separate job                                                        |

## 2. Fixtures and doubles

A new harness needs the following. Durations, sizes and offsets are binding where the group sections say so.

| Fixture or double                       | Production                                                                                                            | Used to provoke                                                       |
| --------------------------------------- | --------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------- |
| audio generator                         | sine, any frequency; silence                                                                                          | finite or infinite audio of known length                              |
| video generator                         | test pattern via the FFmpeg layer's own source, default size and rate                                                 | finite video of known length                                          |
| subtitle cue file                       | 5 SRT cues "First"…"Fifth subtitle line", at 1–3, 4–6, 7–9, 10–12, 13–15 s                                            | exact subtitle timing, track length 15 s                              |
| three copy-concat files                 | 1280x720 30 fps H.264 mp4 of 20, 30, 10 s whose first frame PTS is 1235, 756 and 245 frames (41.17 s, 25.2 s, 8.17 s) | a copied stream whose timestamps jump backwards at each file boundary |
| two mpeg-ts halves                      | 1 s mpeg2video, 320x240 and 240x320, 25 fps                                                                           | a mid-stream size change                                              |
| byte concatenation of the two halves    | plain byte concatenation                                                                                              | a size change coinciding with a backward timestamp jump               |
| remuxed concatenation of the two halves | concat then stream copy by the transcoder                                                                             | a size change with continuous timestamps                              |
| colour change file                      | the same, with bt470bg then bt709 colourspace                                                                         | a mid-stream colourspace change with continuous timestamps            |
| rotation files                          | 2 s 320x240 25 fps mpeg4, remuxed with display rotation 90, 33 and 20 degrees                                         | rotation side data, quarter and non-quarter turns                     |
| long a/v file                           | 30 s 1920x1080 24 fps yuv420p + 440 Hz sine                                                                           | HLS segmenting, resume                                                |
| a/v/subtitle mkv                        | 30 s 1280x720 mpeg4 + aac + srt                                                                                       | three-kind stream copy                                                |
| still images                            | 1280x720 and 50x50 test pattern, written as JPEG; one PNG frame from the long file                                    | image decoding, overlay                                               |
| 504x504 file                            | 2 s test pattern + sine                                                                                               | frame size taken from the first decoded file                          |
| 5 s wav                                 | sine                                                                                                                  | filter argument types                                                 |
| DVD bitmap subtitle file                | fetched from the FFmpeg sample suite                                                                                  | bitmap subtitle burn-in                                               |
| VP9 with alpha                          | fetched from a public media archive                                                                                   | alpha-preserving decode and encode                                    |
| SSIM reference image                    | committed                                                                                                             | burn-in output still frame                                            |
| fetch cache                             | per-user cache keyed by URL stamp; a partial download is never stamped                                                | offline reruns                                                        |

Behavioural doubles:

- an in-memory content format with integer packets and a name-only parameter, built on the same chunk container as the copy and raw contents ([content.md](content.md)), to assert chunk arithmetic without libav;
- hand-built encoder descriptions (the parsed form of an encoder expression) passed to the encoder factory, to assert derived values without a script;
- a read-only byte callback with no seek, tell or length, to drive the stream decoder;
- a child process running a one-line script, to observe startup rejection and its message;
- a sender and a receiver in the same process (SRT on a fixed local port) to test a MIME-described raw stream.

## 3. Content

Content here is the chunked container behind copy and raw tracks: a list of chunks, each with a length in ticks, a stream index, a time base and packets at tick positions ([content.md](content.md)).

### 3.1 Invariants

1. The length of a content is the sum of its chunk lengths. A chunk keeps a positive length with no packets (a sparse stream) through copy, sub, truncate and blit.
   Binding: lengths in ticks; empty-packet chunks survive.
2. Extracting the range `[offset, offset+len)` keeps a packet at position `p` exactly when `offset ≤ p < offset+len`, moves it to `p − offset` relative to its chunk's start in the range, clips each chunk's length, and keeps each chunk's stream index and time base. Example: packets at 10, 30, 50, 70, range 20+40 → packets at 10 and 30. A packet at the end bound is excluded: packets at 50, 100, 150, range 50+100 → 0 and 50.
   Binding: the half-open bound, the shifted positions, unchanged stream index and time base.
3. A range spanning several chunks yields one chunk per overlapped chunk, in order, each clipped. Chunks of 50 and 50, range 25+50 → chunks of 25 and 25; a packet at 10 of the second chunk stays at 10. Ranges compose: a range of a range gives the same packets as the direct range.
   Binding: chunk count, order, lengths, positions.
4. Truncating the first `n` ticks equals the range `[n, length)`.
5. Blitting into an empty destination adopts the source parameters. Adjacent source chunks with the same stream index merge into one chunk, packet positions offset by the preceding chunks' lengths (25+25 with a packet at 10 in the second → one chunk of 50 with that packet at 35). Non-adjacent chunks with the same stream index stay separate. Two successive blits that continue each other in the destination merge when their stream indices are equal and stay two chunks when they differ.
   Binding: the merge rule keyed on stream index and adjacency, the shifted positions.
6. Blitting over a range that overlaps an existing destination chunk removes that whole chunk: a 50-tick blit into the middle of a 200-tick chunk leaves a content of length 50.
   Binding: the whole overlapped chunk goes, not only the overlapped ticks.
7. Appending concatenates chunk lists in order, keeping each stream index and time base.
8. A copy is independent of its original: emptying the original's chunk list leaves the copy's length unchanged.
9. Different time bases on chunks of one content are kept per chunk through copy and range (1/48000, 1/44100, 1/90000).
10. A fresh data value has length 0, stream index 0, time base 1/1 and no packets; making a content with a requested length still yields no chunks.
11. The serialized form of each FFmpeg content format is its name and a colon with nothing after: `ffmpeg.copy:`, `ffmpeg.audio.raw:`, `ffmpeg.video.raw:`. Parsing that form gives back the same format.
    Binding: the strings and the round trip. Incidental: column layout of the dump.
12. A track of the internal media kind and a track of kind `ffmpeg.video.raw` do not unify in either direction.
13. The type annotations `ffmpeg.copy`, `ffmpeg.audio.raw`, `ffmpeg.video.raw`, `ffmpeg.audio.raw(sample_rate=44100, channel_layout="5.1")`, `ffmpeg.video.raw(pixel_format=yuva420p)` are accepted on source tracks, and a copy track type accepts attached methods.

Incidental: the dummy format's own compatibility and checksum rules (the tests only check that the generic operations delegate to them).

### 3.2 Failure modes simulated

None: these are pure value tests. Error paths of content (incompatible parameters on blit, the copy compatibility check) are not exercised ([findings](findings/tests.md)).

Trace: ffmpeg_content_base_test, ffmpeg_dummy_content_test, format_dump_test, types.ml, typing.liq, type_errors.liq.

## 4. Encoder format

The encoder expression and its type ([encoder-format.md](encoder-format.md)).

### 4.1 Invariants

1. A track's kind comes from its name or an explicit content argument. A name containing `audio` or `video` as an underscore-separated part (`%foo_audio`, `%bla_video_gni`, `%audio_2`) sets the kind; `%foo(audio_content)` and `%bla(video_content)` set it explicitly. An encoded or raw track with neither (`%foo`, `%bla_gni`) is rejected. A copy track (`.copy`) accepts any name and rejects a content argument.
   Binding: accepted versus rejected. Incidental: the error raised (any error counts as rejection).
2. `channels` and `ac` take only literal values; a variable is rejected.
3. `channel_layout=5.1` (bare or quoted) gives the track the type `pcm(5.1)`. From the implementation: `%ffmpeg(format="mp4", %audio(codec="aac", channel_layout="5.1"), %video.copy)` has type `format(audio=pcm(5.1), video=ffmpeg.copy)`, and `%ffmpeg(format="mp4", %audio(channel_layout=5.1))` has `format(audio=pcm(5.1))`.
4. An inline encoder's type names the FFmpeg content types even where libav is not linked: `ffmpeg.encode.audio` takes `format(audio=pcm('C), 'B)` and `source(audio=pcm('C), 'D)` and returns `source(audio=ffmpeg.copy('A))`.
   Binding: argument and result track types. Incidental: the list of source methods in the snapshot, the type-variable letters.
5. `pcm_s16` and `pcm_f32` input kinds are accepted only by `%ffmpeg`; `%mp3(mono, pcm_s16)`, `%vorbis(pcm_f32)` and `%ogg(%vorbis(pcm_s16))` are rejected with error kind `encoder`.
   Binding: the error kind.
6. Accepted at typecheck: `metadata=[...]` at the top level; container options (`movflags`, `frag_duration`, `fflags`); codec options as strings or numbers; `hwaccel` and `hwaccel_device` on a hardware codec; `ignore_keyframe` and `wait_for_keyframe` on copy tracks; `.drop` tracks.
7. The bitrate an encoder reports for HLS is absent when no track sets `b`, and is 128000 when one track sets `b="128k"`.
   Binding: absent versus present, the integer in bits per second.

Trace: type_errors.liq, encoders.liq, ffmpeg_encoder.liq (snapshot), ffmpeg_track.liq (snapshot), encoder_pcm_kind_test, ffmpeg_quality.

## 5. Encoder

Muxing, encoding and copying into a file or segment ([encoder.md](encoder.md), [encoder-copy.md](encoder-copy.md)).

### 5.1 Invariants

1. Every matrix configuration (§1.4) produces a file whose prober output lists each declared track kind with at least one packet.
   Binding: stream presence by kind. Incidental: durations (the condition is a disjunction with the packet count).
2. With video tracks declared last, output streams appear in the order the encoder declares them, whatever mix of copy and encode (video declared before a build-time stream comes after it: [encoder-internal.md §2.2](encoder-internal.md#22-video-at-mk_stream)). Declaring `en` (copy), `fr` (encode, 1 ch), `audio_fr` (2 ch), `encoded_fr` (1 ch), `director_cut_video`, `encoded_director_cut` gives streams with 2, 1, 2, 1 channels then two video streams. Declaring copy `audio` (stereo source) then encoded `audio_2` (1 ch) gives 2 then 1.
   Binding: order and channel counts.
3. Default encoded parameters, from generated inputs: aac gives `stereo`, `fltp`, `44100`; libx264 gives `h264`, `yuv420p`.
   Binding: these four values; they are the oracle of most media tests.
4. An encoded video stream's frame rate is the internal video frame rate, not the input's: with the internal rate at 45, a 25 fps input re-encoded has `r_frame_rate` `45/1`.
5. Copy tracks taken from two different files into one output keep both codecs (mp3 audio from one file, H.264 video from another).
6. A source with tracks the encoder does not declare is encoded with only the declared ones (a video-only encoder on an audio+video source runs).
7. Dropping the metadata and track-mark fields of a muxed source before the output means metadata insertions and track marks upstream produce no metadata and no new track at the output; the file holds exactly the three declared streams (aac stereo, mp3 mono, copied h264 yuv420p).
   Binding: zero tracks and zero metadata seen at the output, stream count 3.
8. The output filename callback is called once for a file the encoder opens once.
9. Three FFmpeg encoders on one shared source, with different bitrates, run to the end and all three files close.
10. HLS variants built from libx264 report the codec string `avc1.64001f` for both an mpeg-ts and an mp4 variant.
    Binding: that the codec string comes from the encoded stream and is the same across containers. Incidental: the exact profile/level digits, which depend on the host's x264.
11. ID3 metadata: an mpeg-ts variant with `id3_version=4` and an adts variant with `id3_version=3` carry the title in-band; the first mpeg-ts segment's bytes contain the title text; an mpeg-ts variant with `id3=false` carries none.
    Binding: present versus absent, the title bytes.
12. Subtitles: subrip, ass, mov_text and webvtt tracks encode alongside audio and video; two subtitle tracks (subrip and ass) in one file; a copied and an encoded subtitle track in one file; subtitles decoded from a produced file re-encode to ass next to copied video. None of these raises an output error.
13. A custom `text_to_ass` function receives the cue index and text and its return value becomes the cue's ASS event fields after the layer: returning `"<i>,0,CustomStyle,,0,0,0,,MARKER:<text>"` yields `Dialogue: 0,0:00:01.00,0:00:03.00,CustomStyle,,0,0,0,,MARKER:First subtitle line`, and so on for the five cues.
    Binding: the full Dialogue lines, times included.
14. A bitmap subtitle track added onto video is burned in: the output has audio and video and no subtitle stream, and its still frame at 4.00 s has SSIM ≥ 0.95 against the reference.
15. VP9 with alpha re-encoded with libvpx-vp9 keeps `yuva420p` and a positive duration.
16. One FFmpeg encode feeding copy variants into mpeg-ts and fragmented mp4 HLS, run once with persisted state and then resumed from that state with strict persistence, produces a playlist that probes as h264 + aac stereo fltp 44100.
    Binding: the resumed run starts and produces a probeable playlist. Not asserted: timestamp continuity across the resume.

### 5.2 Failure modes simulated

- Output errors during a write: subtitle tests install an error handler that fails the test, so any reopen counts as a failure.
- Several encoders on one source: concurrency on shared input frames.
- Upstream metadata and track marks that must not reach the muxer.

Trace: the encoder matrix (encoder\_\*.liq.in), ffmpeg-naming-convention.liq, ffmpeg-copy-encode.liq, multitrack.liq, ffmpeg_video_decoder.liq, GH3840.liq, video-only.liq, GH2842.liq, GH5110.liq, hls_global_headers.liq, hls_id3v2.liq, GH5084.liq, subtitle_multi.liq, subtitle_copy_encode.liq, subtitle_reencode.liq, test_subtitle_text_to_ass.liq, test_bitmap_subtitle_to_video.liq, test_ffmpeg_vp9_alpha.liq, test_ffmpeg_distributed_hls_setup.liq, test_ffmpeg_distributed_hls.liq, test_ffmpeg_hls_mp4.liq, test_ffmpeg_hls_copy_mp4.liq, ffmpeg_raw_hls.liq, GH2926.liq, GH5259.liq, GH5261.liq, hls_large_frame.liq.

The HLS-specific tests (GH2926, GH5259, GH5261, hls_large_frame, the persistence and segment-naming parts of the distributed HLS tests) protect the HLS output; they use `%ffmpeg` only as a convenient segment encoder.

## 6. Timestamps

What the suite pins about timestamps, on both the encoding and decoding sides ([timestamps.md](timestamps.md) owns the rules). No test inspects individual packet timestamps: every timestamp assertion is on an aggregate (a stream duration, a cue time, a segment duration sum, a signal level after a seek).

### 6.1 Invariants

1. **Copy across files whose timestamps go backwards.** Three copied H.264 files of 20, 30 and 10 s, whose first frames are at 41.17 s, 25.2 s and 8.17 s, played in sequence into one copy output, give a single stream whose duration is exactly `60.0`.
   Binding: exact equality with the sum of the inputs, so no gap, no overlap and no dropped tail at either boundary, although each new file starts earlier than the previous one ended. Incidental: where the output timeline starts (the prober's duration is relative to the stream's own start).
   This run sets the copy compatibility check to relaxed, because the three files differ in reported frame rate.
2. **Subtitle cue times across repeated tracks, encoded.** The 5-cue file repeated under a 32 s video, encoded to subrip in Matroska and extracted back, gives 11 cues: cues 1–5 at their own times, cues 6–10 shifted by 15 s, cue 11 at 31–33 s.
   Binding: every cue time to the millisecond, so a repeated subtitle track restarts at the previous track's end, and that end is the last cue's end (15 s), not the file's length. A cue that starts before the video ends keeps its full end time.
3. **The same, copied.** Two subtitle tracks in sequence into a copy subtitle output give the identical 11 cues.
4. **Decoded subtitle absolute times.** With the FFmpeg decoder, the 5-cue file played twice reports its 7th cue at absolute start time `19.0` s (second pass offset 15 s plus 4 s).
5. **Cue times into ASS.** The five cues re-encoded as ASS keep `0:00:01.00`…`0:00:15.00` exactly (§5.1 item 13).
6. **Mid-stream video format change, decoded and re-encoded.** A 2 s mpeg-ts stream whose frame size changes at 1 s, with a backward timestamp jump at the change, decoded and encoded to mp4, has a duration strictly between 1.5 and 2.5 s.
7. **Mid-stream format change through a filter.** A 2 s stream whose size, or colourspace, changes at 1 s with continuous timestamps, decoded to raw frames, sent through a filter and raw-encoded, has a duration strictly between 1.5 and 2.5 s.
   Binding for 6 and 7: neither half is lost and neither is doubled. Incidental: the exact duration.
8. **Raw audio across format changes.** Three 10 s files (48 kHz aac, mono wav, stereo wav) decoded to raw audio and raw-encoded to one aac stream at 44100 Hz give a duration above 29 s.
   Binding: the lower bound only.
9. **Segment durations sum to the grid.** An aac mpeg-ts HLS output with a 2 s segment duration: the durations of its first 10 segments sum to 20 s within one internal frame duration.
   Binding: the total; a single segment may be off by one frame.
10. **Encoder matrix durations.** Each matrix output has a duration of at least 10 s or at least one packet (§1.4).
11. **Seek pre-roll.** Seeking to 2 s in a 4 s 32 kbps mono mp3 (decoded by whichever decoder ranks first), the RMS over the 0.1 s following the seek exceeds 0.1.
    Binding: audio is present immediately after a seek, so the decoder is fed the packets before the seek point. Incidental: the threshold.
12. **Remaining time known.** An FFmpeg-decoded flac reports a positive remaining time at position 0.1 s.
13. **Image duration.** An image annotated with a 0.2 s duration ends its track: at least three tracks follow one another.
14. **Content time bases.** Chunk time bases survive every content operation (§3.1 item 9), and chunks of different stream indices never merge (§3.1 item 5), which is what a per-stream timestamp offset relies on.

Run but not asserted on timestamps: a filter fed alternately 25 fps and 12 fps files across nine tracks with a file reopen in the middle (asserts codec parameters only); inline encode then decode (codec parameters only); the HLS resume (codec parameters only); a skipped HLS discontinuity test whose expected playlists pin exact segment durations (2.000, 1.980, 1.960, 0.040) and a discontinuity tag.

### 6.2 Failure modes simulated

- Backward timestamp jumps at a file boundary in copy mode (item 1) and inside one decoded stream (item 6).
- Format changes with and without a timestamp discontinuity (items 6, 7).
- Repetition of a short subtitle track under a longer video (items 2–4).
- A seek into a codec with a bit reservoir (item 11); the 32 kbps rate makes the silent window longest.

Trace: test_ffmpeg_copy_concat.liq, subtitle_concat.liq, subtitle_concat_copy.liq, test_ffmpeg_on_subitles.liq, test_subtitle_text_to_ass.liq, test_ffmpeg_video_format_change.liq, test_ffmpeg_raw_format_change.liq, test_ffmpeg_raw_implicit_conversion.liq, GH5319.liq, encoder matrix, ffmpeg_seek_preroll.liq, GH-action-919422659.liq, test_image_decoder_duration.liq, ffmpeg_dummy_content_test, test_ffmpeg_filter_changing_rate.liq, hls_discontinuity.liq (skipped).

## 7. Decoder

The file and stream decoders ([decoder.md](decoder.md), [image-decoder.md](image-decoder.md)).

### 7.1 Invariants

1. Copy decoding preserves each stream's parameters as the prober sees them. Decoding the a/v matrix files with every track in copy mode and muxing them into Matroska gives, per kind, the same `channel_layout`, `sample_rate`, `sample_fmt`, `pix_fmt` and `codec_type` as the input. With an a/v/subtitle file, the same holds for audio, video and subtitles, and `codec_name` matches too.
   Binding: equality with the input, per kind. Incidental: stream indices (removed before comparison).
2. Mixed modes from one file work in every combination tested: copy audio + encoded video, encoded audio + copied video, copy audio + raw-encoded video, raw audio + raw video, encoded audio + raw video.
3. The content type requested for each track decides decode versus copy per track: decoded audio only, decoded video only, both decoded, each mixed combination, both copied; audio + subtitles in the four decode/copy combinations. A track kind absent from the file (subtitles on an a/v file, video on an audio/subtitle file) is absent from the result. Opening with an explicit input format detects the same format as probing.
4. The stream description of each matrix file pins: the container format name (`mp4`, `matroska,webm`, `ogg`, `wav`, `aac`, `mp3`, `flac`), and per stream the field name (`audio`, `audio_2`, `video`, `video_2`, `subtitles`), kind, codec name, time base, sample rate, channel count and layout, or width, height, pixel format and frame rate.
   Binding: field names, kinds, codec names, audio and video parameters. Incidental: time-base values, which the host muxer chooses (for example 1/12800 for 25 fps H.264 in mp4, 1/1000 in Matroska).
5. Decoding into `pcm_s16` or `pcm_f32` content succeeds on an mp3.
6. Decoded video is scaled to the frame size the source's type annotation requests (47x52), and the internal frame size is taken from the first decoded file when not set (504x504).
7. The stream decoder for MIME `application/ffmpeg` decodes wav and mp3 from a read-only callback until end of input; invalid-data errors from the decode step are skipped. The MIME parameters `format=s16le,ch_layout=mono,sample_rate=48000` describe a headerless PCM stream received over SRT, which then produces a track.
8. A per-codec decoder override (`settings.decoder.ffmpeg.codecs.vp9` set to `libvpx-vp9`) selects the decoder that keeps the alpha plane.
9. Demuxer options passed through a request annotation (`ffmpeg_options='fflags="+bitexact"'`) reach the input.
10. Subtitles: an SRT file through the FFmpeg decoder yields cues with absolute and relative times, text, format and forced flag; a raw FFmpeg input on an SRT file yields copy subtitles; a DVD file yields bitmap subtitles usable as a video overlay.
11. With `settings.decoder.decoders` set to `["ffmpeg"]`, an mp3 or flac is decoded and re-encoded to wav with the requested channel count and 48000 Hz.
    Binding: none on the decoder itself; the output encoder fixes both values ([findings](findings/tests.md)).

### 7.2 Failure modes simulated

- Input whose timestamps jump and whose format changes (§6.1 items 6, 7).
- Seek (§6.1 item 11).
- A read callback with no seek, tell or length.

Trace: ffmpeg_copy_decoder.liq, ffmpeg_copy_and_encode_decoder.liq, ffmpeg_raw_decoder.liq, ffmpeg_raw_and_encode_decoder.liq, ffmpeg_raw_and_copy_decoder.liq, test_ffmpeg_copy_subtitle.liq, ffmpeg_get_type_test, ffmpeg_stream_description_test (all_media_file_descriptions.json), pcm_s16_decode.liq, pcm_f32_decode.liq, video_size.liq, test_video_size_autodetect.liq, stream_decoder_test, srt_raw_pcm.liq, test_ffmpeg_vp9_alpha.liq, test_bitmap_subtitle_to_video.liq, test_ffmpeg_on_subitles.liq, subtitle_copy_encode.liq, ffmpeg_audio_decoder.liq, mono.liq, stereo.liq, ffmpeg_video_decoder.liq, test_image_decoder_duration.liq, GH-action-919422659.liq, ffmpeg_seek_preroll.liq.

## 8. Inline codecs

Encoding and decoding as source operators ([inline-codecs.md](inline-codecs.md)).

### 8.1 Invariants

1. Inline audio, video and audio+video encoders produce copy tracks that a copy output muxes as is, and inline decoders turn them back into decoded tracks that an encoding output re-encodes. Both files probe as aac stereo fltp 44100 and/or h264 yuv420p.
   Binding: both outputs exist with those parameters.
2. Three inline aac encoders at 48k, 96k and 192k on one shared source each feed an adts copy output to completion.
3. An inline encoder on an external raw input produces a track.
4. Raw inline encode, then a filter, then raw inline decode keeps producing frames after the filter graph has ended on input underrun: at least 25 frames in the 5 s after start, while the buffered input is empty for most of the first second.
   Binding: the graph is rebuilt and output resumes. Incidental: 25 frames.
5. An inline raw audio+video encode feeds an HLS output whose variant raw-encodes to aac 128k and libx264.
6. An encoder after a `pcm_s16` decode of a silent source runs.

Trace: ffmpeg_inline_encode_decode.liq, ffmpeg_inline_encode_decode_audio.liq, ffmpeg_inline_encode_decode_video.liq, GH5110.liq, GH2602.liq, GH3944.liq, ffmpeg_raw_hls.liq, unified-pcm-types.liq.

## 9. Filters

Filter graphs and bitstream filters ([filters.md](filters.md), [bitstream-filters.md](bitstream-filters.md)).

### 9.1 Invariants

1. A graph with one audio and one video input and output, built from named filter operators, processes a decoded a/v file into a file with aac and h264 streams.
2. A textual graph description with named input and output pads (`[v_in]…[v_out];[a_in]…[a_out]`) is bound to the given inputs and its named outputs are returned by kind and name.
3. Filter arguments of every scalar type (float, int, bool, string with units such as `-18dB`) and array arguments (`sample_fmts=["s32","fltp"]`) are accepted.
4. A filter that sets frame metadata (`ametadata` add `tag=test`) with metadata passing enabled on input and output makes that metadata appear on the output source's metadata track.
   Binding: the key and value seen by a metadata handler.
5. Filters absent from the host FFmpeg are guarded by a definition check (`aexciter`, `loudnorm`); the graph builds without them.
6. A two-input overlay graph produces h264 yuv420p video with aac audio.
7. A graph rebuilt after its input ran dry produces output again (§8.1 item 4).
8. A frame-size or colourspace change at the graph input passes through without losing either half (§6.1 item 7).
9. A filter input declared with 2 channels on a mono-typed source is a typing error (this test is not run, [findings](findings/tests.md)).
10. Bitstream filters `h264_mp4toannexb` and `aac_adtstoasc` on copy tracks leave the parameters a prober reports unchanged.
11. A text overlay operator produces aac stereo and h264 yuv420p.

Trace: ffmpeg_filter.liq, ffmpeg_filter_parse.liq, test_ffmpeg_filter_array_args.liq, test_ffmpeg_complex_filter.liq, test_ffmpeg_transparency_filter.liq, test_ffmpeg_filter_changing_rate.liq, test_ffmpeg_raw_format_change.liq, GH3944.liq, test_ctype.liq, ffmpeg_bitstream_filter.liq, ffmpeg_add_text.liq.

## 10. Side data

Rotation side data on video tracks ([side-data.md](side-data.md)). One test, seven outputs, all from 320x240 files stored with a display rotation.

### 10.1 Invariants

| Path               | Input rotation | Operation                                                    | Binding result                                                                           |
| ------------------ | -------------- | ------------------------------------------------------------ | ---------------------------------------------------------------------------------------- |
| copy               | 90             | read side data, copy                                         | 320x240, rotation 90; the track's rotation reader returns 90.0                           |
| copy               | 90             | set rotation 180                                             | rotation of absolute value 180                                                           |
| copy               | 90             | remove the `Display Matrix` entry                            | rotation 0                                                                               |
| decode, encode     | 90             | none                                                         | 240x320, rotation 0: decoding applies the rotation to the pixels and drops the side data |
| decode, raw encode | 90             | set rotation 90 on the raw track                             | rotation 90 on the encoded stream                                                        |
| decode, raw encode | 33             | the undecided-rotation handler returns a clockwise transpose | rotation 0                                                                               |
| decode, raw encode | 20             | the handler returns no decision                              | rotation within 2° of 20                                                                 |

The undecided-rotation handler is called at least once, every call has reason `odd_rotation`, and every reported angle is within 2° of 33 or 20 (sign ignored).

Binding: dimensions, the rotation values to the degree (to 2° for non-quarter turns), the reason string. Incidental: the sign of a rotation, the number of handler calls.

Trace: test_ffmpeg_side_data.liq.

## 11. Converters

Audio resampling and video scaling or pixel-format conversion through FFmpeg ([converters.md](converters.md)).

### 11.1 Invariants

1. Decoded video is scaled to an annotated frame size (47x52) and the result encodes at that size.
2. Raw audio of different sample rates and layouts (48 kHz, mono, stereo) converts to one raw encoder's 44100 Hz stereo stream (§6.1 item 8).
3. An alpha pixel format (`yuva420p`) survives decode and encode.
4. An overlay of a 50x50 still over a 1280x720 still converts both to the graph's format and encodes `yuv420p`.

Most decoder tests force the native resampler, and the others take the first available resampler in the default order (libsamplerate before FFmpeg), so the FFmpeg resampler runs only on hosts without libsamplerate.

Trace: video_size.liq, test_ffmpeg_raw_implicit_conversion.liq, test_ffmpeg_vp9_alpha.liq, test_ffmpeg_transparency_filter.liq.

## 12. IO

The raw input and its error reporting ([io.md](io.md)).

### 12.1 Invariants

1. Opening a nonexistent input raises an error of kind `ffmpeg` to the source's error handler.
   Binding: the kind; the cause is "no such file or directory". Incidental: the message wording (`Avutil.Error(No such file or directory)`) and the source file named by the last trace position.
2. Leading and trailing whitespace around an input URL is trimmed: `" ./file1.mp3\n"` connects and the error handler is not called.
3. A lavfi-format input started from a URL string produces audio. When it is replaced inside a dynamic source it is stopped, and then garbage-collected after a forced compaction on the domain that created it; stopping precedes collection.
4. An HTTP input typed for copy feeds an SRT output with a copy audio encoder and the script starts.

### 12.2 Failure modes simulated

- A missing file (ENOENT).
- A URL padded with whitespace.
- An active input dropped while running.

Trace: ffmpeg-errors.liq, ffmpeg-trim.liq, GH5389.liq, ffmpeg-copy-input-http.liq.
