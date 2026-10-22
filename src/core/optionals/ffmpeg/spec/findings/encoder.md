# Findings: the `%ffmpeg` encoder

Scripts and outputs of every "checked" item are under
`/var/tmp/claude-ffmpeg-spec/encoder/`.

## Defects

### D1. HLS segments start after the keyframe, not at it

`Ffmpeg_encoder.wrap` (`on_stream_keyframe`, ffmpeg_encoder.ml:63–68) calls
the split callback before setting the track's own keyframe flag, so
`can_split` inside the callback (ffmpeg_encoder_common.ml:326–330) never sees
the arriving keyframe's track as ready. The split is taken at the start of the
next frame instead. Matters: every HLS segment after the first starts with
non-key packets; players must discard up to a frame of data or fail to decode
the segment start; the key packet ends the previous segment.
**Checked**: `hls.liq` (mpegts, AAC + H.264 `g=25`, 2 s segments): `v_3.ts`
begins `pts 2.16 / 2.08`, non-key; `v_2.ts` ends with the key packet
`pts 2.00 K`. ffprobe reports `non-existing PPS 0 referenced` at each
segment start.

### D2. Readiness count does not make same-source tracks wait for each other

encoder-copy.md §12.3. `check_stream` (ffmpeg_copy_encoder.ml:91–101) sets
the shared record's `ready <- not waiting_for_keyframe` at each switch, which
overrides the count. An intra-only (audio) track always starts at its switch,
before the sibling video's keyframe, contradicting the intent stated in
`mk_stream_store`'s comment. **Checked**: `cut1.liq` and `cut2.liq` (second
source cut 0.6 s before a keyframe, both field orders): B's audio is muxed from
the switch on while B's video is absent.

### D3. `total_count` counts all copy tracks, not those of one source

`copy_count` (ffmpeg_encoder.ml:79–82) is the number of copy tracks in the
format. For tracks from different sources the per-source count can never
reach it; they start only through the override of D2 (added for GH3840,
#3841). Removing the override would bring back GH3840; keeping it defeats the
same-source wait. Matters: no value of the current scheme meets both halves of
the intent. **Read only** (consistent with GH3840 history and D2 runs).

### D4. Shared `last_start` rises after a track has begun: forward jump

timestamps.md §7, §11.2. A track that begins a source before a sibling with
a larger candidate writes its first packet(s) with its own offset, then jumps
forward when the sibling raises the shared max. **Checked**: `seq2.liq`
(`%video.copy, %audio.copy`): audio packet 1 at 0.0000 s, packet 2 at
0.0800 s (expected 0.0232 s); same at the switch (2.0769 → 2.1569 s). The
order of tracks in the format decides whether this happens (`seq.liq`, the
other order, shows no jump).

### D5. Explicit removal of a source record while a sibling still holds it

encoder-copy.md §12.5, language-notes §5.3. `remove` drops the entry at
`ready_count <= 1` (ffmpeg_encoder_common.ml:78–81) even when another track
has it as `current_stream` and has not begun; that track's `begin_stream`
then gets a fresh record, losing the shared `last_start` and readiness.
Example: audio and video both waiting on source S; audio begins (count 1),
S ends and audio switches away before the video's keyframe (count 1 →
removed); the video's late keyframe gets a fresh S record. **Read only**.

**Verification** (verification pass): Narrower: the track that gets a fresh record is the only one left on that source, so only its own offset changes (to its own candidate). No cross-track desync beyond D4.

### D6. A packet without dts disables the next monotonicity check

timestamps.md §6.3. `last_dts := dts` (ffmpeg_copy_encoder.ml:149) stores
`None`, so the packet after a dts-less one is never compared. A
non-increasing dts right after can reach the muxer (which then raises).
**Read only**.

### D7. Static and runtime channel counts disagree

encoder-format.md §5 vs §10.1. Type checking gives `channel_layout`
precedence; the runtime parser applies arguments in order.
**Checked**: `ch.liq` with `%audio(codec="aac", channel_layout="stereo",
channels=1)` fails at run time with `Error 5: this value has type
source(audio=pcm(stereo), _) but it should be a subtype of
source(audio=pcm(mono), _)` at "Unknown position".

### D8. `subtitle_content` marker is passed to the codec as an option

lang_ffmpeg.ml:187–190 consumes `audio_content` and `video_content` only. A
`%sub(subtitle_content, codec=…)` track stores option `"" = "subtitle_content"`,
which is then reported as an unrecognized option. **Read only** (needs a
subtitle source to run).

### D9. Codec-string fallback matches codec names against the muxer name

encoder.ml:183–184: `format = Some "libmp3lame"` / `Some "aac"` are codec
names; no muxer has them, so the fallback never fires for real formats
(`mp3`, `adts`). **Read only**.

### D10. Copy stream parameters are fixed by the first source

encoder-copy.md §12.7. **Checked**: `cut1.liq` — switching from an
mp4-sourced H.264 (AVCC) to a TS-sourced one (Annex B) makes the mpegts
muxer's bitstream filter raise `Invalid data found when processing input`;
the output stops (fallible). Nothing compares the new source's parameters.

### D11. `to_string` is lossy and not re-parsable

encoder-format.md §11: `metadata=["title"="x"]` is not the input syntax;
`pcm_s16`/`pcm_f32`, `sample_format`, `pixel_format`, `hwaccel_pixel_format`,
`text_to_ass` are dropped. **Checked** (`p.liq`).

## Asymmetries

### A1. Raw video re-anchors on a new source; raw audio does not

timestamps.md §2–§4. Raw audio keeps its first resampler and ignores source
ids; a new source with another rate or layout goes through the old resampler
(ffmpeg_internal_encoder.ml:189–216). **Read only**.

### A2. Copy timestamps follow source dts; internal ones follow sample/frame counts

A copy track's output time can drift from Liquidsoap time (gaps after a
source's last packet are not counted, packets dropped while waiting are not
counted, timestamps within a source are trusted), while internal tracks
follow sample/frame counts. A format mixing copy and internal tracks has no
common reference. **Read only**.

### A3. `interleaved=default` counts drop tracks

ffmpeg_encoder_common.ml:168: `1 < List.length ffmpeg.streams` includes
`%x.drop`; `%audio(…), %video.drop` is interleaved. **Read only**.

### A4. `+global_header` is not added when the track has `flags`

ffmpeg_encoder.ml:47 uses `replace_default`; `q`/`qscale` always set
`flags`, so a quality-based track never gets `+global_header` from the
encoder (the bindings still request global headers when the muxer needs them,
[avformat.md §4.4](../../../../../modules/synced/ffmpeg/spec/avformat.md)).
**Read only**.

### A5. Container option check runs after stream checks

encoder.md §3.4.6. An unknown container option is reported only when all
stream options are valid; the target file is already created. **Checked**
(`opt.liq`).

### A6. Ogg restart only for `Stream` outputs

`output.file` of `%ffmpeg(format="ogg")` (a `Url` output) drops in-stream
metadata after the header (ffmpeg_encoder_common.ml:356–364). **Read only**.

### A7. Type-level alpha differs from instantiation-level alpha

encoder-format.md §10. **Checked** (`t.liq`: `pixel_format="yuva420p"` types
as `yuv420p`).

## Gaps

### G1. Copy store survives Ogg restart, with GC-dependent effect

timestamps.md §6.5. Whether new copy tracks restart at 0 or continue from the
old offset depends on whether the old records were collected; `ready_count`
can exceed `total_count`, and `ready` is only set on equality. **Read only**.

### G2. Frame-rate converters dropped without draining

timestamps.md §4.3: frames held by the old `fps` graph are lost at each
source / format change (ffmpeg_internal_encoder.ml:454–461). **Read only**.

### G3. Audio re-framer and resampler never flushed

encoder-internal.md §8: `flush = fun () -> ()` for audio; the re-framer is
never sent `Flush`. **To verify** with a precise sample count: `tail.liq`
runs (0.4644 / 0.49 / 0.5 / 0.51 s of sine → 21 / 22 / 23 / 23 AAC packets)
did not settle whether the last partial frame reaches the encoder.

**Verification** (verification pass): Worse: confirmed by run. 1 s stereo sine at 44100 Hz through `%ffmpeg(format="adts", %audio(codec="aac"))` gives 44 packets; the ffmpeg CLI gives 45 for the same input. The last partial frame is lost on every file.

### G4. Copy content without parameters crashes `mk_stream`

ffmpeg_copy_encoder.ml:58 `Option.get params`. **Read only**.

### G5. `header ()` is empty for Harbor listeners

encoder.md §7, §10: a listener joining a shared encoder of a container format
(ogg, matroska, mp4) gets no container header. `dedicated_encoder=true` is the
workaround. **Read only**.

### G6. Stream-level `Unrecognized options` for video is late

Video streams are created at the first frame, so a bad video option is
reported when data arrives, not at output start. **Read only**.

### G7. Copy pts is never checked

Only dts is checked (timestamps.md §6.3); pts < dts or non-increasing pts of
non-B streams pass. **Read only**.

### G8. Last audio packet of each copied file missing

Observed while making timestamps.md §11: each 88-packet AAC file contributes
87 packets to the output (`seq.liq`: 174 audio packets for two files). The
missing one is the last; the encoder never sees it. Belongs to the decoder
side; reported here because it shows in the encoder's output. **Checked**.

**Verification** (verification pass): Narrower: reproduced only when a video stream is also selected. `A.mp4` to `%audio.copy` keeps 88/88 packets; `%audio.copy, %video.copy` keeps 87/88 (matroska and mpegts). The video ends 0.06 s before the audio; probable cause is the end-of-track cut at the shortest field, not isolated. Owner: decoder.

## To verify

### V1. Rounding to a coarse stream time base

Strictly increasing dts in main ticks (1/44100) can round to equal values in
a 1/1000 muxer time base; whether strict muxers then reject the packet is
FFmpeg behaviour, not checked.

### V2. `setpts`/`fps` with missing or decreasing input pts

timestamps.md §4.4: what FFmpeg's filters do with `NOPTS` and backward pts
under `A+PTS-STARTPTS` was not checked.

### V3. webm HLS init segment

encoder.md §8.3: the first `init_encode` for webm returns an empty init
(nothing written before the header) and the HLS output then discards that
frame's data on `Not_enough_data`; the second call's init segment may start
mid-stream. Read only; not run.

### V4. Reconnect as source switch

io.md states each reconnect allocates a new source id; read in
`ffmpeg_decoder.ml:538` (one id per decoder) — consistent with
encoder-copy.md §12.6. Not run against a live reconnect.
