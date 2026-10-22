# Conformance of the FFmpeg layer

This file owns the conformance specification of the layer: the rules a test harness MUST follow, the fixtures and doubles it needs, the failure modes it simulates, the scenarios that cross several subsystems, and the check each known-complexity entry gets. The conformance items of each subsystem are owned by that subsystem's file, at the section they check; this file links them by subsystem, in the reading order of [README.md](README.md), and adds only what a harness needs on top. A conforming implementation passes every linked item and every item of this file, under the harness rules of §1.

## 1. Harness rules

These rules are binding. A suite that breaks one of them reports no conformance, whatever its tests say.

### 1.1 Outcomes and reporting

Each test ends in exactly one outcome:

| Outcome | Reached by                                                                                  | Counts as |
| ------- | ------------------------------------------------------------------------------------------- | --------- |
| pass    | the test's oracles ran on the produced data, every assertion held, and §1.2's count was met | pass      |
| fail    | an assertion that does not hold, an uncaught error in any thread, the count of §1.2 not met | failure   |
| skip    | a prerequisite the host lacks, detected at run time and named (a codec, a filter, a device) | skip      |
| timeout | no outcome before the test's deadline                                                       | failure   |
| crash   | the process dies on a signal or an abort                                                    | failure   |

- A run report lists the count of each outcome, and names every skipped test with its missing prerequisite. A skip is never reported or counted as a pass.
- A test MUST NOT skip unconditionally. A test that cannot run anywhere is removed or fixed, not skipped.
- A missing fixture is a skip when the fixture is fetched (§2.2) and the network is unavailable, and a failure in every other case.
- A conformance claim lists the items that were not checked (skipped, or not applicable on the host). An item never run is unknown, not passed.
- An assertion that fails inside a callback fails the test even where the callback's caller swallows errors.
- One failing test or fixture never prevents an unrelated test from running. A fixture failure fails exactly the tests that use the fixture.

### 1.2 A check that can say the opposite

A pass is evidence only when the same check would have failed on a broken implementation.

1. **Count what ran.** Every test declares what it examines (packets, frames, samples, cues, segments, assertions) and asserts, before passing, that the count examined reaches the declared value. A test that examined nothing fails.
2. **Pass after the oracle.** A test passes from the event that completes its oracles (output closed and probed, the last expected item seen), never from the first track, the first frame, a fixed delay, or any file close. A test whose subject is start-up states so and asserts on the start-up result (the type, the error, the first item's content).
3. **Discriminating values.** Each asserted value is one the subsystem under test decides. The input differs from the expected output on that property, and no later stage forces the property on its own. Examples: a decoder's channel handling is asserted on the decoded content, or through an output whose parameters follow its input, never through an encoder configured to the asserted value; a filter's sample-format change is asserted through an encoder and container that keep that sample format.
4. **Selected component.** A test of one implementation among several (a decoder, a resampler, a converter) asserts that this implementation ran, from the layer's own report of the chosen component. Raising another implementation's priority above it, or forcing another one, makes the test a test of something else.
5. **Positive control.** A check that looks for an absence (no error logged, no gap, no warning) first sees the presence on a case built to have it, in the same harness.

### 1.3 Negative controls

Every check of §23 (the known-complexity entries) and every timestamp check (§1.4) MUST be seen to fail against a deliberately broken build before its pass counts.

- The harness keeps, per such check, one or more named **mutations**: a minimal change to the implementation that breaks the rule the check protects (§23 names one per entry). A mutation is a build variant used only by the harness; it is not shipped and adds no surface.
- The mutated build MUST fail the check, on the assertion that protects the rule, not on an unrelated error. The harness records which assertion failed.
- The negative control is repeated whenever the check, its fixture, or the code under the rule changes. A check whose mutation passes is broken and counts as a failure of the suite.

### 1.4 Timestamps at packet level

A timestamp check asserts on individual packets. Aggregate values (a stream duration, a cue time, a segment sum) MAY be asserted in addition, never instead.

The packet oracle lists, per output stream and in file order: stream index, DTS, PTS, duration, key flag, size, and the stream's time base. From it a check derives, as its subject requires:

- DTS strictly increasing per stream ([timestamps.md §10](timestamps.md#10-summary-how-order-is-kept));
- PTS ≥ DTS for every packet;
- for a copied stream, PTS − DTS of every packet equal to its source packet's value within one rounding of one time-base conversion;
- the first packet of each stream, of each source and of each segment, its key flag and its timestamps;
- packet counts per stream, compared with the inputs' counts;
- gaps and overlaps at each join, as end of the previous packet (DTS + duration) against the next DTS.

Source packets for these comparisons are listed by the same oracle on the input fixtures. Tolerances are stated per check in units of a time base or of a codec frame, never in wall-clock time.

### 1.5 Rejections pin their reason

A test that expects a rejection (a type error, an evaluation error, a start-up failure, a runtime error) asserts:

- the error kind;
- every item the owning rule says the error names (the field, the option key, the codec, the parameter sets);
- the place: type check, evaluation, output start, or switch time, as the owning rule states.

Any other error fails the test, a parse error in the test's own text first of all. An "expects an error" helper MUST fail when the call returns normally. Message wording is incidental unless the owning rule makes it binding.

### 1.6 Oracles

- Media assertions use an oracle independent of the implementation under test: an external prober and transcoder for container, stream and packet properties, decoded sample and frame counts, extracted subtitle text, still frames and image similarity.
- The prober's output is parsed against a declared shape; a missing required field fails the test.
- Values the host's libraries choose and the layer does not decide are never pinned as literals: muxer-chosen time bases, codec-string profile and level digits, encoder-chosen frame sizes. Where such a value matters, the test computes the expected value from the produced file with the oracle (for example, the HLS `CODECS` entry against the codec string derived from the stream's own parameters) and asserts that the two agree.
- Log lines are an oracle only for what the owning rule makes binding: that a warning or error was logged, at which level, naming which item. Wording is incidental.

### 1.7 Snapshots

A snapshot check (printed types, dumps, stream descriptions) fails the run on any difference. Accepting a new snapshot is a separate, manual step, never part of a conformance or CI run. Snapshots hold only values the layer decides (§1.6).

### 1.8 Inventory and wiring

- Every test of the conformance suite is reachable from the entry point the CI runs. The harness enumerates the suite from one list and fails when a listed test has no runner, or when the count of tests run plus tests skipped differs from the list's length.
- **Surface coverage.** Every operator, builtin and settings key of the frozen surface (README "Frozen") that this layer defines has at least one test that exercises it and asserts on its effect under §1.2. The harness lists that surface from the running program and fails on any item with no covering test. An item that needs hardware or a library the host lacks is a named skip (§1.1).
- **Error paths.** Every row of every failure summary ([encoder.md §11](encoder.md#11-failure-behaviour-summary), [io.md §9](io.md#9-failure-summary-inputffmpeg-inputhttp), [filters.md §18](filters.md#18-failure-summary), [inline-codecs.md §15](inline-codecs.md#15-failure-behaviour), [bitstream-filters.md §7](bitstream-filters.md#7-failure), [decoder.md §13.2](decoder.md#132-errors), [content.md §4.3](content.md#43-invalid-content)) has a test that provokes it and asserts the outcome of its fault class ([faults.md §2](faults.md#2-classes), §6.1).

### 1.9 Pacing and deadlines

- Media tests run on a clock without synchronisation (CPU-paced) unless their subject is real time. Long-run checks (hours of media) run CPU-paced on looped or generated input; what they count is joins, packets and ticks, not wall time.
- Tests whose subject is real time (live inputs, `self_sync`, network retry delays, HLS segment publication) run on real time with a wall-clock deadline that fails the test. Their tolerances are the owning rule's.
- Every test has a deadline; a deadline is a failure, never a skip.

### 1.10 Repetition

A check whose owning rule says "identical output" (stream declaration order, memory reclamation timing) runs the scenario in each variant and compares the packet oracles (§1.4) of the outputs: equal stream by stream, packet by packet, payload bytes included. Container-level bytes MAY differ where the container records wall-clock time.

## 2. Fixtures

Each fixture has stated properties. Before a test uses a fixture, the harness verifies those properties with the oracle (§1.6) and fails the fixture's tests when one does not hold. Generated fixtures come from a recorded command and are reproducible. Sizes, durations and offsets below are binding where a check depends on them; the production method is not.

### 2.1 Generated media

| Fixture                          | Properties (verified)                                                                                                                                                                          | Provokes                                                                             |
| -------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------ |
| audio and video generators       | a sine of known frequency, silence, a test pattern; finite (known length) or infinite                                                                                                          | inputs of known sample and frame count                                               |
| subtitle cue file                | 5 SRT cues, distinct texts, at 1–3, 4–6, 7–9, 10–12, 13–15 s                                                                                                                                   | exact cue timing; a track whose media end (15 s) is the last cue's end               |
| B-frame concat set               | three H.264 mp4 files of 20, 30, 10 s, 1280x720 30 fps with B-frames (PTS ≠ DTS on some packets), whose first DTS are about 41.17 s, 25.2 s and 8.17 s                                         | a copied stream whose timestamps jump backwards at each join; PTS − DTS preservation |
| B-frame concat set, no durations | the same packets with every packet duration unset                                                                                                                                              | join end computed without packet durations                                           |
| mid-GOP pair                     | two AAC + H.264 mp4 files, fixed GOP, B-frames; the second starts 0.6 s before a keyframe: its first video packet is not a key packet, its first key packet is at 0.6 s, its audio starts at 0 | keyframe wait, whole-output versus shared switch                                     |
| A/V offset set                   | AAC + H.264 files whose audio ends 200 ms before their video                                                                                                                                   | one offset per source, aligned on the longest stream                                 |
| incompatible H.264 pair          | the same H.264 picture content in mp4 (length-prefixed, extradata set) and in MPEG-TS (Annex B); a third mp4 whose extradata differs from the first                                            | copy parameter mismatch at type level and at the encoder                             |
| codec-change pair                | an AAC file and an MP3 file of the same rate and layout; a Vorbis and an Opus file in Ogg                                                                                                      | another codec id under copy, strict and relaxed; a restart in a new Ogg link         |
| frame-rate variants              | two H.264 files identical but for their reported average frame rate                                                                                                                            | a difference the strict check accepts                                                |
| size-change stream               | 2 s MPEG-TS, mpeg2video 25 fps, 320x240 for 1 s then 240x320, continuous timestamps                                                                                                            | mid-stream resolution change                                                         |
| size-change with jump            | the two halves byte-concatenated, so timestamps jump back at the change                                                                                                                        | a declared discontinuity coinciding with a format change                             |
| colour-change stream             | the same with bt470bg then bt709                                                                                                                                                               | a change that counts only with `format_change.color`                                 |
| pixel-format change stream       | a video whose pixel format changes twice                                                                                                                                                       | converter rebuild, frame-rate grid continuity                                        |
| rate-change audio set            | 10 s files: 48 kHz AAC, 44.1 kHz mono wav, 44.1 kHz stereo wav; a single file whose sample rate changes mid-stream                                                                             | resampler rebuild and drain                                                          |
| rotation files                   | 2 s 320x240 25 fps, display rotation 90, 33 and 20 degrees                                                                                                                                     | autorotate, undecided rotations, side-data edits                                     |
| start-time stream                | MPEG-TS of 3.004 s whose container start time is 1.4 s, with subtitles                                                                                                                         | positions, remaining time and seek relative to the start time                        |
| backward-packet Matroska         | one packet whose DTS goes back once mid-file                                                                                                                                                   | an invalid item dropped                                                              |
| restarting Matroska              | timestamps that restart for good mid-file                                                                                                                                                      | an input ended after `max_consecutive_faults` drops                                  |
| restarting copy source           | a copy producer whose DTS restarts at 0 mid-stream under one source id                                                                                                                         | rebase after `max_consecutive_faults` drops                                          |
| raw content with gaps            | raw audio and raw video content with a 1 s gap in positions, from the synthetic producer                                                                                                       | timestamp gaps, no frame repeated across a gap                                       |
| chained Ogg                      | two 3 s Vorbis links, the second with `title=two`                                                                                                                                              | discontinuity, metadata change, cue-in                                               |
| late-video live stream           | a live feed whose audio starts 2 s before its first video frame                                                                                                                                | waiting for all streams                                                              |
| silent-stream file               | a file with a selected stream that never yields an item, longer and shorter than `max_interleave_duration`                                                                                     | interleave wait bound                                                                |
| sparse subtitle source           | a subtitle stream whose first cue is at 20 min, in a source whose other streams start at 0; a variant whose previous source's last cue ends 10 min before its end                              | sparse heads and tails at joins                                                      |
| long A/V file                    | 30 s 1920x1080 24 fps yuv420p + 440 Hz sine                                                                                                                                                    | HLS segmenting and resume                                                            |
| A/V/subtitle Matroska            | 30 s 1280x720 + AAC + SRT                                                                                                                                                                      | three-kind copy                                                                      |
| keyframe-interval videos         | H.264 with keyframes every 4 s; H.264 with keyframes every segment duration                                                                                                                    | HLS segment starts                                                                   |
| alpha image and video            | a PNG with an alpha plane holding fully transparent and opaque regions; a VP9 stream with alpha (`yuva420p`)                                                                                   | alpha through overlay, converters, decode and encode                                 |
| still images                     | 1280x720 and 50x50 test patterns as JPEG; a PNG frame of the long file; the same PNG named `IMG.PNG` and `img.png`                                                                             | image decoding, overlay, extension case                                              |
| first-file size file             | 2 s 504x504 test pattern + sine                                                                                                                                                                | frame size taken from the first decoded file                                         |
| short codec inputs               | 1 s 44100 Hz stereo sine; 1 s 48000 Hz audio; 4 s 32 kbps mono mp3; 2 s AAC                                                                                                                    | packet and sample counts against the FFmpeg command line                             |
| 10-hour relay input              | AAC at 44.1 kHz and 30 fps video in a 1/1000 time base (FLV), real or looped to 10 h of media                                                                                                  | drift of chunk lengths                                                               |
| undecodable-turning source       | decodes normally, then yields only undecodable data for a set time, then recovers; served as a file and by the fake HTTP server                                                                | source fault, fallible versus infallible, recovery                                   |
| corrupt packet file              | a copy-decodable file with one packet whose payload the codec rejects                                                                                                                          | data-fault containment                                                               |

Every matrix output (§5) is also a fixture of the decoder tests.

### 2.2 Fetched media

- A DVD bitmap subtitle stream with a 720×576 canvas, for bitmap decoding, placement and burn-in.
- A similarity reference image for the burn-in still frame.

Fetched fixtures are cached per user, keyed by URL, verified by checksum; a partial download is never cached.

### 2.3 Fixture checks binding on the harness

- The B-frame concat set: the oracle reads each file's first DTS and start time and confirms they differ and decrease from file 1 to 3 as stated, and that each file holds packets with PTS ≠ DTS.
- The mid-GOP pair: the oracle confirms the key flags and offsets stated.
- The incompatible H.264 pair: the oracle confirms the two bitstream forms ([content.md §3.1](content.md#31-ffmpegcopy)).
- The alpha image: the oracle confirms a pixel format with alpha.

## 3. Doubles

| Double                    | Behaviour                                                                                                                                                        | Used by                    |
| ------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------- |
| synthetic copy producer   | emits copy content with chosen codec parameters, source id, time base, and per packet DTS, PTS, duration and key flag, each settable to absent                   | §12, §13, §23 entries 1–5  |
| recording muxer           | records every packet handed to the muxer, in call order, with stream, timestamps, key flag and size; can be told to reject one packet as invalid data            | §10, §12, §13              |
| coarse time base          | a producer or container time base of 1/1000, and steps smaller than one unit of the output stream's time base                                                    | §6, §13, §23 entries 5, 13 |
| fake HTTP server          | serves a file or a live feed; answers 404 then starts serving; closes every connection at once; accepts slowly; sends headers then nothing; changes content type | §21, §22                   |
| fake ingest server        | accepts an `output.url` stream (HTTP, SRT or RTMP); refuses; is unreachable; drops the connection mid-stream; records received bytes                             | §21, §22                   |
| harbor client             | sends a file as a source client, pauses, closes; sends a stream with fewer media than the source's type                                                          | §21                        |
| read-only byte callback   | delivers bytes with no seek, tell or length                                                                                                                      | stream decoder (§14)       |
| child process             | runs a one-line script and returns its exit status and messages                                                                                                  | start-up rejections (§1.5) |
| content stand-in          | the content of [content.md §10](content.md#10-without-libav), with integer items, for chunk arithmetic without libav                                             | §7                         |
| hand-built encoder format | the parsed form of an encoder expression handed to the encoder factory                                                                                           | §9, §10                    |
| memory reclamation        | forces a full collection and compaction at a chosen point of a scenario                                                                                          | §12, §23 entry 4           |
| live clock                | a clock with a hardware-like sync source and a CPU-paced one                                                                                                     | §21                        |

A hand-built encoder format MUST have a shape the parser produces: a field appears once, each track has the mode and arguments its written form gives ([encoder-format.md §1](encoder-format.md#1-shape-of-a-format-value)).

## 4. Failure modes the harness simulates

| Failure                                     | How                                                                           | Checked in        |
| ------------------------------------------- | ----------------------------------------------------------------------------- | ----------------- |
| backward timestamps at a join               | B-frame concat set                                                            | §13, §22          |
| backward timestamp inside one source        | backward-packet Matroska; synthetic producer                                  | §7, §13, §14      |
| timestamps restarting for good              | restarting Matroska; restarting copy source                                   | §13, §14          |
| declared discontinuity                      | size-change with jump; chained Ogg                                            | §14               |
| packets with no DTS, no PTS, no duration    | synthetic producer; B-frame set without durations                             | §13               |
| mid-GOP cut                                 | mid-GOP pair                                                                  | §12, §13, §22     |
| incompatible copy source                    | incompatible H.264 pair; codec-change pair; strict and relaxed                | §7, §12, §22      |
| mid-stream format change of decoded media   | size, colour, pixel-format and rate change fixtures                           | §11, §14, §17     |
| source turning undecodable, then recovering | undecodable-turning source under a `fallback`, fallible and infallible        | §6, §22           |
| corrupt packet                              | corrupt packet file; recording muxer rejecting a packet                       | §10, §22          |
| network drop and reconnect                  | fake HTTP server closing; fake ingest server dropping                         | §21, §22          |
| server 4xx then recovery                    | fake HTTP server answering 404 then serving                                   | §21               |
| unreachable peer, slow peer, silent peer    | fake servers                                                                  | §21               |
| input running dry                           | a `buffer` whose input stops for a few seconds                                | §17, §22          |
| sleep and wake                              | an inline encoder or a producer put to sleep and woken                        | §16, §23          |
| seek                                        | seek in decoded, raw and copied pipelines                                     | §14, §22          |
| output stop and restart mid-stream          | stop while a switch waits; stop while an open is pending; restart on metadata | §10, §12, §21     |
| unknown codec, format, option, filter       | configuration errors                                                          | §9, §10, §17, §21 |
| missing file                                | a nonexistent path                                                            | §21               |
| memory reclamation at an arbitrary moment   | memory reclamation double                                                     | §12, §23          |
| host without a codec or filter              | named skip (§1.1)                                                             | every section     |

## 5. Encoder matrix

The matrix runs every encoder configuration of a list over generated input and checks each output. It is the widest encoder coverage and its pass condition is binding.

The list MUST cover at least: every audio-only codec and container pair the layer documents (AAC in ADTS and mp4 at several rates and input sample formats including `pcm_s16` and `pcm_f32`, MP3, FLAC, WAV, Vorbis, FLAC and Opus in Ogg, mono and stereo); H.264 video alone; AAC + H.264 at several channel counts and a non-default frame rate; a multitrack output with two audio and two video fields; audio, video, and audio + video with each subtitle codec (subrip and ASS in Matroska, WebVTT in WebM, mov_text in mp4).

Per configuration, the input is 10 s (16 s with subtitles) of generated media, and the output passes when, for every declared field:

1. the stream exists with the codec and parameters the configuration declares;
2. the decoded duration equals the input duration within one codec frame plus the priming the container signals, and the packet count is the one the codec's frame size gives for that duration;
3. the packet oracle (§1.4) shows DTS strictly increasing and PTS ≥ DTS;
4. an independent decoder decodes the whole stream without error;
5. for subtitles, the extracted cues equal the input cues.

Also per configuration: [encoder-format.md §10.2](encoder-format.md#102-conformance) (printed type equals instantiated type) and [§11.1](encoder-format.md#111-conformance) (print, parse, print round trip). A configuration whose codec the host lacks is a named skip. A failing configuration fails itself only (§1.1).

## 6. Common, faults and draining

### 6.1 Faults

Items: [faults.md §6](faults.md#6-conformance).

Every fault check names the class of [faults.md §2](faults.md#2-classes) it provokes and asserts that class's outcome, and only that outcome:

| Class               | The check asserts                                                                                                                                                                                                                                                                                                                     |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| configuration error | the error before any data of the stream it concerns is written: at evaluation, at start-up, or at the first build of a stage started later (a late output, a reopen, a restart), when the failure follows from the configuration and the host alone ([faults.md §2](faults.md#2-classes)); the script fails; the reason pinned (§1.5) |
| item fault          | the item missing from the output and nothing else (packet oracle, §1.4); one warning for the first drop of the kind; repeats logged with their count at most once per `fault_log_interval`; the output still streaming                                                                                                                |
| source fault        | end of track at the fault with the data before it drained; then not ready for a fallible source while the fault persists; for an infallible source that cannot produce content, a hard error that fails the script naming the source and the cause; one error-level log; the source's callbacks run                                   |
| output fault        | only a byte-sink, header or peer failure, or an incompatible copy source with no clean restart (user rule D2): the output stops or restarts under its restart policy, its callbacks run, and another output on the same clock keeps producing. A data fault never produces this class.                                                |

Additional checks:

1. **Escalation** ([faults.md §4](faults.md#4-item-faults-escalate)): every dropping site counts against the one bound `max_consecutive_faults`. On the input side, a run of the bound minus one media-carrying faults stays item faults, a run of the bound becomes a source fault, and one delivered item in between resets the count. Faults on items without media (a raising `on_undecided` handler, an unparsable tag, an unhonoured seek) never escalate, however many.
2. **Output-side escalation**: the recording muxer rejects every packet of a copied field for longer than the bound: the field's group restarts on its next keyframe, and again at each further run past the bound; the output never stops. The same on an encoded field: the drops stay item faults, logged at the bounded rate, and the other fields keep streaming.
3. **Fallible and infallible** ([faults.md §3](faults.md#3-source-faults-reach-the-script)): the undecodable-turning source, once typed fallible under a `fallback` (the fallback switches to its other branch within the bound, and switches back when the fixture recovers) and once infallible with no `fallback` (the script fails with an error naming the source and the cause). A positive control (§1.2 item 5) checks that the same fixture behind `mksafe` keeps the output running, the filler coming from `mksafe`.
4. **Nothing raises out of a fallible source**: in every scenario of §4 that injects a fault into input data of a fallible source (inline, bitstream-filter and side-data operators included, which follow their input's fallibility), the clock's error handler is never called and no source is detached.
5. **No invented content** ([faults.md §3](faults.md#3-source-faults-reach-the-script)): in every fault scenario, each sample and frame a faulty source delivers is traced to data it received or computed. The oracle compares the source's output with its input, decoded independently: no silence, black or repeated frame appears where the input had undecodable data, and the source's output length during the fault is zero.
6. **Bounded waits** ([faults.md §5](faults.md#5-waiting)): each named wait bound (keyframe wait, group members, interleave wait, split point) reached on a fixture that never meets its condition; the input-side wait becomes a source fault, the output-side wait proceeds with a warning.
7. **Log rate**: a source producing faulty items for a long run logs a bounded number of lines, each repeat line carrying a count, and the counts sum to the items dropped.

### 6.2 Common and draining

Items: [common.md §1](common.md#1-settings), [§2](common.md#2-log-bridge), [§5](common.md#5-time-base-conversion), [§6.6](common.md#66-conformance), [§9](common.md#9-video-format-comparison), [§11](common.md#11-hardware-contexts); [draining.md §6](draining.md#6-conformance).

Harness additions:

- The duration-helper items of §6.6 run on the synthetic producer with the coarse time base; their 10-hour spans are pushed as data, not played.
- Each settings key's default is read from a script that does not set it, through the surface inventory (§1.8).
- The draining items run once per stage family of [draining.md §1](draining.md#1-stages): decoder, encoder, resampler, re-framer, frame-rate stage, filter graph, bitstream filter, interleaving queue, muxer. For each, the counts in equal the counts out, under each ending event of [draining.md §2](draining.md#2-the-rule): end of input, replacement mid-stream, end of stream, output stop. A seek and a stop by the reader discard instead ([draining.md §5](draining.md#5-discarded-on-purpose)); the check then asserts nothing reaches the output after the event from before it.
- A partial final unit ([draining.md §4](draining.md#4-partial-final-units)) is checked with a sample count that is not a multiple of the codec frame size: decoded output trimmed as the container signals holds exactly the input samples.

Binding: counts and the level of log lines. Incidental: log wording, the order of hardware candidates the host lists.

## 7. Content

Items: [content.md §2.1](content.md#21-annotation-syntax), [§2.6](content.md#26-dumps), [§3.1.1](content.md#311-where-copy-parameters-are-merged), [§4.3](content.md#43-invalid-content), [§5](content.md#5-stream_idx-and-time_base), [§6.9](content.md#69-consolidation-view-layer), [§9](content.md#9-raw-video-conformance), [§11](content.md#11-checksum), [§14](content.md#14-raw-audio-conformance).

Additional checks, on the content stand-in and on real content:

1. `sub_run` keeps an item at position `p` exactly when `offset ≤ p < offset + len`, at `p − offset` ([content.md §6.4](content.md#64-sub-of-a-run)): items at 10, 30, 50, 70, range 20+40 → 10 and 30; items at 50, 100, 150, range 50+100 → 0 and 50.
2. Collapse merges exactly the pairs [content.md §6.5](content.md#65-collapse) lists: a pair differing only in source id, only in time base, or only in copy source parameters stays apart; a pair with a gap between stays apart.
3. Time bases of runs survive copy, sub, blit and consolidation unchanged (1/48000, 1/44100, 1/90000 in one value).
4. A copy is independent of its original: changing the original's runs leaves the copy's length and items unchanged.
5. Every format's dump reloads to an equal format, for each kind with and without parameters.
6. The type annotations `ffmpeg.copy`, `ffmpeg.audio.raw`, `ffmpeg.video.raw` with and without parameters are accepted on source tracks; a track of the internal media kind and a track of `ffmpeg.video.raw` do not unify in either direction.
7. Strict compatibility ([content.md §3.1](content.md#31-ffmpegcopy)): for each compared property, two parameter sets that differ only in it are rejected, with an error naming the property; for each "when set" property, unset on one side is accepted. Relaxed: only media type and codec id are compared.
8. The source-id items of §5 run with decoders opened concurrently on several domains; uniqueness is asserted over every id seen in the run.

Binding: positions, lengths, run counts, the accepted and rejected sets, the property an error names. Incidental: dump layout, error wording.

## 8. Stream description

Items: [stream-description.md §3](stream-description.md#3-fields), [§6](stream-description.md#6-get_type-ctype-container).

Harness additions:

- The description of each matrix output is compared on the values the layer decides: container format name, per stream the field name, kind, codec name, sample rate, channel count and layout, or width, height, pixel format and frame rate. Time bases are compared with the oracle's reading of the same file, not with literals (§1.6).

## 9. Encoder format

Items: [encoder-format.md §1.1](encoder-format.md#11-conformance), [§10.2](encoder-format.md#102-conformance), [§11.1](encoder-format.md#111-conformance).

Additional checks:

1. Every row of [encoder-format.md §13](encoder-format.md#13-errors-summary) is provoked and rejected at the place the row states, with error kind `encoder` naming the offending argument (§1.5).
2. The media type of an encoded track is found by each rule of [encoder-format.md §2.1](encoder-format.md#21-media-type-of-an-encoded-track) in turn: content argument, field name, literal codec; a track that matches none is rejected at run time and typed unconstrained at type check.
3. Input sample formats `pcm_s16` and `pcm_f32` are accepted by `%ffmpeg` and rejected by every other encoder format with error kind `encoder`.
4. HLS bit rate: absent when no stream reports one; with two fields each setting `b`, the sum of both.
5. A field named twice, any argument on a `.drop` track, and a `.copy` argument other than `wait_for_keyframe` and `ignore_keyframe` are each rejected at type check (`liquidsoap --check`), not only at run time.

## 10. Encoder: container, HLS, restart

Items: [encoder.md §3.1](encoder.md#31-conformance), [§4.1](encoder.md#41-conformance), [§5.1](encoder.md#51-conformance), [§6.1](encoder.md#61-conformance), [§7.1](encoder.md#71-conformance), [§8.8](encoder.md#88-conformance), [§13.4](encoder.md#134-conformance).

Additional checks:

1. Item faults ([encoder.md §5.2](encoder.md#52-data-faults-and-output-faults), [faults.md §2](faults.md#2-classes)): the recording muxer rejects one packet, and a codec refuses one frame; each item is missing, one warning names the field, the other streams are complete, the output keeps streaming. Repeated faults on one field log at most one warning per `fault_log_interval`, with the count.
2. Output faults: a byte sink that fails; the output stops or restarts under its restart policy and reports through its callbacks (§6.1). Late configuration errors: an output started late, reopened, or restarted whose first build fails for a reason that follows from configuration and host (a hardware device that cannot open, a codec that cannot create the configured format) fails the script before writing data of that stream.
3. Restart at a source switch ([encoder.md §13](encoder.md#13-container-restart)): every member of the switching source's group moves to the new container and no packet of the new source is in the old one; the parameter sets are logged at warning on a restart and at error on a stop.
4. HLS restart keeps user rule D5: on the variant timeline, the first DTS of the restarted stream is at or after the previous segment's end, the new codec's priming does not overlap it, and an Ogg restart starts a new timeline at 0.
5. Media timeline: after a whole-output switch, timed ID3 timestamps, the segment grid and each `EXTINF` follow the media timeline (output position minus the skipped time, [timestamps.md §1.4](timestamps.md#14-the-output-timeline)); each `EXTINF` equals its segment's media duration from the packet oracle. The ID3 stream does not keep a switch from being whole-output.
6. HLS properties: the codec string of each variant agrees with the produced stream (§1.6); timed ID3 is present in-band in MPEG-TS and ADTS variants when enabled, absent when disabled, and the first segment holds the first tag.
7. HLS resume from persisted state: the resumed playlist probes as the original, and the packet oracle shows DTS continuing across the resume on every stream.
8. Several `%ffmpeg` encoders on one shared source run to the end, each output passing §5's per-configuration condition.
9. Metadata and track marks removed before an output do not reach it: no metadata and no new track in the produced file.

## 11. Encoded streams

Items: [encoder-internal.md §4.8](encoder-internal.md#48-conformance), [§8.1](encoder-internal.md#81-conformance); [timestamps.md §3](timestamps.md#3-audio-internal-and-raw), [§4.5](timestamps.md#45-encoder-output).

Additional checks:

1. Defaults: an AAC stream from generated stereo input is stereo, `fltp`, at the output's sample rate; an H.264 stream is `yuv420p`.
2. A re-encoded video stream's frame rate is the track's, not the input's (25 fps input, 45 fps track → `45/1`), and its frame count is the duration times that rate.
3. Subtitles: subrip, ASS, mov_text and WebVTT encode beside audio and video; two subtitle fields in one file; a copied and an encoded subtitle field in one file. A custom `text_to_ass` receives each cue's index and text and its result becomes the cue's event fields, cue times unchanged.
4. A bitmap subtitle track burned onto video: no subtitle stream in the output, and the still frame at a cue's time is similar to the reference above a stated threshold.
5. Alpha: VP9 with alpha re-encoded with an alpha pixel format keeps `yuva420p`, and a transparent pixel of the input decodes transparent.
6. Content gaps (raw content with gaps fixture): the re-encoded audio carries a PTS gap equal to the content gap within one codec frame ([timestamps.md §3](timestamps.md#3-audio-internal-and-raw)); the re-encoded video has no frame inside the gap and its frame count equals the input's, so the frame-rate converter repeats nothing across the gap ([faults.md §3](faults.md#3-source-faults-reach-the-script)).

## 12. Copied streams and switching

Items: [encoder-copy.md §2](encoder-copy.md#2-stream-creation), [§8](encoder-copy.md#8-flush-and-end), [§12.3](encoder-copy.md#123-readiness-and-group-start), [§12.4](encoder-copy.md#124-sparse-sources), [§12.5](encoder-copy.md#125-lifetime-of-per-source-state), [§12.7](encoder-copy.md#127-parameters-across-sources).

Harness additions:

- Every switching item runs on the packet oracle (§1.4), on file outputs and through the recording muxer.
- "Identical output" items follow §1.10: fields declared in each order; memory reclamation forced before and after the switch.
- Readiness (user rule D4 in [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)): tracks from one source start together, at or after the new source's first key packet on every track; tracks from different sources each start on their own readiness. Checked on the mid-GOP pair: same source; audio from one file and video from another; audio and video from A, then audio from B and video from C.
- Per-source state lifetime: the playlist of thousands of sources runs CPU-paced with memory reclamation forced between tracks; the check samples the memory held by per-source state at fixed intervals and asserts it bounded by a constant independent of the number of sources.
- Stream creation: an output whose copy field starts with empty runs writes nothing for any stream until the first frame with known copy parameters ([encoder-copy.md §2](encoder-copy.md#2-stream-creation)).
- Parameters across sources run once with the default strict check and once with `settings.ffmpeg.content.copy.relaxed_compatibility_check` on. Each run asserts the outcome its mode specifies; neither run is a substitute for the other. The type merge rejects incompatible files when they open ([content.md §3.1.1](content.md#311-where-copy-parameters-are-merged)), so encoder-side checks feed the second source through a path with its own format: a pass-through bitstream filter or a `track.ffmpeg.side_data` output ([encoder-copy.md §12.7](encoder-copy.md#127-parameters-across-sources)). A check whose second source never reaches the encoder fails (§1.2 item 1).
- Copy from two different files into one output keeps both codecs (MP3 audio from one, H.264 video from another).

## 13. Timestamps

Items: [timestamps.md §1.4](timestamps.md#14-the-output-timeline), [§3](timestamps.md#3-audio-internal-and-raw), [§4.5](timestamps.md#45-encoder-output), [§6.2](timestamps.md#62-beginning-a-source-rebase), [§6.3](timestamps.md#63-monotonicity-check).

Every check of this section is a packet-level check (§1.4) with a negative control (§1.3). Additional scenarios, each asserting DTS strictly increasing per output stream (user rule D5) and the behaviour its owning rule states:

1. **Missing values** ([timestamps.md §6.4](timestamps.md#64-missing-values)): packets with no DTS, no PTS, neither, no duration, from the synthetic producer; each is written at the value the table gives.
2. **B-frames across a join**: the B-frame concat set, three runs: strict check, relaxed check, and the set without durations. Exact equality: packet count equals the inputs' sum; the output duration equals the inputs' sum within one frame. The run under the strict check is binding: the files differ only in properties the strict check accepts.
3. **Cross-stream interleaving**: in an interleaved container, packets of all streams are in non-decreasing DTS order after conversion to a common base, within the muxer's interleaving window.
4. **New encoder instance and restarts**: output stop and restart, `reopen_on_metadata`, and a new link of an Ogg output each start a timeline near 0; an HLS restart keeps the variant's timeline, DTS strictly increasing across it ([timestamps.md §2](timestamps.md#2-lifetimes-what-resets-what), [§6.5](timestamps.md#65-restarts)).
5. **Metadata with a new track** inserted into an output: timestamps are unaffected (no reset row in [timestamps.md §2](timestamps.md#2-lifetimes-what-resets-what)).
6. **Reconnect**: a copied live input dropped by the fake HTTP server and reconnected ([io.md §13](io.md#13-timestamps-across-reconnects)).
7. **Seek** in a copied pipeline: a source switch downstream ([content.md §5](content.md#5-stream_idx-and-time_base)).
8. **Filter generation**: a graph rebuilt after its input runs dry; output timestamps continue.
9. **Inline encoder rebuild**: a sleep and wake, with and without a sibling encoder awake.
10. **Frame-rate changes through a filter**: input alternating 25 fps and 12 fps files across nine tracks with an output reopen; the output frame count equals duration times output rate within one frame per track.
11. **Coarse steps**: copied DTS that differ by less than one unit of the output stream's time base keep the output running.
12. **Subtitle cue times**: the cue file repeated under a 32 s video, encoded and copied, gives 11 cues: cues 1–5 at their times, cues 6–10 shifted by 15 s, cue 11 at 31–33 s, to the millisecond; the decoded seventh cue reports absolute start 19.0 s.

Binding: strict DTS order, PTS − DTS preservation, packet counts, and the join rule of [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase): a copy-only output switching every stream to one source (whole-output switch) has no PTS/DTS gap at the join; an output with any continuing stream keeps the gap of the keyframe wait. Incidental: where the output timeline starts in the container (the oracle reads timestamps relative to the stream's first packet).

## 14. Decoder

Items: [decoder.md §3.1](decoder.md#31-files), [§4.3](decoder.md#43-image-options-and-unconsumed-options), [§6](decoder.md#6-stream-selection-and-field-mapping), [§8.3](decoder.md#83-positions-and-sparse-advance), [§8.4](decoder.md#84-waiting-for-all-streams), [§8.5](decoder.md#85-start-and-seek-threshold), [§9.2](decoder.md#92-discontinuities-and-invalid-timestamps), [§10.2](decoder.md#102-stream-metadata-during-decoding), [§10.4](decoder.md#104-tags-for-requests-metadata-resolver), [§11.2](decoder.md#112-remaining-time), [§12](decoder.md#12-seeking), [§13.1](decoder.md#131-end-of-input), [§16.1](decoder.md#161-internal-video-converter), [§16.2.1](decoder.md#1621-delay); [decoder-internal.md §4.4](decoder-internal.md#44-bitmap-subtitles), [§6](decoder-internal.md#6-flush); [decoder-raw.md §6](decoder-raw.md#6-flush); [decoder-copy.md §5](decoder-copy.md#5-timestamps).

Harness additions:

- Every decoder test asserts that the FFmpeg decoder decoded the request (§1.2 item 4) and asserts on decoded content, not on an output that fixes the property (§1.2 item 3): channel count and sample rate are read on the decoded frames, or through a copy or raw output whose parameters follow the decoder.
- Copy decoding preserves each stream's parameters: matrix outputs decoded with every field copied and muxed to Matroska give, per stream, the input's codec name and parameters and the input's packet count, last packet included.
- Per-field mode selection: decode, raw and copy in every combination on one A/V file, and audio with subtitles in the four decode/copy combinations; a field absent from the file is absent from the result.
- The stream decoder (`application/ffmpeg`) reads through the read-only byte callback and delivers every sample of a WAV and an MP3 input; each invalid-data error it skips is counted and logged ([decoder.md §13.2](decoder.md#132-errors)). The MIME parameters describing a headerless PCM stream produce a track of the described format.
- Demuxer options from a request annotation reach the input: an option whose effect the oracle sees.
- A per-codec decoder override selects the named decoder (`libvpx-vp9` for VP9 with alpha: alpha kept).
- `pcm_s16` and `pcm_f32` decoding produce content of that sample format with the file's sample count.
- Decoded video is scaled to an annotated frame size, and the internal frame size is taken from the first decoded file when unset (504x504 fixture).
- Discontinuities ([decoder.md §9.2](decoder.md#92-discontinuities-and-invalid-timestamps)): an MPEG-TS input whose streams jump back a few packets apart produces one new stream index for the whole mapping; each stream moves to it at its own jump, and no item of one stream appears under the other id.
- Copied items are positioned by DTS ([decoder.md §8.3](decoder.md#83-positions-and-sparse-advance)): a copied AAC stream whose first DTS is later than the video's starts with an empty run of the difference, and every copied field carries a run, empty when it has no packet, in every frame.
- Equal DTS within one input are valid on the decoding side: a file with two successive packets of equal DTS decodes both.
- Text subtitles: cues with absolute and relative times, text, format and forced flag. Bitmap subtitles: usable as a video overlay.

## 15. Image decoder

Items: [image-decoder.md §2](image-decoder.md#2-acceptance-check).

Harness additions: an image annotated with a 0.2 s duration ends its track: at least three tracks of the right length follow one another.

## 16. Inline codecs

Items: [inline-codecs.md §3.2](inline-codecs.md#32-transform-lifetime), [§3.3](inline-codecs.md#33-metadata-and-track-marks), [§4.2](inline-codecs.md#42-output-parameters), [§5](inline-codecs.md#5-stream-index), [§8](inline-codecs.md#8-chunker), [§10](inline-codecs.md#10-audio-decoding), [§10.1](inline-codecs.md#101-copy-trackffmpegdecodeaudio), [§13](inline-codecs.md#13-flush-and-end).

Harness additions:

- Every `track.ffmpeg.encode.*`, `track.ffmpeg.decode.*`, `ffmpeg.encode.*`, `ffmpeg.decode.*` and `ffmpeg.raw.*` operator runs at least once with a count assertion (§1.8).
- Encode then decode inline, then re-encode: packet and sample counts match the direct path within the codec priming.
- Several inline encoders at different bit rates on one shared source each feed a copy output to completion, each passing §5's per-configuration condition.
- The one-hour chunker item runs CPU-paced.
- `codec=` on a raw-mode inline encoder is rejected like any other non-media argument (§1.5).
- An inline rebuild whose new parameters fail the merge is a source fault at the producer: end of track, nothing emitted for that source, both parameter sets logged; then not ready, or a hard error with an infallible input.
- An `audio_video` wrapper is ready only when both producers are; a source fault on its video ends the wrapper's track.

## 17. Filters

Items: [filters.md §3](filters.md#3-script-values), [§5.4](filters.md#54-validation-and-rendering), [§7.4](filters.md#74-conformance), [§11.7](filters.md#117-draining-a-generation), [§12.3](filters.md#123-resource-limits), [§13.3](filters.md#133-decreasing-video-timestamps-from-a-filter), [§14.2](filters.md#142-out-of-the-graph).

Harness additions:

- Every filter test asserts the filter's effect (§1.2 item 3): `fps=30` gives `30/1` and the frame count for the duration; `aformat` with `sample_fmts=["s32","fltp"]` gives `s32` through a PCM encoder and container that keep it; text overlay changes the pixels of the text region against the same graph without the text; overlay of the alpha image leaves the background visible under the transparent region.
- Filter arguments of every scalar type and array arguments are accepted; a value outside the option's range is rejected at the script position.
- Frame metadata set by a filter, with metadata passing on, reaches the output source's metadata.
- Filters the host lacks are named skips.
- A filter input declared with 2 channels on a mono-typed source is a type error (§1.5).

## 18. Bitstream filters

Items: [bitstream-filters.md §6](bitstream-filters.md#6-drain).

Harness additions: the oracle checks the bitstream form, not only codec parameters. After `h264_mp4toannexb`, the output extradata is empty or starts with a start code and every packet starts with a start code. After `aac_adtstoasc`, the output carries an AudioSpecificConfig in its extradata and no packet starts with an ADTS sync word. The input fixture is in the other form (§1.2 item 3).

## 19. Side data

Items: [side-data.md §3.2](side-data.md#32-converting-and-draining), [§3.4](side-data.md#34-undecided-cases), [§5.3](side-data.md#53-methods).

Additional checks, on the rotation files:

| Path               | Input rotation | Operation                                           | Binding result                                    |
| ------------------ | -------------- | --------------------------------------------------- | ------------------------------------------------- |
| copy               | 90             | read side data, copy                                | 320x240, rotation 90; the track reader returns 90 |
| copy               | 90             | set rotation 180                                    | rotation of absolute value 180                    |
| copy               | 90             | remove the display matrix                           | no display matrix in header or packets            |
| decode, encode     | 90             | none                                                | 240x320, no rotation                              |
| decode, raw encode | 90             | set rotation 90 on the raw track                    | rotation 90 on the encoded stream                 |
| decode, raw encode | 33             | the undecided handler returns a clockwise transpose | no rotation                                       |
| decode, raw encode | 20             | the handler returns no decision                     | rotation within 2° of 20                          |

The undecided handler is called exactly once per run of frames with the same question ([side-data.md §3.4](side-data.md#34-undecided-cases)): once per decoder of each odd-rotation file in this scenario. Every call reports reason `odd_rotation` and the stored angle within 2°. Binding: dimensions, rotations, the reason, the call count. Incidental: the sign convention of a rotation.

## 20. Converters

Items: [converters.md §2](converters.md#2-the-ffmpeg-samplerate-converter), [§4](converters.md#4-the-ffmpeg-video-converter).

Harness additions: the `ffmpeg` samplerate converter is selected explicitly by the test, whatever other converters the host has, and the test asserts it ran (§1.2 item 4). An alpha pixel format survives conversion; an overlay of a 50x50 still over a 1280x720 still converts both to the graph's format.

## 21. IO

Items: [io.md §1.1](io.md#11-inputffmpeg-and-inputhttp), [§1.2](io.md#12-script-wrappers), [§2.1](io.md#21-states), [§2.3](io.md#23-failure-of-an-attempt), [§2.5](io.md#25-end-of-a-connection), [§3](io.md#3-interrupting-blocking-libav-calls), [§6](io.md#6-metadata), [§7](io.md#7-clock-and-self_sync), [§10.2](io.md#102-per-client), [§10.6](io.md#106-feeding), [§11.3](io.md#113-errors), [§12](io.md#12-other-outputs-special-casing-ffmpeg).

Harness additions:

- Network items run against the fake HTTP and ingest servers (§3), never against a third-party service. Retry delays are measured on real time against the owning rule's parameters.
- "No container left open" is measured by a count the harness can read (open containers, open sockets of the process), sampled after the event.
- Leading and trailing whitespace around an input URL is trimmed: the padded URL connects and no error is reported.
- An input replaced inside a dynamic source is stopped, then collected after forced memory reclamation; stopping precedes collection.
- Copy input over HTTP feeding a copy output: the output receives the input's packets, counted.
- Decoding never raises: a feeding error on an input becomes a source fault, with what the decoder drained delivered first.
- A content-type mismatch backs off as a permanent failure would ([io.md §2.3](io.md#23-failure-of-an-attempt)).
- A copy relay whose remote restarts with other codec parameters is rejected at reconnect and stays not ready until a compatible stream returns (user rule D2).
- `output.url`: a stream parameter the codec or muxer rejects from configuration alone, an unknown option, codec or muxer, fail the script at open; a failure that depends on the source's data or the peer is an output fault and reconnects ([io.md §11.3](io.md#113-errors)).

## 22. Cross-subsystem scenarios

Each scenario runs end to end on the packet oracle (§1.4) and asserts, besides its own items, DTS strictly increasing per output stream and the stated counts.

1. **Playlist of copied files into HLS.** A playlist of AAC + H.264 files (mid-GOP pair, A/V offset set, B-frame set) copied into an MPEG-TS and an fMP4 variant. Per variant: DTS monotonic across every join and segment; every segment's first video packet is a key packet; no `EXTINF` exceeds the target duration; the packet count per stream equals the inputs' sum less the packets the owning rules drop before a keyframe, no tail lost; group switching follows user rule D4; no PTS/DTS gap at any join, every join being a whole-output switch ([timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)); `EXTINF` equal to each segment's media duration. Run strict and relaxed.
2. **Copied video, re-encoded audio.** Fifty mid-GOP files: [timestamps.md §1.4](timestamps.md#14-the-output-timeline)'s item, plus audio PTS equal to the cumulative sample count.
3. **Incompatible copy source** (user rule D2). Decoder side: the incompatible H.264 pair in one copy playlist is rejected when the second file opens, as a source fault (not ready under a `fallback`) ([content.md §3.1.1](content.md#311-where-copy-parameters-are-merged)). Encoder side, the second source through a pass-through bitstream filter (§12): into an MPEG-TS file, the output stops with an error-level log naming both parameter sets and every earlier packet is in the file; into an HLS variant, a clean restart logged at warning, a new segment after a discontinuity that decodes alone, DTS strictly increasing across it; into `output.url`, a reconnect; the Vorbis then Opus pair into Ogg, a new link. With the relaxed setting, the codec-change pairs still restart or stop, and for one codec id the muxer's rejections are item faults and the output keeps streaming.
4. **Live relay with drops.** `input.ffmpeg` from the fake HTTP server (live feed), copied to `output.url` on the fake ingest server; the server drops the input twice and the ingest once. The input reconnects, the output reconnects per `restart_delay`, each output connection is a new container with timestamps from 0, and within each connection DTS is strictly increasing. No error ends the process.
5. **Fallback between live and file.** A fallback switching from a copied live input to a copied file and back, mid-GOP: switching per D4, no non-monotonic DTS reported by the muxer, playback decodable after each switch.
6. **Format change through a filter into HLS.** The size-change stream decoded raw, through a filter, raw-encoded into HLS: no lost frame at the change, the warning logged once, no track mark, every segment starts on a key frame.
7. **Seek in a pipeline.** Seek in a decoded, a raw and a copied pipeline feeding encoders: the decoder's held data is discarded, downstream sees a new source id, output DTS strictly increasing, audio present within the decoder's pre-roll after the seek.
8. **Fault containment (input side).** A playlist under a `fallback` feeding a running output, holding the corrupt packet file and the backward-packet Matroska (item faults: the item dropped with one warning, the playlist continues), the restarting Matroska (source fault: the track ends with an error, the playlist moves on), and the undecodable-turning source (source fault: the `fallback` switches away and back on recovery). The output never stops and the clock's error handler is never called.
9. **Inline encode, sleep, copy.** An inline encoder put to sleep and woken, with a copy encoder downstream, with and without a sibling encoder awake: no packet dropped as non-monotonic, ids per [inline-codecs.md §5](inline-codecs.md#5-stream-index), outputs identical under memory reclamation at different moments.
10. **Ten-hour relay.** The 10-hour relay input copied, CPU-paced: each stream's content length equals its media duration within one tick, no buffer grows past a constant, the output never fails.
11. **Stop with data held.** Stop an output while a copy switch waits for a keyframe, while the resampler, the re-framer and the frame-rate stage hold data, and while an inline encoder holds its tail: every held item reaches the output before the trailer ([draining.md §3](draining.md#3-order)).
12. **Mixed mapping start.** A file decoded to PCM audio with copied H.264 video, played with a cue-in: both fields start at the first copied-video key packet at or after the cue-in, positions agreeing within one audio frame ([decoder.md §8.5](decoder.md#85-start-and-seek-threshold)), and a downstream copy-plus-encode output starts both streams together.

## 23. Known-complexity checks

Each entry of [known-complexity.md](known-complexity.md) is checked by the items listed, under the negative control of §1.3 with the mutation named. A mutation describes the broken behaviour, not a change to any particular code.

| Entry | Checked by                                                                                                                                                                                                  | Mutation the check must fail on                                                                                                              |
| ----- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------- |
| 1     | §13 items 1–2; [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase) first item                                                                                                                  | separate PTS and DTS offsets; a join end taken as the last DTS without its duration; DTS synthesised from positions                          |
| 2     | [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase) last item; §1.10 declaration order                                                                                                         | one offset per stream; the offset taken from the first stream to switch                                                                      |
| 3     | [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start); §12 readiness; §22 item 5                                                                                                           | no keyframe wait; one readiness shared by tracks of different sources; offset taken from the first packet seen instead of accepted           |
| 4     | [encoder-copy.md §12.5](encoder-copy.md#125-lifetime-of-per-source-state); [inline-codecs.md §5](inline-codecs.md#5-stream-index); §22 item 9                                                               | per-source state never released; state released while a track still uses it; readiness count decremented for a track that never became ready |
| 5     | [timestamps.md §6.3](timestamps.md#63-monotonicity-check); §13 items 1, 11                                                                                                                                  | packets without DTS dropped; a non-increasing DTS written to the muxer                                                                       |
| 6     | [timestamps.md §3](timestamps.md#3-audio-internal-and-raw), [§4.5](timestamps.md#45-encoder-output); [encoder-internal.md §8.1](encoder-internal.md#81-conformance)                                         | a rebuilt frame-rate stage restarting its timestamps at 0; a replaced stage released without draining; frames repeated across a content gap  |
| 7     | [filters.md §11.7](filters.md#117-draining-a-generation), [§13.3](filters.md#133-decreasing-video-timestamps-from-a-filter); [common.md §6.6](common.md#66-conformance) (equal timestamps in arrival order) | items at one position reordered; the previous end not converted into the new time base                                                       |
| 8     | [decoder.md §8.4](decoder.md#84-waiting-for-all-streams); [encoder-copy.md §12.4](encoder-copy.md#124-sparse-sources); [encoder.md §8.8](encoder.md#88-conformance)                                         | an interleaving writer waiting on a never-written metadata stream; a sparse stream's head length dropped                                     |
| 9     | [encoder.md §8.8](encoder.md#88-conformance); §10 item 5                                                                                                                                                    | leftover codec samples moving the segment grid; a split without a key frame; `EXTINF` taken from output position instead of media timeline   |
| 10    | [draining.md §6](draining.md#6-conformance); §6; [decoder.md §13.1](decoder.md#131-end-of-input); [inline-codecs.md §13](inline-codecs.md#13-flush-and-end); §22 item 11                                    | for each stage family, its final drain skipped                                                                                               |
| 11    | [content.md §3.1.1](content.md#311-where-copy-parameters-are-merged); [encoder-copy.md §12.7](encoder-copy.md#127-parameters-across-sources); §22 item 3                                                    | strict compatibility comparing codec id only                                                                                                 |
| 12    | [timestamps.md §1.4](timestamps.md#14-the-output-timeline); §22 item 2                                                                                                                                      | a copied stream anchored on its previous end alone, ignoring the output position                                                             |
| 13    | [common.md §6.6](common.md#66-conformance); [decoder-copy.md §5](decoder-copy.md#5-timestamps); [inline-codecs.md §8](inline-codecs.md#8-chunker); §22 item 10                                              | chunk lengths converted and truncated per step                                                                                               |
