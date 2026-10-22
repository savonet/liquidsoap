# Findings: cross-file re-read of Part A

Scope: the twenty Part A files. Each item names both places, says which one
the code supports, and the fix for the orchestrator. File:line references are
to the worktree. **Checked** means the code was read for this item and settles
it; **read only** means the text-to-text conflict is certain but the code was
not traced to the end.

## Contradictions

### C1. Icecast stream info for raw audio tracks

encoder.md §10 says samplerate and channels come "from the internal `audio`
track's parameters, none for copy/raw tracks". io.md §12 says they come
"from the `audio` field when it is an encoded stream". `icecast2.ml:110-128`
matches any `` `Encode `` audio stream, `Raw` included. **io.md is right.**
Fix: encoder.md §10 drops the sentence and links io.md §12 (see D9).
**Checked.**

Applied: encoder.md §10 links io.md §12; the container-table remark moved to io.md §12.

### C2. "Encoders detect a new stream by a change of parameters"

side-data.md §5.1 justifies its cache with "encoders detect a new stream by a
change of parameters ([encoder.md])". encoder-copy.md §12.1 and §12.7 say a
source switch is a change of stream index only and that parameters are never
compared. The only physical-equality test on copy parameters outside the
side-data operator is the inline copy video decoder
(`builtins_ffmpeg_decoder.ml:338`, inline-codecs.md §11.1);
`ffmpeg_copy_encoder.ml` has none. The side-data sentence restates the code
comment at `builtins_ffmpeg_side_data.ml:50`, which is itself stale.
**encoder-copy.md is right.** Fix: side-data.md §5.1 says the cache keeps
`track.ffmpeg.decode.video` from rebuilding its stream
([inline-codecs.md §11.1](../inline-codecs.md)); the stale code comment goes
to findings/side-data (no such file yet: add to the side-data owner's
findings). **Checked.**

Applied: side-data.md §5.1 rewritten with links to inline-codecs.md §11.1 and encoder-copy.md §12.7. Not applied: the stale code comment entry, because there is no side-data findings file and findings files other than this one are out of this edit's scope.

### C3. What "the encoder" does on a new stream index

- decoder-raw.md §5: "The content's time base and stream index are what the
  encoder rebases on".
- decoder-copy.md §5: "The encoder resets its per-stream offsets on a new
  stream index".
- content.md §5: encoders use `stream_idx` "to reset their timestamp offsets".

timestamps.md §2 and §3 say raw audio ignores every input timestamp, time
base and stream index; §4.3 says raw video re-anchors its frame-rate
converter; §6.2 says copy rebases with a shared `max`, which keeps a larger
offset rather than resetting it. **timestamps.md is right** (it is the owner
and matches `ffmpeg_internal_encoder.ml:475-480` for video). Fix: the three
sentences become one link to [timestamps.md §2](../timestamps.md) with no
restated effect. **Checked** for raw video, **read only** for raw audio.

Applied: decoder-raw.md §5, decoder-copy.md §5 and content.md §5 link timestamps.md §2.

### C4. Stream index allocation omits seeks

content.md §5 table: decoder, "one per opened container". encoder-copy.md
§12.1: "one per decoder instance"; §12.6 lists reconnects only. decoder.md §6
and §12: every seek rebuilds the mapping with a new stream index. The
allocation is inside the mapping builder (`ffmpeg_decoder.ml:538`), which
seek re-runs. **decoder.md is right.** Fix: content.md §5 row becomes "one
per stream mapping: at open and at every seek"; encoder-copy.md §12.1 links
content.md §5 instead of listing producers (D12), and §12.6 names seeks
beside reconnects. **Checked.**

Applied, checked (`ffmpeg_decoder.ml` `seek` re-runs `prepare_decoder` → `mk_streams` → `new_stream_idx`; a seek with no known position returns 0 and allocates nothing): content.md §5 row; encoder-copy.md §12.1 links content.md §5 and states seek → rebase and keyframe wait; §12.6 names seeks (heading kept, known-complexity.md links it); timestamps.md §2 row lists seek.

### C5. Field numbering of the probe and of the decoder differ

stream-description.md §3: streams are numbered among those described
successfully; a stream whose parameters fail is skipped and the next one takes
its number (`ffmpeg_stream_description.ml:174`, `List.length streams`).
decoder.md §6: fields are numbered over all streams of the media in container
order (`ffmpeg_decoder.ml:542-568`, `pos`). Each file is right about its own
code, and decoder.md §5.2 says the probe follows stream-description.md, so the
content type the probe returns and the fields the decoder fills disagree
after a failed stream. Neither file says so. Fix: decoder.md §6 states the
divergence and links stream-description.md §3; a defect entry in
findings/decoder.md. **Checked** (code), the failing-stream case not run.

Also decoder.md §6 writes "the n-th audio stream maps to `audio_n`"; the
names are `audio`, `audio_2`, … (stream-description.md §3,
`fields.ml` `audio_n`). Fix the wording to point at that table.

Applied: decoder.md §6 states the divergence and points at stream-description.md §3 for names. Not applied: the defect entry in findings/decoder.md (out of scope).

### C6. A field named twice in `%ffmpeg`

encoder-format.md §1: "A field named twice appears twice in the list".
encoder.md §3 step 4.5 and §12: "every non-drop track builds its stream
encoder", "each non-drop track has one stream encoder". The instance stores
stream encoders in a field map whose `add` removes the previous binding
(`lang/data/methods.ml:41`, used by the fold in `ffmpeg_encoder.ml`), so the
second track replaces the first's stream encoder, while an audio or copy
stream encoder has already added or reserved its muxer stream at build. The
first stream then stays in the container and is never fed; `interleaved`
(§3 step 4.3) and `total_count` (encoder-copy.md §12.1) still count both.
Fix: encoder.md §3 step 4.5 states the replacement and its effect; defect in
findings/encoder.md. **Read only** (traced, not run).

Applied: encoder.md §3 step 4.5. Not applied: the defect entry in findings/encoder.md (out of scope).

### C7. Output stream order of internal video

encoder-internal.md §2.2 ends: "Because video is created at the first frame,
its stream index is fixed by build order like the others". Audio and
subtitle streams are added at build (`ffmpeg_internal_encoder.ml:247, 623`),
copy streams are reserved at build (`ffmpeg_copy_encoder.ml:44`), and internal
video is added only at `mk_stream` (`ffmpeg_internal_encoder.ml:399`), which
runs in field iteration order (reverse of track order, encoder.md §5). So
`%ffmpeg(%video(codec=…), %audio(codec=…))` gives audio stream 0, video
stream 1, and two video tracks come out in reverse. tests.md §5.1 item 2
("appear in the order the encoder declares them, whatever mix") generalises
from a case with video declared last. **The code contradicts
encoder-internal.md.** Fix: encoder-internal.md §2.2 states the rule (video
streams come after every build-time stream, in reverse track order among
themselves); tests.md §5.1 item 2 narrows the claim to the tested layout.
**Read only** (traced, not run).

Applied: encoder-internal.md §2.2 states the order; tests.md §5.1 item 2 narrowed.

### C8. The last copied packet of a file

decoder-copy.md §6: `Flush` emits the held packets as one chunk.
timestamps.md §11.1: "A's final packet, dts 88064, is not in the content the
decoder delivers" (findings/encoder.md G8, checked). The copy decoder does
emit them on `Flush` (`ffmpeg_copy_decoder.ml:60-62`), so the packet is lost
after the per-stream decoder: decoder.md §13.1 sends `Flush` from `eof`, then
adds a track mark, and says nothing of whether the generic file decoder still
delivers what `eof` put. Unsettled. Fix: decoder.md §13.1 states where the
content put by `Flush` goes; timestamps.md §11.1 links that instead of
asserting the decoder's behaviour. **To verify.**

Applied, read only: `Decoder.mk_decoder` puts `eof`'s output in the generator, and `Generator.slice` and `add_track_mark` cut at the shortest media field (`generator.ml` `_slice`, `default_pos`), so audio past the video's end is not delivered. decoder.md §13.1 says so; timestamps.md §11.1 links it.

### C9. HLS never splits with a subtitle track

encoder.md §8.2: subtitle tracks get no keyframe hook. §8.4 step 1: a split
needs the flag of "every track that has a stream encoder". `can_split`
(`ffmpeg_encoder_common.ml:306-312`) includes subtitle fields, whose flag is
never set, so a `%ffmpeg` HLS variant with a subtitle track never returns
`Ok`. The two sections agree with the code; the consequence is stated
nowhere and is not in findings/encoder.md. Fix: encoder.md §8.4 states it;
defect in findings/encoder.md. **Read only.**

Applied: encoder.md §8.4. Not applied: the defect entry in findings/encoder.md (out of scope).

## Duplicated rules

| #   | Rule                                                                                                             | Owner                                                            | Restated in                                                                                                                                                                                                                                                                  |
| --- | ---------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| D1  | `convert_time_base`: products, `den = 0` → `Int64.max_int`, truncation toward zero                               | common.md §5                                                     | timestamps.md §1.2 (whole paragraph); inline-codecs.md §8 (formula paragraph)                                                                                                                                                                                                |
| D2  | Duration helper: grouping, lag of one item, non-monotonic log and raise, flush length                            | common.md §6                                                     | inline-codecs.md §8 (the whole chunker table); filters.md §13.1 bullet 1; bitstream-filters.md §5 "Duration converter". filters.md keeps its own realignment rule (bullet 3) and its tail (§13.2), which differs from `flush` on purpose (`ffmpeg_filter_io.ml:432`)         |
| D3  | Main ticks, time bases, default 44100 main rate                                                                  | common.md §4                                                     | timestamps.md §1.1 (rows and defaults line); decoder.md §8.1 last paragraph; filters.md §1 "main time base" row (links content.md)                                                                                                                                           |
| D4  | Display stage: creation, convert, eof, `expected_size`, `pixel_aspect`, `set_on_undecided`                       | side-data.md §3 (it declares ownership of "what the stage does") | filters.md §16.1 in full. Pick one: side-data.md §3, with filters.md §16.1 cut to a pointer; then decoder.md, decoder-internal.md, decoder-raw.md, image-decoder.md (which link filters.md, Display) and inline-codecs.md (which links side-data.md) all link the same place |
| D5  | Hardware context selection for encoders                                                                          | common.md §11                                                    | encoder-internal.md §2.2 step 3                                                                                                                                                                                                                                              |
| D6  | Encoder pixel format choice                                                                                      | common.md §7                                                     | encoder-internal.md §2.2 step 2                                                                                                                                                                                                                                              |
| D7  | Scaling settings: accepted values, defaults, threads `0` = per core                                              | common.md §10                                                    | encoder-internal.md §2.2 and §4.4; inline-codecs.md §7 table and §16; filters.md §2                                                                                                                                                                                          |
| D8  | Settings index with effects                                                                                      | common.md §1 (index), each key's owner                           | filters.md §2 rows for `autorotate`, `format_change.*`, `scaling_algorithm` (only `filter_max_buffer` is filters.md's); inline-codecs.md §16                                                                                                                                 |
| D9  | `%ffmpeg` special cases of `output.icecast`/`output.harbor`: MIME, mp3 `id3v2_version`/`write_xing`, stream info | io.md §12 (its opening paragraph claims it)                      | encoder.md §10 bullets 1–3; keep `header ()` and `dedicated_encoder` in encoder.md. Also C1                                                                                                                                                                                  |
| D10 | `output.url`: format check message, error and restart                                                            | io.md §1.4, §11                                                  | encoder.md §9 row `output.url` and the restart sentence; keep `output.file` in encoder.md                                                                                                                                                                                    |
| D11 | Script-visible stream records of `input.harbor.dynamic`                                                          | stream-description.md §7                                         | io.md §10.4 "Stream record" paragraph                                                                                                                                                                                                                                        |
| D12 | Who allocates a `stream_idx`                                                                                     | content.md §5                                                    | encoder-copy.md §12.1 first paragraph (with the gap of C4)                                                                                                                                                                                                                   |
| D13 | Fps helper graph: filters, `setpts` expressions, `start_time`                                                    | filters.md §16.2                                                 | timestamps.md §4.2 bullets (keep the anchor of §4.3, which is timestamps.md's); encoder-internal.md §4.6; inline-codecs.md §7 step 2                                                                                                                                         |
| D14 | Subtitle display times `trunc(seconds × 1000)`                                                                   | timestamps.md §5                                                 | encoder-internal.md §4.7 third bullet                                                                                                                                                                                                                                        |
| D15 | Global side data given to a raw video encoder                                                                    | side-data.md §7                                                  | encoder-internal.md §2.2 step 4 (states it and links side-data.md; cut to the link)                                                                                                                                                                                          |

The restated texts agree with their owners except where C-items say

Applied: D1 (timestamps.md §1.2, inline-codecs.md §8), D2 (inline-codecs.md §8, filters.md §13.1, bitstream-filters.md §5), D3 (timestamps.md §1.1, decoder.md §8.1, filters.md §1), D4 (side-data.md §3 is the owner; filters.md §16.1 is a pointer; decoder, image and inline links go to side-data.md §3), D5, D6, D7 (encoder-internal.md §2.2, §4.4; inline-codecs.md §7, §16; filters.md §2), D8, D9, D10, D11, D12, D13 (timestamps.md §4.2, encoder-internal.md §4.6, inline-codecs.md §7 step 2), D14, D15.
otherwise; the risk is drift, not a present disagreement.

## Bad links

| #   | Where                                       | Link                                                                                                         | Problem                                                                                                                                                                              | Fix                                                                                                                     |
| --- | ------------------------------------------- | ------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------- |
| B1  | content.md §9 step 3                        | [converters.md] for the fit filter                                                                           | converters.md covers the generic samplerate and image converters, not Fit                                                                                                            | filters.md §16.4                                                                                                        |
| B2  | encoder-internal.md §4.5                    | [converters.md]                                                                                              | same                                                                                                                                                                                 | filters.md §16.4                                                                                                        |
| B3  | stream-description.md §6, video "any other" | [converters.md] for setting alpha                                                                            | no such rule there                                                                                                                                                                   | common.md §7 "Set format alpha"                                                                                         |
| B4  | content.md §8                               | [encoder.md], [timestamps.md] for keyframe waiting                                                           | neither owns it                                                                                                                                                                      | encoder-copy.md §12.2–§12.3                                                                                             |
| B5  | content.md §6.9 last line                   | [encoder.md] for consumers iterating runs                                                                    | encoder.md has no input section                                                                                                                                                      | encoder-internal.md §3, encoder-copy.md §3                                                                              |
| B6  | content.md §10 last paragraph               | [encoder.md] for the encoder's frame type                                                                    | it is encoder-format.md's                                                                                                                                                            | encoder-format.md §10                                                                                                   |
| B7  | filters.md §1, "main time base"             | [content.md]                                                                                                 | main ticks are common.md's                                                                                                                                                           | common.md §4                                                                                                            |
| B8  | filters.md §13.1, Duration converter        | "`timestamps.md` describes the same mechanism for packets"                                                   | timestamps.md has no duration helper                                                                                                                                                 | common.md §6                                                                                                            |
| B9  | bitstream-filters.md §5                     | [filters.md, Frame flow and timestamps]                                                                      | the shared mechanism is common.md §6; filters.md adds a realignment the bitstream filter does not use                                                                                | common.md §6                                                                                                            |
| B10 | inline-codecs.md §3                         | "every bitstream filter, see [filters.md]"                                                                   |                                                                                                                                                                                      | bitstream-filters.md §4                                                                                                 |
| B11 | inline-codecs.md intro and §4 end           | [encoder.md, Format]                                                                                         | no such section; format is another file                                                                                                                                              | encoder-format.md (§5–§6 for defaults)                                                                                  |
| B12 | inline-codecs.md intro                      | [encoder.md, Internal stream encoder]                                                                        |                                                                                                                                                                                      | encoder-internal.md                                                                                                     |
| B13 | inline-codecs.md §6 step 2                  | [encoder.md, Audio frame writer]                                                                             |                                                                                                                                                                                      | encoder-internal.md §5.1, timestamps.md §3                                                                              |
| B14 | inline-codecs.md §7                         | [encoder.md, Pixel format], [encoder.md, Hardware acceleration]                                              |                                                                                                                                                                                      | common.md §7, common.md §11                                                                                             |
| B15 | inline-codecs.md intro, §9, §11             | [decoder.md, Internal video conversion]                                                                      | decoder.md has no such section                                                                                                                                                       | decoder-internal.md §2.2                                                                                                |
| B16 | inline-codecs.md §9 bullet 3                | "the copy encoder sees the same index with timestamps going back to 0: timestamps.md owns what happens then" | timestamps.md states no such case. By §6.3 (same source, no switch, no rebase) every packet with `last_start + dts ≤ last_dts` is dropped until the new timestamps pass the old ones | add the case to timestamps.md §2 table (row "same source id, timestamps restart") after checking it, then link that row |
| B17 | filters.md §16.4 Users; §16.2 Users         | "the internal encoder ([encoder.md])", "the encoders' video paths ([encoder.md])"                            |                                                                                                                                                                                      | encoder-internal.md §4.5; encoder-internal.md §4.6 and inline-codecs.md §7                                              |
| B18 | io.md §10.3 step 4                          | [decoder.md], container description                                                                          | the description is stream-description.md's                                                                                                                                           | stream-description.md §2, §5                                                                                            |
| B19 | filters.md §16.1                            | "properties ignored by format comparison (§2: colour, pixel aspect)"                                         | §2 of filters.md is a settings table                                                                                                                                                 | common.md §9                                                                                                            |
| B20 | side-data.md §5.1                           | [encoder.md]                                                                                                 | wrong subject (C2)                                                                                                                                                                   | inline-codecs.md §11.1                                                                                                  |
| B21 | encoder-copy.md §10                         | "Errors: §2.1"                                                                                               | §2 has no subsection                                                                                                                                                                 | "§2 step 1"                                                                                                             |

Links verified and correct, for the record: every `src/core/clock/spec/clock.md`
anchor (§10, §11, §15 with "Shared child clocks"), `pacing.md` "Finding the
sync source", and the bindings' `avformat.md §4.4`, `§7.1.4`, `avfilter.md
§4.13`, `§11.4`, `§11.5`.

Applied: B1–B21. B16 checked (`ffmpeg_copy_encoder.ml` `check_dts`: same index, begun, offset `last_start`) and added as a timestamps.md §2 row. Also anchored every link that names a section, and fixed content.md intro, encoder-internal.md intro and common.md §1 owners, which linked converters.md or a bare file for the same reasons.

## Layout

- **L1. Fourth per-stream decoder has no file.** decoder.md §1 speaks of "the
  four per-stream decoders" and lists three files. The fourth, timed ID3, is
  in decoder.md §10.3 and does not follow §7's numbering. Per the brief, give
  it its own file (`decoder-timed-id3.md`, §1–§8) or rewrite §10.3 in that
  numbering and fix the count.
  Applied: decoder.md §1 says three families and points at §10.3 for timed ID3. No new file.

- **L2. Contract numbering is followed** by encoder-internal.md and
  encoder-copy.md (§1–§11 = encoder.md §12.1–§12.11) and by
  decoder-internal.md, decoder-raw.md, decoder-copy.md (§1–§8 = decoder.md
  §7.1–§7.8). encoder-copy.md appends a §12 "Source switching", which reads
  as "§12" next to encoder.md's contract §12; acceptable since references
  carry the file name, but a title-level note ("§12 is specific to copy")
  avoids misreading.
  Applied: encoder-copy.md intro notes that its §12 is specific to the copy encoder.

- **L3. Shared helpers inside one implementation's file.** decoder-internal.md
  §2.2 (internal video converter, used by inline decoders) and §7.1 (subtitle
  timeline, used by decoder-copy.md) sit inside contract sections of one
  implementation. decoder.md §15 already lists them as reused interfaces; they
  belong in decoder.md (the seam) or a helpers section there.
  Not applied: no ruling to move them; moving would renumber sections other files link.

- **L4. Part B material in Part A:**
  - encoder-copy.md §12.2 heading names the function `check_stream`;
    §12.5 and timestamps.md §6.5 explain lifetime by "the GC reclaims";
    state the observable rule only ("a record no track holds is forgotten at
    an unspecified later time") and leave the GC to language-notes.
  - inline-codecs.md §5: "weak set", "physical identity", "hashed by its id";
    §3.2: `mk_process_frame generator`; title "(Part A)".
  - content.md §3 "A format is a mutable cell (unifier)", §4 `mutable` fields
    in the record notation, §10 "of which exactly one is linked".
  - bitstream-filters.md intro: module name `Child_support`.
  - io.md §2.1–§2.3: "atomic state", CAS steps; §1 table "same class".
  - encoder-format.md §11: "hash table's iteration order", "forces the lazy
    rates" (§6 "all lazy").
  - Binding function paths used as explanation: common.md §2
    (`Avutil.Log.set_level`, `set_callback`, `clear_callback`), §11
    (`Avcodec.hw_configs`); side-data.md §1, §3.1, §3.3, §5.2
    (`Avcodec.params_side_data`, `Avfilter.Utils.init_display_converter`,
    `display_layout`, `params_with_side_data`, `Frame.dup`). Replace with the
    bindings' spec section names.
    Applied: every listed item reworded; Part B detail added to language-notes/encoder.md (`check_stream`, `begin_stream`, `check_dts`, `Hashtbl` order) and language-notes/common.md (binding function names); the others were already there. The encoder-copy.md §12.2 heading change breaks known-complexity.md lines 471 and 474 (anchor now `#122-accepting-a-packet`), which this edit does not touch.

## Judgements in the text

Pointers to findings inside as-built sentences (move the qualifier to the
findings file, keep the fact):

- encoder-copy.md §2 step 1 "(finding)"; encoder-internal.md §4.2
  "(finding)", §8 "Whether the partial frame is lost was not settled by
  measurement (findings, To verify)" (also hedging); encoder.md §8.4
  "(finding)"; encoder-format.md §4 "(finding)", §10.1 "which differs from
  §5 (finding)", §12 "Those are codec names, not muxer names (finding)";
  timestamps.md §4.4, §6.5, §9 "(findings, To verify)" / "(finding)";
  filters.md §5.2 "(see findings)", §12.2 "(see findings)"; inline-codecs.md
  §3.3 "see [findings]"; tests.md intro "Weak, vacuous and unwired tests",
  §1.5 table, §3.2, §7.1 item 11, §9.1 item 9 "(see findings)".

Evaluative wording:

- encoder-format.md §11 "The rendering is not always valid script" → state
  the two facts (`metadata=[…]` form, `pcm_s16` not rendered); the
  evaluation is findings/encoder.md D11.
- filters.md §17.3 "because `ebur128`'s output PTS are not usable" → state
  what they are, or cite the code comment as the reason.
- common.md §10 "which a real-time stream pays every frame and rarely earns
  back" → keep only if the code comment or history says it, marked as the
  reason; otherwise to findings. **Read only.**
- inline-codecs.md §5 "The index existed for one operator encoding audio and
  video together; splitting it into track operators kept the index shared"
  is a history trail; reword as "Reason (history): …" or drop.

Applied: every pointer is a bare link to the area's findings file or was reworded; the four evaluative sentences were restated as facts (common.md §10 now cites the setting's description as the reason; filters.md §17.3 cites the code comment).

No "bug", "wrong" or "should" was found in the as-built prose; the `must` /
`should` hits are quoted error messages or plain requirements of input.
