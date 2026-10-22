# Findings: known complexity (history mining)

Divergences between the as-built spec and the lessons in
[known-complexity.md](../known-complexity.md) or the user's stated intent
([encoder-copy.md §12.3](../encoder-copy.md#123-contributions-and-readiness)). Each finding names the as-built file
and section that would need to change. Where another findings file already
holds the evidence, the entry points to it (E- = findings/encoder.md,
I- = findings/inline-codecs.md) instead of restating it.

## Defects

### Same-source streams do not wait for each other (Contradicts, entry 3)

encoder-copy.md §12.2 step 1.5 and §12.3; `ffmpeg_copy_encoder.ml:96`
overwrites the shared `ready` flag at every switch, so an intra-only audio
stream starts at its switch, before its sibling video's keyframe. The
shared count cannot be the fix alone: `total_count` (§12.1,
`ffmpeg_encoder.ml:79-82`) counts every copy track of the format, so a
source feeding only some of them never reaches it (GH3840, which the
override was added for). The readiness target has to be the number of
tracks currently on that source. Sections to change: encoder-copy.md §12.1
(`total_count`), §12.2 step 1.5, §12.3 table. **Checked** (E-D2: `cut1.liq`,
`cut2.liq`; E-D3 read only).

### Shared offset: order-dependent jump and early read (Contradicts, entry 2)

timestamps.md §6.1 step 4 and §7. Two effects of the declaration order of
the tracks: (a) a track that begins before a sibling with a larger
candidate jumps forward when the sibling raises `last_start` (§11.2: audio
packet 2 at 0.0800 s instead of 0.0232 s); (b) the first track to switch
away reads the shared `position` before its siblings have written their
last packets of the same frame, so its new source can start before the
previous source's other streams end. Known-complexity §2 names both as the
pitfall. **Checked** for (a) (E-D4: `seq2.liq` vs `seq.liq`); (b) read only.

### Per-source state outlives or predeceases its streams (Contradicts, entry 4)

The observable rule "state exists exactly while a stream of that source is
muxed" fails three ways:

- early: `remove` drops the record at `ready_count <= 1` while a sibling
  still has it as current and has not begun (E-D5); the sibling's late
  keyframe gets a fresh record without the shared offset;
- late, GC-dependent: after an Ogg restart new copy tracks may find the old
  container's records (E-G1, timestamps.md §6.5);
- late, GC-dependent: an inline encoder rebuilt after sleep gets its old
  stream index if the weak entry survived (I-"Stream index after sleep
  depends on the GC"); downstream copy then sees the same identity with DTS
  back at 0 and drops packets as non-monotonic until DTS catches up, or
  rebases correctly if the entry was collected.
  Sections to change: encoder-copy.md §12.5, timestamps.md §6.5,
  inline-codecs.md §5 and §9. **Read only.**

### Best-effort timestamp fallback removed (Contradicts, entry 6)

f682e8d9b (#1609) made decoded frames without PTS use the frame's
best-effort timestamp. PR #3032 (45f579b8f, "Ffmpeg filter cleanup")
removed the `best_pts` helper with no stated reason; the decoders now read
`Avutil.Frame.pts` only (`decoder/ffmpeg_decoder.ml:113-114`,
`decoder/ffmpeg_raw_decoder.ml:30`). The bindings' `pts` does not fall back
(avutil.md §4.3). A frame with no PTS but a best-effort timestamp is then
decoded "at once" with the no-timestamp warning (decoder.md §8.4) and
joins the held group of the grouping helper (decoder-raw.md §5), i.e. gets
length 0. Sections to change: decoder.md §8.2 step 4 and §9,
decoder-raw.md §5, decoder-internal.md §5. **Read only.**

### HLS split lands after the keyframe (Contradicts, entry 9)

E-D1. encoder.md §8.2 (hook calls the split callback before setting its own
flag) and §8.4: segments start after the key packet, which ends the
previous segment. The history (forced splits without keyframes gave
undecodable segments) shows this is the failure the keyframe test exists to
prevent. The order has been the same since 44e8f661e (PR #4841). **Checked**
(E-D1).

### Trailing empty time of a sparse copy stream is not counted (Contradicts, entry 8)

timestamps.md §6.2 and encoder-copy.md §12.4: `last_position` is the
previous record's `position`, the end of its last packet; empty chunks
after that packet are ignored. For a subtitle stream whose source has no
dense copied sibling (subtitles-only, or subtitles from another source than
audio/video), the next source's cues land early by the time between the
previous source's last cue end and the switch. #4860 added the head half of
this rule; the tail half is missing. With a dense sibling on the same
source the shared `position` hides it. **Read only**
(`ffmpeg_copy_encoder.ml:96-97`, `:118`).

### Missing duration overlaps a join (Contradicts, entry 1)

timestamps.md §6.1 step 5 and §6.4: a packet with no duration (FFmpeg
duration 0 reads as none, avcodec.md §3.2) advances `position` by 1 tick, so
the next source starts one tick after the previous source's last DTS: the
one-packet overlap that 233a3ef84 fixed for packets with a duration.
Sources whose packets carry no duration (some FLV/RTMP inputs) are the ones
concerned. **Read only.**

### Raw audio keeps the first source's resampler (Contradicts, entry 6)

E-A1; encoder-internal.md §4.2, timestamps.md §2 ("new audio parameters
(raw audio): nothing"). When the raw audio type leaves the rate undecided,
a later source at another rate goes through a resampler configured for the
first one; the sample counter of timestamps.md §3 then counts the wrong
number of samples and audio drifts against the other streams. This is the
audio half of #3329 (open); the video half is handled by the re-anchor
(timestamps.md §4.3). **Read only.**

### Held data is lost at ends and changes (Contradicts, entry 10)

Losses collected from other findings, one cause: a stage discarded or ended
without draining. E-G2 (frame-rate converter dropped at a key change),
E-G3 (audio re-framer and resampler never flushed), E-G8 (last audio packet
of each copied file missing, checked), I-"Encoder tail is never delivered",
findings/decoder.md "Raw decoders drop the last frame at end of stream" and
"Pending interleave items are lost at end of stream". History: #3109 and
#5442 fixed earlier instances. Sections to change: encoder-internal.md §4.6
and §8, inline-codecs.md §8 and §13, decoder-raw.md §6, decoder.md §13.1.
G8's cause is not identified; decoder-copy.md §6 says the held packets are
flushed. **Mixed** (G8 checked, rest read only).

### Chunk-length drift matches #4502 (Contradicts, entry 13)

Per-step truncation (common.md §6.2, decoder-copy.md §5, inline-codecs.md
§8) shortens each stream's content by a stream-dependent fraction. #4502
(open): copy relay from RTMP (FLV, time base 1/1000) fails after hours with
`Generator max buffered length exceeded`, buffered video growing about
0.38 s/h with audio flat. Arithmetic: AAC 44.1 kHz steps of 23/24 ms lose
0.3/0.4 tick each, about 1.1 s/h; 30 fps video steps of 33/34 ms lose about
0.8 s/h; the audio runs short faster, so the reader fills audio and video
accumulates at about 0.3 s/h. Consistent with the report; not reproduced.
Sections to change: common.md §6.2 (convert absolute timestamps or carry
the remainder). **Read only** (mechanism checked by I-"Chunk lengths
truncate per packet" and findings/decoder.md "Copy chunk lengths drift").

## Asymmetries

### Copied and re-encoded streams drift apart (Not covered, entry 12)

E-A2. No section of timestamps.md relates copy timestamps (source DTS plus
offset) to internal ones (sample/frame counts); time a copy stream skips
at a switch moves it against the re-encoded streams for good. timestamps.md
§7 would need a rule for mixed outputs. **Read only.**

## Gaps

### Order at equal positions is not stated (Not covered, entry 7)

content.md §6.1, §6.4, §6.5, §6.6 say items are "sorted by position", not
that items at equal positions keep their order. The code uses `List.sort`
(`base/ffmpeg_content_base.ml:43,161`), which is stable only because the
OCaml stdlib defines `sort = stable_sort` (list.ml:452); its documentation
does not promise stability. PR #3764 (341eb4da1) switched the generic
contents to `List.stable_sort` for this reason (non-monotonic PTS crash).
content.md needs the stability rule; the code relies on an undocumented
property. **Checked** (stdlib source).

### Monotonicity check skipped after a DTS-less packet (Not covered, entry 5)

E-D6. timestamps.md §6.3 states the behaviour; known-complexity §5 requires
that the muxer never sees a non-monotonic DTS, which a dts-less packet
followed by a stale DTS breaks. **Read only.**

### Copy parameters across sources (Not covered, entry 11)

E-D10 (checked: AVCC → Annex B switch stops the output),
I-"Audio copy decoder ignores codec changes under one stream index".
encoder-copy.md §12.7 states that nothing compares parameters; no rule
handles a change. #3164 (open) asks for per-source bitstream filtering.
**Checked** (via E-D10).

### HLS segment grid has no owner in this spec (Not covered, entry 9)

The bounded wait for a late keyframe (target duration), and the rule that
leftovers count towards a segment's length without moving the grid
(HE-AAC/AAC-LC drift, #5266, #5319), live in the generic HLS output, which
this spec only references. No file owns them. Pass 2 needs either a link
target or a short contract section in encoder.md §8. **Read only.**

## To verify

### #3329 status

Video raw re-encode re-anchors on a new source (timestamps.md §4.3); audio
counts samples whatever the source (§3), so the PTS part of #3329 looks
obsolete; the resampler part stands (Defects, "Raw audio keeps the first
source's resampler"). Needs a run with a 44.1/48 kHz sequence. **Read only.**

### Stale doc comment in `ffmpeg_inline.ml`

"dropped on flush, so codec state never survives a track boundary": flush
happens on sleep (inline-codecs.md §3.2). PR #5302 text says the inline
encoder is rebuilt "at the next track boundary". Same as I-"Transform
persists across tracks". **Read only.**
