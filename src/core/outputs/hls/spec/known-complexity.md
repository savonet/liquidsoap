# Known complexity: the HLS output

This file covers the HLS file output (segmenting, media and main playlists,
files on disk, persisted state, timed ID3 and inserted tags), the seam
between the output and its encoders, and the stdlib wrappers that write the
main playlist and serve the files over HTTP. It leaves out the HLS input and
the HLS upload to video platforms. Each entry says what the project's history
shows to be hard, in terms that hold for any implementation, and gives a check
a reimplementation can be run against. The FFmpeg layer's own difficulties
(timestamps, keyframe detection, interleaving) are in
[../../../optionals/ffmpeg/spec/known-complexity.md](../../../optionals/ffmpeg/spec/known-complexity.md);
entries here are about the output's side of the same events.

**Counts.** The core output has 167 commits since HLS was added in April 2019
(179 when its earlier file names are followed; 90 since 2022). 37 of them are
fixes by their message. There are 2 reverts, one cosmetic pair; the stdlib
wrappers have 23 commits and one more cosmetic revert. GitHub search for
"hls" gives 151 closed issues: about 40 concern this output, and the rest are
about the HLS input, the upload to video platforms, or questions. 14 issues
are open, of which low-latency HLS is the only one about this output. There
are 6 HLS stream tests, 5 HLS media tests and 6 HLS regression tests. One
field report from 2026-11-05 is also included (entries tagged "source: field
report").

Vocabulary: output, variant, segment, init segment, media playlist, main
playlist, split point, grid boundary, target duration, discontinuity, media
sequence, discontinuity sequence, persisted state, temp file / atomic rename,
file-change callback, timed ID3 / ID3 tag, inserted tag, container restart,
forced split, reopen-on-error.

## Segmenting & grid

### KC1. Target duration against keyframe availability

**Situation.** A variant can only be cut where every stream starts on a key
packet (a split point). Keyframes come at the encoder's or the source's
interval, not the output's. Copied video has whatever keyframes the source
has. The target duration announced in the media playlist may not change for
the life of the playlist.

**What went wrong.** Segments ran to 20 s under a 10 s target duration, and
players stalled. Segments cut where there was no keyframe were undecodable.
With large internal frames, a split that the grid required came after a
metadata cut and pushed the segment past its bound. A user asked for "wait
for the next keyframe and raise the target duration", as FFmpeg's own muxer
does. The request was declined because the target duration cannot change.

**Why it is hard.** Waiting for a keyframe and respecting the target duration
pull against each other. The output only learns where a keyframe is after the
encoder has produced it, which can be a codec delay later than the media time
it has given the encoder. Several reasons to cut can come due in the same
frame (grid, bound, metadata, tag), and which one is taken first decides
whether the bound holds.

**How to tell.** Binding: no `EXTINF`, rounded to the nearest integer,
exceeds the target duration. Every cut made without a split point is logged
as a warning. A variant that can be cut that way does not announce
independent segments. Re-encoded video with keyframes every 4 s and 1 s
segments: every segment starts on a keyframe. The same video copied: the
bound holds, with warnings. Incidental: where inside the bound the cut lands.

**Evidence.** ab05b120b, #3630 / 347d87449, 75e7a8ebd (hls_large_frame), #5266
(declined: target duration is fixed), 4bf0f1d12. Recurred 5 times.

### KC2. Leftovers must not move the grid

**Situation.** A cut at a split point before a grid boundary leaves media
after the cut, which starts the next segment. Codec frames do not line up
with internal frames: a 2048-sample HE-AAC frame spans more than two
960-sample internal frames, so some internal frames produce no packet.

**What went wrong.** Segment boundaries drifted backwards for good. HE-AAC
segments alternated 4.000 / 3.980 s while AAC-LC stayed at 4.000. Variants of
one output drifted apart by about 10.7 ms per segment, without bound. Ten
segments summed to 19.8 s instead of 20.0 s.

**Why it is hard.** The leftover is real media, so it counts towards the next
segment's duration. But if it also counts towards the next split decision,
each boundary is placed relative to the previous cut and not to the grid. The
error is a few milliseconds per segment and depends on the codec, so a test
with one codec, or one that checks a single segment, passes.

**How to tell.** Binding: grid boundaries are absolute multiples of the
segment duration from one origin, so the sum of the first _n_ `EXTINF`s stays
within one frame of _n_ × segment duration. Two variants, HE-AAC and AAC-LC:
after fifty segments, segment _n_ starts at the same media time in both,
within one codec frame. The persisted state carries whatever the grid needs
across a restart, and a state file without it still loads. Incidental: how
the leftover is stored.

**Evidence.** #5319 / 3d21bd2e6 (GH5319); earlier precision fixes 41491e5ab,
86c8e8532. Recurred 3 times.

### KC3. Split points just after every boundary (source: field report)

**Situation.** Copied H.264 with B-frames, a fixed 2 s GOP, and 6 s segments.
The B-frame delay and the audio encoder's priming put the first keyframe
0.04–0.1 s after the variant's start, so every keyframe lands just after its
grid boundary and never at or just before it.

**What went wrong.** Video `EXTINF`s alternated 4.000, 2.000, 4.000, 2.000…
while the audio-only variant was a steady 6.000. Keyframes sat at 2.05, 4.05, 6.05 s… of grid
time. With the rule "cut at the last split point at or before the boundary,
else the first after it", the 6 s boundary is cut at 4.05 s, one GOP early.
The next segment still faces the 6 s boundary, holds no split point at or
before it, and is cut at 6.05 s: a 2 s stub. The
same happened at 29.97 fps with 6.006 s and 6.0 s segments. The output was
conformant to its own rule and to RFC 8216, and still unusable: twice the
request rate, an uneven buffer, and variants out of step (KC4).

**Why it is hard.** "At or before the boundary" is the rule that guarantees
the target-duration bound, and a systematic offset of a few tens of
milliseconds defeats it on every segment. There is a tension between a
tolerance at the boundary (taking the split point nearest the boundary even
when it is slightly after) and the target-duration bound (KC1), which a
tolerance must still respect. The offset comes from the content (B-frame
delay, priming), which the output cannot remove.

**How to tell.** Binding: copied H.264 with B-frames plus AAC-LC and HE-AAC,
2 s GOP, 6 s segments: every `EXTINF` is 6 s within one video frame, and no
two consecutive segments alternate long/short. Repeat with the keyframes
moved a few frames before each boundary: the same result. Incidental: whether
the cut is taken before or after the boundary, as long as the bound of KC1
holds.

**Evidence.** Field report 2026-11-05 (copied H.264 with B-frames + AAC-LC/HE-AAC,
2 s GOP, 6 s segments). Observed once, systematic on that content.

### KC4. Variants of one output on different origins (source: field report)

**Situation.** One output with several variants: video at three resolutions
and an audio-only one. Their streams start at different media times (first
video keyframe at 1.505 s, AAC-LC at 1.463 s, HE-AAC at 1.400 s in one source
file).

**What went wrong.** After the same run, the media sequence was 0 in one
variant and 4 in another. Video variants agreed with each other; the
audio-only one diverged. Adaptive switching needs segment _n_ to cover the
same media time in every variant, so a player switching variants lands at the
wrong point. Part of the divergence is KC3: the stub segments make video
variants count two segments where audio counts one.

**Why it is hard.** Each variant has its own encoder and its own idea of
where its media starts. A grid counted from each variant's first media time
is self-consistent per variant and misaligned across them. The tension is
between a per-variant origin (each variant's grid starts where its media
does) and a per-output origin (all variants share one grid, and a variant
whose media starts late has a shorter first segment). Segment counts, and so
media sequences, also diverge whenever variants are cut a different number
of times (KC3, extra cuts in KC6), even with one origin.

**How to tell.** Binding: in every variant of one output, segment _n_ starts
at the same media time, within one frame of the slowest-framed stream, and
the media sequence and the segment count are equal across variants after any
run without output-wide cuts, including after a restart. Incidental: file names.

**Evidence.** Field report 2026-11-05 (copied H.264 with B-frames + AAC-LC/HE-AAC,
2 s GOP, 6 s segments). Related drift: #5319. Observed once.

### KC5. The first segment opens before there is anything to put in it

**Situation.** The output starts, or resumes, before the first frame and the
first metadata have arrived. The encoder has not yet produced its first
packet, its header, or its init segment.

**What went wrong.** The first segment was an empty file while the media
playlist announced audio in it, which broke a common web player in event
playlists. The first segment's timed ID3 tag was an empty 10-byte header
with no fields. A fresh start sometimes produced no first segment at all, or
a segment opened at a time that did not match the grid.

**Why it is hard.** Opening at start is natural, but at that moment no data
or metadata exists yet. The order in which a frame's metadata and its data
reach the output decides whether the first tag has content. A muxer that
interleaves may hold back every byte until each of its streams has written,
including a sparse metadata stream.

**How to tell.** Binding: no segment listed in a media playlist is empty. The
first segment of an MPEG-TS variant with ID3 on carries a tag with the first
track's fields. The first segment starts at media time 0 of the grid.
Incidental: whether the segment file exists before its first byte.

**Evidence.** 8ead1e3ec, #4398 / 229719d8a, #5084 / edeba8e1c (GH5084).
Recurred 3 times.

### KC6. Extra cuts for metadata and inserted tags

**Situation.** A metadata change or an inserted tag wants a new segment, so
that the tag or ID3 starts the segment it belongs to. This can happen at any
time, including mid-GOP and many times per second.

**What went wrong.** Every repeat of the same metadata reopened a segment,
which gave runs of tiny segments. Metadata cuts competed with the grid's own
cut (KC1). Cuts for metadata were briefly allowed to force a split without a
keyframe.

**Why it is hard.** An extra cut is optional and the grid cut is not: the
extra cut must take a split point only, must not move later boundaries, and
must not fire for metadata equal to what was last sent. Each extra cut adds a
segment to one variant and not necessarily to the others (KC4).

**How to tell.** Binding: sending the same metadata ten times produces no
extra segment. A new title produces at most one extra cut, at a split point.
The boundaries after it are where they would have been without it.
Incidental: whether the extra cut is taken at the very next split point or a
later one.

**Evidence.** 172baa295, #3630 PR (metadata cuts do not force), 7750e4939.
Recurred 3 times.

## Playlists

### KC7. The sliding window: order, media sequence, retention, deletion

**Situation.** The media playlist lists the latest _N_ segments. Older
segments stay on disk for a while for players that are still fetching them,
then are deleted. Each new segment rewrites the playlist.

**What went wrong.** Segments were listed in the wrong order. Segments were
deleted before the playlist that still named them was rewritten, so players
fetched missing files. A retention shorter than the window was accepted.
Users asked for unlimited retention (event and replay use). Deleting a file
that was already gone killed the output.

**Why it is hard.** Three counts (window, retention on disk, media sequence)
must move together on every cut. The order of the two side effects, "publish
the new playlist" and "delete the oldest file", decides whether a reader ever
sees a playlist naming a missing file, and it is easy to get backwards when
the deletion is attached to closing a segment. The media sequence is defined
by the first listed segment, not by a counter, so it is wrong whenever the
list is.

**How to tell.** Binding: at every moment, every segment named by a published
media playlist exists on disk, which holds for the smallest retention allowed
as well. The media sequence equals the number of segments that have left the
window. Segment order is by media time. No segment file stays on disk past
both retention allowances. Incidental: when exactly a file past
retention is removed.

**Evidence.** da858506e, c7722a253, fff33ed19, 2698cb913, c91a70349 / #4374,
#831. Recurred 6 times.

### KC8. Discontinuity accounting across restarts and variants

**Situation.** The output stops and starts again (a fallible source goes
away and comes back, or the process restarts with persisted state). The
segment after the gap is a discontinuity. The discontinuity sequence counts
discontinuities that have left the window.

**What went wrong.** The discontinuity sequence was off after a restart. The
discontinuity marker was written on the first listed segment, where it has no
meaning. Only the first variant of a multi-variant output got its
discontinuity marker after a restart; the others continued as if nothing had
happened. A state flag was left at "restarted" and marked every later
segment.

**Why it is hard.** A discontinuity belongs to one segment of each variant,
and the counter in the playlist header counts those that have scrolled out,
so the marker and the counter move at different times. A single output-wide
flag cleared by the first variant that consumes it leaves the others
unmarked. Clock resets for latency relief look like restarts and are not.

**How to tell.** Binding: stop and restart a three-variant output with
persisted state: each variant's next segment carries a discontinuity marker,
and after the window has scrolled past it, every variant's discontinuity
sequence is 1. The first listed segment carries no marker. A run without a
restart never shows a discontinuity. Incidental: the segment numbers skipped
over the gap.

**Evidence.** 4bbacad8b, 744fc5d01, b0c7dda94 (hls_discontinuity), bc1a4bbae.
Recurred 4 times.

### KC9. Main playlist attributes come from the encoder

**Situation.** Each variant line in the main playlist needs BANDWIDTH and
should carry CODECS and, for video, RESOLUTION. The values are only known
once each encoder has started, and depend on container options.

**What went wrong.** BANDWIDTH was the nominal bit rate, not the peak the RFC
asks for, and an AVERAGE-BANDWIDTH was added then removed. CODECS was
incomplete ("avc1" without profile and level) for MPEG-TS, which broke some
web players. Missing values made the operator fail, then were required from
the script, then inferred. Video size was required when it could not be
inferred.

**Why it is hard.** The codec string needs the encoder's global header, which
some containers do not produce unless asked. A partial CODECS list is worse
than none. The values are known at a different time for each variant, and
the main playlist is written once.

**How to tell.** Binding: the same H.264 variant in MPEG-TS and in fMP4 gives
the same full codec string. A variant whose codec string is unknown has no
CODECS attribute and a warning; no line carries a partial list. Values given
by the script override inferred ones. Incidental: attribute order.

**Evidence.** #862, #925, fcd7f7442, 1c09ba72f, 7d34b80aa, db8f333bd, #2173,
#3483 / 84bf5e8ed (hls_global_headers). Recurred 7 times.

### KC10. URIs inside playlists

**Situation.** Playlists name segments, init segments and media playlists.
The files are written under a directory, possibly in sub-directories chosen
by a naming function, and served from a URL prefix.

**What went wrong.** Absolute file system paths ended up in playlists; init
segment URIs were absolute when segment URIs were not. Sub-directories in
segment names had to be created on the fly. The configured prefix was
applied to segments and not to the init segment.

**Why it is hard.** Each kind of file is named by a different code path
(segment, init segment, media playlist, main playlist), and each needs the
same rule. A segment's name may depend on its duration, which is only known
when it is cut, so the final name is chosen after the bytes are written.

**How to tell.** Binding: every URI in every playlist is relative (or starts
with the configured prefix) and resolves to an existing file under the
output directory. This includes the init segment, and names that contain
sub-directories. Incidental: the naming scheme.

**Evidence.** 2ad9bfa7f, e0b083875, #1643 / 108503c81, #2725, 17b0bceca,
24f373365. Recurred 5 times.

### KC11. The main playlist's lifecycle

**Situation.** The main playlist does not change during a run, so it is
written once. On stop without persisted state, files are removed. A
fallible source stops and starts many times in one run.

**What went wrong.** After a stop and start the media playlists came back but
the main playlist did not, and players lost the stream. Users wanted to
write their own main playlist, or none.

**Why it is hard.** "Written once" plus "deleted on stop" needs a reset that
is tied to the deletion. A script-provided writer can decline to write.

**How to tell.** Binding: stop and start the output five times: after each
start, the main playlist exists once every variant has a segment. A writer that
declines leaves no main playlist file and no error. Incidental: whether the
main playlist is rewritten when nothing changed.

**Evidence.** #3275 / c610db52b, 42d1263d4 (hls_main_playlist). Recurred 2
times.

## Files & atomicity

### KC12. Readers see files while they are written

**Situation.** A web server, a CDN sync, or a player reads the directory
while the output writes it. The temp directory may be on another file system
than the output directory.

**What went wrong.** A reader saw a playlist without its first line and
failed. The atomic write then failed on a cross-device rename and shut the
whole process down. The copy fallback later produced empty files when the
temp directory was on another file system, and kept its error silent. Temp
files were left behind on errors. A segment's permissions came from the temp
file and not from the configured ones.

**Why it is hard.** Rename is atomic only within one file system, and the
fallback (copy) is not atomic unless it copies to a temp file next to the
destination and renames that. The failure mode is silent: a zero-length file
is a valid file. Every exit path, error included, has to remove the temp
file.

**How to tell.** Binding: with the temp directory on another file system,
every published file is complete and non-empty, has the configured
permissions, and the mismatch is reported once at startup. A reader polling
the playlist never sees a partial one. After an error mid-write, no temp file
remains. Incidental: temp file names.

**Evidence.** #3284 / 51342b37e, #3290 / 49c1c276a, 7c4a3d5a9, 26b6a9e67,
0bb1f0eeb. Recurred 5 times.

### KC13. What the file-change callback reports, and when

**Situation.** Scripts mirror the directory to a CDN from the file-change
callback: upload when a file is complete, delete when it goes away.

**What went wrong.** Users received "opened" and "deleted" for segments but
never "closed", so they had no event meaning "complete". With atomic writes
the states became "created", "updated" and "deleted". Callbacks running in
parallel with the output let uploads see a file that was then replaced.

**Why it is hard.** The event must fire after the final rename, with the
final name, and before the file can be deleted. A slow synchronous callback
blocks the output; an asynchronous one races the deletion of the same file.

**How to tell.** Binding: for each file there is exactly one "created", then
any "updated", then at most one "deleted", in that order, and a file is
complete and present when "created" or "updated" fires. The deleted event
names the same path as the created one. Incidental: whether temp files
produce events (they must not).

**Evidence.** f7407bec6, #1150, 51342b37e, ea3cbf4af, c161238b2. Recurred 4
times.

### KC14. Directories and permissions

**Situation.** The output directory may not exist, may exist without write
permission, or may become unwritable later. Segment names can create
sub-directories per hour or per day.

**What went wrong.** "Target directory does not exist" for dynamic paths.
Directories were created non-recursively, then created at start but not when
a new segment needed one. Default permissions were too tight for a web
server. An existing but unwritable directory passed the startup check and
failed at the first segment, after clocks had started.

**Why it is hard.** Existence, directory-ness and writability are three
different checks, and the last is only known by trying. Directories are
needed at start, at each segment, and for the persisted state, each from a
different path.

**How to tell.** Binding: an unwritable output or temp directory is rejected
before streaming starts, naming the argument. A segment name with a new
nested sub-directory works mid-stream. Created files and directories get the
configured permissions, up to the umask. Incidental: probe file names.

**Evidence.** #2725 / f2a5b88f3 / b2348c991 / bf314e7a7, #2930 / 750622cd0,
17b0bceca, #5261 / 88201f14d (GH5261, GH2926). Recurred 7 times.

### KC15. Deleting what may already be gone

**Situation.** Old segments, init segments and playlists are deleted on
retention and on stop. Another process may have removed them already, or the
directory may be gone.

**What went wrong.** A missing file on delete killed the output after a few
minutes, and crashed cleanup on shutdown.

**Why it is hard.** Deletion happens on the hot path and on the shutdown
path, and a failure there is not about the stream at all.

**How to tell.** Binding: delete a segment by hand while streaming; the
output logs it and keeps streaming. Remove the whole directory at stop; stop
completes. Incidental: the log level.

**Evidence.** #831 / 4cece6a67, 3fc765b60, 98806341f. Recurred 3 times.

## Persistence & restart

### KC16. Persisted state round trip and schema evolution

**Situation.** On stop, the output writes its state (segments in the window,
counters, pending tags) so the next start continues the same playlists. The
state file outlives the program version that wrote it.

**What went wrong.** The binary state was replaced by JSON so it could be
read and repaired. New fields made older files fail to load; optional fields
needed an "absent" form that older readers accept. A state that could not be
read stopped the output until reading became lenient by default.

**Why it is hard.** Every new piece of per-variant state (grid carry, last
split point, pending tags) is also a schema change. A field added without a
default breaks every deployed state file at upgrade, and the failure only
shows on restart.

**How to tell.** Binding: stop and start with persisted state: the media
sequence, the discontinuity sequence and the window continue, and the next
segment is a discontinuity (KC8). A state file from the previous version
loads, or gives a fresh start with a log line under the normative spec, which does not read that format. A corrupt state file gives a fresh start with a log line, not a stop.
A state written by a crash-interrupted stop is either complete or ignored.
Incidental: field names.

**Evidence.** c7df0ce46, #1561 / 77c2c86d7, adb383d0c, 955ad9ab4, 347d87449,
3d21bd2e6. Recurred 5 times.

### KC17. Stop and start rebuild every variant

**Situation.** A fallible source makes the output stop and start without the
process restarting. Each variant's encoder has ended its stream at stop.

**What went wrong.** A fallible HLS output that was not available at start
failed to start. After a stop, encoders were reused in their ended state. The
encoder's final bytes at stop were lost or written to the wrong segment.

**Why it is hard.** Stop flushes the encoder into the last segment, which is
then closed, and only after that may the variant be rebuilt. A rebuilt
variant must keep its counters (positions, discontinuity count) and drop its
codec state.

**How to tell.** Binding: a source that is unavailable for 2 s every 10 s
gives, after each return, a segment marked discontinuous whose first packet
is a key packet. The last segment before each gap holds the encoder's final
packets. No variant stops producing segments. Incidental: the segment numbers
over the gap.

**Evidence.** 15113fd0e, 12c0a1354, #1985 / 49cbb498d, 744fc5d01. Recurred 4
times.

## ID3, metadata & tags

### KC18. Timed ID3 and per-segment ID3 tags

**Situation.** MPEG-TS variants carry metadata in a timed ID3 stream. Packed
audio variants (ADTS, MP3, AC-3) must start each segment with an ID3 tag that
carries the segment's timestamp, and may carry metadata. New listeners join
at any segment.

**What went wrong.** No metadata in HLS for years, then support only for
some encoders. The packed-audio timestamp tag had an extra byte, so players
read a wrong owner name. The timed ID3 stream blocked an interleaving muxer
and emptied the first segment. Some players showed only the title.

**Why it is hard.** There are two mechanisms (a stream inside the container,
and a tag prepended to the segment), chosen by container. The prepended tag
has a byte-exact format that players parse strictly. The latest metadata has
to be repeated at the start of each segment for late joiners, but only where
asked.

**How to tell.** Binding: in a packed-audio variant, every segment starts
with an ID3 tag whose timestamp frame parses to the segment's start in
90 kHz units. In an MPEG-TS variant, a metadata change appears as a timed ID3
packet at the media time of the change. With replay on, each segment starts
with the current metadata. A variant with ID3 off has none. Incidental: ID3
version, as long as it is the configured one.

**Evidence.** #3153 / f20d0a89e, 118cba9f7 (#3604), #3817 / 16f9beb81,
#4398 / 229719d8a, #3321. Recurred 4 times.

### KC19. Metadata cost and equality

**Situation.** Metadata arrives with every track and sometimes carries a
cover image. The output keeps the latest metadata per variant to replay it.

**What went wrong.** Covers kept in recent metadata grew memory until
out-of-memory. Equal metadata was treated as new (KC6).

**Why it is hard.** Keeping metadata for replay is what makes late joiners
work, and it keeps large values alive. Equality has to compare what is
exported, not the raw list.

**How to tell.** Binding: a playlist of files with 1 MB covers, played for an
hour, keeps memory flat. Equal metadata sent twice produces one tag.
Incidental: whether covers are exported at all, which is configured.

**Evidence.** #3504 / a86be5bcd, 172baa295. Recurred 2 times.

### KC20. Inserted tags

**Situation.** A script inserts a tag into a variant's media playlist at any
time, from any thread, through the output's variants. The tag must go before
the next segment, which starts with it.

**What went wrong.** Inserted tags ended the current segment immediately,
which the grid then had to absorb. Tags pending at stop had to be kept in the
persisted state, or they were lost.

**Why it is hard.** Insertion comes from another thread while the output is
cutting segments. A pending tag is state that lives across a stop.

**How to tell.** Binding: a tag inserted once appears exactly once, before
the first segment opened after the insertion, in that variant only. A tag
pending at stop appears after restart. Incidental: whether the cut for the
tag is taken at the next split point or the next boundary.

**Evidence.** 7750e4939 (hls_custom_tags), 00cad0dbe. Recurred 2 times.

## Encoder seam

### KC21. Init segments

**Situation.** fMP4 (and WebM) variants have an init segment: the container
header, written before media data and named by the media playlist. The
header is only complete after the muxer has seen its first data, and some
muxers seek back to finish it.

**What went wrong.** The first fMP4 init segment was empty. Audio-only fMP4
worked "quite miraculously", and audio + video fMP4 was broken. A
multi-variant fMP4 output crashed after an hour.

**Why it is hard.** The output asks for the init segment at start, and the
encoder only has it after the first flush of real data, which may need
several frames. The bytes produced in the meantime belong to the first media
segment and must not be lost. A new init segment appears on every container
restart (KC22), and the old one must stay on disk while any listed segment
refers to it.

**How to tell.** Binding: in every fMP4 variant the init segment is
non-empty, the first media segment holds the first frame, and the media
playlist names the init segment before the first segment and again after
every discontinuity that changed it. An init segment is deleted once no
retained segment refers to it, and not before. Incidental: init segment
naming.

**Evidence.** 1aa34b8c5, 5fce9106e, 44e8f661e, #5141. Recurred 4 times.

### KC22. Container restarts mid-stream

**Situation.** The encoder starts a new container inside a variant: copied
media whose codec headers change between files, for instance.

**What went wrong.** Codec header changes were rejected or broke the
variant. Re-encoded audio restarted its timeline at zero after a restart.

**Why it is hard.** A restart is a split point, a discontinuity and possibly
a new init segment at once, and the three must land on the same segment. It
happens in the middle of the encoder's output, so the output learns of it
through the encoder and not from its own schedule. Timestamps must keep
increasing across it.

**How to tell.** Binding: copy two files whose H.264 extradata differ into
an fMP4 variant: the segment after the switch is marked discontinuous, is
preceded by the new init segment's URI, and starts with a key packet;
timestamps keep increasing. The grid does not move. Incidental: whether the
segment before the switch is short.

**Evidence.** 4bf0f1d12 (CHANGES "HLS with MP4 segments starts a new segment
after a discontinuity"). Recurred 1 time.

### KC23. HLS asks the encoder for things a file does not

**Situation.** The same encoder value is used for a file, a stream and an
HLS variant. HLS needs global headers (for codec strings), no interleaving
delay (sparse ID3 stream), a header repeated at each segment for some
formats, and knowledge of whether every segment starts on a keyframe.

**What went wrong.** Wrong codec strings until HLS-specific options were
injected into the encoder. Interleaving held back the first segment. The
encoder header, read at the wrong time, was empty. Playlists announced
independent segments that were not.

**Why it is hard.** HLS blurs container and transport: options that FFmpeg's
own HLS muxer sets implicitly have to be set by the output, but only where
the user did not set them. The seam is wide (init, split, properties, ID3),
and each property is valid only after the encoder has seen data.

**How to tell.** Binding: a user-set container option wins over the injected
one. Independent segments are announced only when every segment starts on a
key packet of every stream. Properties are read only after the init step has
completed. Incidental: which options are injected.

**Evidence.** #3483 / 84bf5e8ed, #4401 / 229719d8a, #3265 / 318cba9f7,
00cf31541, 4bf0f1d12. Recurred 5 times.

## Errors & reopen

### KC24. Transient write errors

**Situation.** The output directory fills up, is unmounted, or loses
permission while streaming, and comes back later. Other outputs share the
process.

**What went wrong.** The whole process exited, other outputs included. With
an error handler installed, stop with persisted state tried to close the
failed segment, raised again from shutdown, and still killed the process.

**Why it is hard.** The error happens mid-segment, with a temp file open, the
encoder in the middle of its output, and the playlists naming the last good
segment. Recovery has to drop the partial segment, keep what is published,
and resume with a discontinuity. The stop path runs the same file operations
that just failed.

**How to tell.** Binding: make the directory unwritable, then writable again:
the output resumes within the retry delay, the published playlists name only
existing files throughout, the first segment after recovery is marked
discontinuous and starts with a key packet, and no temp file remains. Other
outputs never stop. Stop during the outage completes. Incidental: the retry
delay.

**Evidence.** #5259 / f0665a127 (GH5259), #5260 / 20ba1c2fb (GH5260).
Recurred 2 times.

## Wrappers & serving

### KC25. Serving the directory over HTTP

**Situation.** The harbor wrapper serves the output directory, possibly
behind a reverse proxy or a CDN that caches responses.

**What went wrong.** No response headers, not even CORS, so browser players
failed. HTTPS needed its own transport argument. The field report adds that
a CDN in front needs segments to be cacheable and playlists not.

**Why it is hard.** Each kind of file needs its own content type (playlist,
MPEG-TS, fMP4 media, fMP4 init, packed audio), and the init segment's
extension differs from its variant's media extension. The served directory
also holds temp files and possibly the persisted state.

**How to tell.** Binding: every file a playlist names is served with its
correct, non-empty content type, init segments included. Playlists and
segments get distinct cache behaviour. Temp files are not served.
Incidental: header names beyond CORS.

**Evidence.** #913, f8e1db8d5, field report 2026-11-05. Recurred 2 times.

## Answered by the normative spec

Each entry's situation, and the rule that handles it.

| Entry | Rule                                                                                                                                                                                                                                                                                                         |
| ----- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| KC1   | [segmenting.md §5](segmenting.md#5-the-target-duration-bound); forced splits and independent segments: [encoder-contract.md §5–6](encoder-contract.md#5-split_encode)                                                                                                                                        |
| KC2   | [segmenting.md §1](segmenting.md#1-time-the-grid-and-the-target-duration): absolute boundaries from one origin. Each run has its own grid, so the state carries none ([persistence.md §1](persistence.md#1-what-is-saved)).                                                                                  |
| KC3   | [segmenting.md §4](segmenting.md#4-the-cut-rule): the split point nearest the boundary within the bound; [§4.2](segmenting.md#42-worked-example).                                                                                                                                                            |
| KC4   | [segmenting.md §1](segmenting.md#1-time-the-grid-and-the-target-duration) (one origin per output) and [§7](segmenting.md#7-output-wide-cuts) (every non-grid cut is output-wide; what alignment is not promised).                                                                                            |
| KC5   | [segmenting.md §6](segmenting.md#6-opening-a-segment): first segment at media time 0, empty segments never published; first tag: [metadata.md §1](metadata.md#1-track-metadata).                                                                                                                             |
| KC6   | [metadata.md §4](metadata.md#4-cuts-on-metadata-and-tags): cuts on metadata and on inserted tags are sync cuts, by default only where every variant is free-split or there is one variant, and for metadata only when some variant carries ID3; equal metadata ignored ([§1](metadata.md#1-track-metadata)). |
| KC7   | [files.md §4–5](files.md#4-closing-a-segment): playlist before deletion, RFC availability; a window that only moves forward and holds at least three target durations: [playlists.md §2](playlists.md#2-media-playlist); numbering: [§2.1](playlists.md#21-media-sequence).                                  |
| KC8   | [playlists.md §2.1](playlists.md#21-media-sequence); discontinuity at every run start: [operator.md §3.1](operator.md#31-start); output-wide: [segmenting.md §7](segmenting.md#7-output-wide-cuts).                                                                                                          |
| KC9   | [variants.md §3](variants.md#3-derived-properties): peak bandwidth with headroom, measured as a last resort; rewritten on change: [playlists.md §3](playlists.md#3-main-playlist).                                                                                                                           |
| KC10  | [playlists.md §4](playlists.md#4-uris-and-prefix): one URI rule for every kind of file; names stay inside the directory: [files.md §2](files.md#2-file-names).                                                                                                                                               |
| KC11  | [playlists.md §3](playlists.md#3-main-playlist): due at every run start; files kept across a stop that is not final, cleanup by tracked name at a final stop: [files.md §7](files.md#7-cleanup-at-a-final-stop).                                                                                             |
| KC12  | [files.md §3](files.md#3-writing-a-file): modes under the umask; device warning: [operator.md §2](operator.md#2-construction).                                                                                                                                                                               |
| KC13  | [files.md §8](files.md#8-file-change-events): one path per file across its events; an open segment at cleanup is aborted, not published.                                                                                                                                                                     |
| KC14  | [operator.md §2](operator.md#2-construction), [files.md §1](files.md#1-directories).                                                                                                                                                                                                                         |
| KC15  | [files.md §5.1](files.md#51-deleting-a-file); stop completes whatever fails: [operator.md §3.2](operator.md#32-stop).                                                                                                                                                                                        |
| KC16  | [persistence.md](persistence.md): atomic save after every segment close; a checked restore that keeps counters and drops only missing segments. The previous format is not read: a strict restore of it fails, a non-strict one starts fresh.                                                                |
| KC17  | [operator.md §3](operator.md#3-lifecycle): new encoders per run; state and files kept across a stop that is not final; per-variant stop steps.                                                                                                                                                               |
| KC18  | [metadata.md §2](metadata.md#2-id3); timestamp from the tick count: [encoder-packed-audio.md §7](encoder-packed-audio.md#7-insert_id3).                                                                                                                                                                      |
| KC19  | [metadata.md §1](metadata.md#1-track-metadata): binary entries are not kept.                                                                                                                                                                                                                                 |
| KC20  | [metadata.md §3](metadata.md#3-inserted-tags): insertion order, validated; cuts under the alignment rule of [§4](metadata.md#4-cuts-on-metadata-and-tags).                                                                                                                                                   |
| KC21  | [files.md §5–6](files.md#5-retention): tracked by name, deleted when unreferenced. Containers other than RFC 8216 segment formats are rejected ([variants.md §2](variants.md#2-building-a-variant)).                                                                                                         |
| KC22  | [segmenting.md §8](segmenting.md#8-container-restart).                                                                                                                                                                                                                                                       |
| KC23  | [encoder-contract.md §6](encoder-contract.md#6-properties); options: the FFmpeg layer's spec.                                                                                                                                                                                                                |
| KC24  | [operator.md §6](operator.md#6-errors-and-reopening): new encoders after an error, so the next segment starts at a key packet.                                                                                                                                                                               |
| KC25  | [serving.md](serving.md): tracked files by lookup, other files after a lexical check, content types by kind, cache headers, configured headers on every response.                                                                                                                                            |

## Code-only notes (not entries)

- The revert pair 9f0faefeb / 782f031bc and the revert fd2797bd5 only moved
  argument order.
- Several "fix typo" and logging commits (268521635, 8c037bdf2) are about
  log output.
- #3678 (an array fill error seen with HLS) was in the mixing operator, not
  the output.
