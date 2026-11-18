# Known complexity

What the history of `cross` shows to be hard, stated as situations a new
implementation will meet again. Entries say nothing about the old code. The
**Count / traces** line of each entry is the only place where issue numbers,
PR numbers and commit hashes appear. The **As built** line reconciles the
entry with this snapshot: covered by a stated rule, not covered (a finding),
or contradicted.

**How the history was measured.** `git log` on the operator and on the
child-clock support gives 180 commits since 2008, the simple and the "smart"
variants included until they merged in 2019. About 45 read as fixes by their
subject; 3 are reverts. I read every subject, the diffs of 20 fix commits,
the descriptions of 13 pull requests (5194, 4851, 4591, 4605, 4892, 5338,
5377, 5379, 5383, 5554, 5563, 5587, 5589), the 60 entries of CHANGES.md that
name `cross` or `crossfade`, and the header comments of the 22 tests of
[tests.md](tests.md).

**Not read.** Issue discussions, except through the pull requests that close
them; GitHub Discussions; anything about autocue's analysis; the history of
`fade.in` and `fade.out`; the user reports behind CHANGES.md entries that
cite no commit (a deadlock "after a long time", audio artefacts in 2.3 and
2.4). Entries resting on a commit subject alone are marked **read only**.

Ordered by how often the difficulty recurred. The switch family's list is
[../switch/known-complexity.md](../switch/known-complexity.md); its entries
on the seam apply to `cross` and are not repeated.

---

## 1. Metadata across a crossing: doubled, late or lost

**Situation.** A track's metadata sits on its first sample. The operator
reads that sample ahead of time, into a buffer it hands to script code. The
transition may play that buffer, mix it with another at the same position,
or drop it. Listeners must learn of each track once, when it is heard.

**What went wrong.** Every variation was seen. Metadata announced twice
because the operator re-injected what the buffer already carried. Two
announcements landing on one sample of a mix, where only one survives, so
the starting track was announced one crossing late or never. A transition
that keeps only the ending track left the starting track audible and
unannounced. Removing duplicates by value silenced every track that repeats
the previous one, a file on a loop above all. An empty metadata announced
for a child that carries none.

**Why it is hard.** The announcement and the audio travel together until
script code separates them, and the operator cannot see what the script kept.
Any repair made by value (drop equal neighbours) cannot tell a duplicate
from a repeat; any repair made by position breaks when two sources share a
position.

**How to tell.** Three tracks, the third repeating the second's metadata,
through each transition shape: mix, sequence, ending only, starting only,
neither. Binding: three announcements, in order, each no earlier than its
audio; with a child carrying no metadata, none. Duplicate removal on and
off.

**Count / traces.** 10. Trac 257 (twice, 2009); b20577db2; 7abe3438c;
#2153; #4892; #5360, #5377, #5383; "before metadata lost when not
conservative" (CHANGES, 1.4).

**As built.** Covered for the shapes tested:
[script-layer.md §1](script-layer.md#1-the-cross-wrapper),
[cross.md §9](cross.md#9-metadata-kept). Not covered: F19, F23, and the
head case of F3.

## 2. The window the fades are computed over

**Situation.** Each track asks for a fade of its own length, by metadata,
relative to its own end or start. The operator holds a window of each track
whose length depends on something else: its duration parameter, the other
track, how much data there was. The fades have to end where the data ends
and the next track has to start where the track said.

**What went wrong.** A fade-out as long as the window never started. A
transition lasted the shorter of two unrelated durations, cutting a 3 s fade
to 1 s. A fade-in vanished when the ending track asked for no fade-out. The
next track started at the start of the window instead of where the ending
track asked, when the window was longer than the fade. Making the fade
parameters follow metadata changes during the fade broke existing fades and
was withdrawn.

**Why it is hard.** Three lengths meet: what each track asks, and what was
actually buffered. Every key that is relative to one of them must be
re-expressed when another one wins, and there are five such keys. Fixing one
moves another.

**How to tell.** For ending fades shorter than, equal to and longer than the
window, with and without a start-next offset, and starting fades longer than
the ending one: the ending fade reaches silence on the last sample of the
ending source; the starting track becomes audible at the offset the ending
track asked, counted in the track; the starting fade is whole. Compare
levels over time, not frames.

**Count / traces.** 8. #1351; #3688, #3716; #3704; #4492 reverted by #4591
(#4563); #4605; #5562/#5563; #5581/#5587; #5589.

**As built.** Covered:
[transition.md §2](transition.md#2-metadata-adjustments),
[cross.md §5](cross.md#the-ending-track-is-over),
[cross.md §7](cross.md#7-building-the-compound). No test asserts the
adjusted values; the plots do, by frame ([tests.md §9](tests.md#9-curves)).

## 3. Being ready across the handovers

**Situation.** The operator's stream is made of three stretches read from
different places: the buffered track, the transition, the track again.
Between two of them there is an instant where the one is over and the next
is not designated yet.

**What went wrong.** The operator answered "not ready" at such an instant
and its reader took it for the end: tracks stopped before their end, the
stream ended after a transition that failed while the child still had data,
a buffer was returned that had nothing yet, a frame was asked again right
after a transition and got the wrong boundaries.

**Why it is hard.** Readiness is asked before the read, and the handover is
discovered by the read. Whatever is ready "in principle" has to be ready in
fact, in the same cycle.

**How to tell.** A long run of short tracks under a transition that
sometimes returns a source that fails at once. Binding: the output never
stops while the child has data; every track is heard.

**Count / traces.** 6. "smart_cross would stop before the end of tracks"
(CHANGES, 1.0); c72361267; 9aac6bed2; 688a811ff; #1170; #4177.

**As built.** Covered: [cross.md §5](cross.md#5-probe) decides and commits
in one act, which is what F12 holds against it. Pass 2 has to keep this
entry true once the two are separated.

## 4. When the transition is over

**Situation.** The transition is an arbitrary source built by script code.
The operator has to know when to go back to the track. The transition may be
a sequence of short items with gaps between them, may end a track of its own
in the middle, may never end.

**What went wrong.** A short frame from the transition was taken for its
end, so transitions made of several items were cut at the first join and
data after a mark inside them was skipped. Going back to the track put a
mark where none belonged. The rest of the starting track was merged or cut
depending on how the transition's own tracks fell.

**Why it is hard.** "Has nothing now", "ended a track" and "will never
deliver again" look the same from a short frame. Only the last one ends a
transition, and a script-built source does not always say it.

**How to tell.** A transition of four items, two of them shorter than a
second, with a gap between items. Binding: every item is heard in full, in
order; no mark where the transition gives way to the track. And a
transition that never ends: see F13.

**Count / traces.** 5. #596; #652, #683; #2247 (AzuraCast 5109); #3729;
e79028b37.

**As built.** Covered for ending:
[transition.md §4](transition.md#4-end-of-the-transition). Not covered: a
transition with no end (F13).

## 5. The crossing begins one track, on the right sample

**Situation.** A crossing is where the next track starts for the listener.
The child's own mark for that track is inside a buffer given to script code,
where a mix usually loses it. The operator changes what it reads twice per
crossing; only one of the two is a track start.

**What went wrong.** No track began at all after the first one, so nothing
registered downstream fired again. Or two began per crossing. Or one began
where the transition gave way to the track. Which of these happened depended
on whether the change fell inside a frame or on its edge.

**Why it is hard.** The mark has to be put by the operator, not relayed,
and at a join the operator makes at a frame edge every time, unlike its
siblings whose joins fall mid-frame.

**How to tell.** Count track events over several crossings with a mix
transition: one per child track, each with that track's metadata, at the
first sample where the track is audible. Repeat with the crossing forced
onto a frame edge and off it.

**Count / traces.** 4. 9aac6bed2; #2247 (shared with 4); #5379; #5360.

**As built.** Covered in steady state: [cross.md §8](cross.md#8-delivery).
Not covered: F3, F6, F14.

## 6. Tracks shorter than the window

**Situation.** The starting track ends while its beginning is still being
buffered; or the ending track was never longer than the window.

**What went wrong.** The starting track's buffer was left open-ended, so its
length was unknown and the transition misjudged it. Buffering stopped at a
track boundary though the child still had data. Fades were computed over
more than one of the tracks had.

**Why it is hard.** The window is sized before the track's length is known,
and a track can be shorter than any window. One side then has less than the
other, and whatever is left over on the long side still has to be played,
before or after the transition.

**How to tell.** Tracks of 0.5, 1 and 3 times the duration in every order.
Binding: every sample of every track is heard exactly once outside the
overlaps; both sources handed to the transition report their true length;
each track begins once. A track exactly as long as the window behaves as one
a frame longer.

**Count / traces.** 5. a5cd73bd0; #1074; e79028b37; #3688; "workaround for
short tracks" with autocue (CHANGES, 2.3).

**As built.** Covered: [cross.md §7](cross.md#7-building-the-compound).
Not covered: F3 (the mark with a head), F17 (the exact length), F6.

## 7. The duration: invalid, zero, changing

**Situation.** The duration comes from a function and from metadata. It can
be negative, zero, shorter than a frame, change while a track is buffered,
and each track may carry its own.

**What went wrong.** A negative duration looped for ever. Zero was first
refused, then allowed, then given a workaround, then silently turned into
one frame. The function was read once and never again. An override stuck to
every later track, then was reset by default, with an option to keep it. A
track's override sized the window taken from its start, when it should
govern its end.

**Why it is hard.** One number is used for three things: how far to read
ahead, how much of the ending track to hand over, how much of the starting
one. It changes at a moment (the starting track's first sample) that sits in
the middle of the computation that uses it.

**How to tell.** A track's override governs the crossing at that track's
end and no other, for tracks shorter than, equal to and longer than it, with
persistence on and off. A duration function is read again for every track.
Negative, zero and sub-frame values terminate and cross over one frame.

**Count / traces.** 9. a84a8e3b7, bd798309b; ac14c1c66; 27436404e; #2287;
#2488/#2499; 31a2dba2d; ba356e83c; #5587 (shared with 2).

**As built.** Covered: [cross.md §3](cross.md#3-cross-duration). Not
covered: F4, F10, F17.

## 8. Which clock the pieces live in

**Situation.** The child runs in a clock the operator ticks at its own pace.
The buffers and the transition are created while streaming and are read at
the pace of the operator's reader.

**What went wrong.** Sources created for a transition were assigned to the
wrong clock: the default one, in the middle of their creation; or the
child's, for years, where they were ticked at the wrong rate and returned
stale cached frames, heard as repeated or missing audio. A child also read
from outside the operator made a clock depend on itself. Creating the
operator while another thread started clocks failed with a clock conflict.

**Why it is hard.** There are two rates in one operator, and every source it
builds belongs to exactly one of them. A source created at run time has no
clock until someone says so, and a wrong answer does not fail: it glitches.

**How to tell.** With a paced reader and a child read ahead, the clock
listing shows the child and nothing else in the child clock; the buffers and
the transition in the reader's. The sample check of entry 10 passes. A child
also mixed with the operator's output is refused at start. Creating many
such operators while clocks start never fails.

**Count / traces.** 7. d951bdc1f; f859083b0; 44d395b2c, 46b44b3ed; #3318
(read only); #4851; #5548/#5554.

**As built.** Covered: [child-clock.md §1](child-clock.md#1-parts),
[cross.md §2](cross.md#2-state),
[cross.md §7](cross.md#7-building-the-compound) step 9.

## 9. What the script leaves behind

**Situation.** The transition function runs at every crossing. It builds
sources and registers callbacks on the sources it is given and on the child,
which outlives the crossing.

**What went wrong.** Each crossing left sources and callbacks behind:
resources grew until the stream fell behind, seen as a CPU climb rather than
a memory alarm. A transition source was cleaned up while it still had data
to play.

**Why it is hard.** The operator owns what it built and not what the script
built, yet only the operator knows when the crossing is over.

**How to tell.** Over many crossings with a transition that registers on the
child: the number of callbacks on the child, read inside the transition,
stays bounded, and is at least one. Live sources do not grow.

**Count / traces.** 4. "resources accumulation leading to catchup"
(CHANGES, 2.2, read only); d7ee9334b; #2247 (shared with 4); #5335/#5338.

**As built.** Covered: [transition.md §5](transition.md#5-callbacks). Not
covered: F11 (sleep).

## 10. Results that depend on where frames are cut

**Situation.** Tracks do not end on frame edges. The operator reads its
child a frame at a time and decides on what a frame looks like: short, or
starting with a mark.

**What went wrong.** A first frame lost when a fade-in was zero. The first
piece of a starting track rejected when both sources of a sequence
transition had data in the same frame. Marks handled one way inside a frame
and another on its edge. A check that compares frames, not samples, moving
whenever anything upstream changed where frames fall.

**Why it is hard.** A short frame carries two meanings (a boundary, or
nothing more for now), and position zero of a frame is both "the start of a
track" and "where the previous read stopped".

**How to tell.** Run the same script with track lengths that are and are not
multiples of the frame, and with two frame sizes. Binding: the same samples,
the same number of transitions, the same track events at the same sample
positions.

**Count / traces.** 5. #4887; #5379 (shared with 5);
`cross-sequence-transition`; #4828; `cross-override` asking for an extra
frame.

**As built.** Not covered: F2, F17, F18, F21, F25. This entry has no stated
rule today.

## 11. An abort from another thread

**Situation.** A skip is requested from a thread that is not streaming,
while the streaming thread is inside the operator, possibly many times a
second.

**What went wrong.** The abort rebuilt the operator's state on the caller's
thread and the child failed an internal check.

**Why it is hard.** The abort has to reach the child, and to change what the
operator reads, without either happening under a cycle in progress.

**How to tell.** A thousand skips a millisecond apart on a child that loads
a new item per track: no failure. And what that check never looked at: after
each skip nothing of the skipped track is heard.

**Count / traces.** 2. #5148/#5149, #5194.

**As built.** Covered for safety: [cross.md §10](cross.md#10-abort). Not
covered for what plays: F1. The abort still reaches the child at once, from
the caller's thread.

## 12. What the transition returns must fit

**Situation.** The transition function is generic script code; its result
has to produce exactly what the operator produces: same channels, same
fields.

**What went wrong.** A generic function typed for any content returned a
source of another shape, failing at the first crossing, long after start.

**Why it is hard.** The function is typed once and called at run time, on
sources that did not exist when it was typed.

**How to tell.** A mono operator with a transition written without type
annotations runs through a crossing.

**Count / traces.** 4. 22a66cbd1; #1603; 96c1f69f4; #3239.

**As built.** Covered: [transition.md §3](transition.md#3-the-result).

## 13. A child that paces itself

**Situation.** The child is, or contains, something that controls its own
timing: a live input, a soundcard.

**What went wrong.** Reading it ahead cannot work: it delivers at its own
rate. The failure showed as drift and glitches, not as an error.

**Why it is hard.** Whether a child paces itself can depend on what it is
playing at the moment, which is not known at creation.

**How to tell.** A self-paced input, alone and inside a fallback, a sequence
and a mix, is refused at creation.

**Count / traces.** 3. #1705; fa52a0deb; 403061834.

**As built.** Covered: [child-clock.md §6](child-clock.md#6-creation-check).
Not covered: what is reported afterwards (F16).

## 14. Remaining time

**Situation.** Operators above ask how long the current track still has.
The answer comes from different places in each phase.

**What went wrong.** The buffered end of a track announced an unknown
remaining time for ever; during a transition the answer ignored the
transition.

**Why it is hard.** In the buffered phase the track's remaining time is part
buffer, part child. During the transition the listener's track is the
starting one, of which the operator holds a piece and the child the rest.

**How to tell.** Remaining time decreases by what was delivered, cycle after
cycle, through a whole crossing, and reaches zero where the track event of
the next crossing fires.

**Count / traces.** 3. a5cd73bd0 (shared with 6); c72361267; 688a811ff.

**As built.** Covered in `before`:
[cross.md §11](cross.md#11-other-properties). Not covered in `after`: F22.
