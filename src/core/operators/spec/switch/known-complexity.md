# Known complexity

What the history of the switch family shows to be hard, stated as situations a
new implementation will meet again. Entries say nothing about the old code. The
**Count / traces** line of each entry is the only place where issue numbers, PR
numbers and commit hashes appear.

**How the history was measured.** `git log --follow` on the switch operator
gives 162 commits since 2008; 10 carry "fix" in their subject and 2 are reverts
(plus 2 reverts of activation changes made through it and 1 revert of the
frame-assembly seam). The stdlib wrappers (`fallback`, `rotate`, `random`) have
38 commits. A GitHub title search for switch, fallback, rotate, random, jingle
and transition returns 152 distinct issues and PRs. Of these I read the body and
maintainer replies of about 45, the full discussion of the per-source
composition PR (about 50 comments from users testing nested fallbacks, delayed
jingles, a blank last-resort source and metadata replay), the bodies of about 30
fix PRs, and the diffs of 12 fix commits. I mined the header comments of the
14 `switch_*` regression tests and the 14 `GHnnnn` regression tests that use a
switch, and the CHANGES.md entries that name a switch operator.

**Not read.** Issues before 2019 except where a commit cites them (trac
numbers); the ~105 matching issues whose title was the only part read (most are
support questions); AzuraCast-generated scripts attached to issues; the crossfade
operator's own history beyond where it shares the frame-assembly seam. Items
marked **read only** rest on an issue title or report without a confirmed fix
or reproduction.

Entries are ordered by how often the difficulty recurred.

---

## 1. Metadata at a handoff: replayed, stale, doubled or lost

**Situation.** When a switch selects a child mid-track, listeners must still
learn what is playing, so the switch replays the child's latest metadata at the
start of the first frame it takes from it. That "latest metadata" is a snapshot
held by the child. It can be invalid by the time it is replayed: the child's
track was skipped or ended while it was not selected; the child is shared and
its track moved on elsewhere; the child's first frame already carries its own
metadata; a nested switch has replayed it already; a transition sums the leaving
and entering sources and both bring metadata to the same position; the leaving
source's metadata reaches the output through the transition. The entering
source can also be a live source whose first frame, with its metadata, is lost
when the handoff does not read it right away.

**What went wrong.** "Unknown" title on the first selection of a live input
behind two fallbacks. The previous jingle's title announced over the next
jingle in a delayed-jingle autodj. The title of a track that was skipped while
unselected announced when its source came back. Two `on_metadata` calls for one
track. Metadata replayed with replay turned off. A track announced one
transition late, or never announced, behind an add-shaped transition. Empty
metadata announced. A live source's inserted title never reaching the output.
Telnet "now playing" stalling until the next track after a switch.

**Why it is hard.** Metadata belongs to a position in a stream, but the replay
works from a value detached from any position. Whether that value is still true
depends on what the child did while nobody was watching, which the switch does
not see. Every layer that composes sources (nested switch, transition, wrapper)
is tempted to replay too. Deduplicating by value cannot tell a duplicate from a
track that repeats the previous one. What makes a replay correct is whether
anything was announced since the last track mark, not what was announced.

**How to tell.** For every track that becomes audible at the output, exactly
one announcement arrives, at the position where that track's audio starts,
carrying that track's own metadata. Check it for: first selection; re-selection
mid-track; re-selection after the child's track was skipped or ended while
unselected; a switch nested two and three deep; a transition that keeps only
the leaving side, only the entering side, or both; the same track played twice
in a row; an entering source whose first frame carries its own metadata; a live
entering source with an inserted title. With replay disabled on a child, no
replayed metadata comes from it, ever.

**Count / traces.** 12 fixes, 14 issues or tester reports.
#208 8b36c9449; #2109 208877af4; #2138 9305b48f6; #1476; #3373 (open, read
only); #3123 (open, read only); #3943; #4858; #4803 03bec767d; #4892 f49c071f0;
#5077 c48abb653 (tests switch_autodj_jingle_metadata,
switch_per_source_replay_metadata, switch_skipped_track_metadata); #5360 #5377;
#5383 6557ef171; #5012 #5633 daef21ea7; f3e996beb (not on this branch).

---

## 2. Decisions inferred from whether, or when, a child is pulled

**Situation.** Some state changes have no explicit event and were inferred from
pull patterns instead. A switch decides that it was left and must "resume"
because it was not animated for more than a cycle, and then drops a
track-sensitive selection. A switch inside a passive child clock is animated on
every cycle but read on only some of them. An outer switch leaves an inner
switch for a long time and comes back. A child with a buffer keeps filling it on
every cycle but is drained only when read. A leaving source is told to skip its
track, and the skip takes effect only the next time it is read. A live source
keeps producing whether it is read or not. A delayed source measures its delay
through its own reads. A "keep running when not used" processor behaves exactly
like the passive one.

**What went wrong.** Under a time-stretching or encoding child clock, `rotate`
ignored its weights and cut every jingle after one or two frames. In the
opposite direction, before the resume rule existed: a nested autodj fallback
kept a stale music selection after a long program instead of playing the
delayed jingle that had become ready. With the rule: an outer switch returned to
an inner `rotate` mid-way and the jingle was cut after one frame. An unselected
buffered child grew memory by 60 to 100 MB a minute until restart. The first
second of a live input was dropped along with its track mark and metadata. The
last frame of a skipped track was heard when its source came back, and a
request queue looked frozen in its old state while unselected. A delayed source
got stuck on its first track, or was not available at startup.

**Why it is hard.** Pull patterns are a by-product of every consumer's own
policy: passive child clocks, read-ahead, buffers, nested switches, transitions.
A rule that reads meaning into them ("not pulled for a while means I was left")
holds for the shape it was written for and breaks for the next consumer. The
party that knows the state changed (the parent that preempted the child) is not
the party that guesses it. The maintainer's principle: any logic based on
waiting for a data pull is prone to this kind of issue, and explicit signalling
is always better. The parent tells the child it was left; the child never
deduces it.

**How to tell.** For every state change a child undergoes because it was left
or resumed (keeping or dropping its selection, skipping, clearing its latest
metadata, counting a play), name the explicit event that causes it: a parent's
leave notification, an abort of the track, or a track boundary. No such state
change may depend on tick gaps or read counts. Then run the same script under
three consumer shapes: output directly, a passive child clock read every other
cycle, and an outer switch that leaves and returns. All three must give the same
track sequence and the same duration for each track. A child left unselected for
any length of time holds bounded memory. A source that produces whether or not
it is read loses nothing at the moment it is selected. A skip applied on leave
is complete before the next selection of that source is decided.

**Count / traces.** 11 fixes, 10 issues. Entry 3 and #5594 are the same family.
#1327 9a3e7f84b; #5077 c48abb653 (switch_reselect_after_idle); #5348 #5349
bc7d5f8e3 (GH5348); #5594 (open); #5287 #5295 b680cf284; #5012 #5633 daef21ea7;
#4281 #4289 a9e7cc49a; #5282 5d0e50352; #5283 335055c18; #5291 d106f5d55; #5294
5cea036d4; #4882 #4884 35684fd75; #5074 (skip only on return); #940.

---

## 3. Read-ahead and readiness probes with side effects

**Situation.** To decide whether it can keep filling a frame after a child's
track ends mid-frame, or whether a child is ready, a switch asks the child. When
the child is itself a switch, answering makes it choose a selection, advance its
policy state, fire its track callbacks, and produce the first chunk of the
chosen child. The asking switch can then decide the opposite, for example
preempt the branch for an insert. The produced chunk is cached or dropped, and
the side effects stay. A worked instance: outer switch, then inner
`rotate([jingle, music weight 3])`. Music track 3 ends mid-frame. The inner
produces its whole frame for the cycle, so it crosses its own boundary: it
selects and counts the jingle and emits its first chunk. The outer takes the
frame up to the boundary and moves to an insert. When the
outer returns, the inner treats itself as resuming, re-runs its policy
mid-track with the jingle's weight already used, and cuts the jingle after one
frame at a claimed track boundary.

**What went wrong.** `rotate` counted a play for a track that a safety fallback
played instead, and its weights drifted. Behind a nested switch, a jingle was
announced and then skipped. Metadata produced during the selection phase was
replayed with replay disabled. `on_track` fired for a fallback child 15 s before
that child played. A `predicate.once` never played in a non-track-sensitive
switch (read only).

**Why it is hard.** "Can you produce?" is answered by starting to produce. In a
tree, every level probes the level below. The consequences of a probe land in
state owned by another level (counters, caches, callbacks), and show only when
the outer branch goes away and comes back, which ordinary tests rarely do.

**How to tell.** Probing a child's readiness any number of times in a cycle
has the same observable effect as probing once or not at all. Policy counters,
track and metadata callbacks, and predicate side effects fire only for data
that reaches the output. Specific test: an outer switch over an inner `rotate`,
with the outer leaving for another child exactly when the inner's track ends
mid-frame. On return, the track that plays is the one the policy designates
from the tracks that reached the output, it plays in full, no fragment of
another track precedes it, and the inner's play count equals the number of its
tracks that reached the output. The same shape with a priority policy inside:
on return, a child that became available meanwhile plays first.

**Count / traces.** 2 fixes, 5 issues.
#1327 9a3e7f84b (GH1327); #1476; #2513; #3763 (read only); #5594 (open).

---

## 4. Track boundaries and track marks across a handoff

**Situation.** A handoff can happen mid-frame or exactly at a frame boundary.
The entering source may or may not carry a track mark of its own. The default
transition, or an add-shaped transition, can absorb marks. A child that
crossfades its own tracks never shows a boundary. The policy can return the
same child it just left. A child can come back into the same frame after
another child (s1, s2, s1). A track-sensitive selection whose predicate turns
false mid-track must still be kept.

**What went wrong.** The default transition ate track marks, so the switch saw
one track per selection and ignored its schedule. `on_track` after a crossfade
fired once at startup and never again. A spurious empty track appeared before a
live source. A track-sensitive switch left a child mid-track because its
predicate changed. A switch over a crossfaded child never re-evaluated at track
end. A transition to a short track cut the track after next. Frame assembly
broke when one child appeared twice in a frame with another child between.

**Why it is hard.** A track is defined by marks that every intermediate
operator can create, move, drop or merge. The switch both reads marks (to
decide when it may change) and writes them (to announce a change). The
mid-frame and frame-boundary cases take different paths and drift apart. A mark
on an empty segment announces a track that has no audio.

**How to tell.** Every change of the producing source puts exactly one track
mark where the new source's data starts, at a frame boundary and mid-frame
alike, and never on an empty segment. Tracks counted at the output equal the
number of handoffs plus the children's own internal marks. A track-sensitive
selection is kept until its child's track boundary whatever the predicates say.
A child whose track ends mid-frame is followed within the same frame. No child
appears twice, non-contiguously, in one frame.

**Count / traces.** 9 fixes, 1 revert, 6 issues.
#1999 b4d53d129; #3645 85e2a31e1; #3654 bcbb74c81; #4102 #4108 59de576ec; #4786
2b3aa2ea7; 3c089a3f0; #5083 e30598ef2 reverted f998363bc; #5379 1f1fda60a;
#5633 #5640 275d242b6; #3283; #2026; #3731; #1366 (read only).

---

## 5. When is a handoff over? Leaving-source lifetime and leave notification

**Situation.** A transition is user code that returns arbitrary sources built
around the leaving and entering sources. The switch cannot see inside it to
know when the leaving source stops being used. It still has to tell the
leaving source that it was left, with whether it ended at a boundary or was cut,
then release the transition and allow that source to be selected again. Handoffs
can chain: a second handoff can start before the first transition ends. A
handoff at a track boundary may build no transition at all. Script functions run
at every handoff can register callbacks on the sources they are given.

**What went wrong.** After a stream stopped, the output faded to silence while
files kept playing underneath. A preempted playlist resumed mid-track instead of
skipping, because the leave notification never fired: it was attached to a
transition that a boundary handoff never built. A chained handoff overwrote the
first source's leave notification. A wrapper in the transition kept relaying
the leaving source. The leaving source's next track leaked in under the fade. A
naturally finishing leaving track was reported as cut, so its next track was
skipped (open known limit). "Possible source leak" warnings, then a halted
stream, after many scheduled switches. CPU climbing with each switch from
callbacks piling up. Two attempts at weak activation tracking were reverted
for introducing new leaks.

**Why it is hard.** The end of a transition is a structural fact (nothing still
reads the leaving source), not an event the switch emits. Ownership of what a
transition created spans user code. Every handoff is a fresh allocation in a
long-running process, so a small per-handoff leak becomes an outage over days.

**How to tell.** Each handoff produces exactly one leave notification for the
leaving source, with a correct boundary-or-cut flag. That holds for chained
handoffs and for boundary handoffs that build no transition. Once the
transition's leaving side has run out, the leaving source is no longer read
through it. After N handoffs, the number of live sources and registered
callbacks is the same as after one. The leaving source's next track never plays
during a fade-out of its current one.

**Count / traces.** 9 fixes, 3 reverts, 5 issues.
transition_length (1.x CHANGES); #208 8b36c9449; #5077 c48abb653
(switch_on_leave_chained, switch_on_leave_track_boundary,
switch_per_source_no_reselect_during_fade); #4796 #4802 89d92a298 reverted
9815610a0; #4804 35660b4f7 reverted 16c171ea3; #4808 344974cc7; #5338
aa8183484; #5153 6906e2a70; #5633 daef21ea7 #5640 275d242b6; #940.

---

## 6. Readiness promised against data delivered

**Situation.** A switch answers "ready" at one moment and is asked for data at
another. In between, its selection can change, a transition can run out,
predicates can change, or a child's clock may not have started. Its claim to be
infallible depends on transitions returning infallible sources, which only user
code decides. A last-resort source (blank) can be ready but never produce a
track boundary.

**What went wrong.** Crashes with "source called while not ready" or a source
unavailable mid-stream. "Not able to reselect a source". A stream that never
started when a fallback's last source was blank. A fallback ending on blank
reported as fallible. A buffered fallible input breaking its switch with
invalid content (read only).

**Why it is hard.** Readiness is a promise about the next frame made by a party
that does not control it alone. Caching the answer makes it stale. Recomputing
it at delivery can give a different answer, and the caller has already acted on
the first one.

**How to tell.** In every cycle where the switch reported ready, it delivers a
non-empty frame from a source that was ready in that cycle. The source used for
delivery is the one decided when readiness was answered. A switch whose last
child is always ready and always eligible stays ready at every cycle across
complete transitions in both directions, including the cycle where a transition
ends. A source never reports ready before its clock has started.

**Count / traces.** 6 fixes, 5 issues.
trac #403 3866cf8f3; LS-460 e0b832531; 76dcb23b8; #4420 3c047939d; #4163 #4164
f2117c1e8; #4496 2c38b4475; #5073; #5077 (blank last source,
switch_infallible_across_transition); #1584 (read only); #3644.

---

## 7. Stateful selection policies: weights, rounds, and how often predicates run

**Situation.** `rotate` and `random` are written as predicates that the switch
evaluates "when it needs to". The predicates keep state (position in the round,
plays per child, the next pick). How often and when they are evaluated is a
property of the switch's algorithm, which the policy author cannot see. Children
can be unavailable when their turn comes. Scripts can change what predicates
read while a cycle runs.

**What went wrong.** The first round ignored weights. Weights behaved as
`min(2, weight + 1)`. A short jingle played twice in one round (read only). After
an unavailable child, the round restarted at the wrong position. `random` with
large weights picked again only every N tracks. Selection was inconsistent on
the first evaluation of a round. CPU use went up in a new release because every
predicate ran on every cycle. Half-done script changes were visible to the
selection.

**Why it is hard.** A stateful predicate's result depends on how many times it
has been called. That count is not part of any contract, so any change to the
switch's evaluation strategy silently changes the policy.

**How to tell.** Predicates are evaluated only when a selection is made. Count
the calls during one long track: the number stays constant whatever the track's
length. All predicates for one decision read one consistent snapshot. Policy
state advances only on track boundaries that reached the output. The long-run
ratio of plays equals the weights from the first round on, when unavailable
children are skipped, and when the policy is nested under a switch that preempts
and resumes it.

**Count / traces.** 7 fixes, 9 issues.
#1129 568183780; #1279; #1327 9a3e7f84b; #1468 8b156278c; #1696; #1651 (open,
read only); #2300 4d351c1fe; #5359 cdc894e3c; #5585 #5588 cb8ba5d32
(switch_predicates_on_selection); #3763 (read only); #5594 (open).

---

## 8. Synchronisation role across a selection change

**Situation.** Whether a switch is driven by a hardware or network clock comes
from the child that is producing. That changes on every selection and during a
transition, which is user code that may contain other synchronised sources. The
clock needs the answer at startup (to detect conflicts) and on every cycle (to
decide whether to sleep).

**What went wrong.** Early sync checks failed, or reported conflicts that were
not there. A per-frame auto-sync design was merged and reverted. Sync errors
were unclear. Recomputing the sync role by walking the graph on every tick cost
CPU. A notification-based replacement then leaked callbacks and kept stopped
clocks alive. Some selection paths forgot to notify.

**Why it is hard.** The value is cheap to define and expensive to keep current.
A cache needs a notification from every path that changes what is producing,
and transitions add paths that only user code knows about.

**How to tell.** After every selection change, into and out of a transition,
the clock's view of the switch's sync source equals that of the source actually
producing. The cost per cycle does not grow with the number of children.
Nothing registered for a selection change outlives the switch.

**Count / traces.** 7 fixes, 1 revert, 2 issues.
#1711 65bc80b8d; #1725 1c7a1c823; #1012 reverted 5e8951ad7; #3879 2ba67aba6;
f4ebeba5b; #5133 #5153 6906e2a70; 70c75575e; #5505 a0c56dc00.

---

## 9. Content type of sources built at handoff time

**Situation.** Transitions and handoff wrappers create sources while the stream
runs, long after the script was type-checked. Their content type has to be tied
to the switch's. A user transition can be typed generically while its body is
narrower, for example a fade that only works on PCM. Encoded (copied) content
cannot be faded or mixed. A wrapper the switch adds around a child is visible to
user transition code.

**What went wrong.** A crash (index out of bounds) in a switch with
transitions. A type error when a transition inserted a sequence. A switch over
copied audio and video failed on its first selection with the default handoff.
Transition code that branched on `source.id` received a wrapper's id instead of
the child's.

**Why it is hard.** Typing is static and handoff sources are dynamic. The check
that gives a fresh source its type can reject it before the user's code has had
a chance to branch on the content kind.

**How to tell.** Every source built at a handoff has a content type equal to
the switch's before it produces any data. The default handoff selects and
switches copied content without decoding and without error. User transition
code receives the child the user named, not an internal wrapper. Mid-track cuts
of non-PCM content follow a documented rule.

**Count / traces.** 5 fixes, 4 issues.
#3239 #3240 bc3b17792; 96c1f69f4; 55af07ddd; #4904; #5608 1d45dffbd
(switch_copy_default_on_select); #2333 #3329 (CHANGES).

---

## 10. Whose rule governs a handoff

**Situation.** A handoff has two parties with opposite needs. A live input may
cut into music, while a jingle playlist may not be cut. A playlist cut mid-track
should start a fresh track when it comes back, but a live source has nothing to
resume. A last-resort source (blank) has no track marks, so a rule that waits
for a boundary never lets go of it. A single per-operator setting cannot express
these differences.

**What went wrong.** Users could not say "live may interrupt the song, jingles
may not". Transitions out of a live source had no data to work with once it was
gone. Fallback transitions did not work as expected (several open reports). In
the per-source redesign, a blank last source stopped the stream from starting
until it was tagged as live. That came from a profile rule deciding both how to
switch to a source and how to switch away from it.

**Why it is hard.** Each decision has a natural owner, either the leaving or
the entering source, and putting it on the wrong one looks right for the common
case: a live source taking over from a file. The asymmetry shows only for
blank, nested switches and two files.

**How to tell.** For each pair of leaving and entering profiles, write down
whether the switch cuts or waits, whether it fades, and whether the leaving
track is skipped. Test every pair. A last-resort silence child is left as soon
as a higher-priority child is ready, without any extra tag. A handoff out of a
live source never needs data after that source is gone.

**Count / traces.** 1 redesign, 1 follow-up, 6 issues.
#5074; #5077 c48abb653; #5633 daef21ea7; #2026; #4179 (open); #1547 (open); #719
(open, read only); #5073.

---

## 11. Script changes and other threads during a streaming cycle

**Situation.** Predicates and the selected child are read during a streaming
cycle. Scripts change what predicates read from other threads (server
commands, timers). A skip or abort can come from a handler thread while the
cycle runs.

**What went wrong.** The selection saw a script's change half done. A skip from
a handler thread raised an assertion inside a transition's source (read only for
switch: reported on the crossfade operator).

**Why it is hard.** The streaming cycle is single-threaded by design, but its
inputs are not, and a race shows up rarely and far from its cause.

**How to tell.** Everything a selection reads in a cycle comes from one
consistent snapshot. Skips and aborts from other threads take effect at a cycle
boundary, never inside one.

**Count / traces.** 3 fixes. ff1206ed1; #5359 cdc894e3c; #5148 #5194 (crossfade,
read only for switch).
