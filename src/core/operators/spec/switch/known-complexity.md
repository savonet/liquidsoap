# Known complexity

What the history of the switch family shows to be hard, stated as situations a
new implementation will meet again. Entries say nothing about the old code. The
**Count / traces** line of each entry is the only place where issue numbers, PR
numbers and commit hashes appear. The **Rule** line of each entry names
the rule of this specification that answers it.

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

**Extension to the frame-assembly seam (entries 12 to 18).** The seam is the
code that fills one frame from several sources in turn; `switch`, `sequence`,
`source.dynamic` and `cross` share it. It was created with the streaming model
of December 2023 and has 16 commits of its own since; the file that holds it
has 107 in the same period. `git log --follow` gives 94 commits on `sequence`
since 2008 and 97 on `source.dynamic` since 2010. Pickaxe searches on the
seam's vocabulary and title searches for sequence, source.dynamic, append,
prepend, once, rotate.merge, merge_tracks and the seam's assertion messages
returned about 150 issues and PRs, many of them unrelated. I read the body and
maintainer replies of about 55, and the diffs of about 30 commits, including
11 of the 16 on the seam itself. For `cross` I read only the commits and issues about which source it
designates and how its joins are marked. I mined 13 regression and stream
tests (GH3645, GH4159, GH5019, GH5148, GH5361, GH5389, GH5514, LS354, append,
append-merge, append-merge2, cross-sequence-transition, cross_on_track) and the
CHANGES.md entries naming these operators.

**Not read.** Issues before 2019 except where a commit cites them (trac
numbers); the ~105 matching issues whose title was the only part read (most are
support questions); AzuraCast-generated scripts attached to issues; the crossfade
operator's own history beyond where it shares the frame-assembly seam (its
analysis, buffering and autocue); GitHub Discussions; `append` and `prepend`
before they became scripted operators in 2023, beyond commit subjects. Items
marked **read only** rest on an issue title or report without a confirmed fix
or reproduction.

Entries 1 to 11 (the switch family) are ordered by how often the difficulty
recurred. Entries 12 to 18 (the seam) are ordered the same way among
themselves. A fix that belongs to two entries is listed in both and marked
"shared".

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

**Rule.** Replay inserts the entering source's last metadata on its first sample and loses to metadata already there ([composition.md §4](composition.md#4-default-handoff)). A source's last metadata is cleared when its track ends or is aborted, and a source that produces unread keeps its track start and metadata for its first read ([source-contract.md §4](source-contract.md#4-metadata)). One presented source per selection, entered with one mark ([switch.md §5](switch.md#5-handoff), [switch.md §6](switch.md#6-presented-sources)).

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

**Rule.** No state is derived from probes, requests or ticks ([source-contract.md §9](source-contract.md#9-no-inference)). A selector commits only when its reader takes data past the decision point, so one left at a boundary has nothing to undo ([frame-assembly.md §3](frame-assembly.md#3-commit)). A reader that cuts into a source aborts it, and an aborted selector decides anew ([composition.md §2](composition.md#2-profiles), [switch.md §8](switch.md#8-abort)). Unread data is bounded and an abort discards it before the next decision ([source-contract.md §5](source-contract.md#5-consumption), [source-contract.md §6](source-contract.md#6-abort)).

**Count / traces.** 11 fixes, 10 issues. Entry 3 and #5594 are the same family.
Entries 15 (an item's end guessed from what it returns) and 12 (last use
guessed from reachability) are the same difficulty at the seam.
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

**Rule.** Probes have no observable effect ([source-contract.md §2](source-contract.md#2-probes)). Decisions change nothing and are committed by a request ([frame-assembly.md §2](frame-assembly.md#2-decisions), [frame-assembly.md §3](frame-assembly.md#3-commit)). Predicates change nothing; policy state advances on the track event, which fires for a track being played ([policies.md §6](policies.md#6-predicate-contract), [rotate.md §4](rotate.md#4-events)).

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
false mid-track must still be kept. A crossing changes source twice, into the
transition and out of it to what follows, and only the first change is a
content boundary.

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
on an empty segment announces a track that has no audio. Whether a join begins
a track is known to the operator that makes the join, not to the code that
assembles the frame: a generic rule ("a change of source begins a track") is
wrong for the second join of a crossing. Whether a source's very first frame
begins a track was left without a convention when marks became explicit, and
operators that used to start on a mark (fades) were switched to start at once.

**How to tell.** Every change of the producing source puts exactly one track
mark where the new source's data starts, at a frame boundary and mid-frame
alike, and never on an empty segment. Tracks counted at the output equal the
number of handoffs plus the children's own internal marks. A track-sensitive
selection is kept until its child's track boundary whatever the predicates say.
A child whose track ends mid-frame is followed within the same frame. No child
appears twice, non-contiguously, in one frame. A crossing puts one mark, where
the transition starts, and none where it ends. The first frame of a stream
either always or never begins a track, and the choice is written down.

A join that is asked to merge (no boundary at all) is entry 16. A frame that
holds several boundaries is entry 18. A source designated twice in one frame is
entry 14.

**Rule.** The user states at the end of the source being left whether its own track ends, and that statement fixes the entry of what follows, inside a frame and on a frame edge alike ([frame-assembly.md §5](frame-assembly.md#5-segments), [frame-assembly.md §6](frame-assembly.md#6-joins)). One contiguous span per source and per frame, no mark on an empty segment ([frame-assembly.md §7](frame-assembly.md#7-bounds)). A track-sensitive selection is kept to its track end ([switch.md §4](switch.md#4-decisions)). A crossing has one mark ([cross-seam.md §3](cross-seam.md#3-seam-answers)). Where a stream's first sample begins a track is each user's clause C4 and [source-contract.md §3](source-contract.md#3-tracks).

**Count / traces.** 9 fixes, 1 revert, 1 convention change, 6 issues.
#1999 b4d53d129; #3645 85e2a31e1; #3654 bcbb74c81; #4102 #4108 59de576ec; #4786
2b3aa2ea7; 3c089a3f0; #5083 e30598ef2 reverted f998363bc; #5379 1f1fda60a
(cross_on_track); #5633 #5640 275d242b6; #3577 #3612 a64945bde (convention);
#3283; #2026; #3731; #1366 (read only).

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

**Rule.** One leaving entry per child, completed when nothing reads it, with exactly one `on_leave` and a `preempted` flag settled at completion ([switch.md §7](switch.md#7-leaving)). The ending source stops at the end of its track ([switch.md §6](switch.md#6-presented-sources)). Callbacks registered during a handoff are released with the selection ([switch.md §5](switch.md#5-handoff)).

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

**Rule.** A probe answer is stable for the cycle and is delivered by the next request ([source-contract.md §2](source-contract.md#2-probes), [frame-assembly.md §4](frame-assembly.md#4-readiness)). A decision is kept until committed ([frame-assembly.md §2](frame-assembly.md#2-decisions)). An infallible switch counts leaving children when nothing else can play, and falls back to `starting` when a hook's result has nothing to deliver ([switch.md §4](switch.md#4-decisions), [switch.md §5](switch.md#5-handoff), [switch.md §9](switch.md#9-other-properties)).

**Count / traces.** 6 fixes, 5 issues. Entry 14 holds the crashes where the
source promised is the one that was just used.
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

**Rule.** The predicate contract ([policies.md §6](policies.md#6-predicate-contract)): predicates change nothing, are read only for a decision, one snapshot per decision ([switch.md §3](switch.md#3-predicates)). Policy state advances on tracks played ([rotate.md §4](rotate.md#4-events), [random.md §4](random.md#4-events)).

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
clocks alive. Some selection paths forgot to notify. A sequence kept reporting
its first item's sync source after it had moved to the second, so the clock
paced the stream for a source that was no longer playing.

**Why it is hard.** The value is cheap to define and expensive to keep current.
A cache needs a notification from every path that changes what is producing,
and transitions add paths that only user code knows about.

**How to tell.** After every selection change, into and out of a transition,
the clock's view of the switch's sync source equals that of the source actually
producing. The cost per cycle does not grow with the number of children.
Nothing registered for a selection change outlives the switch.

**Rule.** A user's sync source is that of what it reads after its last commit; every commit, release of a leaving source and loss of designation updates the clock's view at constant cost ([frame-assembly.md §9](frame-assembly.md#9-contract-clauses)), under the clock's rule ([pacing §3](../../../clock/spec/pacing.md#3-finding-the-sync-source)).

**Count / traces.** 8 fixes, 1 revert, 2 issues.
#1711 65bc80b8d; #1725 1c7a1c823; #1012 reverted 5e8951ad7; #3879 2ba67aba6;
f4ebeba5b; #5133 #5153 6906e2a70; 70c75575e; #5504 187bd8b59; #5505 a0c56dc00.

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

**Rule.** The result of `on_select` is checked against the switch's content type before it delivers; a failed handoff is undone ([switch.md §10](switch.md#10-failure)). Hooks receive sources that present the child's id ([switch.md §6](switch.md#6-presented-sources)). Content that is not PCM audio is cut without a blend ([composition.md §4](composition.md#4-default-handoff)).

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

**Rule.** Track sensitivity of the pair decides cut or wait ([switch.md §4](switch.md#4-decisions)); the pair table states blend and fate of the leaving track ([composition.md §2](composition.md#2-profiles)). A source that never ends a track is `live` ([composition.md §1](composition.md#1-tags)).

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

**Rule.** One atomic snapshot per decision ([switch.md §3](switch.md#3-predicates), [source-contract.md §11](source-contract.md#11-concurrency)). An abort takes effect when the running cycle is over ([source-contract.md §6](source-contract.md#6-abort)).

**Count / traces.** 3 fixes. ff1206ed1; #5359 cdc894e3c; #5148 #5194 (crossfade,
read only for switch).

---

## 12. Sources created or handed over while the stream runs

**Situation.** A dynamic selector gets its sources from script code at run
time. They can be created on the spot, created ahead of time and kept until
needed, or be long-lived sources returned again and again. Each one must be
woken before its first use (a file source has to be ready at once) and released
after its last, exactly once. The same source can be in use elsewhere, can be
part of both the old and the new answer (a mix rebuilt with one more input), or
can be prepared and then never returned. A sequence holds all its items from
the start and uses them one at a time. A live input owns a connection, a
decoder and threads.

**What went wrong.** Replaced sources were never released. Once they were,
releasing the old answer shut down the sources it shared with the new one:
adding a playlist to a running mix made the other playlists start a new track.
Returning to a long-lived playlist restarted it from the top at each return. A
source prepared ahead and never returned had its release tied to garbage
collection; that created a reference cycle and nothing was released at all, so
the release became an explicit handle given to the script. A replaced live
input was detached but not stopped: one connection, decoder and thread pool
leaked per replacement. A sequence kept finished items awake until the whole
sequence was done, and earlier had to stay ready long enough to see an item's
end before it could let it go. A timer that retried a failed source misbehaved
with sources shorter than the timer, and a "set the source now" command had no
defined moment of effect; both were removed. One activation cleanup was
reverted the next day. `append` leaked a source per track (1.x).

**Why it is hard.** Creation belongs to the script, use to the operator, and
release needs both to agree on what "last use" is. Wake and release are
counted per owner, and their order matters: the new answer must be woken
before the old one is released. Reachability is not use, so the collector
cannot be the signal. The maintainer's conclusion after the collector-based
attempt: do what should have been done from the start, pass an explicit
release and let the party that created the source call it.

**How to tell.** After N replacements the number of live sources, threads and
connections equals that after one. A source present in both the old and the
new answer is neither stopped nor restarted, and its position continues. A
long-lived source that is returned to either continues or restarts, and which
one is written down. A source prepared and never returned is released when the
script says so. A replaced live input is stopped in the cycle of its
replacement, and its stop callback fires. An item a sequence has finished with
is released when the sequence leaves it.

**Rule.** The entered source is awake before the one left is released; each is released exactly once, in the cycle it is replaced; reachability is not use ([frame-assembly.md §8](frame-assembly.md#8-lifetime)). A source designated again continues, and a prepared source is released by the script ([dynamic.md §3](dynamic.md#3-designation)). A sequence releases an item when it drops it ([sequence.md §3](sequence.md#3-items)).

**Count / traces.** 10 fixes, 1 revert, 1 design replaced, 5 issues.
append leak (1.x CHANGES); eb4297578; 25b7e1e05 reverted 76fdccfbf; #4055
4e447e7fe; #4156 1dc6faca0; #4268; #4713 #4725 c4eeb258d; #4995; #4810 59a223f34
aa9dcd173 02937d577 replaced by #4835 d129f78a8; #5079 b0227a7d7; #5389 #5391
138d0f8d8 (GH5389); #4796 (entry 5).

---

## 13. Data produced but not consumed

**Situation.** A source produces a whole frame in a streaming cycle, but a
reader may take only the start of it: up to a track boundary, up to the room
left in its own frame, or up to the shortest of several sources it combines.
What is left is the source's cached remainder and must come first in the next
cycle. A mid-frame join puts the first chunk of the entering source into the
room the leaving source left, which can be a few samples. A crossing hands its
transition sources fed from data read ahead in a child clock. A wrapper may
need a source's first frame, for its metadata, before it decides anything.

**What went wrong.** A remainder was cut at the wrong offset. A new remainder
replaced what was already cached instead of being queued with it. A source
that held a remainder but could produce nothing new was asked to produce. The
continuation of a source inside the frame was marked as a new track a second
time. An operator combining several sources kept the shortest length and lost
what the others had produced beyond it. A sequence dropped the first chunk of
its next item, with its metadata, when both items had the same amount of data
in the frame; the stream advertised the previous track until the next
boundary. Exactly one frame of the entering track was lost when its fade-in
was zero. The sources of a crossing were paced by the child clock instead of
the reader's: a click at the mixing point when a cue-out fell exactly on a
frame edge, gone with one sample less. A delay in front of a live source loses
data (warning only). A chunk cached before a source was left played on return
(open). Transitions skipped data after a track mark (1.x).

**Why it is hard.** How much was consumed is known to the reader, how much was
produced to the source, and when the remainder is carried over to the clock.
The bugs need an exact alignment: a boundary on a frame edge, two items of
equal length, a zero duration. Round test durations never hit them, and a lost
chunk of 20 ms is inaudible while the metadata it carried is not. The reader
must state what it consumed; it cannot be deduced from what it asked for.

**How to tell.** For every source and every cycle: samples produced equal
samples that reached a reader plus samples still cached. Nothing is dropped at
a join and nothing is delivered twice. Test joins at every offset in a frame,
including zero and a full frame, with item lengths of an exact number of
frames and one sample more or less. Metadata on the first sample of an
entering item is announced at the join. A reader that takes the shortest of
several sources loses nothing from the others. With a zero fade or delay, the
first sample of the entering track reaches the output. A source left with a
remainder does not play it on return unless the remainder is still current.

**Rule.** Samples produced equal samples used plus samples held, and held data belongs to the current track ([source-contract.md §5](source-contract.md#5-consumption)). A user of the seam holds no data ([frame-assembly.md §8](frame-assembly.md#8-lifetime)). An abort discards what was not used ([source-contract.md §6](source-contract.md#6-abort)). No operator reads ahead of its reader ([source-contract.md §1](source-contract.md#1-delivery)).

**Count / traces.** 10 fixes, 8 issues.
#683 #652 (1.x CHANGES); #3695 ab8beafbf; #3718 b054da31b; #3720 707d004ea;
0566ec0e7; 24362d387; #4828 #4851 ad75580e0; #4736 #4739 #4759 2e28966b9; #4887
553752e1c; #5296 905c57fda (cross-sequence-transition; shared with 14); #4850
(read only); #5594 (open); #5012 (entry 2).

---

## 14. The same source designated again inside one frame

**Situation.** When a frame is not full after a source's chunk, the assembler
asks which source comes next. The answer can be the source it just used.
Either that source's next track starts later in the frame it already produced,
and it can go on from where the last chunk stopped, or it has nothing beyond
that point and must not be used again in this cycle. The difference is a
position in that source's frame. The one who answers can be script code that
returns the current source, a policy that comes back to a child it left
earlier in the frame (s1, s2, s1), a sequence looping on its last item, or a
crossing whose leaving side has run out.

**What went wrong.** A track-sensitive fallback ignored a queue that had become
ready, because at a boundary the same child was accepted again without a new
selection. A sequence failed on its last item. A sequence applied the position
reached in the item it had finished to the item it moved to, and rejected it.
A child that came back after another child in one frame broke the single
"last source" position: crash under an audio processor. A script selector
returning a source with nothing more to give ended the process on an
assertion, when alternating between two live inputs. A crossing handed back a
leaving side that was not ready, and the two sides of a crossing followed the
wrong re-selection rule. A selection made when readiness was answered differed
from the one made at delivery ("not able to reselect"). The process exits on
repeated skips (open, read only).

**Why it is hard.** "The same source" means two things, continue or repeat,
and only a position that belongs to that source and to this cycle tells them
apart. One remembered position is wrong as soon as two sources alternate or
the selector moves on between the measure and its use. Every user of the seam
answers the question with its own exceptions. A wrong answer is an assertion
that stops the stream, reachable from script.

**How to tell.** A source contributes at most one contiguous span to a frame,
and each sample of its frame is used at most once. A position is compared only
with the frame of the source it was measured on. For every user of the seam,
at a boundary in mid-frame: a different ready source is taken from its start;
the same source with more data continues without a gap; the same source with
nothing more ends the frame short, with no crash and no repeat. Run each with
a script selector that always returns the same source, one that alternates
between two on every call, and a source that ends exactly on a frame edge. No
script can reach an assertion.

**Rule.** One contiguous span per source and per frame; a decision the seam cannot serve ends the frame and is logged, never an error ([frame-assembly.md §7](frame-assembly.md#7-bounds)). The same source designated again continues ([dynamic.md §3](dynamic.md#3-designation)).

**Count / traces.** 8 fixes, 4 issues.
#3645 85e2a31e1 (GH3645; shared with 4); #3706 7bf657fce; c21588fc0; #4102
#4108 59de576ec (shared with 4); #4159 #4172 7c8f2d9ed (GH4159); #4177
4d1c2ea0b; #4420 3c047939d (shared with 6); #5296 905c57fda (shared with 13);
#4238 (open, read only); #3881 (read only).

---

## 15. An item's end guessed from a mark, a short frame or a missing answer

**Situation.** An operator that plays items in turn must know when an item is
finished. Sources show it in three ways: a track mark, a frame shorter than
asked, not being ready. None of them means "finished" on its own. A compound
item (a transition) has marks inside it. A slow item (a request being
resolved) is not ready yet. An item can have zero length, be shorter than a
frame, or be shorter than the read-ahead taken from it. The last item loops by
design. The list can be empty or hold one item.

**What went wrong.** A sequence of a transition and its tail jumped to the tail
at the transition's first mark (hotfix, then an option to play each item until
it is unavailable). An item not ready when its turn came was dropped for good:
`once` over a source that needs time to prepare never plays (rewrite proposed,
not merged). A zero-length silence, used as an idiom to pass straight to the
next item, stopped working when every source had to produce some data. An
entering track shorter than the read-ahead of a crossing cut the track after
next or repeated a jingle; only a warning was added. A sequence stayed ready
forever and sent a mixer into an endless loop; before that it went unready
without having seen the end, so its items were never shut down. A dynamic
selector moving to a failing source inside a mixer produced a mark beyond the
end of the data: crash. A merged rotation over an item that ends went to
silence instead of looping (read only). A sequence that is left and reached
again resumes where it stopped, without its first items (by design, reported
as a bug, read only). "Infinite sequence of empty tracks" (open, read only).

**Why it is hard.** The end is the item's decision, but the operator sees only
by-products of it. Each by-product is right for a plain file and wrong for a
compound, live or empty item. This is entry 2 at the scale of one frame: the
party that knows does not say, and the party that needs to know guesses.

**How to tell.** For each kind of item (file, live, compound with marks
inside, zero length, shorter than a frame, shorter than the read-ahead, not
ready at its turn and ready later), write down what ends it and test that
nothing else does. An item not ready at its turn is either waited for or
dropped; the rule is written and the drop is logged. An empty item takes no
audio from its neighbours. A mark never lies beyond the data of its frame. The
operator stops being ready within a bounded time after its last item ended,
and not before that end was delivered.

**Rule.** A segment says why it ended, and exhaustion is declared ([source-contract.md §1](source-contract.md#1-delivery), [source-contract.md §7](source-contract.md#7-lifecycle)). Only the item finishes itself; a late item is waited for; an item exhausted at its turn is skipped and logged ([sequence.md §3](sequence.md#3-items)). A merged rotation passes over a child with nothing to deliver ([rotate-merge.md §2](rotate-merge.md#2-rounds)).

**Count / traces.** 8 fixes, 1 warning, 1 fix not merged, 8 issues.
eb4297578; #115 3c35e2ccf; cc6fcf228; crossfade buffering (2.0 CHANGES); #3728
#3729 8ae85b066; #3731 50eede125 (warning); #4736 #4759 2e28966b9 (shared with
13); #4887 553752e1c (shared with 13); #4897 (not merged); #5044 #5054
6f9f2bcc4; #478 #2872 #280 (read only).

---

## 16. Merge decided at the wrong moment, or by the wrong party

**Situation.** A merged join has no boundary: the listener hears one track.
The rule since 2013 is that joins between items are merged and consecutive
tracks of the last item are not, because transitions are built from sequences.
The decision is per join. The operator advances its own state when it names
the next source, before the join is assembled. A join in mid-frame and a join
on a frame edge are assembled on different paths. Several things are built on
merging: a fade-in after a delay (silence, then the track), `prepend` and
`append` with merge, a merged rotation (one round is one track), the head and
tail around a crossing's transition.

**What went wrong.** Merging with a single source was fixed, reverted, and
redone with a separate operator for "merge everything" (2013). A fix for a
sequence failing on its last item made the merge answer depend on how many
items are left, read after the operator has advanced: the join into the last
item is never merged, merge does nothing in a two-item sequence, the track
reported is the second item's, and the first mark of the stream is dropped
(open). A targeted fix was closed as incomplete: the path used by crossings
read the same late answer, so a mid-frame join kept its mark and a
frame-aligned one dropped it; a join inside the very first frame was taken for
the stream start; a skip on a merged sequence lost its boundary. Callers had
come to rely on the defect: the merged rotation pads each round with a frame of
silence so that the mark left unmerged separates the rounds, and a delayed
fade-in or a merged `prepend` would move its track start to the leading
silence or jingle. `append` with merge took three fixes in ten days: the
appended item fired the track callback, a two-track appended sequence
announced its metadata wrongly, and the marks of a source continuing in the
frame were not dropped.

**Why it is hard.** The answer belongs to the join, that is to the source
being left and the source entered. It is asked of the operator, whose state
has already moved on. Two assembly paths must agree. Merging removes a
boundary but must keep the entering item's metadata, a skip's boundary and the
start of the stream. Fixing the rule changes what four stdlib operators report.

**How to tell.** For a merged sequence of 1, 2 and 3 items: one track is
announced, the first item's, at the start of the stream; each later item's
metadata is announced at its join as metadata, with no boundary; the result is
the same for joins in mid-frame, on a frame edge and inside the first frame.
Consecutive tracks of the last item keep their boundaries. A skip on a merged
sequence gives a boundary. Rounds of a merged rotation are separated by one
boundary and no audio is inserted to get it. For a delayed fade-in and a
merged `prepend`, where the track starts is written down and tested.

**Rule.** Merging is stated per join, when the source being left ends ([frame-assembly.md §5](frame-assembly.md#5-segments), [sequence.md §4](sequence.md#4-merging)). An abort of a merged unit gives a boundary ([sequence.md §5](sequence.md#5-abort)). Rounds are tracks with nothing inserted ([rotate-merge.md §3](rotate-merge.md#3-tracks)). Where the helpers' tracks start is in [helpers.md](helpers.md).

**Count / traces.** 7 fixes, 1 revert, 2 fixes not merged, 2 issues.
487aef08b (shared with 18); cf8a6f05f reverted 3f272c65a; 4fe509051; #3706
7bf657fce (shared with 14; origin of #5634); #4152 a25ebbe93 (append,
append-merge); #4153 (closed); #4156 1dc6faca0 (shared with 12); #4169
fdb4b752c (append-merge2); #5379 1f1fda60a (entry 4); #5632 (closed); #5634
(open); #3847 (read only).

---

## 17. Several reads of one source in one cycle

**Situation.** In a streaming cycle a source computes its frame once and every
reader sees the same frame. Readers are many: the consumer, a readiness probe,
a script callback fired while the frame is being computed that asks the same
source for its time or its frame, script code that makes a source compute its
frame ahead of use to see its first metadata. A wrapper and the source it
wraps can each hold a callback for the same boundary.

**What went wrong.** A synchronous metadata callback that asked its own source
for its elapsed time restarted the computation it was running in. The source
produced several frames in one cycle; with a source that announces metadata
again on each track the loop never ended (hang, or a crash after 600 MB of
output). In the previous streaming model a prepend-style operator asked a
source for data it had already advanced in the cycle: assertion, on `prepend`
and `append` with merge. Selection ran from the readiness question and from
generation at once and had to be made safe against itself. A fade held on a
wrapper started from an end-of-track callback that fired, on a skip, after the
inner source's new-track callback: the whole next track was muted. A fade-in
whose start ran after the first samples of the new track were processed gave a
click at full level. A change that computed frames early to cope with a
zero-length item was reverted the next day and redone.

**Why it is hard.** "Once per cycle" is a property of the whole graph, and
each reader acts locally. Script callbacks make any reader re-entrant. The
order of callbacks between a wrapper and what it wraps is stated nowhere, and
fades depend on it. Entry 3 is the same difficulty seen from a selector.

**How to tell.** Any number of reads of a source in a cycle, from anywhere,
return the same frame, and the source computes once: the count of computations
per cycle is 1. A callback fired during the computation that reads the same
source gets a defined answer and returns. For one boundary, the order in which
the callbacks of a wrapper and of the source it wraps fire is written down and
is the same for a natural end and for a skip.

**Rule.** A segment is produced once and every reader sees it ([source-contract.md §1](source-contract.md#1-delivery)). Callback order at a boundary, and re-entrant reads ([source-contract.md §10](source-contract.md#10-callbacks)).

**Count / traces.** 6 fixes, 1 revert, 5 issues.
LS-354 908588f4e (LS354); #3125; 53b032acc reverted 27f1964bf; #4759 2e28966b9
(shared with 13); #5019 #5062 e86a14b22 (GH5019); #5361 #5375 8446a9aaf
(GH5361); #5514 #5516 7339f785e (GH5514).

---

## 18. More than one track boundary in a frame

**Situation.** A track can be shorter than a frame. A skip can end a track in
the frame where it began. A join adds a boundary next to a source's own. An
inserted item can last a few milliseconds. So one frame can hold several
boundaries. The streaming model before December 2023 allowed one boundary per
read and asserted it. The current one lets a frame hold several marks, and a
reader splits at the first.

**What went wrong.** Merging operators left boundaries that meant nothing, and
the one-per-read rule was introduced to forbid them (2008). Under that rule, a
skip issued from a metadata callback below a `prepend` added one boundary too
many and shut the stream down; the maintainer judged it not fixable in that
model. In the current model, a dynamic selector alternating between live
inputs ended the process with "multiple tracks in a single frame". A mark was
put on an empty chunk (entry 4), and a mark was placed beyond the data (entry
15).

**Why it is hard.** A bound on boundaries per frame is a comfort for readers
and a crash for writers, who cannot know how many other parties add one. With
no bound, every reader must handle the second and third boundary, and tests
exercise only the first.

**How to tell.** A frame with 0, 1, 2 and 3 boundaries (tracks of a few
samples, a skip in the first frame of a track, a join next to a source's own
boundary) goes through every user of the seam with every track announced in
order, none merged unless asked, and no assertion.

**Rule.** A frame may hold any number of boundaries; each is a segment end ([source-contract.md §3](source-contract.md#3-tracks)). Every request delivers a sample or ends the frame ([frame-assembly.md §7](frame-assembly.md#7-bounds)).

**Count / traces.** 2 fixes, 1 redesign, 3 issues.
trac #204 487aef08b; #3696; #3577 77cf76e88 (redesign); #4159 #4172 7c8f2d9ed
(shared with 14); 3c089a3f0 (entry 4); #5044 #5054 (entry 15).
