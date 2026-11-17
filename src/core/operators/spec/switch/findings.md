# Findings (as-built)

What reading the code surfaced. Each is **checked** (run) or **read only**
(reasoned from the code; a suspect until refuted). The agenda for the
normative pass.

Nothing here was run against the full test suites.

## Defects

**D1. Resuming cuts a track that is in progress.** _Checked._
A switch that resumes re-selects and calls it a boundary
([switch.md §3](switch.md#3-cycle-start), [§6](switch.md#6-get_source)). The
rule's premise is that nothing is playing. It is false when the selection has
a track in progress: started, not ended, with its first chunk still cached
([frame-assembly.md §6](frame-assembly.md#6-consequences)). Seen with an
outer switch over a `rotate`: three songs, an insert, then the jingle that was
due is heard for one frame and replaced by a song. The same script plays the
whole jingle when a resuming switch keeps a track-sensitive, ready selection;
that change makes the "stale selection after idle" test fail (D2).

**D2. Resuming undoes a commitment it cannot see the reason for.** _Checked_
for the boundary case (the "stale selection after idle" test hands over at a
boundary, with every source on the file profile); _read only_ for the
preemption case.
A nested switch that resumes is in one of two states. Left at a boundary: it
has already started its next track (D3), which nobody heard. Preempted: its
selection's track was dropped by the file profile's skip, and abort track left
the selection in place ([switch.md §11](switch.md#11-other-properties)). The
tick-gap rule re-selects in both, to get rid of a choice made under old
conditions. In the first state that choice should not have been made yet; in
the second the switch was told and did not act on it. The rule repairs both
from outside and breaks any policy that counted the unheard track (D1).

**D3. A frame generated whole starts a track nobody heard.** _Checked._
Generating a frame crosses the operator's own track boundary: next selection,
`on_select`, track callbacks, policy counters, logs
([frame-assembly.md §6](frame-assembly.md#6-consequences)). When the consumer
stops at the mark and goes elsewhere, all of that has happened for a track
that has not aired. `rotate` has counted it ([rotate.md §4](rotate.md#4-events)).

**D4. A stale chunk plays on resume.** _Checked._
The cached first chunk of D3 is served when the operator is next read, before
anything is re-selected. With D1 the listener hears one frame of the old
choice, then the new one.

**D5. `on_leave` is documented differently from what it gets.** _Read only._
The method's help says `track_sensitive` is true "if the ending source's last
frame had a track mark". The switch passes true whenever the handoff had no
ending: `after_position`, no selection, selection not ready, or resuming
([switch.md §8](switch.md#8-handoff)). Under D1 a source cut mid-track is told
it ended naturally, so the file profile does not skip it.

**D6. Clearing the selection reports a natural end.** _Read only._
When a search finds nothing, the selection is left with
`track_sensitive = true` ([switch.md §6](switch.md#6-get_source)), including
when it was a non-track-sensitive child whose predicate went false while its
track was playing.

**D7. `all_predicates` no longer does what its name says.** _Read only._
Every predicate is evaluated at the first need in a cycle
([switch.md §4](switch.md#4-predicates)) whatever the flag. What the flag
still adds is testing the readiness of every remaining child, which opens
their cycles and, for nested switches, makes them select
([source-contract.md §1](source-contract.md#1-readiness)).

**D8. Asking a switch if it is ready changes it.** _Read only._
Readiness runs a selection: predicates, `on_select`, wake, sleep, logs. A
parent's search tests readiness last, so a child found ready is chosen; but
D7's flag, and any other reader of readiness, triggers it without choosing.

**D9. `merge` describes the wrong join.** _Checked_ for `sequence`; _read
only_ for the other two places.
The seam asks the operator whether to merge after the operator has moved on
([frame-assembly.md §8](frame-assembly.md#8-when-merge-is-read)). In a merged
sequence the join into the last item is never merged and the stream's first
track mark is dropped: the track reported is the last item's, where the first
item's is expected. The same late read is in two more places: a change of
source that does not begin a track keeps a mid-frame mark but drops a
frame-aligned one; and the continuation of the same source past its own
boundary drops marks when `merge()` holds, although that is not a join
between sources. A targeted fix in `sequence` was tried and closed as
incomplete; the fix is on hold for the rebuild.

Intended, as stated by the session that analysed it: merging is decided per
join, from the source being left. Nothing left (stream start): never merged.
Same source continuing: not a join, never merged by `sequence`. Another
source: merged when the operator says so. The operator that advances is the
one that knows and should say so for that join.

Behaviour that today's callers rely on, and that a fix moves:

- A looping merged rotation pads each round with one frame of silence, so that
  the mark kept at the last join separates the rounds.
- A delayed fade-in and a merged prepend would have their track start, and the
  metadata given to track callbacks, move to the leading silence or jingle.

**D10. The merged rotation does not separate its rounds.** _Checked_; cause
not traced; not compared with an earlier release.
Three children with short, equal, titled tracks: one track is reported for the
whole stream; the first child's title is seen once, then only the other two;
the inner rotation logs a handoff to each child on every frame
([rotate-merge.md §5](rotate-merge.md#5-shape)).

**D11. Aborting a merged sequence drops items without releasing them.** _Read
only._ With `merge` asked, abort track cuts the queue down to its last item
and aborts that item's current track
([sequence.md §5](sequence.md#5-other-properties)). The items in between are
not released, unlike items dropped by designation. The last item loses the
track it was on, if it had started one.

**D12. Asking `source.dynamic` if it is ready runs script code.** _Read
only._ Readiness consults the script function whenever the operator is not
track-sensitive, has no current source, or its current source is not ready
([dynamic.md §3](dynamic.md#3-designation)). That call can create and wake
sources, and raises when the operator was declared infallible. An instance of
D8.

**D13. `append(merge=true)` merges everything.** _Checked_ by the test-suite
pass. Its merge getter tests whether the holder of the pending source exists,
not whether a source is pending, so it is always true: one track mark in a
long run where the unmerged form gives over a hundred. The test for it passes
because it only asserts an absence.

**D14. Dead and misnamed merging helpers.** _Checked_ by search. The help of
`sequence`'s `merge` names a helper that is deprecated in the script library;
a core wrapper with the same purpose has no caller
([sequence.md §4](sequence.md#4-merging)).

## Asymmetries

**A1.** The four users treat `force` four ways. `switch` honours it for
keeping and turns it into `ok` for the search. `sequence` and `cross` turn it
into `ok`. `source.dynamic` takes it as "ask the script", then treats it as
`ok` ([frame-assembly.md §7](frame-assembly.md#7-contract-clauses), clause
C1 of each file). _Read only._

**A2.** `rotate` and `random` have predicates that change state; `fallback`
does not. The switch gives no rule on when a predicate runs
([policies.md §6](policies.md#6-what-a-predicate-may-assume-today)).
`rotate` is only right when a search means "a track ended". _Checked_ for
resuming (D1).

**A3.** A kept selection is not added to `excluded`; a re-selected one is
([switch.md §11](switch.md#11-other-properties)). _Read only._

**A4.** Track sensitivity is per pair for a takeover but switch-wide for the
reselect mode: one non-track-sensitive child makes every cycle a search, and
every predicate run every cycle. _Read only._

**A5.** A merged join keeps or drops the mark depending on whether it falls
mid-frame or on a frame boundary (D9). _Read only._

**A6.** `cross` also reads its "begins a track" answer after it has moved
([cross-seam.md §3](cross-seam.md#3-seam-answers), C4). There it is the
intended reading; in `sequence` the same late read is D9. _Read only._

**A7.** Lifetime of what the seam reads differs per user (clause C5): a
`switch` keeps a leaving source awake until nothing reads it; `sequence`
releases a finished item at once; `source.dynamic` puts the previous source to
sleep as soon as the next is awake; `cross` puts the previous buffer or
compound to sleep when it creates the next. _Read only._

## Gaps

**G1.** No stated contract for predicates: when they run, how often, what a
run means.

**G2.** No signal from a parent to a nested selector other than abort track,
and that one does not say "your track is over" to the selector itself (D2).

**G3.** No notion of "heard": the only track event fires at generation (D3).
A policy cannot count tracks that aired.

**G4.** The reason for the outer `fallback` in `rotate` and `random` is not
recorded ([rotate.md §5](rotate.md#5-shape)).

**G5.** `resuming` is true on the first cycle of a switch first animated after
tick 0. No effect found with no selection. _Read only._

**G6.** Nothing bounds the `leaving` list when a returned effective source
keeps old proxies up. _Read only._

**G7.** An `on_select` that raises leaves a proxy that was never woken and no
record of the attempt ([switch.md §12](switch.md#12-failure)). _Read only._

**G8.** Still outside this snapshot: everything in `cross` that is not its
use of the seam; and the other operators built on `source.dynamic`
([dynamic.md §7](dynamic.md#7-built-on-it)).

**G9.** `source.dynamic` adopts a source that is not ready: it becomes the
current one and stays awake, while the operator reports no source
([dynamic.md §3](dynamic.md#3-designation)). What happens on the next cycle
depends on the script returning it again. _Read only._

**G10.** Nothing states why the merged rotation's outer switch should search
when its flag flips ([rotate-merge.md §5](rotate-merge.md#5-shape)).

## To verify

**V1.** `single`: a `single` child whose track ends with no other child ready
is cleared, then selected again on the next cycle. Whether "two consecutive
tracks" is meant to allow that.

**V2.** `rotate` with a weight of 0 or less: each predicate call picks again.
Outcome not traced.

**V3.** Default handoff with an unknown remaining time: fade is `max_fade`
and the tail is cut at `max_fade`. Not run.

**V4.** Whether a selection kept across an idle period replays its metadata
when the switch resumes. The proxy and the effective source are unchanged, so
by reading: no replay from this switch; the parent's handoff replays the
nested switch's last metadata.

**V5.** Log lines and script `print` interleave unreliably in captured
output: logs are written by another thread. Orderings in this document were
taken from synchronous prints only.

**V6.** `sequence`: an item that is not ready when its turn comes is dropped
for good, and `started` turns true when any item, not only the head, is ready
([sequence.md §3](sequence.md#3-designation)). Whether a temporarily
unavailable head is meant to be skipped. _Read only._

**V7.** `sequence` with `single_track` off and a head that never becomes
unavailable never reaches the next item. By reading; not run.

## Reconciliation with the known-complexity list

Each entry of [known-complexity.md](known-complexity.md) against the as-built
text.

| Entry                                           | Verdict                    | Where                                                                                                                                                                |
| ----------------------------------------------- | -------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1. Metadata at a handoff                        | Covered                    | [composition.md §4](composition.md#4-default-handoff), [switch.md §9](switch.md#9-proxy), [§11](switch.md#11-other-properties)                                       |
| 2. Decisions inferred from pulls                | Contradicts                | D1, D2: the resume rule is such an inference                                                                                                                         |
| 3. Read-ahead and probes with side effects      | Contradicts                | D3, D4, D8                                                                                                                                                           |
| 4. Track marks across a handoff                 | Covered                    | [frame-assembly.md §4](frame-assembly.md#4-building-the-frame), [§5](frame-assembly.md#5-track-marks-at-joins), `excluded` in [switch.md §6](switch.md#6-get_source) |
| 5. When a handoff is over                       | Covered, one contradiction | [switch.md §8](switch.md#8-handoff), [§10](switch.md#10-leaving); R1                                                                                                 |
| 6. Readiness against delivery                   | Covered                    | [frame-assembly.md §3](frame-assembly.md#3-readiness), [source-contract.md §1](source-contract.md#1-readiness)                                                       |
| 7. Stateful policies                            | Not covered                | G1, A2, A4                                                                                                                                                           |
| 8. Sync role across a change                    | Not covered                | R2                                                                                                                                                                   |
| 9. Content type at handoff                      | Partly covered             | [switch.md §12](switch.md#12-failure), [composition.md §4](composition.md#4-default-handoff); R3                                                                     |
| 10. Whose rule governs a handoff                | Covered, one contradiction | [switch.md §7](switch.md#7-search), [composition.md §2](composition.md#2-profiles); R4                                                                               |
| 11. Other threads during a cycle                | Partly covered             | [switch.md §4](switch.md#4-predicates); R5                                                                                                                           |
| 12. Sources created or handed over at run time  | Covered, asymmetric        | Clause C5 of each user; A7                                                                                                                                           |
| 13. Data produced but not consumed              | Contradicts                | [source-contract.md §2](source-contract.md#2-frames); D3, D4                                                                                                         |
| 14. Same source designated again inside a frame | Covered, one gap           | [frame-assembly.md §4](frame-assembly.md#4-building-the-frame), [sequence.md §3](sequence.md#3-designation); R6                                                      |
| 15. An item's end guessed                       | Contradicts                | V6, R7                                                                                                                                                               |
| 16. Merge decided at the wrong moment           | Contradicts                | D9, D13                                                                                                                                                              |
| 17. Several reads of one source in a cycle      | Covered                    | [source-contract.md §1](source-contract.md#1-readiness), [§2](source-contract.md#2-frames)                                                                           |
| 18. More than one track boundary in a frame     | Covered                    | [source-contract.md §2](source-contract.md#2-frames)                                                                                                                 |

**R1. A track that finishes under the fade is reported as cut.** _Read only._
A child preempted with less than `max_fade` left plays out under the starting
source. Its `on_leave` still gets `track_sensitive = false`, and the file
profile skips: the skip lands on its next track. Recorded in the history as a
known limit, not a choice.

**R2. Sync role.** The as-built text says only whose self-sync a switch
reports. It does not say how the clock learns of a change at a handoff or at
the end of a transition. _Not read._

**R3. `on_select` gets the proxy, not the child.** _Read only._ `starting`
and `ending` are proxies, with ids of the form `<child id>.proxy`. Script code
that branches on the id of what it is given does not see the child's.

**R4. A silence child of last resort does not let go.** _Read only._ Silence
is a synthesis source, so it resolves to the file profile: track-sensitive,
and it never ends a track. A higher-priority file child cannot take over
unless the script retags the silence `live` or marks it not track-sensitive.

**R5. Skip from another thread.** The as-built text does not say when an
abort issued from another thread takes effect relative to a cycle. _Not read._

**R6. A source can come back inside one frame.** _Read only._ The history
settled that a frame never holds the same source twice with another between.
`switch` enforces it per cycle ([switch.md §6](switch.md#6-get_source)).
`sequence` cannot produce it. Nothing prevents it for `source.dynamic`, whose
designation is script code.

**R7. "Not ready" is taken as "finished".** _Read only._ `sequence` drops an
item that is not ready at its turn and never returns to it (V6). The history
has a report of exactly this on `once`; its fix was not merged.

**R8. The merged join is a regression that callers now rely on.** _Read
only._ The rule first shipped, and still the documented one, merges every
join between items and spares only the last item's own tracks. A later change
made the join into the last item never merged (D9). The merged rotation, the
delayed fade-in and the merged prepend were then written against that
behaviour.
