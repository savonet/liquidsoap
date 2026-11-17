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

## Asymmetries

**A1.** `cross` and `sequence` turn `force` into `ok`; `switch` honours it
([frame-assembly.md §7](frame-assembly.md#7-users)). _Read only._

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

**G8.** Two users of the seam and one policy were left out of this snapshot's
scope and are described only where they touch it: `source.dynamic` (used by
the default handoff), and the merged looping rotation built on `rotate`, which
overrides `on_select` on its first child and wraps the result in a `switch`
whose two predicates read one flag.

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

## Reconciliation with the known-complexity list

Each entry of [known-complexity.md](known-complexity.md) against the as-built
text.

| Entry                                      | Verdict                    | Where                                                                                                                                                                |
| ------------------------------------------ | -------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1. Metadata at a handoff                   | Covered                    | [composition.md §4](composition.md#4-default-handoff), [switch.md §9](switch.md#9-proxy), [§11](switch.md#11-other-properties)                                       |
| 2. Decisions inferred from pulls           | Contradicts                | D1, D2: the resume rule is such an inference                                                                                                                         |
| 3. Read-ahead and probes with side effects | Contradicts                | D3, D4, D8                                                                                                                                                           |
| 4. Track marks across a handoff            | Covered                    | [frame-assembly.md §4](frame-assembly.md#4-building-the-frame), [§5](frame-assembly.md#5-track-marks-at-joins), `excluded` in [switch.md §6](switch.md#6-get_source) |
| 5. When a handoff is over                  | Covered, one contradiction | [switch.md §8](switch.md#8-handoff), [§10](switch.md#10-leaving); R1                                                                                                 |
| 6. Readiness against delivery              | Covered                    | [frame-assembly.md §3](frame-assembly.md#3-readiness), [source-contract.md §1](source-contract.md#1-readiness)                                                       |
| 7. Stateful policies                       | Not covered                | G1, A2, A4                                                                                                                                                           |
| 8. Sync role across a change               | Not covered                | R2                                                                                                                                                                   |
| 9. Content type at handoff                 | Partly covered             | [switch.md §12](switch.md#12-failure), [composition.md §4](composition.md#4-default-handoff); R3                                                                     |
| 10. Whose rule governs a handoff           | Covered, one contradiction | [switch.md §7](switch.md#7-search), [composition.md §2](composition.md#2-profiles); R4                                                                               |
| 11. Other threads during a cycle           | Partly covered             | [switch.md §4](switch.md#4-predicates); R5                                                                                                                           |

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
