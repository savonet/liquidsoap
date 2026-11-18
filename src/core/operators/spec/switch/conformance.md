# Conformance checks

The checks an implementation is run against, by principle. Each names what
is **binding**. Unless said otherwise, ids, wording, durations and timing
are incidental. Entries marked K are the situations of
[known-complexity.md](known-complexity.md); entries marked S are the
scenarios of [brief.md §3](brief.md#3-scenarios-that-must-all-hold).

Every check signals pass or fail explicitly; a timeout is a failure. A check
that asserts an absence MUST also assert that the thing it watches happened
at least once.

## Doubles a harness needs

- An endless tone with no track end; a tone of fixed duration; silence,
  endless and of fixed duration.
- A tagger that titles every track, and a chopper that cuts an endless tone
  into titled tracks of a chosen length, down to a few samples.
- Track lengths of an exact number of frames, and one sample more or less.
- A gate: a source whose availability follows a boolean set by the test. A
  source that is exhausted from the start. A source that is late for a
  chosen time, then delivers.
- A live double: produces whether or not it is read, with a title inserted
  at a chosen moment.
- Files with title tags, a playlist over some of them, a queue that can be
  pushed at run time.
- Three **reader shapes** for the same source: an output; an operator that
  reads it through a child clock on every other tick; an outer switch that
  leaves it and returns.
- A relay: an operator that passes its child through unchanged, to put
  between two selectors.
- Observation points: per track, per metadata, per segment with its end
  reason, per sample count; a counter of probes, of predicate calls, of
  `next` calls, of live sources and of registered callbacks.
- An unsynced clock for most checks; a clock ticked by hand for the ones
  that count cycles.

## Source contract

- A source's segments end with the stated reason; a segment holds a sample
  unless it ends with `late` or `track end`. Binding: the reasons.
- K17: any number of readers get the same data at the same positions; each
  segment is produced once. Binding: the production count per segment is 1.
- K3: N probes of a source in a cycle, for N in 0, 1, 10, leave every
  counter, callback and selection below it identical. Binding: all of them.
- A probe answer is delivered: after `more` or `new track`, the next request
  gives a sample of that kind.
- K13: for every source and cycle, samples produced equal samples used plus
  samples held. Checked for joins at every offset of a frame, zero and a
  full frame included, with the track lengths above.
- K18: frames holding 0, 1, 2 and 3 boundaries go through every user of the
  seam with every track announced in order and nothing merged unless asked.
- A mark never lies beyond the data of its frame, and never on an empty
  segment.
- An abort ends the track at what was used; nothing of the aborted track is
  delivered afterwards; last metadata is null afterwards.
- K11: an abort issued from another thread, and one issued from a callback
  of the running cycle, both take effect when that cycle is over. Binding:
  no segment of the running cycle changes. Hammering aborts at 1 ms
  intervals for a thousand aborts neither stops nor fails the stream.
- K17: for one boundary, end-of-track callbacks of every level precede
  new-track callbacks of any level, inner before outer, the same for a
  natural end and for an abort. A callback that reads the source being
  produced returns.
- K2: a source left unread for any time holds bounded memory. A live double
  selected after a long time unread delivers its track start and its title.
- No relaying operator reads ahead: with the relay between a selector and
  its reader, every check of this document gives the same result as without
  it.

## Commitment

- Left at a boundary: a selector whose reader takes data up to its track
  end and then reads something else has, at that moment, selected nothing
  new, fired no track event, run no `on_select`, and its policy state is as
  at the track end. Binding: all four.
- S1. Binding: the track after the insert is the jingle; it plays for its
  full length; no song starts before it ends; the rotation's count of
  played tracks equals the tracks that reached the output. Run with the
  boundary inside a frame and on a frame edge, and under a crossfade over
  the outer switch.
- S2. Binding: the first track after the programme is a jingle; no sample
  of a song precedes it.
- S2b. Binding: as S2, and the cut song does not resume.
- S3, K2: the same script under the three reader shapes gives the same
  track sequence and the same duration for every track.
- K2: every state change of a selector while it is not read has a named
  cause among: a request, a reported use, an abort, a track end, a wake, a
  release. Checked by ticking a selector's clock any number of times without
  reading it: nothing changes.
- A selector cut in the middle of a track and not aborted resumes that
  track where it was. Cut and aborted, it decides anew at its next request.

## Seam

- K4: a join entered with `track` has exactly one mark, on the first sample
  of the entered source, inside a frame and on a frame edge alike; a join
  entered with `continue` has none and keeps the entered item's metadata.
- K14: a source contributes one contiguous span per frame. A decision that
  returns to a source used earlier in the frame ends the frame. The same
  source with more data continues without a gap; with nothing more, the
  frame ends short. Run with a designation function that always returns the
  same source and one that alternates on every call. Binding: no error, no
  repeat, no lost sample.
- K12: after N joins the count of live sources, threads, connections and
  registered callbacks equals that after one. A source on both sides of a
  join is neither stopped nor restarted.
- K8: after every commit, into and out of a transition, the clock's view of
  the user's sync source is that of the source delivering. The cost per
  cycle does not grow with the number of children.
- Each user's file answers clauses C1 to C7. Binding: one check per clause
  per user.

## Switch

- Priority: at a boundary the first child in list order that is eligible,
  whose predicate holds and which is available is chosen. An unavailable
  child does not delay the output.
- A track-sensitive selection is kept to its track end whatever its
  predicate says. A selection that is not track-sensitive is left in the
  cycle after its predicate turns false, when another child can play.
- A takeover in the middle of a track happens only when the selection or
  the candidate is not track-sensitive.
- K7: predicate calls during one long track of a track-sensitive selection
  with only track-sensitive children: none. All predicates of one decision
  see one snapshot: two predicates written together are never seen half
  written.
- Handoff: `on_select` receives `starting`, `ending` only on a preemption,
  and the replay flag; what it returns is streamed. Both sources present the
  child's id (K9).
- K9: the result of `on_select` has the switch's content type before it
  delivers data; a generic hook on a narrower switch stays narrow. A switch
  over encoded content hands off without decoding and without error.
- An `on_select` that raises leaves the selection as it was. An `on_select`
  whose result has nothing to deliver falls back to `starting`.
- K5: each handoff gives exactly one `on_leave` for the child left, at a
  boundary and on a preemption, for chained handoffs too. `preempted` is
  true for a cut, false for a boundary, false for an unavailable child, and
  false when the track ran out under the fade. It fires once nothing reads
  the child through the transition.
- K5: the leaving child's next track is never heard during its fade. A
  child is not selected again while it is leaving, unless nothing else can
  play; a third child can take over meanwhile.
- K1: for every track that becomes audible, exactly one announcement, at
  the position where its audio starts, with that track's own metadata.
  Checked for: first selection; re-selection in the middle of a track;
  re-selection after the child's track ended or was aborted while it was not
  selected; switches nested two and three deep; a transition keeping the
  leaving side, the entering side, or both; the same title twice in a row;
  an entering source with metadata on its first sample; the live double.
  With replay off for a child, nothing is inserted at its entry.
- `single`: after a boundary on a `single` child the next track is another
  child's; with no other child, the switch delivers nothing.
- K6: an infallible switch is ready in every cycle across repeated
  transitions in both directions, the cycle where a transition ends
  included. In every cycle where a switch answered ready it delivers a
  sample from the source it had decided.
- Abort: the selected child's track is skipped, its last metadata is null,
  and the next request decides at a boundary. It reaches the child selected
  inside nested switches.
- `selected()` changes only at a commit.

## Composition

- K10: one check per row of the pair table of
  [composition.md §2](composition.md#2-profiles). Binding: cut or wait,
  blend or not, the fate of the leaving track.
- K10: an endless silence as last child is left in the cycle after a
  higher-priority child becomes available, with no tag set by the script.
- Default handoff, PCM audio: the starting child is heard from the first
  sample; the leaving child is mixed beneath it for at most `max_fade`. With
  less than `max_fade` remaining it finishes its track, its next track is
  never played and is not skipped. With an unknown remaining time the fade
  lasts `max_fade`.
- The move from the mix to the starting source loses, repeats and delays no
  sample and adds no boundary.
- `composition_type` reads `"file"` or `"live"`; writing anything else
  raises `invalid`. A changed tag or profile applies at the next use.

## Policies

- K7: `rotate` plays each available child for `weight` consecutive tracks,
  from the first round on. Binding: exact order prefixes, and exact counts
  over ten rounds, under the three reader shapes and nested under a switch
  that preempts it and returns.
- An unavailable child is passed over and picked at its next turn. Weights
  may be getters. A child of weight 0 has no turn.
- `random`: the sole available child is chosen; the sole child of positive
  weight is chosen; a new draw is made at every track played, whatever the
  weights. Binding: deterministic cases only.
- K3: `rotate`'s count of plays equals the number of its tracks that reached
  the output, in S1 and with an outer switch that leaves exactly when the
  inner track ends inside a frame.
- `fallback`, `rotate` and `random` over an infallible child are infallible.

## Sequence

- Each item but the last plays one track; the next is entered in the same
  frame. The last item plays for as long as it delivers.
- K15: what finishes an item, for each kind: file, live, compound with
  marks inside, zero length, shorter than a frame, late at its turn.
  Binding: its track end or its exhaustion finishes it. A late item is kept
  and then plays in full when the last item cannot deliver; it is passed
  over when the last item can. A sequence whose last item is infallible is
  ready in every cycle. An item exhausted at its turn is skipped, with a log
  entry, and takes no audio from its neighbours.
- A sequence is exhausted after its last item is, and not before the end of
  what it delivered was taken. Mixed with other sources it neither blocks
  nor fails them.
- S4, K16: merged sequences of 1, 2 and 3 items report one track, at the
  start of the stream, with the first item's metadata; each later item's
  metadata is announced at its join as metadata. The same for a join inside
  a frame, on a frame edge and inside the first frame. Later tracks of the
  last item keep their boundaries.
- K16: an abort of a merged sequence gives a boundary; the items in between
  are released; the last item starts at its beginning.
- A sequence left and read again resumes where it was.
- `queue()` shrinks only at a commit.

## source.dynamic

- `next` is called once per decision point: at each boundary; at each cycle
  start when not track-sensitive; never while a track plays on a
  track-sensitive operator. Binding: the call count.
- A consult from a probe that no request follows enters no source and fires
  no track event.
- Returning the current source keeps it without waking it again: a playlist
  reused across designations is not reloaded. Returning another source makes
  it current at the commit and releases the previous one in that cycle; a
  replaced live input is stopped and its stop callback fires.
- A source prepared with `prepare` and never returned is released when the
  script releases it.
- A designated source that is late leaves the operator with nothing to
  deliver; it plays when it is ready. Declared infallible, the request
  fails with a script error and a probe does not.
- The `merge` getter, read at the track end, decides the next entry; a
  getter that changes between joins gives one merged join and one boundary.
- The designated source has the operator's content type before it delivers.

## Merged rotation and helpers

- S5, K16: one track per round, on the first sample of the round; the
  children heard in order, each for one full track, in every round; no
  sample inserted between rounds. An unavailable child is passed over for
  that round.
- `once` over a late source plays one track when the source is ready, then
  is exhausted.
- K16: delayed fade-in: no mark and no metadata during the delay; one mark,
  with the track's metadata, on the first sample of the source. With a zero
  delay the first sample of the source is the first delivered (K13).
- K16: `prepend` without `merge`: a mark at the prepended track and a mark
  with the main track's metadata at the main track. With `merge`: one mark,
  at the prepended track.
- `append` without `merge`: the appended track is a track with only its own
  metadata. With `merge`: one mark per main track and none at the appended
  track. Binding: the count of marks equals the count of main tracks, which
  is more than one.
- Whether a helper's stream starts with a mark is the same with and without
  a wrapping operator.

## Cross, as a user of the seam

- A crossing puts one mark, on the first sample of the transition, and none
  where the transition ends, with and without a head.
- The transition function runs when the reader takes data past the end of
  the buffered track, and not on a probe.
- A transition of several tracks is played to its end; its own marks pass.
- Returning from the compound to buffering adds no boundary.

## Checks in the tree

Under `tests/regression/`. "Fails before" names what the check reported
against the build the rebuild started from; a check without it passes there
too and only guards against a regression.

| Check                                       | Covers                                | Fails before                                 |
| ------------------------------------------- | ------------------------------------- | -------------------------------------------- |
| `switch_jingle_after_insert`                | S1, K3                                | jingle cut to one frame                      |
| `switch_reselect_after_idle`                | S2, K2                                |                                              |
| `switch_skipped_track_metadata`             | S2b, K1                               |                                              |
| `rotate_under_child_clock`                  | S3                                    |                                              |
| `sequence_merge_single_track`               | S4, K16                               | yes                                          |
| `append-merge2`                             | K16                                   |                                              |
| `rotate_merge_rounds`                       | S5, K16                               | yes                                          |
| `switch_many_tracks_per_frame`              | K18, K14                              | 4513 sources played for 500 tracks announced |
| `infallible_stays_full`                     | K6                                    | fails when the empty-cycle bound is removed  |
| `switch_infallible_across_transition`       | K6                                    |                                              |
| `switch_on_leave_track_boundary`            | K5: `preempted`                       |                                              |
| `switch_on_leave_chained`                   | K5: one `on_leave` per handoff        |                                              |
| `switch_per_source_no_reselect_during_fade` | K5: no reselection while leaving      |                                              |
| `switch_default_transition_finish`          | K5, K10: track ran out under the fade |                                              |
| `switch_predicates_on_selection`            | K7: no predicate read while kept      |                                              |
| `switch_copy_default_on_select`             | K9                                    |                                              |

Not covered by a check yet: K4 at every offset of a frame, K8, K11, K12,
K13, K15 by kind of item, K17, the probe-count part of K3, and the pair
table of [composition.md §2](composition.md#2-profiles) row by row.
