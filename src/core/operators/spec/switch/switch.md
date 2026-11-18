# Switch (Part A)

Selects one of its children and streams it. Segments, decisions, commits and
joins are [frame-assembly.md](../../../source/spec/frame-assembly.md); the defaults of the
per-child methods are [composition.md](../../../source/spec/composition.md).

## 1. Children

`switch(~id, [(predicate, source)])`, with the method `selected()`.

Each child has:

| Field             | Meaning                                                                   |
| ----------------- | ------------------------------------------------------------------------- |
| predicate         | Script function, no argument, returns a boolean. See [§3](#3-predicates). |
| source            | The source to play.                                                       |
| `track_sensitive` | Boolean getter: the child is left, and started, at track boundaries.      |
| `replay_metadata` | Boolean getter.                                                           |
| `single`          | Boolean, read once at creation.                                           |
| `on_select`       | `{ending: source?, replay_metadata: bool, starting: source} -> source`    |
| `on_leave`        | `{source: source, preempted: bool} -> unit`                               |

A method the child's value does not carry is read from the profile of the
child's source, at each use
([composition.md §3](../../../source/spec/composition.md#3-per-source-methods)).

List order is priority: where several children qualify, the first wins.

## 2. State

| Field     | Meaning                                                                        |
| --------- | ------------------------------------------------------------------------------ |
| selection | None, or: child, presented source, effective source, and `playing` or `ended`. |
| leaving   | Per child, at most one entry: presented source and whether it was cut.         |
| last      | The child whose track the switch last delivered. None at start.                |

- The selection is `ended` when its effective source's track ended or the
  switch was aborted ([§8](#8-abort)); `playing` otherwise.
- The selection, `leaving` and `last` change only in a commit
  ([frame-assembly.md §3](../../../source/spec/frame-assembly.md#3-commit)), in an abort, and when
  the switch is released. A decision that finds nothing changes none of them.
- `selected()` returns the selected child's source, or null with no
  selection. It is the last committed selection: it MAY name a child that has
  nothing to deliver now.

## 3. Predicates

- A predicate MUST NOT change anything when it is read
  ([policies.md §6](policies.md#6-predicate-contract)).
- Predicates are evaluated only to make a decision ([§4](#4-decisions)), and
  only those the decision can depend on.
- All predicates and `track_sensitive` getters read for one decision are read
  together inside one atomic section
  ([source-contract.md §11](../../../source/spec/source-contract.md#11-concurrency)): one
  consistent snapshot per decision.
- A decision that is kept is not evaluated again
  ([frame-assembly.md §2](../../../source/spec/frame-assembly.md#2-decisions)).

## 4. Decisions

`S` is the selected child, if any. A child is **available** when the probe of
what would be read for it does not answer `nothing`: for `S`, the effective
source; for any other child, its source.

At a boundary, an `S` whose effective source answers `nothing` while its
source does not is treated as a child that is not selected: it is available
by its source, and committing it is a handoff ([§5](#5-handoff)).

**Mid-track decision.** Made at the start of a cycle when the selection is
`playing` and its effective source's probe answers `more`.

1. The **candidates** are the other children `c` that are not leaving and
   such that `S` is not track-sensitive or `c` is not track-sensitive.
2. If `S` is track-sensitive and there is no candidate: keep `S`. No
   predicate is evaluated.
3. Otherwise take the first, in list order, of: a candidate whose predicate
   holds and which is available; `S` itself if its predicate holds.
4. If there is none: keep `S` if it is track-sensitive; else decide as at a
   boundary, which counts leaving children when nothing else can play.

Rationale for the last step: a leave completes only when its transition has
been read. A switch that answered none while its only other child was
leaving would not be read, so the leave would never complete and the switch
would never be ready again.

A decision other than `S` is a **preemption**.

**Boundary decision.** Made when there is no selection, when the selection
is `ended`, or when its effective source's probe answers `new track` or
`nothing`.

1. A child is **eligible** unless it is leaving, or it is `single` and it is
   `last`.
2. Take the first eligible child, in list order, whose predicate holds and
   which is available.
3. If there is none and some child is leaving: decide again with leaving
   children counted as eligible. Committing such a decision first releases
   the selection and completes every leave ([§7](#7-leaving)).
4. If there is still none: the decision is none.

Consequences:

- A track-sensitive selection is kept until its track ends, whatever its
  predicate says, unless a child that is not track-sensitive takes over.
- A child that was left is not selected again while its transition runs,
  unless nothing else can play.
- A `single` child is never delivered for two consecutive tracks of the
  switch. With no other child to play, the switch has nothing to deliver
  until another child's track was delivered.
- A selection whose effective source has nothing to deliver is passed over
  like any unavailable child. The switch does not wait for it.

## 5. Handoff

Committing a decision ([frame-assembly.md §3](../../../source/spec/frame-assembly.md#3-commit)):

- **`S` again, at a boundary**: nothing. The selection is `playing`; the
  child's next track comes with its own mark. `on_select` is not called.
- **None**: nothing.
- **Another child `c`**, or `S` by its source ([§4](#4-decisions)): a
  handoff. In the second case the previous selection's leave completes in
  the same commit, before the child is selected again.

A handoff:

1. Make a new presented source for `c` ([§6](#6-presented-sources)).
2. Call `c`'s `on_select` with `starting` = that source, `replay_metadata` =
   `c`'s getter read now, and `ending` = the previous selection's presented
   source when this is a preemption, null otherwise. The result is the
   **effective source**.
3. If the effective source's probe answers `nothing` while `starting`'s does
   not: use `starting` as the effective source and log it.
4. Wake the effective source. Callbacks that `on_select` registered on the
   switch's children, on `starting` or on `ending` are collected; they are
   released with this selection.
5. The previous selection, if any, becomes a leaving entry, cut exactly when
   `ending` was given. Its effective source is released after the new one is
   awake.
6. The selection is `c`, `playing`. Its first sample is entered with `track`
   ([frame-assembly.md §6](../../../source/spec/frame-assembly.md#6-joins)).

`ending` is given only when the previous track was cut: a handoff at a
boundary has nothing to blend.

Each track start the switch delivers fires its track events and sets `last`
to the selected child. Since a track start is delivered only to a request,
these events mean the track is being played.

## 6. Presented sources

The switch never hands a child to script code directly. It makes one
**presented source** per selection.

- It presents the child's id and delegates to the child: fallibility,
  probes, remaining, abort, sync source, effective source, last metadata.
- The first sample it delivers begins a track: a mark is ensured on it.
- It holds no data: what its reader uses is reported to the child, which
  keeps the rest.
- Given as `ending`, it delivers at most the rest of the child's current
  track. When that track ends or is aborted it is exhausted
  ([source-contract.md §7](../../../source/spec/source-contract.md#7-lifecycle)) and records that
  the track ended. The child's next track is never delivered through it.

## 7. Leaving

- A leave **completes** when nothing reads the entry's presented source any
  more. The switch checks at each commit and at the end of each cycle in
  which it delivered.
- On completion the switch calls the child's `on_leave` with `source` = the
  presented source and `preempted`, and drops the entry. The child is
  eligible again.
- Which `on_leave` is called is settled when the child is left, not when the
  leave completes: a child whose profile changes in between (a playlist
  behind a silence of last resort, left while on the silence) is not handled
  by a profile that was not in force when it was cut.
- `preempted` is true when the switch cut into the child's track and that
  track had not ended when the leave completed. It is false when the child
  was left at its track end, when it had nothing to deliver, and when its
  track ran out under the transition.
- When the switch is released: the selection is released and every pending
  `on_leave` runs.

There is at most one leaving entry per child, so the list is bounded by the
number of children. Each handoff produces exactly one `on_leave` for the
child that was left, chained handoffs included.

## 8. Abort

An abort of the switch ([frame-assembly.md §9](../../../source/spec/frame-assembly.md#9-contract-clauses)):

1. is forwarded to the selection's effective source;
2. makes the selection `ended`;
3. drops a kept decision.

The next request makes a boundary decision. Leaving entries are not
affected.

## 9. Other properties

- **Infallible** when some child has an infallible source, is not `single`,
  and has a predicate known to hold without being evaluated. Such a switch
  MUST be ready in every cycle, the cycle where a transition ends included.
- **Exhausted** when every child's source is.
- **Remaining**: the effective source's; 0 with no selection.
- **Effective source**: that of the selection's effective source; the switch
  itself with no selection.
- **Sync source**: by [frame-assembly.md §9](../../../source/spec/frame-assembly.md#9-contract-clauses).

## 10. Failure

Script code that raises is not a major error
([faults.md §0](../../../../spec/faults.md#0-the-rule)): it is logged at
`severe` with its backtrace, at the bounded rate of
[faults.md §4](../../../../spec/faults.md#4-item-faults-escalate), and the
switch goes on.

- A predicate that raises counts as false for that decision. A
  `track_sensitive` or `replay_metadata` getter that raises counts as its
  profile's value.
- An `on_select` that raises, or whose result has a content type the
  switch cannot carry, is treated like a result with nothing to deliver: the
  child is played as it is, what the call registered is released, and the
  attempt is logged. The result of `on_select` is checked before it delivers
  any data.
- An `on_leave` that raises counts as having done nothing: the child is
  left as it would otherwise be.
- Nothing else in a switch raises or aborts
  ([frame-assembly.md §7](../../../source/spec/frame-assembly.md#7-bounds)).

## 11. Logs

Content is specified; wording is free.

- A first selection; a handoff at a boundary; a preemption. Each names the
  child.
- An `on_select` result replaced by `starting`; a handoff undone.

## 12. Seam answers

Clauses of [frame-assembly.md §9](../../../source/spec/frame-assembly.md#9-contract-clauses).

- **C1.** The boundary decision of [§4](#4-decisions). Its commit is
  [§5](#5-handoff).
- **C2.** Yes, at every cycle start while a track plays, by the mid-track
  decision of [§4](#4-decisions). It evaluates nothing when every child is
  track-sensitive. Its commit is a handoff with `ending`.
- **C3.** Always `track end`: every track of a child is a track of the
  switch.
- **C4.** `track`.
- **C5.** The switch wakes its children for its whole life. It wakes each
  effective source at the handoff and releases it when the selection is
  replaced or the switch is released.
- **C6.** [§8](#8-abort), [§9](#9-other-properties).
- **C7.** [§9](#9-other-properties).
