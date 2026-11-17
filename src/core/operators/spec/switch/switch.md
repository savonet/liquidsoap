# Switch (Part A, as-built)

Selects one of its children and streams it. Frame building is
[frame-assembly.md](frame-assembly.md); the defaults of the per-child methods
are [composition.md](composition.md).

## 1. Children

`switch` takes an ordered list of children and one flag.

Each child has:

| Field             | Meaning                                                                |
| ----------------- | ---------------------------------------------------------------------- |
| predicate         | Script function, no argument, returns a boolean.                       |
| source            | The source to play.                                                    |
| `track_sensitive` | Boolean getter.                                                        |
| `replay_metadata` | Boolean getter.                                                        |
| `single`          | Boolean, read once at creation.                                        |
| `on_select`       | `{ending: source?, replay_metadata: bool, starting: source} -> source` |
| `on_leave`        | `{source: source, track_sensitive: bool} -> unit`                      |

A method the child's value does not carry falls back to the profile of the
child's source, resolved at each use
([composition.md §3](composition.md#3-per-source-methods)).

`all_predicates` (default false): when a selection is searched, every
remaining child is also tested after the first match.

## 2. State

| Field          | Meaning                                                               |
| -------------- | --------------------------------------------------------------------- |
| selection      | None, or: child, effective source, proxy, and how to put it to sleep. |
| leaving        | List of (proxy, pending `on_leave` call).                             |
| excluded       | Children already handed out in this cycle.                            |
| last animated  | Tick count at which the switch's cycle last opened. Starts at -1.     |
| resuming       | Boolean, set at each cycle start.                                     |
| switch-wide ts | Whether every child is track-sensitive, set at each cycle start.      |

Per cycle and per child, two settled values: the predicate's result and the
`track_sensitive` getter's result.

`selected()` returns the selected child's source, or null.

## 3. Cycle start

When the switch's cycle opens:

1. `resuming` := the tick count is more than 1 above `last animated`.
2. `last animated` := the tick count.
3. `excluded` := empty.
4. `switch-wide ts` := every child's `track_sensitive` is true. This reads
   every child's getter, once per cycle.

Rationale recorded in the code: a selection only holds while the switch is
animated; a parent that is not playing it does not animate it; when it is
animated again the selection "has to be re-evaluated, and nothing is playing
for a new one to interrupt". Being animated without being read (a child clock
ticked by an operator that reads nothing in that tick) does not count as
quiet.

## 4. Predicates

- Predicates are not evaluated until a selection search needs one.
- The first need in a cycle evaluates, inside one atomic section, **every**
  child's predicate and `track_sensitive` getter. The results hold for the
  cycle.
- A selection that is kept (§6) costs no predicate call when its child is
  track-sensitive.

## 5. Readiness, frames

By [frame-assembly.md](frame-assembly.md) with: `track_sensitive` =
switch-wide ts; `merge` = never; a change of source always begins a track.

So readiness asks `get_source(ok)` when all children are track-sensitive,
`get_source(force)` otherwise; a track ending inside a frame asks
`get_source(after_position(p))`.

## 6. get_source

Given a reselect mode:

**Keep.** If there is a selection, and not `resuming`, and its child is
**still wanted**, and **keep-selected** holds: return its effective source.

- _Still wanted_: the child is track-sensitive, or its predicate holds.
- _Transition in progress_: some leaving proxy is still up.
- _Keep-selected_, during a transition: `after_position` → the effective
  source passes `can_reselect`; `ok`, `force` → yes.
- _Keep-selected_, otherwise: `after_position`, `force` → no; `ok` → the
  effective source is ready.

**Otherwise re-select:**

1. `boundary` := the mode is `after_position`; or there is no selection; or
   `resuming`; or the selection's effective source is not ready.
2. Search (§7), with `force` turned into `ok`.
3. Act on (selection, found):
   - none, none: nothing.
   - some, none: add the selection to `leaving` with `track_sensitive = true`,
     then clear the selection (§10). Every pending `on_leave` runs now.
   - none, child: log; hand off (§8) with no ending.
   - some, same child: nothing; the selection stays as it is.
   - some, other child: log "at track boundary" or "with transition"; hand off
     (§8) with ending = the old proxy unless `boundary`.
4. If there is now a selection whose effective source is ready: add its child
   to `excluded` and return the effective source. Else return none.

## 7. Search

Return the first child, in list order, for which all three hold, tested in
this order:

1. its predicate holds (§4);
2. it **may be selected**;
3. its source is ready.

_May be selected_:

- It is the selected child: it is not `single`, and the selection's effective
  source passes `can_reselect` for the mode.
- Another child is selected: (`boundary`, or the selected child is not
  track-sensitive, or this child is not track-sensitive), and this child is
  not `excluded`.
- Nothing is selected: this child is not `excluded`.

Testing a child's readiness opens that child's cycle
([source-contract.md §1](source-contract.md#1-readiness)).

## 8. Handoff

To select `child`, with an optional `ending`:

1. Make a new **proxy** of the child's source (§9).
2. Call the child's `on_select` with `ending`, `starting` = the proxy and
   `replay_metadata` = the child's getter read now. The result is the
   **effective source**.
3. Callbacks that the call registers on any of the switch's children, on the
   proxy or on `ending` are collected. They are released when this selection
   is put to sleep.
4. Wake the effective source.
5. If there was a selection, add it to `leaving`, with
   `track_sensitive` = (there is no `ending`).
6. Replace the selection. The old effective source is put to sleep after the
   new one is awake, so a proxy held by both stays up throughout.

`ending` is given only when the old selection's track was cut: a handoff at a
boundary has nothing to blend.

## 9. Proxy

A proxy wraps one source for one selection.

- The first frame it gives carries a track mark at 0, added if absent.
- It delegates to the wrapped source: fallibility, readiness, remaining,
  abort track, self-sync, effective source, last metadata and clearing it.
- It does not reset last metadata on track marks.
- It keeps no data: what its reader consumes is reported straight to the
  wrapped source, which keeps the rest.

## 10. Leaving

- After each frame the switch generates: for every leaving entry whose proxy
  is no longer up, call its `on_leave` with `source` = the proxy and the
  recorded `track_sensitive`, and drop the entry.
- When the selection becomes none, or the switch goes to sleep: put the
  selection to sleep and call every pending `on_leave`, whether or not its
  proxy is up.

`track_sensitive` in `on_leave` is the switch's own verdict: true when the
source had nothing left for its track, false when the switch cut into it. A
source that ran out of data without emitting a track mark was not preempted.

## 11. Other properties

- **Fallible**: unless some child has an infallible source, is not `single`,
  and has a predicate that is literally the constant `true`.
- **Remaining**: the effective source's, or 0 with no selection.
- **Abort track**: clear the effective source's last metadata, then forward.
  The selection itself is not changed. Rationale recorded: the metadata of a
  dropped track must not be replayed on re-selection.
- **Effective source**: the selection's effective source's effective source,
  or the switch itself with no selection.
- **Self-sync**: of the selection's effective source.
- A child is handed out at most once per cycle by a re-selection (`excluded`).
  A selection that is kept is not added to `excluded`.

## 12. Failure

- A predicate, getter, `on_select` or `on_leave` that raises propagates out of
  the streaming cycle. Nothing is rolled back: an `on_select` that raises
  leaves the previous selection in place and the new proxy unused.
- The result of `on_select` must have a frame type compatible with the
  switch's; a mismatch raises at the handoff.
- The frame-assembly assertions
  ([frame-assembly.md §4](frame-assembly.md#4-building-the-frame)) abort the
  process path that hits them.

## 13. Logs

`Switch to <id>.` on a first selection; `Switch to <id> at track boundary.` or
`Switch to <id> with transition.` on a change. Level: important.

## 14. Seam answers

Clauses of [frame-assembly.md §7](frame-assembly.md#7-contract-clauses).

- **C1.** [§6](#6-get_source). All three modes are honoured as given, except
  that the search runs with `force` turned into `ok`. A call can evaluate
  predicates, run a handoff and run `on_leave`.
- **C2.** True when every child is track-sensitive in this cycle
  ([§3](#3-cycle-start)).
- **C3.** Never.
- **C4.** Always.
- **C5.** The switch wakes the effective source at the handoff and puts it to
  sleep when the selection is replaced or cleared
  ([§8](#8-handoff), [§10](#10-leaving)). Children are also woken by the
  switch as its sources, for its whole life.
- **C6, C7.** [§11](#11-other-properties).
