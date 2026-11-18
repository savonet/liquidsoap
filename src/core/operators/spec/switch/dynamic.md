# source.dynamic (Part A)

Plays whatever source a script function designates. Segments, decisions,
commits and joins are [frame-assembly.md](frame-assembly.md).

## 1. Parameters

`source.dynamic(~id, ~init, ~track_sensitive, ~infallible, ~self_sync, ~merge, next)`.

| Parameter         | Default | Meaning                                                                                                       |
| ----------------- | ------- | ------------------------------------------------------------------------------------------------------------- |
| `next`            |         | Function, no argument. Returns a source, or null to keep the current one. The source may carry an activation. |
| `init`            | null    | Source to start with.                                                                                         |
| `track_sensitive` | false   | Boolean getter. True: consult `next` at boundaries only.                                                      |
| `merge`           | false   | Boolean getter. True: what comes next continues the current track ([§3](#3-designation)).                     |
| `infallible`      | false   | Declares the result infallible. Not checked by typing.                                                        |
| `self_sync`       | null    | Fixes the sync answer. Null: by the general rule.                                                             |

Methods: `current_source()`; `prepare(s)`, which wakes `s` on behalf of this
operator and returns `s` carrying the activation.

## 2. State

- `current`: none, or a source with the activation that keeps it awake.
- `pending`: none, or a source designated and not yet read, with its
  activation.
- the **statement**: `track` or `continue`, the entry of what comes next.

`current` changes only in a commit and when the operator is released.
`current_source()` returns it.

## 3. Designation

**Decision points.** A boundary: no `current`, `current`'s track ended or was
aborted, or `current`'s probe answers `nothing`. Mid-track: the start of a
cycle while `current`'s track plays, when `track_sensitive` is false.

**Consult.** At a decision point the operator calls `next()`, at most once
for that point, under its own lock:

- null, or `current`: the decision is `current` if its probe does not answer
  `nothing`; else none.
- `pending`: unchanged.
- another source `s`: `s` becomes `pending`. It is woken, unless it came
  with an activation, which the operator then owns. A previous `pending` is
  released. The decision is `s` if its probe does not answer `nothing`; else
  none, and `s` stays `pending`.

A source whose content type or clock cannot be made the operator's is
refused: it is released, the decision is as for null, and the error is
reported under the clock's source error rule.

**The one probe with an effect.** A probe of the operator at a decision
point runs the consult: script code runs and a source may be woken. This is
the exception named in [source-contract.md §2](source-contract.md#2-probes).
It is bounded: one call per decision point. Therefore:

- `next` MUST accept being called without its answer being played. A consult
  commits nothing: no source is entered, no track event fires.
- `next` MUST NOT be called anywhere else: not while a track plays on a
  track-sensitive operator, not at wake-up.

**Statement.** The `merge` getter is read when `current`'s track ends, at the
first designation, and at a mid-track decision that changes source. True
gives `pause` and the entry `continue`; false gives `track end` and the entry
`track`. It applies to whatever is read next, the same source or another.

**Commit.** When the reader requests past the decision point and the
decision is `pending`: it becomes `current`, entered by the statement; the
previous `current` is released after that. A decision that keeps `current`
commits nothing.

**Same source again.** A source that is `current` or `pending` and is
designated again is neither released nor woken again: it continues. When an
answer carries an activation other than the one held, the extra one is
released. A script that wants a source to survive between designations holds
it with `prepare` and releases that activation itself when it will not
return the source.

**Bounds.** By [frame-assembly.md §7](frame-assembly.md#7-bounds): a
function that returns the same source with nothing more to give, or that
alternates between two sources on every call, ends the frame. It cannot
reach an error.

## 4. Lifecycle

- At wake-up: `init`, when given, becomes `pending` and is woken.
- At release: `current` and `pending` are released and forgotten.
- Each designated source is given the operator's content type and clock
  before it delivers any data.

## 5. Other properties

- **Remaining**: `current`'s, or unknown.
- **Abort track**: forwarded to `current`; the operator is at a boundary.
  `pending` is kept.
- **Effective source**: `current`'s, or the operator itself.
- **Sync source**: fixed by the parameter, or by
  [frame-assembly.md §9](frame-assembly.md#9-contract-clauses).

## 6. Seam answers

- **C1.** The consult of [§3](#3-designation). The commit makes `pending`
  current and releases the previous source.
- **C2.** When `track_sensitive` is false: a consult at every cycle start.
  The getter is read at each cycle start.
- **C3.** The statement of [§3](#3-designation).
- **C4.** The statement, read at that designation.
- **C5.** The operator wakes what `next` returns, or adopts its activation,
  and releases it when it is replaced, in the cycle of the replacement.
- **C6.** [§5](#5-other-properties).
- **C7.** Fallible unless declared `infallible`. When declared and a request
  finds nothing to deliver, the request fails with a script error saying the
  operator could not prepare a source in time. A probe never fails. Never
  exhausted.
