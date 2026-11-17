# source.dynamic (Part A, as-built)

Plays whatever source a script function designates. Frame building is
[frame-assembly.md](frame-assembly.md).

## 1. Parameters

`source.dynamic(~id, ~init, ~track_sensitive, ~infallible, ~self_sync, ~merge, next)`.

| Parameter         | Default | Meaning                                                                                                         |
| ----------------- | ------- | --------------------------------------------------------------------------------------------------------------- |
| `next`            |         | Function, no argument. Returns a source, or null to keep the current one. The source may carry an `activation`. |
| `init`            | null    | Source to start with.                                                                                           |
| `track_sensitive` | false   | Boolean getter. Consult `next` only at track boundaries.                                                        |
| `merge`           | false   | Boolean getter. Merge at joins.                                                                                 |
| `infallible`      | false   | Declares the result infallible. Not checked by typing.                                                          |
| `self_sync`       | null    | Fixes the self-sync answer. Null: that of the current source.                                                   |

Methods: `current_source()`; `prepare(s)`, which wakes `s` on behalf of this
operator and returns `s` carrying the activation.

## 2. State

`current`: none, or a source with the activation that keeps it awake.

## 3. Designation

`get_source(mode)`:

- If there is a `current`, the mode is `ok`, and `current` passes
  `can_reselect`: return it. `next` is not called.
- Otherwise **consult** (below). That covers: no `current`; `force`;
  `after_position`; `current` not ready.

**Consult**, under the operator's lock:

1. Call `next()`.
2. - Null, and `current` passes `can_reselect` (with `force` as `ok`): return
     `current`.
   - The same source as `current`: return it if it passes `can_reselect`
     (with `force` as `ok`); else **no source**.
   - Another source: **exchange** to it.
   - Anything else: **no source**.

**Exchange** to `s`: wake `s`, unless it came with an activation, which is
then used; make it `current`; put the previous `current` to sleep. Return `s`
if it is ready, else **no source**. `s` stays `current` either way.

When `next` returns `current` with an activation that is not the one held,
that extra activation is put to sleep.

**No source**: when `infallible`, raise a script error saying the operator
could not prepare a source in time; otherwise none.

So `next` is called: on every cycle when not track-sensitive; at each track
boundary of `current`; whenever `current` is not ready; and once at wake-up
(§4).

## 4. Lifecycle

- At wake-up: if there is no `current` and `init` was given, exchange to
  `init`. Then `get_source(force)` is called once.
- At sleep: `current` is put to sleep and forgotten.
- Each new source is given the operator's frame type and clock when it is
  woken.

## 5. Other properties

- **Remaining**: `current`'s, or unknown.
- **Abort track**: forwarded to `current`.
- **Effective source**: `current`'s, or the operator itself.
- **Self-sync**: fixed by the parameter, or `current`'s.

## 6. Seam answers

- **C1.** §3. `force` is honoured as "consult"; inside the consult it is
  treated as `ok`. A call can run script code, wake and put sources to sleep,
  and raise.
- **C2.** The getter, read at each readiness.
- **C3.** The getter, read at each join.
- **C4.** Always.
- **C5.** The operator wakes what `next` returns (or adopts its activation)
  and puts the previous one to sleep after the new one is awake.
- **C6.** §5.
- **C7.** Fallible unless declared `infallible`.

## 7. Built on it

The default handoff ([composition.md §4](composition.md#4-default-handoff)):
track-sensitive, `next` returns the mix while it is available and then the
starting source, `merge` true for the one join from mix to starting source.
Also: request-driven sources, `prepend`, video and fade helpers. Those are
outside this spec.
