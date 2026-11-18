# The script layer, as built (Part A)

What the standard library puts around the operator. `fade.in` and
`fade.out` are not described: only what they are given.

## 1. The `cross` wrapper

Scripts never reach the operator of [cross.md](cross.md) directly. The name
`cross` is a wrapper with the same parameters plus `deduplicate` (default
true). It returns the operator behind two filters, and re-exports
`cross_duration`, `buffered` and `duration`.

**Replayed metadata.** A metadata holding `liq_cross_replayed_metadata` is
the fallback announcement of [cross.md §7](cross.md#7-building-the-compound),
step 8. The wrapper keeps a flag, cleared at every track of the operator and
set by every metadata it lets through:

- the key is always removed;
- a replayed metadata is dropped when the flag is set, delivered otherwise.

So the fallback is heard only when nothing announced the starting track
since the crossing began. This does not depend on `deduplicate`.

**Deduplication.** With `deduplicate`, the result goes through
`metadata.deduplicate`, which drops a metadata equal to the previous one
within a track.

Callbacks registered on the wrapper's result therefore see the filtered
stream; `cross_duration()` is the operator's.

## 2. `cross.simple`

`cross.simple(~fade_in=3., ~fade_out=3., ~initial_fade_in_metadata=[], ~initial_fade_out_metadata=[], ~ending_map, ~starting_map, a, b)`
returns `add(normalize=false, [starting_map(fade.in(b)), ending_map(fade.out(a))])`.

- `fade.out(a)`: sine shape, duration `fade_out`, overrides persisting,
  initial metadata `initial_fade_out_metadata`.
- `fade.in(b)`: sine shape, duration `fade_in`, overrides persisting,
  initial metadata `initial_fade_in_metadata` with `liq_fade_in_delay`
  replaced by the **fade-in delay**:
  1. `liq_fade_in_delay` of `initial_fade_in_metadata` when present;
     otherwise `liq_fade_out_start_next` of `initial_fade_out_metadata`, or
     zero;
  2. plus `duration(a) − duration(b)` when `b` is the shorter, so that both
     end together.

A delayed fade-in plays silence for the delay, then the source, as one
track.

## 3. `crossfade`

`crossfade(~id, ~duration, ~override_duration, ~persist_override, ~width, ~fade_in=3., ~fade_out=3., ~deduplicate=true, s)`
is `cross` with the transition

```
cross.simple(fade_in, fade_out,
             initial_fade_in_metadata = starting.metadata,
             initial_fade_out_metadata = ending.metadata,
             ending.source, starting.source)
```

It logs every pair of both metadata at level 4. The levels are not used.

## 4. Metadata keys

| Key                                                         | Written by                | Read by                                  |
| ----------------------------------------------------------- | ------------------------- | ---------------------------------------- |
| `liq_cross_duration`                                        | autocue, scripts          | `cross`: the default `override_duration` |
| `liq_fade_out`                                              | autocue, scripts, `cross` | `cross`, `fade.out`                      |
| `liq_fade_out_delay`                                        | autocue, scripts, `cross` | `cross`, `fade.out`                      |
| `liq_fade_out_start_next`                                   | autocue, scripts, `cross` | `cross`, `cross.simple`                  |
| `liq_fade_in`                                               | autocue, scripts, `cross` | `cross`, `fade.in`                       |
| `liq_fade_in_delay`                                         | scripts, `cross.simple`   | `cross`, `cross.simple`, `fade.in`       |
| `liq_fade_in_type`, `_curve`, `liq_fade_out_type`, `_curve` | autocue, scripts          | `fade.in`, `fade.out`                    |
| `liq_cross_replayed_metadata`                               | `cross` (operator)        | `cross` (wrapper), which removes it      |

Autocue sets `liq_cross_duration` to `liq_fade_out + liq_fade_out_delay`.
Reason, from the history: the ending track's fade sets the length of the
crossing.

## 5. Plots

`cross.plot` and `autocue.plot` (extra library) record, for the first
crossing, the level of each side over time into two text files and a PNG.
They exist for the tests ([tests.md §9](tests.md#9-curves)).
