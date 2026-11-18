# The script layer (Part A)

What the standard library puts around the operator. `fade.in` and
`fade.out` are not specified here: only what they are given.

## 1. The `cross` wrapper

Scripts reach the operator of [cross.md](cross.md) through a wrapper of the
same name, with the same parameters plus `deduplicate` (default true). It
re-exports the operator's methods.

**Deduplication.** With `deduplicate`, the result goes through
`metadata.deduplicate`, which drops a metadata equal to the previous one
within a track. It MUST NOT compare across a track mark: a track repeating
the previous one's metadata is announced.

The wrapper does nothing else. Announcing the next track when a transition
did not is the operator's rule ([cross.md §9](cross.md#9-metadata)), and
holds with `deduplicate` on or off.

Callbacks registered on the wrapper's result see the filtered stream.

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
track ([helpers.md](../switch/helpers.md)).

The result MUST declare exhaustion once both `a` and `b` have, so that the
transition ends ([transition.md §4](transition.md#4-end-of-the-transition)).

## 3. `crossfade`

`crossfade(~id, ~duration, ~override_duration, ~persist_override, ~width, ~fade_in=3., ~fade_out=3., ~deduplicate=true, s)`
is `cross` with the transition

```
cross.simple(fade_in, fade_out,
             initial_fade_in_metadata = starting.metadata,
             initial_fade_out_metadata = ending.metadata,
             ending.source, starting.source)
```

It SHOULD log both metadata. The levels are not used.

## 4. Metadata keys

These keys are an interface with the producers of cue data, autocue first.
Their names and meanings are fixed.

| Key                                                         | Written by                | Read by                                  |
| ----------------------------------------------------------- | ------------------------- | ---------------------------------------- |
| `liq_cross_duration`                                        | autocue, scripts          | `cross`: the default `override_duration` |
| `liq_fade_out`                                              | autocue, scripts, `cross` | `cross`, `fade.out`                      |
| `liq_fade_out_delay`                                        | autocue, scripts, `cross` | `cross`, `fade.out`                      |
| `liq_fade_out_start_next`                                   | autocue, scripts, `cross` | `cross`, `cross.simple`                  |
| `liq_fade_in`                                               | autocue, scripts, `cross` | `cross`, `fade.in`                       |
| `liq_fade_in_delay`                                         | scripts, `cross.simple`   | `cross`, `cross.simple`, `fade.in`       |
| `liq_fade_in_type`, `_curve`, `liq_fade_out_type`, `_curve` | autocue, scripts          | `fade.in`, `fade.out`                    |

`cross` writes only into the copies it hands the transition function
([transition.md §2](transition.md#2-metadata-adjustments)): the stream it
delivers carries the keys as the child gave them.

Autocue sets `liq_cross_duration` to `liq_fade_out + liq_fade_out_delay`:
the ending track's fade sets the length of the crossing.

## 5. Plots

`cross.plot` and `autocue.plot` (extra library) record, for the first
crossing, the level of each side over time into two text files and an
image. A recording SHOULD be indexed by time, not by request, so that it
does not move with the frame size.
