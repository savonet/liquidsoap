# The script layer (Part A)

What the standard library puts around the operator. `fade.in` and
`fade.out` are not specified here: only what they are given. Used on their
own they follow their arguments; they read a track's `liq_fade_*` keys only
with `allow_metadata_override=true`, and keep what a track changed for the
tracks that follow only with `persist_overrides=true` as well.

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

## 2. The plan of a crossing

`cross.plan(~fade_in=3., ~fade_out=3., ending, starting)` takes the two
arguments of the transition function and returns where each fade sits, in
seconds from the start of the transition. It reads; it builds no source and
changes nothing.

```
{ duration,
  ending   = { fade_start, fade_duration, type, curve },
  starting = { start, fade_duration, type, curve } }
```

It is the only reader of the fade keys in a crossing.

Rationale: where each fade sits in a crossing used to be computed in three
places (the operator rewriting keys, `cross.simple`, and the fades reading
the keys again), passing values as strings in metadata, five of them each
relative to something else. One function that returns numbers lets a
script change one thing by changing one number of the plan.

With `x` and `s` the exact lengths of the ending and starting sources, as
they report them, and a key counting as absent when it is missing or not a
float (which SHOULD be logged). Under `cross` both lengths are known; a
source of unknown length, handed to the plan outside of it, counts as long as
its fade (`out` or `in` below).

**Ending side.** `out` ← `liq_fade_out`, or `fade_out`.

- `fade_duration` ← `min(out, x)`;
- `fade_start` ← `x − fade_duration`: the fade reaches silence on the last
  sample of the ending source, whatever `liq_fade_out_delay` says;
- `type` ← `liq_fade_out_type`, or `"sin"`; `curve` ← `liq_fade_out_curve`,
  or none.

**Starting side.** `in` ← `liq_fade_in`, or `fade_in`.

- `fade_duration` ← `min(in, s)`;
- `start`, where the starting source begins to play, is the first of:
  1. `liq_fade_in_delay` of the starting track;
  2. `liq_fade_out_start_next` of the ending track, moved to the window
     held: `max(0, start_next + x − out − delay)`, with `delay` the ending
     track's `liq_fade_out_delay`, or zero;
  3. zero;

  plus `x − s` when the starting source is the shorter, so that both end
  together;

- `type` ← `liq_fade_in_type`, or `"sin"`; `curve` ← `liq_fade_in_curve`, or
  none.

`duration` ← `max(x, start + s)`.

Rationale. `liq_fade_out_start_next` is relative to the start of a window of
`delay + out` that ends where the track ends; when the window held is `x`,
the value moves so that it names the same place in the track. One function
owns this arithmetic: a script that wants one thing different changes one
number of the plan.

**Examples.** Ending track with `liq_fade_out = "3."`,
`liq_fade_out_start_next = "1."`; next track with `liq_fade_in = "7."`,
`liq_fade_in_delay = "1."`; sources of 5 s and 8 s:

```
ending   = { fade_start = 2., fade_duration = 3. }
starting = { start = 1., fade_duration = 7. }
duration = 9.
```

Ending track with `liq_fade_out = "3."`, `liq_fade_out_delay = "1."`,
`liq_fade_out_start_next = "2.5"`, no fade-in delay; sources of 2 s and 2 s:

```
ending   = { fade_start = 0., fade_duration = 2. }
starting = { start = 0.5, fade_duration = 2. }
duration = 2.5
```

## 3. Applying a plan

`cross.apply(~ending_map, ~starting_map, plan, ending, starting)` returns
`add(normalize=false, [starting_map(b), ending_map(a)])`, with:

- `a`: the ending source, at full level for `fade_start`, then faded to
  silence over `fade_duration` with the plan's shape;
- `b`: `start` of silence, then the starting source, faded in over
  `fade_duration` with the plan's shape. The silence carries no mark.

Neither fade reads metadata. The result declares exhaustion once both
sources have, so that the transition ends
([transition.md §4](transition.md#4-end-of-the-transition)).

`cross.simple(~fade_in=3., ~fade_out=3., ~initial_fade_in_metadata=[], ~initial_fade_out_metadata=[], ~ending_map, ~starting_map, a, b)`,
over two sources and their metadata, is `cross.apply` of `cross.plan`.

`crossfade(~id, ~duration, ~override_duration, ~persist_override, ~width, ~fade_in=3., ~fade_out=3., ~deduplicate=true, s)`
is `cross` with the transition
`cross.apply(cross.plan(fade_in, fade_out, ending, starting), ending, starting)`.
It SHOULD log both metadata. The levels are not used.

## 4. Metadata keys

These keys are an interface with the producers of cue data, autocue first.
Their names and meanings are fixed.

| Key                                                         | Written by       | Read by                                  |
| ----------------------------------------------------------- | ---------------- | ---------------------------------------- |
| `liq_cross_duration`                                        | autocue, scripts | `cross`: the default `override_duration` |
| `liq_fade_out`                                              | autocue, scripts | `cross.plan`                             |
| `liq_fade_out_delay`                                        | autocue, scripts | `cross.plan`                             |
| `liq_fade_out_start_next`                                   | autocue, scripts | `cross.plan`                             |
| `liq_fade_in`                                               | autocue, scripts | `cross`, `cross.plan`                    |
| `liq_fade_in_delay`                                         | scripts          | `cross`, `cross.plan`                    |
| `liq_fade_in_type`, `_curve`, `liq_fade_out_type`, `_curve` | autocue, scripts | `cross.plan`                             |

Nothing writes them in a crossing: `cross` hands them to the transition
function and delivers them in its stream as the child gave them. `cross`
reads the two fade-in keys only for the fade-in window
([cross.md §4](cross.md#4-reading-the-child)). `fade.in` and `fade.out`,
used on their own, read their track's keys from the stream they play; that
is outside this specification.

Autocue sets `liq_cross_duration` to `liq_fade_out + liq_fade_out_delay`:
the ending track's fade sets the length of the crossing.

## 5. Plots

`cross.plot` and `autocue.plot` (extra library) record, for the first
crossing, the level of each side over time into two text files and an
image. A recording SHOULD be indexed by time, not by request, so that it
does not move with the frame size.
