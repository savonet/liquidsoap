# Fades in a crossing: a proposal

**Status: adopted by the maintainer** ([brief.md §6](brief.md#6-rulings)).
The normative text is
[script-layer.md §2 and §3](script-layer.md#2-the-plan-of-a-crossing); this
file keeps the reasons. It was written from the documented interface only.
What was built: the plan and its application. `fade.in` and `fade.out`
themselves are unchanged and keep reading their track's keys when used on
their own; a crossing no longer goes through them.

Goals, set by the maintainer: easy for a plain overlap of two tracks,
advanced use possible, a design as simple and clear as possible.

## 1. What makes it hard today

One rule, where each fade sits in a crossing, is owned by three places:

- the operator rewrites three keys in the metadata it hands over
  ([script-layer.md §2](script-layer.md#2-the-plan-of-a-crossing));
- `cross.simple` derives a fade-in delay from both metadata and both
  lengths ([script-layer.md §2](script-layer.md#2-crosssimple));
- `fade.in` and `fade.out` are given "initial metadata" and persisting
  overrides, to read the same keys again and place themselves.

The values travel as strings in metadata between the three, so a script
that wants one thing different has to know all of them. Five keys are each
relative to something else: the end of the track, the start of a window,
the other track.

## 2. Proposal

**One owner: a plan.** A pure script function, here `cross.plan`, takes the
two arguments of the transition and returns numbers, in seconds from the
start of the transition:

```
cross.plan(~fade_in=3., ~fade_out=3., ending, starting) =
  { duration,                       # length of the transition
    ending   = { fade_start, fade_duration, type, curve },
    starting = { start, fade_duration, type, curve } }
```

It is the only reader of the `liq_fade_*` keys in a crossing. It holds the
arithmetic of [script-layer.md §2](script-layer.md#2-the-plan-of-a-crossing) and
the delay of [script-layer.md §2](script-layer.md#2-crosssimple), computed
from the keys and from the exact lengths of the two sources, which they
report. The keys keep their names and meanings: what autocue and partners
write is read as before.

**Plain fades.** `fade.in` and `fade.out` take what they need as arguments
and read no metadata to be placed in a crossing:

```
fade.in(~duration, ~delay=0., ~type, ~curve, s)
fade.out(~duration, ~delay=0., ~type, ~curve, s)
```

`fade.out` over a finite source ends on its last sample; `delay` is what
plays unfaded before it. "Initial metadata" and persisting overrides have
no use left in a crossing. Whether a standalone fade still reads a track's
own `liq_fade_*` keys is a separate, opt-in feature of the fades, read from
the stream they play and nowhere else.

**Applying a plan.** `cross.apply(plan, ending, starting)` is the sum of the
two faded sources as the plan places them. Then:

```
# plain overlap, no fade
cross(fun (a, b) -> add(normalize=false, [a.source, b.source]), s)

# the usual crossfade
crossfade(s)        # = cross(fun (a, b) -> cross.apply(cross.plan(a, b), a, b), s)

# advanced: change one thing
cross(fun (a, b) -> begin
  p = cross.plan(a, b)
  cross.apply(p.{starting = p.starting.{fade_duration = 0.}}, a, b)
end, s)
```

`cross.simple` is `cross.apply` of `cross.plan`.

## 3. What it changes in this specification, if confirmed

- [script-layer.md §2](script-layer.md#2-the-plan-of-a-crossing) leaves the
  operator: the `metadata` fields are handed as the tracks carried them.
  The same arithmetic is specified once, for the plan.
- The operator keeps one reading of the keys: the fade-in window, to know
  how much of the next track to buffer
  ([cross.md §4](cross.md#4-reading-the-child)).
- [script-layer.md §2 and §3](script-layer.md#2-crosssimple) are restated
  on the plan.
- The checks on adjusted values
  ([conformance.md §5](conformance.md#5-the-transitions-arguments)) become
  checks on the plan's numbers, which is easier: they are floats, not
  strings in a metadata.
- Scripts that read the adjusted keys from `ending.metadata` or
  `starting.metadata` in their own transition, or pass
  `initial_fade_*_metadata`, have to move to the plan.

## 4. Not known

- What else in `fade.in` and `fade.out` exists for crossings. The proposal
  removes the need for it, not the code: that is for whoever may read it.
- The case `liq_fade_out` equal to the window with a delay present: the
  adjustment leaves the delay as it was, so delay plus fade exceed the
  window. A plan would give `fade_start = window − fade_duration` in every
  case. Left as it is in the normative text, the keys being stable.
