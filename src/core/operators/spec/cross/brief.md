# Brief for the cross extraction

Framing for the sessions that extract and normalise the specification of
`cross`. It applies to passes 1 and 2. The rebuild has its own note,
[pass3-handoff.md](pass3-handoff.md).

## 1. What to extract from

Branch `switch-rewrite`, not `main`. `cross` already changed there, and the
as-built pass describes it as it stands on that branch:

- it is a user of the rebuilt frame-assembly seam: `get_source ~boundary`,
  `next_entry`, one span per streaming cycle;
- the sequences it builds use the rebuilt `sequence`;
- its buffer sources declare exhaustion, except the one still being filled;
- it puts what it wakes in its own clock.

The contract it has to meet is the switch specification, in
[../switch/](../switch/README.md). Read `source-contract.md` and
`frame-assembly.md` there before the normative pass; `cross-seam.md` holds
what was already ruled for `cross` as a user of the seam.

## 2. Principles that already bind

They were set by the maintainer during the switch rebuild and are not open
again here.

- **Explicit signals.** Nothing is deduced from being probed, read or
  ticked. The party that decides tells the parties it affects.
- **No commitment before the consumer takes data.** A decision is made when
  a source is probed and acted on when it is read.
- **A streaming cycle is static.** What a source answers in a cycle does not
  change until the cycle is over. A source that answers ready delivers at
  once; nothing waits inside a cycle.
- **Infallibility is never broken once established.** A source found
  infallible at start that leaves a frame short is a failure, raised loudly,
  not a glitch.
- **A track mark sits on the logical start of content.** Filler carries
  none. A `merge` the script asks for is honoured.
- **A skip is a request.** It is carried out once the running cycle is over.

## 3. What the normative pass has to settle

From [../switch/cross-seam.md §4](../switch/cross-seam.md#4-open):

1. How the buffer stays one crossfade duration ahead when the child is read
   only while a request is served.
2. A track shorter than the crossfade duration, and a starting track that
   ends inside the transition.
3. Which metadata the mark of a crossing carries, and how the rest of the
   starting track is announced.
4. The effect of an abort during a transition.

Added by the switch rebuild:

5. **Probes that act.** `cross` reads its child, runs the transition function
   and builds sources when it is probed. `switch`, `sequence` and
   `source.dynamic` no longer do. State what `cross` may do at a probe and
   what waits for the read. Opening what is needed to answer truthfully may
   happen at the probe; what commits a track may not.
6. **Reading ahead.** `cross` is its child's consumer through a child clock:
   the child commits relative to what `cross` buffers, not to what is heard.
   State what that means for a child that is a selector: which track events,
   policy counts and `on_select` calls happen a crossfade ahead of the
   listener, and whether any of them must not.
7. **A track exactly as long as the crossfade.** It behaves differently from
   one a frame longer. `tests/streams/cross-override.liq` asked for the extra
   frame explicitly rather than settle it.
8. **What the transition returns.** It is built by script code out of the
   two buffers. State what it must declare for `cross` to know it is over
   (track end, exhaustion), and what `cross` does with one that never says
   so. Today its internal sequences move on when an item has nothing to
   deliver and their last item can play.
9. **Fallibility and readiness.** `cross` is always fallible. State when it
   is ready in each phase, so that ready always means delivering now.
10. **Sync source.** The child's is tracked by the child clock; `cross`
    reports none on its behalf. State what `cross` reports for the
    transition's sources, which run in its own clock.
11. **Composition.** State what profile `cross` resolves to in each phase,
    for a switch above it.

## 4. Scenarios to turn into checks

From the tests ([tests.md](tests.md)) and the findings
([findings.md](findings.md)). Known before the as-built pass:

- A crossing puts one mark, on the first sample of the transition, and none
  where the transition ends.
- The audio of a crossfaded sequence of files is the same, sample for
  sample, wherever frames are cut (`tests/regression/GH4828.liq` checks a
  checksum per frame, which is not that).
- A sequence under a crossfade ends: the output stops when its last track
  did (`tests/regression/GH3239-2.liq`).
- The transition function receives the two sources, their levels and their
  metadata (`tests/streams/cross-transition-arguments.liq`).

Added by the as-built pass:

- Each track is announced once, in order, whatever the transition keeps of
  its two sources: both, the ending one only, neither (tests.md §2).
- A track's override governs that track's end and no other, including when
  the track is shorter than the duration (F4) or exactly as long (F17).
- A skip is heard at once in either phase: nothing of the skipped track
  plays after it (F1).
- The last track ends the same way whether or not its length is a whole
  number of frames: no transition against nothing (F2).
- A starting track shorter than the window: where the mark falls and which
  metadata it carries (F3). A first track shorter than the duration still
  begins a track (F6).
- A track exactly as long as the window is crossed at its end like one a
  frame longer (F17).
- A transition that never ends, and one that ends early: what bounds it
  (F13).
- The same script gives the same audio with `persist_override` and a
  `duration` function that changes (F10).
- `cross` put to sleep and woken resumes cleanly (F11).
- Callbacks a transition registers do not pile up over crossings
  (tests.md §8).

Every entry of [known-complexity.md](known-complexity.md) carries a check,
under "How to tell". Pass 2 answers each with a rule; pass 3 turns each
into a conformance check.

## 5. Constraints

- Target: OCaml, in this tree, on top of `switch-rewrite`.
- The maintainer does passes 1 and 2. Pass 2 runs from the documents of
  pass 1 only.
- Pass 3 keeps the backbone of the existing code and cuts it down to the
  specification, as was done for `switch`.
