# Findings

What reading `cross` on `switch-rewrite` (75acc5e67) turned up. Each is
**checked** (seen in a run of that build, script in hand) or **read only**
(a suspect: to be refuted or confirmed before anything is built on it).
None of this is in the as-built text except as a reference.

Nothing was run against a live input, a soundcard or a paced clock: every
run used finite tones on an unpaced clock.

## Defects

The code contradicts itself, its documentation or a rule it is bound by.

- **F1. An abort in `before` plays on.** The buffered end of the aborted
  track, about one cross duration, is still delivered, then the next track
  starts without a crossing. The contract says aborted data is discarded.
  _Checked_: `duration=3`, skip at 5.0 s, next track at 8.0 s. In `after`
  the same skip takes effect at once.
- **F2. The end of the stream depends on frame alignment.** A last track
  whose length is not a multiple of the frame ends in a short piece with no
  mark, which `cross` takes for a track end: the transition is called with
  an empty starting source and a level that is not a number, and a track
  begins with no metadata. A last track of a whole number of frames gets
  none of this. _Checked_: tones of 3.00 s give 2 transitions, of 3.01 s
  give 3.
- **F3. With a head, the crossing's mark is not on the transition.** When
  the starting track gave less than the ending store held, the compound
  begins with the rest of the ending track, and the mark and track event
  land there, with no metadata; the starting track's metadata follows later
  as plain metadata. [cross-seam.md](../switch/cross-seam.md) C3 and the
  brief say the mark is on the first sample of the transition. _Checked_:
  6 s, 1 s, 6 s tracks, `duration=3`: track event at 3.02 with no title,
  title `b` at 5.0 as metadata only.
- **F4. An override is read a second time from the output.** `cross`
  applies the override of every metadata it delivers. An ending track
  shorter than the duration has its metadata inside the transition, so its
  override is applied again after the reset and governs the end of the
  starting track. _Checked_: a 1.5 s track carrying an override of 2 under
  `duration=1`: the next crossing, of a track with no override, is 1.98 s
  long.
- **F5. `width=0.` fails at start**, with an index error in the level
  analysis. _Checked_.
- **F6. A first track shorter than the duration starts unmarked.** The
  stream begins in the compound, entered as is, so no track begins unless
  the transition source carries a mark. _Checked_: no track event at 0.0
  with a sum transition.
- **F7. The override is processed twice in a row** for every metadata: the
  body is duplicated. Visible as a doubled log line. _Read only._
- **F8. The log for a shortened `liq_fade_in` prints a negative amount.**
  _Read only._
- **F9. The level windows are approximate.** The ending window's slot
  arithmetic is exact only for pieces of equal size dividing the width; a
  short piece shifts the slots and leaves values that are never subtracted
  until the next crossing. The starting window compares a length in ticks
  with a width in audio samples. _Read only._
- **F10. With `persist_override`, a `duration` function is read once**, at
  creation, since only a reset reads it. _Checked_.
- **F11. Sleep leaves the designated source behind.** The buffer source or
  compound stays awake and designated; on wake the stores are replaced, so a
  buffer source in `before` is left on a store nothing fills: its leftover
  plays, then `cross` is not ready until the track being buffered ends.
  Transition callbacks are not released either. _Read only._

## Asymmetries

`cross` against the rules its siblings on the seam now meet.

- **F12. The probe acts.** The child is read, the transition function is
  run, sources are built, woken and released, and callbacks released, all
  at the probe. `switch`, `sequence` and `source.dynamic` decide at the
  probe and commit at the read. The first top up after `idle` and after
  every compound reads a whole cross duration from the child in one probe.
  _Read, and visible in every run: the transition's prints precede the
  track event._
- **F13. A transition source that never ends is played for ever**, and the
  child is never read again. Nothing bounds a transition. _Checked_:
  a transition returning an endless tone, still playing at 30 s, the child's
  third track never started.
- **F14. No end is stated before a crossing.** The segment before the
  compound ends `full`; the next begins with a mark. A reader learns of the
  boundary only from the mark, which costs a selector above `cross` an empty
  cycle. `cross` never states `late` either: a short frame is `pause`.
  _Read only._
- **F15. Composition.** `cross` resolves to its child's profile before it
  starts and to `live` from then on, because a buffer source is a leaf.
  _Checked_: `file` before start, `live` in `before` and in `after`.
- **F16. Sync source.** `cross` reports its child's answer as its own.
  [cross-seam.md](../switch/cross-seam.md) C6 and the brief say it reports
  none. The creation check makes that answer "none" at first; a child whose
  answer changes later is relayed. Nothing is reported for the transition's
  sources, which run in the clock of `cross`. _Read only._
- **F20. Exhaustion is never declared**, though its buffer sources declare
  theirs. A `cross` whose child is exhausted and whose stores are empty is
  only not ready. _Read only._

## Gaps

Nothing states or enforces what the system relies on.

- **F17. A track exactly as long as what was buffered of it is not
  crossed at its end.** Its end is not seen while it is the starting track;
  the buffer that follows the compound starts on the next track's mark,
  which `first` lets through. That track ends with no transition, and with
  no reset of an override. One frame longer and it is crossed. _Checked_ for
  the missing crossing (3 s track, `duration=3`); _read only_ for the
  reset.
- **F18. Marks after the first sample of a first piece are not looked
  at.** A track shorter than a frame at the start of a buffer goes
  unnoticed. _Read only._
- **F19. The ending metadata is the last one seen.** A metadata update in
  the middle of a track replaces the track's initial metadata whole, fade
  keys included; and the ending metadata of a crossing starts as the
  adjusted starting metadata of the previous one. _Read only._
- **F21. A child that is late inside a track looks like a track end.** Any
  piece shorter than a frame ends the ending track. _Read only_; F2 is the
  one case seen.
- **F22. Remaining time in `after` is the compound's**: with a tail, that
  of the transition source alone, not of the track. _Read only._
- **F23. The fallback announcement exists only with a tail.** A transition
  that drops the starting source when all of its buffered data went to the
  transition leaves the track unannounced. _Read only._
- **F24. The reader of the child is one whole crossing ahead.** Track and
  metadata events of the child, and every commitment below it, happen when
  data enters the intake: a cross duration before it is heard, and for the
  start of a track, as soon as the end of the previous one is found.
  Nothing says which of these may run ahead. _Checked_: the child's track
  event for `b` precedes the transition call.

## Tests

- **F25.** The sample check compares one checksum per frame
  ([tests.md §6](tests.md#6-audio)): it moves with frame boundaries.
  Already in the brief.
- **F26.** [`streams/crossfade`] passes or does nothing: it never fails by
  itself. _Read only._

## To verify

- Whether a mark sitting exactly at the end of a piece is returned with it
  or left for the next one. The description assumes the next one.
- What ends a `fade.in`/`fade.out` sum in a compound with a tail: declared
  exhaustion, or the rule that passes over an item that is not ready.
- The entry `track` after a compound whose last segment stated `track end`
  ([cross.md §8](cross.md#8-delivery)): read, never produced in a run. A
  finite tone as transition, with a tail, states `pause`.
- The float syntax accepted for overrides and fade keys is the host
  language's.
