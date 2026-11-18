# Conformance checks

The checks an implementation of `cross` is run against, by principle, and
those of the [child-clock seam](../../../source/spec/child-clock.md). Each
names what is **binding**. Unless said otherwise, tone frequencies, track
lengths, ids, log wording and wall-clock timing are incidental. Entries
marked K are the situations of [known-complexity.md](known-complexity.md);
entries marked S are the [scenarios](#scenarios) below. The checks of the
seam and of the source contract are
[../switch/conformance.md](../switch/conformance.md) and apply to `cross`
as to any user. Every check follows
[testing.md](../../../../spec/testing.md).

## Scenarios

The scenarios the maintainer set as binding. Each is a check.

- A crossing puts one mark, on the first sample of the transition, and none
  where the transition ends.
- The audio of a crossfaded sequence of files is the same, sample for
  sample, wherever frames are cut.
- A sequence under a crossfade ends: the output stops when its last track
  did.
- The transition function receives the two sources, their levels and their
  metadata.
- Each track is announced once, in order, whatever the transition keeps of
  its two sources: both, the ending one only, neither.
- A track's override governs that track's end and no other, including when
  the track is shorter than the duration or exactly as long.
- A skip is heard at once in either phase: nothing of the skipped track
  plays after it.
- The last track ends the same way whether or not its length is a whole
  number of frames: no transition against nothing.
- A starting track shorter than the window: where the mark falls and which
  metadata it carries. A first track shorter than the duration still begins
  a track.
- A track exactly as long as the window is crossed at its end like one a
  sample longer.
- A transition that never ends, and one that ends early: what bounds it.
- The same script gives the same audio with `persist_override` and a
  `duration` function that changes.
- `cross` put to sleep and woken resumes cleanly.
- Callbacks a transition registers do not pile up over crossings.

## Doubles a harness needs

- A finite tone of exact length, in samples, that ends its track where it
  ends; lengths that are a whole number of frames, and one sample more or
  less.
- A way to stamp metadata on the first sample of a track, and to insert
  one in the middle of a track.
- A sequence of sources; a child that is exhausted after its last track.
- A gate: a child that is not available for a chosen time between two
  tracks, and one that is late for a chosen time inside a track.
- A selector as child, with counters of its selections and track events.
- A request source reading one file, and a child that loads a new item per
  track, for the skip checks.
- A self-paced input double, for the creation check.
- Observation points on the output of `cross`: per track event with its
  metadata and sample position, per metadata, per segment with its end
  reason; the written samples. On the child: its track events with the
  position `cross` had delivered when they fired. Inside the transition:
  its arguments, the remaining time of both sources, the number of
  callbacks registered on the child.
- An unpaced clock; a clock ticked by hand for the checks that count
  cycles; two frame sizes.

## 1. Frame independence

- **K10, S.** The same script, run with two frame sizes and with track
  lengths that are and are not whole numbers of frames. Binding: the same
  samples, the same number of transition calls, the same track events and
  metadata at the same sample positions.
- **S.** The audio of a crossfaded sequence of files is the same, sample
  for sample, in two runs and with two frame sizes.
- **S.** The last track ends the same way whether or not its length is a
  whole number of frames. Binding: no transition is called for it; no
  track event after its start; the output stops when its last sample was
  delivered.
- A child that is late inside a track, at any offset. Binding: no
  transition is called there, no track event; the samples are those of a
  run with no delay.

## 2. Tracks and marks

- **K5, S.** Over several crossings with a mix transition: one track event
  per child track, each with that track's metadata, on the first sample of
  the transition. Binding: the count, the metadata, the sample position.
  Repeated with the crossing point on a request edge and off it.
- **S.** No mark where the transition gives way to the track. Binding: no
  track event at that sample; the segment before it ends `pause`.
- The segment that reaches a crossing point ends `track end`. Binding: the
  reason, whether or not the request was filled.
- **S.** A next track shorter than the window: the rest of the ending track
  plays first, with no mark; the mark is on the first sample of the
  transition, with the next track's metadata.
- **S.** A first track shorter than the duration begins a track on the
  first sample of the stream.
- A transition source carrying marks after its first sample: each begins a
  track.

## 3. Short tracks

- **K6, S.** Tracks of 0.5, 1 and 3 times the duration, in every order.
  Binding: every sample of every track is heard exactly once outside the
  overlaps; both sources handed to the transition report their true
  length; each track begins once.
- **S.** A track exactly as long as the window, one a sample shorter and
  one a sample longer. Binding: the transition is called at the end of
  each, with an ending source of zero, zero and one sample; the audio of
  the three runs differs by those samples only.
- A cross duration of zero, negative, and of one sample. Binding: the run
  terminates; the transition is called at every boundary; with zero and
  negative both sources are empty and no sample of either track is lost.

## 4. Cross duration

- **K7, S.** A track's override governs the crossing at that track's end
  and no other, for tracks shorter than, equal to and longer than it.
  Binding: the remaining time of the ending source at each crossing;
  `cross_duration()` read when each track's metadata is delivered.
- With `persist_override` the override holds for the tracks that follow.
- **S.** A `duration` function that changes is read once per track, with
  `persist_override` on and off. Binding: with no override in the stream,
  the two runs give the same samples.
- An override carried by a track shorter than the duration is not applied
  to the track after it.
- A non-positive `width` is refused at creation.

## 5. The transition's arguments

- **S.** Each crossing hands the function the right track on each side,
  their levels and their metadata, and the ending track's own keys.
  Binding: the pairs of titles; a key set on one track is in the ending
  metadata of that track's crossing only; no call for the end of the last
  track.
- **K2.** For ending fades shorter than, equal to and longer than the
  window, with and without a start-next offset, and starting fades longer
  than the ending one. Binding: the numbers of the plan
  ([script-layer.md §2](script-layer.md#2-the-plan-of-a-crossing)); the
  metadata handed to the function is what the tracks carried; the remaining
  time of the ending source is the overlap; that of the starting source
  covers the whole fade-in; through `crossfade`, the ending fade reaches
  silence on the last sample of the ending source and the next track
  becomes audible at the offset asked, compared by level over time.
- A metadata inserted in the middle of the ending track does not remove the
  fade keys of its first sample from the ending metadata.
- The stream `cross` delivers carries the fade keys as the child gave them.
- Levels: a tone of known amplitude gives its level within 0.1 dB on each
  side; an empty side gives minus infinity; the values are the same with
  two frame sizes.
- **K12.** A mono `cross` with a transition written without type
  annotations runs through a crossing.

## 6. End of the transition

- **K4.** A transition of four items, two of them shorter than a second,
  with a gap between items. Binding: every item is heard in full, in
  order; no mark where the transition gives way to the track.
- **K3.** A long run of short tracks under a transition that sometimes
  returns a source that delivers nothing. Binding: the output never stops
  while the child has data; every sample of every track is heard; `cross`
  is ready in every cycle in which it holds data or its child is
  available.
- A transition source that stops being ready without declaring exhaustion
  is passed over when the track can go on.
- A transition function that raises a script error. Binding: the output
  goes on, both tracks are heard in full, the next crossing is unaffected.
- **S.** A transition that never ends plays on; the child is read no
  further than one cross duration meanwhile. Binding: the child's read
  position.
- **S.** A crossed sequence ends: the output stops when its last track did,
  and `cross` declares exhaustion.

## 7. Metadata

- **K1, S.** Three tracks, the third repeating the second's metadata,
  through each transition shape: mix, sequence, ending only, starting
  only, neither. Binding: three announcements, in order, each no earlier
  than the first sample of its track that is heard. With a child carrying
  no metadata: none. With `deduplicate` on and off.
- A next track whole inside a transition that drops it is not announced.

## 8. Reading ahead and readiness

- N probes of `cross` in a cycle, for N in 0, 1, 10, in each phase and at a
  crossing point. Binding: the child's read position, the count of
  transition calls and every counter below the child are unchanged.
- The transition function runs when the segment after the crossing point
  is requested. Binding: no call when `cross` is probed and not read.
- The child is read at most one duration plus one request past what `cross`
  delivered of the current track, and no further than the starting window
  of the next. Binding: the positions recorded on the child's events.
- The samples read from the child in one request never exceed duration,
  request and starting window together, after any idle time.
- A child that is not available between two tracks for less than the
  duration: the ending track plays on, then a crossing takes place over
  what is left. For longer: no transition, the track ends, the next one
  starts with a plain track event.

## 9. Abort

- **K11.** A thousand skips a millisecond apart, from another thread, on a
  child that loads a new item per track. Binding: no failure, no deadlock;
  the child is aborted on the streaming thread only.
- **S.** A skip in `track`, and one during a transition. Binding: no sample
  of the skipped track is delivered after the cycle of the skip; the next
  track begins with a track event and its own metadata; no transition is
  called for that boundary.
- A skip after the end of the current track was found. Binding: the child
  is not aborted; the next track plays from its first sample.

## 10. Clocks and lifetime

- **K8.** With a paced reader: the clock listing shows the child and
  nothing else in the child clock, the transition and its sources in the
  reader's. A child also mixed with the output of `cross` is refused at
  start. Creating many such operators while clocks start never fails.
- **K13.** A self-paced input as child, alone and inside a fallback, a
  sequence and a sum, is refused at creation.
- **K9.** Over many crossings with a transition that registers on the
  child: the number of callbacks on the child, read inside the transition,
  stays bounded and was at least one. Live sources do not grow.
- **S.** `cross` put to sleep in each phase and woken. Binding: nothing it
  built is awake while it sleeps; the callbacks of a transition in progress
  are released; after the wake a track begins and the stream goes on.

## 11. Other properties

- **K14.** Remaining time decreases by what was delivered, cycle after
  cycle, through a whole crossing, and reaches zero where the track event
  of the next crossing fires.
- Composition: `cross` over a `file` child resolves to `file` in every
  phase; a selector above it does not cut into its tracks.
- Sync source: none while a track plays; that of the transition source
  while it plays, and the clock is told at both changes.
