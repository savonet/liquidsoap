# Tests, by principle

What the suite protects, grouped by the part of `cross` each check is
about. Names in brackets are a trace back to the sources, under `tests/`.

**Harness.** Every check builds a child out of finite tones in a
`sequence`, stamps a title or other metadata on the first sample of each
track, runs on a clock with no pacing, and reads the result with a dummy
output. The verdict is given when the output stops, which is when `cross`
is no longer ready, or from a callback. A new harness needs these doubles:

- a finite source of exact length that ends its track where it ends;
- a way to stamp metadata on the first sample of a track;
- a sequence of sources;
- an unpaced clock and an output that reports when its source ran out;
- a request source reading one file, for the skip check;
- one audio file with cue points, for the sample check.

Throughout, **incidental**: tone frequencies, track lengths unless stated,
wall-clock delays, log wording, internal source names.

## 1. Tracks

- **One track event per crossing, carrying the starting track's metadata.**
  A two-track child under `crossfade`, whose transition is a sum and carries
  no mark. Binding: the track events are exactly `a`, `b`, in order, once
  each. [`streams/cross_on_track`]

## 2. Metadata delivery

- **Each track's metadata is delivered once.** With a transition that plays
  the ending source then the starting one, and with `crossfade`. Binding:
  no title is seen twice; every title is seen.
  [`streams/cross`, `streams/crossfade`]
- **Each track is announced once, in order, with a sum transition and
  deduplication off.** Binding: the metadata sequence is `a`, `b`, `c`.
  [`streams/cross-transition-metadata`]
- **A child with no metadata announces none.** Binding: no metadata event
  at all, deduplication off. [`streams/cross-no-empty-metadata`]
- **A transition that drops the starting source still gets the starting
  track announced, once, when its audio starts**, and a track repeating the
  previous title is not mistaken for a duplicate. Binding: the metadata
  sequence is `a`, `b`, `b`. [`streams/cross-transition-drops-incoming`]
- **Deduplication does not compare across a track mark.** Belongs to
  `metadata.deduplicate`; `cross` is its motivating user.
  [`regression/GH5360`]

## 3. Cross duration

- **An override applies to the track that carries it and is dropped at its
  end.** Three tracks slightly longer than the duration, the second
  carrying an override. Binding: `cross_duration()` is the override when
  the second track's metadata is delivered, and the default when the
  third's is. The extra length is deliberate: see F17.
  [`streams/cross-override`]
- **With `persist_override` it stays.** Same, tracks exactly as long as the
  duration. Binding: the override at the second and at the third.
  [`streams/cross-persist-override`]
- **An override of zero crosses over one frame, and both sources of a
  sequence transition may share a frame.** Binding: the metadata sequence
  is `a`, `b`, `c`: the starting track's first data and metadata are not
  lost. [`streams/cross-sequence-transition`]

## 4. Transition arguments

- **Each crossing hands the transition the right track on each side, and
  the ending track's own keys.** Binding: the pairs of titles are
  (`a`, `b`), (`b`, `c`), and `liq_fade_out` set on `a` is present in the
  ending metadata of the first only. Also binding, by the exact list: no
  transition for the end of the last track.
  [`streams/cross-transition-arguments`]
- **The ending track's cross duration sets the length of both sources,
  whatever the starting track asks.** Autocue metadata. Binding: remaining
  time of both sources within 0.1 s of the ending track's fade.
  [`regression/GH5581`]
- **The starting track's fade-in is buffered in full.** Binding: remaining
  time of the starting source within 0.1 s of its fade-in; `liq_fade_in`
  handed over unchanged. [`regression/GH5581-2`]

## 5. End of the transition

- **A transition made of several short sources plays all of them.**
  Binding: the metadata of the third of four items is delivered: gaps
  between items do not end the transition. [`regression/AC5109`]
- **A crossed sequence ends.** Binding: the output stops. Also a typing
  check: a generic transition returns a mono source for a mono `cross`.
  [`regression/GH3239-2`]
- **A level difference of 20 dB between the tracks runs.** A smoke test:
  binding only that nothing fails for 8 s. [`streams/crossfade2`]

## 6. Audio

- **The crossed audio of a file played twice is reproducible.** Binding
  today: a digest of one checksum per frame, marks and metadata included.
  That follows where frames are cut, not only what is heard (F25).
  [`regression/GH4828`]

## 7. Abort

- **A skip from another thread, a thousand times, 1 ms apart, does not
  break `cross`.** Binding: no failure, no deadlock. Nothing is checked
  about what plays. [`regression/GH5148`]

## 8. Callbacks

- **Callbacks a transition registers on the child do not pile up.**
  Binding: the count on the child, read inside the transition, never
  exceeds 4 over 45 crossings, and was at least 1 once, which proves the
  transition ran. [`regression/callback_counts`]

## 9. Curves

- **The fade curves of a crossing, with delays, types and curves set by
  metadata, match a recorded plot.** Six autocue cases besides. Binding:
  the two text files of levels over time. They are compared under a
  separate alias and move with frame boundaries.
  [`streams/test_crossfade-plot`, `streams/test_autocue-plot`]

## 10. Creation

- **A child that is, or may become, a sync source is refused.** Four
  shapes: a self-paced input alone, and inside a fallback, a sequence and a
  sum. Binding: creation fails. [`language/type_errors`]

## 11. Around `cross`

Not about `cross`, but they would move with it: `fade.in` and `fade.out`
overrides [`streams/fades-overrides`, `streams/fades-persistent-override`];
scripts that use `crossfade` in passing [`streams/radio2`].

## Not covered by any test

- Where the mark of a crossing falls relative to the audio.
- A starting track shorter than the window; an ending track shorter than
  the duration; a head.
- What plays after an abort, in either phase.
- The level values, the metadata adjustments of
  [transition.md §2](transition.md#2-metadata-adjustments).
- Sleep and wake, a child that is late inside a track, the end of the
  stream on a partial frame.
- Remaining time, composition, sync source.
