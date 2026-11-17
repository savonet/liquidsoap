# Switch family: conformance suite (as-built)

Scope: the switch operator, the frame-assembly seam shared with cross and
sequence, composition profiles, and the `fallback` / `rotate` / `random`
wrappers.

Conventions:

- "Selection": the child currently streamed. "Handoff": a change of selection.
- "Boundary": the selected child has no more data for its current track.
- "Preemption": a handoff that cuts into a track still playing.
- Every test signals pass or fail explicitly; a timeout is a failure. Tests
  with a fixed deadline turn "never happened" into a failure.
- "Verified" means the behaviour was observed by running the test. Everything
  else is read only.

Shared fixtures a new harness needs:

- An endless tone with no track marks, and a tone of fixed duration (one
  track, then unavailable).
- A tagger that sets a title on every track (insert-missing), and a chopper
  that cuts an endless tone into tracks of fixed length with counted titles
  (`music1`, `music2`, ...).
- A gate: a source whose availability follows a mutable boolean.
- Silence, endless or of fixed duration. Generated synthesis sources (tone,
  silence) resolve to the file composition profile unless retagged.
- Generated mp3 files with title tags (a 1 s and a 10 s tone tagged `music0`,
  two 1 s tones tagged `jingle0` / `jingle1`, a 3 s tone tagged `program`), and
  a playlist file listing the two jingles.
- A queue that can be pushed requests at run time.
- An unsynced clock (runs as fast as the CPU allows) for most tests. Tests
  that schedule wall-clock events (thread delays) run on a real-time clock.
- Observation points: per-track callback (first frame of a track), per-metadata
  callback, per-frame callback, position callback (fires when a track reaches a
  given position, optionally only on full frames).

## 1. Priority selection

Invariant: when a selection is made, the first child in declaration order
whose predicate holds and which is ready is chosen. An unavailable child is
passed over without delaying the output.

Binding:

- Order of track titles across handoffs (e.g. delayed source, then fallback,
  then delayed source again).
- With an initially delayed source the order starts with the fallback.

Incidental: track durations (3 s), delay length, titles.

Failure modes: a higher-priority child that becomes ready only after a delay;
a child ready for a single cycle (see group 9).

Fixtures: a delayed source that is available only once per delay interval.

Traces: GH4281, GH4281-2, GH1129, switch_on_leave_track_boundary.

## 2. Track-sensitive retention

Invariant: a track-sensitive selection is kept until its track ends, even
after its predicate turns false. A track-insensitive selection is dropped as
soon as its predicate turns false.

Binding:

- With an endless one-track child whose predicate goes false, the output does
  not stop (no stop event during the observation window).
- The outer switch waits for the selected child's boundary before handing over
  when both children are track-sensitive (verified: handoff logged "at track
  boundary").

Incidental: delays (0.1 s, 1 s), tone frequencies.

Note: the default track sensitivity comes from the composition profile. A
tone resolves to the file profile, which is track-sensitive.

Traces: track_sensitive, switch_predicates_on_selection,
switch_reselect_after_idle.

## 3. Mid-track takeover gating

Invariant: a preemption is allowed only if the child being left accepts
interruption or the child starting does not require a boundary. Otherwise the
handoff waits for the boundary.

Binding:

- A track-insensitive higher-priority child takes over immediately from an
  endless child with no track end (special source over silence; a gated live
  source over music; a stripped-blank source over a fallback tone).
- The takeover is observed: a track from the starting child appears within the
  window (1 to 2 s after it becomes available).

Incidental: the gating mechanism (gate predicate, nested switch, blank strip).

Failure modes:

- Silence as the lower-priority child: it has no track end, so a takeover that
  waits for a boundary never happens.
- A child whose availability flips between the decision and the next frame
  (group 9).

Traces: smooth_add, 115-2, GH4163, GH5012, BUG403,
switch_default_transition_finish.

## 4. Transitions and ending under the default handoff

Invariants:

- On preemption of a PCM-audio-only child, the starting child plays at once and
  the leaving child is mixed beneath it with a fade.
- If the leaving child has at most the maximum fade duration left, it finishes
  its track beneath the starting child; its next track is never played.
- Any other content (encoded, video) switches without mixing; encoded content
  passes through untouched, both on first selection and on preemption.
- While a transition is running the switch does not re-select the child it
  left, even if that child's predicate holds again. It re-selects it at the
  next normal opportunity after the transition ends.
- A switch with an infallible, always-eligible child stays ready through every
  frame of repeated transitions, including the frame where the transition ends.

Binding:

- Tracks seen in order `a`, `live` and `b` never seen in metadata, checked two
  seconds into the starting child on a full frame.
- Title sequence prefix `s1, s2, s1` (no premature `s1` during the fade).
- Ready on every one of 900 frames with selection flipping every 60 frames.
- Copy-encoded output receives a track from the second child (the encoder
  rejects any decoded/remixed content).

Incidental: maximum fade setting (1 s default, 1000 s to force the "finish"
branch for encoded content), tone durations, flip period.

Failure modes:

- Preemption 0.5 s before track end (remaining < maximum fade).
- Predicate of the left child restored on the first frame of the transition.
- The transition graph running out mid-frame.

Fixtures: a dynamic source that rebuilds a tagged tone per track; an mp4
(aac + h264) file; a copy encoder.

Traces: switch_default_transition_finish, switch_per_source_no_reselect_during_fade,
switch_infallible_across_transition, switch_copy_default_on_select,
switch_on_leave_chained.

## 5. Custom selection hook

Invariants:

- A per-child selection hook receives the starting child, the ending child
  (only on preemption) and the replay flag; the source it returns is what is
  streamed.
- The returned source is constrained to the switch's content type, even if the
  hook is generic (a generic fade-in on a mono output stays mono).
- Hook signatures are type-checked: wrong arity, extra required labels, or a
  non-source return are rejected; extra optional arguments are accepted.
- A hook that only replays metadata (legacy hook) keeps the child's selection
  logic intact (rotate weights still observed).

Binding: a marker added by the hook appears in the starting child's metadata;
the mono output finishes without a runtime type error.

Incidental: marker name, tone durations.

Traces: switch_per_source_on_select, GH3239, GH3239-2, type_errors,
GH1327, blank_strip_resume_metadata.

## 6. Leave callback

Invariants:

- The leave callback fires on every handoff away from a child, at a boundary
  as well as on preemption.
- Its flag says how the child ended: `true` when it had nothing left for its
  track, `false` when it was cut into.
- It fires once the transition no longer holds the child.
- Chained handoffs (a left for b, b left for c while a is still fading) fire
  the callback for every child left; none overwrites another.
- Under the file profile, a child preempted mid-track has its track skipped
  and its latest metadata cleared. Under the live profile nothing happens.

Binding: flag values per case (`false` for the cut, `true` for the boundary);
membership of both `a` and `b` in the left list; callback fired before the
second track of the starting child.

Incidental: list order of callbacks, ids, the 4 s deadline.

Failure modes: the boundary case (no transition built); a handoff during a
running transition.

Fixtures: silence of 0.5 s per track (ends tracks on its own); endless tones
marked track-insensitive (every handoff is a preemption). Predicates written
together inside an atomic section so no intermediate state is observed.

Traces: switch_on_leave_track_boundary, switch_on_leave_chained,
switch_per_source_on_leave, switch_skipped_track_metadata.

## 7. Track marks and metadata replay on selection

Invariants:

- A new selection starts a new track: its first frame carries a track mark.
- On selection, the child's latest metadata is replayed unless the child
  starts with metadata of its own in the same frame.
- A child's latest metadata is forgotten when its track ends at a boundary or
  is skipped; re-selecting it never announces the old track again, including
  through nested switches.
- A child that resumes with a new track and new metadata after a stripped
  blank delivers that track mark and metadata to the output.
- Metadata emitted while the child was unavailable is delivered with the track
  it resumes with.
- A live child taking over delivers its first track mark and metadata
  (nothing it produced before being read is lost).

Binding:

- The second jingle selection reports `jingle1`, not `jingle0`.
- After a skip and re-selection, the title is the new one (`a2`).
- The nested child's latest metadata is null after its track was skipped.
- Track list ends with `live` when the live track reaches 1 s.
- Track list is exactly `music, live` for the harbor takeover.
- Title order `fallback, first_blank, after_blank` for a stripped blank that
  starts blank.

Incidental: titles, durations, frequencies, the harbor transport (an icecast
client streaming to a local harbor), port number.

Failure modes: delayed jingles re-selected after music; skip forced by the
output; blank strip at start; client sending no metadata.

Fixtures: generated mp3 files with titles; a jingle playlist under a delay;
silence retagged live as last resort; a queue pushed at run time; a local
harbor input with an icecast client.

Traces: switch_autodj_jingle_metadata, switch_per_source_replay_metadata,
switch_skipped_track_metadata, blank_strip_resume_metadata,
blank_strip_metadata_during_blank, GH4163, GH5012.

## 8. Single

Invariant: a child marked single is never selected for two consecutive
tracks; another eligible child is chosen at the next boundary.

Binding: after a forced boundary on the single child, the next title is the
other child's, never the single child's.

Incidental: both children always eligible; titles.

Traces: switch_per_source_single.

## 9. Predicate evaluation

Invariants:

- Predicates are evaluated only when a selection is needed. Keeping a
  track-sensitive selection evaluates none.
- Within one streaming cycle all predicates are read as one snapshot, taken
  atomically with respect to script writes.
- A side-effecting predicate is read once per cycle; the cycle that read it
  `true` commits to that child without re-reading it.

Binding:

- Fewer than 5 predicate calls over 2 s of streaming (and more than 0).
- Never a round where only the intermediate state of two jointly written
  predicates is visible.
- A one-shot predicate (true once, then false) does not crash the output.

Incidental: the threshold 5, the 2 s window.

Failure modes: a predicate that resets itself on read; two predicates changed
together.

Traces: switch_predicates_on_selection, switch_on_leave_track_boundary,
BUG403.

## 10. Resume after idle

Invariants:

- A switch that resumes after not being streamed re-evaluates its selection:
  a higher-priority child that became available meanwhile plays at once.
- Resuming is detected from the switch's own animation: a consumer on a slower
  child clock that reads it only on some cycles is not a resume
  (track sensitivity holds there).
- A fallback restarted after its child was skipped and finished elsewhere
  re-checks the child's readiness instead of streaming it.

Binding:

- The first jingle comes out less than 0.5 s after the outer switch resumes
  the inner one (10 s deadline).
- Under the slow child clock, track durations stay 1 s / 2 s within 0.1 s and
  weights hold (see group 12).

Incidental: chop lengths, frequencies, the stop/start mechanism (server
commands).

Failure modes: the inner selection was left at a boundary (verified:
outer handoff "at track boundary"); re-preempting a stale selection; a shared
child consumed by another output.

Fixtures: chopped tones with counted titles; inner switches gated by booleans;
a time-stretch operator that puts its input in a child clock.

Traces: switch_reselect_after_idle, GH5348, LS460.

## 11. Nested switches and clocks

Invariants:

- A switch nested as a child behaves as a single child of its parent; its
  readiness and tracks are what the parent sees.
- Children in different clocks cannot be combined: building such a switch at
  run time raises an error of kind clock, catchable by kind, visible to error
  monitors and to thread error handlers.

Binding: the error kind (`clock`), not its message.

Incidental: outputs used to pin the clocks.

Traces: GH1129, GH4734, GH5348, random, switch_skipped_track_metadata.

## 12. Rotate weights

Invariants:

- Rotate plays each ready child for `weight` consecutive tracks, then moves to
  the next ready child in order.
- Weights may be getters.
- A child that is not ready is skipped, and the rotation continues from the
  next one; a child that became ready is picked on its next turn.
- Nested rotations compose: counts over N rounds are exact.

Binding:

- Exact selection order prefixes (`s1, s1, s2, s1, s1[, s2]`;
  `s1, s3, s1, s2, s3`).
- Exact counts over 10 rounds of 8 tracks: 10 jingles, 30 of music, 40 of
  music2.
- Two jingles out of six tracks under a slow child clock.

Incidental: track length (one frame of silence in some tests), ids.

Fixtures: tagged silence of one frame per track; a playlist directory.

Traces: GH1279, GH1327, GH1129, rotate, GH5348.

## 13. Random weights

Invariants:

- Random picks among ready children with probability proportional to weight.
- A weight of 0 or an unavailable child is never picked.
- Large child lists load and stream.

Binding: deterministic choices only: the sole ready child, and the sole
nonzero-weight child.

Incidental: the third-round weights (2 and 1) are not asserted
statistically; the 100-playlist count and the 5 s window only check liveness.

Traces: random, radio2, many-playlists.

## 14. Callback lifetime

Invariants:

- Callbacks a selection hook registers on reachable sources live only for that
  selection; repeated selections do not accumulate them.
- A released callback never fires; releasing twice is harmless.
- Unselected children do not hold resources without bound.

Binding: hits ≤ frames of the shared child after at least 5 selections;
released counters stay 0; the test fails if callbacks never fired at all
(non-vacuity). A delay-line child left unselected buffers at most 1 s.

Incidental: flip period (10 frames), 500-frame window.

Fixtures: a child shared between the hook and the switch.

Traces: callback_release, GH5287. source_cleanup (source collection) is
skipped in the suite: not a signal.

## 15. Fallibility

Invariant: the switch is infallible iff some child is infallible, not single
and has a trivially true predicate. An infallible switch is ready on every
frame.

Binding: readiness on every frame (group 4); fallible outputs used everywhere
else.

Traces: switch_infallible_across_transition, BUG403.

## 16. Abort track and skip

Invariants:

- Skipping the switch skips the selected child's track and clears its latest
  metadata, so the next selection is evaluated from a boundary.
- The skip reaches the child selected inside nested switches.

Binding: see groups 7 and 8 (new title after skip; null latest metadata;
single respected after skip).

Traces: switch_per_source_replay_metadata, switch_per_source_single,
switch_skipped_track_metadata, LS460.

## 17. Frame assembly across sources

Invariants:

- One output frame may contain data from several selections: when a track ends
  mid-frame the rest of the frame is filled from the next selection, with a
  track mark at the junction.
- Cross and sequence use the same assembly; their output (audio, metadata,
  track marks) is deterministic.
- An empty switch at the end of a sequence ends it cleanly.

Binding: frame checksum digest (including metadata and track marks) of a
cross over a sequence; full-frame vs partial-frame position callbacks.

Incidental: the checksum value itself is a golden; any change in frame layout
breaks it.

Fixtures: a flac asset with cue points; frame checksum hooks.

Traces: GH4828, GH3239-2, GH1279, switch_default_transition_finish.

## Tensions

1. Resume re-evaluation vs keeping a started track. One test expects a resumed
   track-sensitive fallback to drop its selection and pick a newly available
   higher-priority child. Issue #5594 (no test) expects a resumed rotate to
   keep a jingle whose track already started. Verified by running: in the
   test, every source resolves to the file profile (tones; chop is not an
   operator, it returns the tone itself; inner switches pass through to their
   child). The outer fallback therefore waits for the inner one's boundary:
   the handoff is logged "at track boundary", and no mid-track skip occurs. On
   resume, the inner selection holds a track that started while the boundary
   frame was generated and that nobody heard (verified: when a resuming switch
   keeps a ready track-sensitive selection, the test fails with the jingle
   arriving a full song later). Retagging the outer high-priority child live (scratch copy) makes
   the outer preempt mid-track; the file profile's leave callback then skips
   the inner track; the test still passes. Neither path exercises a
   half-played inner track. The test and #5594 are the same situation, a
   nested selector left at a boundary with its next track already started,
   with opposite expectations about that track; #5594 has no test.
   Related: rotate counts a track when its first frame leaves the switch,
   which can happen inside the frame where the outer switch takes its
   boundary (group 17).
2. Profile-dependent defaults. Tone and silence resolve to the file profile
   (track-sensitive, skip on preemption). The predicate-count test passes only
   because its tone is track-sensitive; a live tone would evaluate predicates
   every cycle. Silence as last resort blocks takeovers (it never ends a
   track): smooth_add needed its special child marked track-insensitive, and
   the jingle-metadata tests retag silence live.
3. Finishing under the tail vs skip on preemption. A child preempted near its
   track end finishes that track beneath the starting child, yet its leave
   callback reports a cut (`false`), and the file profile then skips. The test
   only asserts that the next track `b` is not played within the window. Read
   only: whether the skip lands on `b` (discarding it unplayed on
   re-selection) is untested.
4. No re-selection during a transition vs takeover during a transition. One
   test keeps the left child out until the fade ends; another requires a third
   child to take over while a fade runs. Both hold because the current
   selection is kept only while it is still wanted (track-sensitive or
   predicate true). Read only.
5. Resume detection from both sides. One test needs a gap in animation to
   trigger re-evaluation; another needs a child clock reading every other
   cycle not to. Both pin the same detector; the boundary is the switch's own
   clock tick. A detector based on parent cycles would fail one of them.

## Incidental users

delay_dry_between_tracks, GH3645, GH4159, GH4882, GH4995, GH5019, GH5389,
GH5514, GH5527, cleanup, source_dynamic, media/multitrack,
media/test_ffmpeg_copy_concat, streams/hls_stop_start,
streams/test_on_subtitle, streams/test_ffmpeg_on_subitles,
harbor/harbor_output_keep_listeners, harbor/icecast_server_mounts,
harbor/post, harbor/put, srt/srt-av-reconnect, srt/srt-silent-peer. Not run
by the suite: fixme/test_sequence-1, fixme/test_sequence-2.
