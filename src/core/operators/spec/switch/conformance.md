# Switch family: conformance suite (as-built)

Scope: the switch operator, the frame-assembly seam shared with cross and
sequence, composition profiles, and the `fallback` / `rotate` / `random`
wrappers.

Groups 18 to 24 extend the scope to the seam itself, `sequence`,
`source.dynamic`, the stdlib helpers built on them (once, append, prepend,
fade-in delay, merged rotation) and `cross` as a user of the seam.

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

## 18. A change of designated source begins a track

Applies to every user of the seam that does not merge.

Invariants:

- The first chunk taken from a newly designated source carries a track mark
  at its start, and the operator's track callback fires once for it. This
  holds for the very first chunk of the stream.
- The callback receives the metadata found at the mark, or none. The
  operator's previous metadata is not replayed onto the new track.
- A source designated after a period with no source at all starts with its
  own track: its track callback fires when it is first read.

Binding:

- A two-item sequence fires its track callback (verified).
- A track appended after each track of a main source fires the track callback
  with empty metadata, alternating with the main source's tagged tracks, more
  than twice each (verified).
- A one-second silence handed to a dynamic source two seconds into the run
  fires its own track callback before a four-second deadline (verified).

Incidental: durations, frequencies, the safety wrapper around the dynamic
source, the count threshold (2).

Failure modes: no source for the first two seconds; a never-ready source
designated first, then a ready one (the operator must survive the first).

Fixtures: a mutable cell read by the designation function; a source that is
never ready.

Traces: on_track, append, LS354-1, LS354-2.

## 19. Reselect inside a frame

Invariants:

- When the current source ends its track mid-frame, the operator is asked
  again in the same cycle, with the position already consumed. The same
  source is kept only if its frame for this cycle goes past that position.
- The position constraint applies to the source it was computed against. A
  source moved to in the same cycle is accepted on readiness alone, even when
  it has exactly as many samples available as the one it follows; its first
  chunk and the metadata on it are delivered.
- A child that became ready again after its track ended is selected for a new
  track, not rejected as "same source, no more data".

Binding:

- Metadata order `a, b, c` through a cross whose transition is a two-item
  sequence, where `b` leaves a partial frame and both sides land in one frame
  with equal sample counts (verified).
- A request pushed to a queue three seconds into the run, above an endless
  file in a fallback, has its annotation seen in metadata (read only).

Incidental: the 4.02 s duration (any length leaving a partial frame), the
zero cross duration on `b`, the cross operator itself.

Failure modes: equal positions on both sides of a join; a source reselected
that has nothing past the consumed position (critical error, abort).

Fixtures: tones of a duration that is not a multiple of the frame; per-track
cross-duration override metadata.

Traces: cross-sequence-transition, GH3645, GH4828.

## 20. Sequence: one track per item, then the last item

Invariants:

- Each item but the last plays one track; at that track's end the item is
  dropped for good and the next one is designated in the same frame.
- The last item plays for as long as it is available.
- The sequence is ready once any item was ever ready; it is fallible iff its
  last item is.
- An item that is not ready when its turn comes is dropped, and the next one
  tried in the same call.
- A sequence ending with a never-ready item plays its other items once, then
  is unavailable for good ("once"). A fallible output on it stops.
- A sequence that runs out while mixed with other sources, or below an
  operator that discards whole items, neither blocks nor aborts.

Binding:

- The last item's metadata reaches the output although the item before it (5 s
  of silence) is consumed entirely by a blank-eating operator (verified).
- Three seconds of streaming with a two-item sequence ending in a once-source,
  summed with an endless tone (verified, liveness only).
- The output stops after a once-source; an output filename getter is called
  exactly once (read only).
- An empty switch as last item ends the stream; frame digest (group 17).

Incidental: durations; the summing operator; encoders and files in the tests
that use a once-source only to get a finite stream.

Failure modes: an item skipped whole by a downstream consumer; the last item
unavailable; skip hammered from another thread through a cross (no crash).

Fixtures: a source that is never ready; an empty switch.

Traces: on_track, GH3132, 115-1, 115-2, GH2842, GH4828, AC5109, LS460.

## 21. Merged joins

As built (see Tensions 6 to 8):

- A merging operator puts no track mark at a join it merges and does not fire
  its track callback there. Metadata at the join passes through unchanged.
- A merged sequence merges every join except the one into its last item.
- A dynamic source reads its merge flag at the join, after the designation
  function returned.
- The appended-track helper with merging on emits no track mark where the
  appended track starts.

Binding:

- Metadata of main and appended tracks both seen more than twice; the track
  callback never fires with the appended track's metadata (verified).
- Metadata order `music, jingle, jingle` three times over, where the appended
  source is a merged sequence of two jingle tracks and a never-ready item
  (verified). Track marks are not asserted.

Incidental: the count threshold, chop length, the never-ready tail of the
merged sequence (it only makes the appended source finite).

Failure modes: none simulated. Both tests assert metadata, and one asserts an
absence; neither asserts where a track mark must be.

Fixtures: chopped tones with fixed metadata; a per-track tagger.

Traces: append-merge, append-merge2.

## 22. Dynamic source: designation

Invariants:

- The designation function returns a source to use, or nothing to keep the
  current one.
- It is called when there is no current source, on wake-up, on every cycle
  when not track-sensitive, and whenever the current track ends inside a
  frame. When track-sensitive it is not called while the current source can
  continue.
- Returning the current source again keeps it without preparing it anew: a
  playlist reused across designations is not reloaded.
- Returning another source wakes it, makes it current and puts the previous
  one to sleep. A replaced active input is stopped.
- A designated source that is not ready leaves the operator unavailable for
  that cycle; when declared infallible this is a run-time error.
- The designated source's content type is unified with the operator's at
  designation time, not when the script is typed.
- When the designated source ends and the function keeps returning it, the
  operator becomes unavailable.

Binding:

- No playlist reload after the first tagged tone was seen; two tone
  announcements before an 8 s deadline (read only).
- A mono constraint coming from a sum reaches a tone created inside the
  function; 1 s of streaming (read only, liveness).
- An image request and an audio request each served through a per-track
  queue source created by the function; 3 s of streaming (read only,
  liveness).
- The output stops once a 0.2 s tone designated on every cycle is over (read
  only).

Incidental: file names, the playlist, deadlines.

Failure modes: function returning a fresh source on every call; the same
source on every call; nothing; a never-ready source.

Fixtures: a playlist with a reload callback; request-backed sources built
inside the function; an lavfi input (skipped tests).

Traces: GH4995, GH4159, ctype2, source_dynamic, LS354-1, LS354-2,
switch_per_source_on_leave, switch_per_source_single,
switch_per_source_no_reselect_during_fade. Skipped in the suite, not a
signal: GH5389, cleanup.

## 23. Appended track

Invariants:

- After each track of the main source, one track built from that track's
  metadata is played, then the main source resumes with its next track.
- The appended source is built when the main track's metadata (or its
  metadata-less track start) is seen, and woken ahead of use.
- The appended track is a track of its own and carries only its own metadata.
- With merging on, see group 21.

Binding: alternation counted per origin (more than two of each); empty
metadata on appended tracks; metadata order with a multi-track appended
source.

Incidental: tone durations; the fallback to the main source wrapped around
the dynamic source.

Fixtures: chopped tone with fixed metadata; a builder returning a finite
tone.

Traces: append, append-merge, append-merge2.

## 24. Cross as a user of the seam

Out of scope: buffering, level analysis, duration overrides.

Invariants:

- While a track plays, cross designates a buffer holding the track's data.
  When the track ends, it designates the transition source, followed by the
  rest of the incoming track. When that is no longer available it designates
  a new buffer.
- Entering the transition begins a track. Returning to the buffer does not.
- With a transition that carries no track mark, each crossing fires the track
  callback exactly once, with the incoming track's metadata.
- Each track's metadata is announced exactly once and in order, whether the
  transition plays both sides in sequence, mixes them, or drops the incoming
  side. A track repeating the previous one's metadata is still announced.
- Sources without metadata announce none.
- The transition function receives the ending and starting tracks' metadata.
- A transition made of several tracks is played to its end.
- Skipping repeatedly from another thread during crossings does not abort.

Binding:

- Track callback titles exactly `a, b` with a mixing transition (verified).
- Metadata lists exactly `a, b, c` (mixing), `a, b, b` (incoming dropped),
  empty (no metadata) (verified).
- `a` and `b` each announced once with a sequence-shaped transition
  (verified).
- Transition arguments `(a, b, override present)`, `(b, c, absent)` (read
  only).
- Metadata of the third of four transition tracks seen (verified).
- More than 1000 skips at 1 ms intervals (verified; the log shows failed
  skips, the assertion is liveness).

Incidental: durations, cross length, deduplication setting, frame rate.

Failure modes: transition dropping the incoming source; add-shaped transition
putting both announcements at one position; tracks without metadata; tracks
shorter than the cross length are not simulated.

Fixtures: a sequence of tagged finite tones as the crossed stream (all of
these tests); transition functions of the three shapes.

Weak signals: one test passes on a timer after reading the titles; another
(default crossfade, counts of `a` and `b`) never fails explicitly and relies
on the timeout.

Traces: cross_on_track, cross-transition-metadata,
cross-transition-drops-incoming, cross-no-empty-metadata,
cross-transition-arguments, cross-sequence-transition, cross, crossfade,
crossfade2, AC5109, GH5148, GH4828, callback_counts. Sequence used only as a
track fixture: GH5581, GH5581-2, fades-overrides, fades-persistent-override.

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

6. The join into the last item of a merged sequence keeps its mark, and the
   first mark of the stream is dropped (#5634, no test). Scratch runs, all
   verified: merged `first, last` reports one track, `last`, at the join,
   whether the join is frame-aligned or mid-frame; merged `a, b, c` reports
   one track, `c`. Stdlib users that only produce a sensible stream because
   of that kept mark, or are unaffected by merging at all:
   - Fade-in with an initial delay (merged silence, then the source). The
     only mark of the first track is the one at the join: the track callback
     fires at the end of the delay with the source's metadata, and nothing
     fires at stream start. A join that honoured the merge would leave the
     first track without any mark.
   - Prepended track with merging on. Track marks are identical to merging
     off: one where the prepended track starts (the enclosing dynamic source
     begins a track on a source change) and one at the join into the main
     track.
   - Merged rotation (a round padded with one frame of silence in a merged
     two-item sequence, all track marks then removed). Observed with two
     different fixtures: one track mark in the whole run; the first source is
     heard in the first round only; later rounds contain the padding frame
     and the other sources. Cause not established.
   - Cross wraps the buffered head and the transition in a merged two-item
     sequence with "new track on source change" off. The join leaves the
     transition's own marks alone under either reading. Read only.
     No test covers any of the four.
7. A test passing on an incidental property. The appended-track helper
   computes its merge flag as "merging asked and a pending source exists",
   but tests the cell holding the pending source, not its content: the flag
   is constant. Verified by scratch run: with merging on, one track mark in
   1600 frames (the stream's first), against 111 with merging off. The only
   test asserts that no track callback carries the appended metadata, which
   holds because no track callback fires at all after the first. The stated
   purpose (merge a track with its appended track) implies a mark at each
   main track; a harness asserting that fails as built.
8. A merged-sequence test that says nothing about merged joins. The appended
   source is a merged sequence of two tracks and a never-ready item. The join
   into the last item never happens, and the dropped first mark is hidden by
   the enclosing dynamic source, which begins a track on the change of
   source. Only metadata order is asserted. It passes under the as-built
   rule and under a rule that merges every join.
9. One track per item vs accepting the next item at an equal position. To
   leave an item at its track end, sequence refuses to continue it within
   the frame; to deliver the next item's first chunk, it drops the position
   constraint once it has moved. One test pins the second half. The first
   half is pinned only by the golden digest, which was re-recorded when the
   "new track on source change at a frame boundary" rule was introduced
   (#5379). Read only.
10. Track count at a crossing depends on the transition's shape. Verified by
    scratch run over three tagged 3 s tracks, cross length 1 s: a mixing
    transition gives 3 tracks (`a, b, c`); a sequence-shaped transition gives
    5, an untitled one where the transition starts and a titled one at the
    transition's own join; a transition dropping the incoming source gives 3,
    the last two untitled, with the metadata announced later without a mark.
    The exactly-once track test uses the mixing shape. The tests using the
    other shapes assert metadata only. The golden digest is the only test
    sensitive to the extra mark.
11. Fallback wrappers hide the first mark. The appended and prepended helpers
    return a fallback over their dynamic source; fade-in with delay and the
    default handoff return the dynamic source bare. Whether a stream starts
    with a track mark therefore depends on the wrapper, not on the seam:
    appended stream, mark at frame 0 (verified); delayed fade-in, none
    (verified).

## Untested

Behaviours of the seam, sequence, dynamic source and their stdlib users that
no test pins. "Observed" means seen in a scratch run, not asserted anywhere.

Seam and sequence:

- Merged two-item sequence: which track is reported. Observed: the last
  item's, at the join; the first item's track mark is dropped (#5634).
- Stream's first track mark on a merged sequence. Observed: absent.
- Frame-aligned vs mid-frame merged joins. Observed: same marks, one frame
  apart.
- Skip on a merged sequence. Observed on three items, skip during the first:
  every item but the last is dropped, the last starts at once with a track
  mark, the second is never played. Skip on an unmerged sequence beyond "no
  crash".
- Remaining time of a merged sequence (sum over items; infinite if any is).
- Item not ready at its turn; sequence resumed after its head became
  unavailable (two tests exist outside the suite and are not run).
- Play-until-unavailable mode per item; "new track on source change" off.
  Both reached only through cross internals.
- The queue accessor.
- The stand-alone "merge all tracks" class: not registered as an operator,
  no caller.

Dynamic source:

- The merge flag, directly, including a flag that changes between joins (the
  default handoff relies on a one-join flag; only its metadata and titles are
  asserted).
- Initial source; prepare-ahead with a carried activation; the infallible
  error when the designated source is not ready; a track-sensitivity getter
  that changes; "keep current" while the current source is not ready.
- Stop and collection of a replaced source (both tests skipped).

Stdlib users:

- Prepended track: everything (order, marks, opt-out metadata, builder
  returning nothing, merging).
- Appended track: opt-out metadata, pending/cancel/skip accessors, marks at
  main track starts when merging.
- Merged rotation: everything.
- Fade-in initial delay: everything (mark position, data loss on active
  sources, delay applied once).
- Once on a source that never ends a track.

Cross as seam user:

- Track marks for sequence-shaped and incoming-dropping transitions.
- No new track on return from the transition to the buffer.
- A crossed track shorter than the cross length.
- Effect of a skip during a transition, beyond liveness.

## Incidental users

delay_dry_between_tracks, GH4882, GH5019, GH5514, GH5527, media/multitrack,
media/test_ffmpeg_copy_concat, streams/hls_stop_start,
streams/test_on_subtitle, streams/test_ffmpeg_on_subitles,
harbor/harbor_output_keep_listeners, harbor/icecast_server_mounts,
harbor/post, harbor/put, srt/srt-av-reconnect, srt/srt-silent-peer. Not run
by the suite: fixme/test_sequence-1, fixme/test_sequence-2, fixme/test_cross2,
fixme/test_cross3.

Once-source used only to get a finite stream: GH3840, GH4246, GH5110, GH5557,
GH5643, GH3239-bis, ffmpeg_seek_preroll, and the media encoder and decoder
tests.
