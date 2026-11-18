# Brief for the rebuild

Framing set by the maintainer. It applies to every session and every agent
that works from this directory. The rules it led to are in the specification;
this file keeps the reasons and the scenarios.

## 1. Principle: explicit signals, never inference from pulls

> Any logic based on waiting for a data pull is prone to this kind of issues.
> Explicit signaling is always better.

A component MUST NOT deduce what happened to it from whether, when or how
often it was asked for data: tick gaps, "not animated since", "read but not
consumed", readiness probes. The party that makes a decision tells the parties
it affects.

It applies to every rule, not only to the one that prompted it. The
inferences it replaces:

| Inferred                                                              | Who actually knows                                            |
| --------------------------------------------------------------------- | ------------------------------------------------------------- |
| "I was not animated for more than one tick, so my selection is stale" | The parent: it preempted the child or left it at a boundary.  |
| "A track mark was generated, so a track played"                       | The consumer: it took the data or did not.                    |
| "My readiness was asked, so I am about to be read"                    | The asker: it is probing or committing.                       |
| "A search is running, so a track ended"                               | The switch: it knows why it searches.                         |
| "More than one item is left, so this join is merged"                  | The operator that advances: it knows which join it is making. |
| "This item is not ready, or gave a short frame, so it is finished"    | The item: it ended its track, or it is only late.             |

## 1b. One file per operator

Each operator is specified in isolation: its own file, answering every clause
of the seam contract
([frame-assembly.md §9](frame-assembly.md#9-contract-clauses)). A rule for one
user of the seam that its siblings do not meet is a gap to close, not a
footnote.

## 1c. Marks sit on the logical start of content

A track mark and its metadata are held together and sit on the logical
start of a unit of content, typically a song. Over a playlist or any
file-based source, an operator keeps the marks and metadata as the content
gives them. Filler is not a track: the silence in front of a delayed fade-in
is there so that the result can be mixed, and carries no mark; the one mark,
with its metadata, is where the faded-in track starts. `prepend` adds a
track before a track: a mark, with metadata when there is some, at each.

This is the default. A `merge` the script asks for is a different request
and is honoured: the merged units are one track.

## 2. Direction for the resume rule

The tick-based resume rule goes. Two explicit rules replace it. Neither is
enough alone.

**a. No commitment before the consumer takes it.** S1 and S2 (§3) are the same
situation: a nested selector is left at a clean boundary, having already
started its next track while generating the frame that held the boundary. S2
wants that unheard track dropped; S1 breaks when it is dropped, because the
policy has already counted it. Both hold only if the selector has not
committed: its next selection, its track events and its policy's counters
happen when the consumer takes data past the boundary, not before. On return
the selector is simply at a boundary and searches, with nothing to undo.

**b. An aborted selection has ended.** A parent that preempts a child says so
(`on_leave` with `preempted = true`, and the file profile's
skip). A selector told that its track was aborted treats its selection as
ended and searches at its next read.

Consequence to state in the spec: a source that is not skipped when preempted
(live profile) resumes where it was.

An earlier version of this section proposed rule b alone. It does not cover
S2 as reported: there the programme source is a file source and the nested
selector is left at a boundary, not preempted.

## 3. Scenarios that must all hold

They are conformance checks, not spec text.

**S1. Jingle due after an insert.** An outer switch plays a rotation of one
jingle then three songs, all file sources. During the third song a condition
makes a one-track insert due. The third song ends; the insert plays; the
rotation returns. Binding: the track after the insert is a jingle and it plays
for its full length; no song starts before it ends. Variant: the same under a
crossfade over the outer switch, where the listener must not get a "crossfade
longer than the track" situation from a jingle cut to one frame.

**S2. Stale selection after idle.** A priority list holds a programme source
above a nested priority list of [jingles, songs], all file sources. A song is
playing; the programme becomes available and takes over when the song ends;
while the programme plays, jingles become available; the programme ends.
Binding: the first track after the programme is a jingle, within a fraction of
a second; no song, and no fragment of one, is heard first.

**S2b. The same with a preemption.** The programme source is live and cuts
into the song. Binding: as S2, and the cut song does not resume.

**S3. Child clock.** A rotation inside an operator that reads its child on
some ticks only. Binding: weights are honoured and no track is cut short.

**S4. Merged sequence.** A two-item sequence asked to merge: a short item
titled "first", then a longer one titled "last". Binding: exactly one track is
reported, at the start of the stream, with the first item's metadata; a join
inside the very first frame is not merged into the stream start; skipping or
aborting the merged sequence still produces a track boundary. Both a join
that falls mid-frame and one that falls on a frame boundary behave the same.

**S5. Merged rotation.** Three children with short titled tracks, rotated and
merged. Binding: one track is reported per round, at the start of the round;
within a round the three children are heard in order, each for one full
track; this repeats for every round.

## 4. Rulings

Given by the maintainer before the normative pass.

- **Reach of rule a.** It is part of the contract of every source, not only
  of the seam's users: a source stops at its own decision points until its
  reader continues, and operators in between relay that.
- **Script surface.** The next release may break it. Confirmed changes:
  `switch` loses `all_predicates`; the argument of `on_leave` is
  `{source, preempted}`. Any further removal or rename is confirmed with the
  maintainer first.
- **Policies.** `rotate` and `random` stay script-level, on a stated
  predicate contract: predicates change nothing, state advances on tracks
  played.
- **Merged rotation.** Specified by behaviour (S5); construction is free.
- **Silence.** A source that never ends a track resolves to the `live`
  profile.
- **Cross.** The rest of `cross` gets its own extraction before the rebuild
  changes it ([cross-seam.md §4](cross-seam.md#4-open)).

## 5. Constraints on the rebuild

- Target: OCaml, in this tree. The source contract changes as
  [source-contract.md](source-contract.md) states, for every source. The
  clock has its own specification at `src/core/clock/spec/`.
- Fixes land on `main` only.
- Read boundary for the rebuild session: this specification, the
  known-complexity list, build and CI files, fixtures. Not the previous
  sources of `switch`, `sequence`, `source.dynamic`, `cross`, the source base
  class or the script libraries built on them, not their tests, not
  `ocaml-notes.md` unless the maintainer says so.
