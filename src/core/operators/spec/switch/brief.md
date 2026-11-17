# Brief for the normative pass and the rebuild

Framing set by the maintainer during the as-built pass. It applies to every
session and every agent that works from this directory.

## 1. Principle: explicit signals, never inference from pulls

> Any logic based on waiting for a data pull is prone to this kind of issues.
> Explicit signaling is always better.

A component MUST NOT deduce what happened to it from whether, when or how
often it was asked for data: tick gaps, "not animated since", "read but not
consumed", readiness probes. The party that makes a decision tells the parties
it affects.

Apply it to every rule in the as-built spec, not only to the one that
prompted it. Known instances, each to be replaced by a stated signal:

| Today (inferred)                                                      | Who actually knows                                            |
| --------------------------------------------------------------------- | ------------------------------------------------------------- |
| "I was not animated for more than one tick, so my selection is stale" | The parent: it preempted the child or left it at a boundary.  |
| "A track mark was generated, so a track played"                       | The consumer: it took the data or did not.                    |
| "My readiness was asked, so I am about to be read"                    | The asker: it is probing or committing.                       |
| "A search is running, so a track ended"                               | The switch: it knows why it searches.                         |
| "More than one item is left, so this join is merged"                  | The operator that advances: it knows which join it is making. |

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
(today: `on_leave` with `track_sensitive = false`, and the file profile's
skip). A selector told that its track was aborted treats its selection as
ended and searches at its next read.

Consequence to state in the spec: a source that is not skipped when preempted
(live profile) resumes where it was.

An earlier version of this section proposed rule b alone. It does not cover
S2 as reported: there the programme source is a file source and the nested
selector is left at a boundary, not preempted.

## 3. Scenarios that must both hold

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

## 4. Questions the normative pass must ask before rewriting

- Which of the script-visible names in [README.md](README.md) are frozen.
- Whether `all_predicates` survives ([findings.md](findings.md) D7).
- Whether `rotate` and `random` stay script-level policies, which requires a
  stated predicate contract and a "track heard" event, or move into the
  operator.
- Whether the frame-assembly seam may change for `cross`, `sequence` and
  `source.dynamic` too. A fix to merged sequences is waiting for this rebuild
  and assumes it may ([findings.md](findings.md) D9). If so: what happens to
  the callers D9 lists, whose track start moves.
- Whether `source.dynamic` and the merged looping rotation join the scope
  (findings G8).

## 5. Constraints on the rebuild

- Target: OCaml, in this tree, against the existing source and clock
  contracts. The clock has its own spec at `src/core/clock/spec/`.
- Fixes land on `main` only.
- Read boundary for the rebuild session: the normative spec, the
  known-complexity list, build and CI files, fixtures. Not the previous
  `switch` source, not its tests, not `ocaml-notes.md` unless the maintainer
  says so.
