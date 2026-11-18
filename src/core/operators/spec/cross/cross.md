# Cross (Part A)

Plays the tracks of its child, and at each boundary between two of them
plays, in place of the end of one and the start of the next, a source that a
script function builds out of both.

The seam and the source contract are
[../switch/frame-assembly.md](../switch/frame-assembly.md) and
[../switch/source-contract.md](../switch/source-contract.md). The way the
child is read is [child-clock.md](child-clock.md). The transition function,
its arguments and its result are [transition.md](transition.md).

## 1. Parameters

`cross(~id, ~duration, ~override_duration, ~persist_override, ~width, transition, child)`,
with the method `cross_duration()`.

| Parameter           | Default                | Meaning                                                                              |
| ------------------- | ---------------------- | ------------------------------------------------------------------------------------ |
| `duration`          | `5.`                   | Seconds, a constant or a function. The **cross duration** ([§3](#3-cross-duration)). |
| `override_duration` | `"liq_cross_duration"` | Metadata key that replaces `duration` for a track ([§3](#3-cross-duration)).         |
| `persist_override`  | `false`                | Keep a replaced duration for the tracks that follow.                                 |
| `width`             | `2.`                   | Seconds. The window of the level analysis ([§7](#7-level-analysis)).                 |
| `transition`        | none                   | The transition function ([transition.md](transition.md)).                            |
| `child`             | none                   | The source whose tracks are crossed. Audio is required.                              |

- `cross_duration()` returns, in seconds, the cross duration of the track
  being buffered.
- A `width` that is not positive MUST be refused at creation, with an
  invalid-value error on that argument.
- The child MUST pass the creation check of
  [child-clock.md §6](child-clock.md#6-creation-check).

Scripts reach this operator through the standard library
([script-layer.md §1](script-layer.md#1-the-cross-wrapper)).

## 2. State

Lengths and positions are counted in samples of the stream. A duration in
seconds is converted by truncation. Nothing in this file depends on the
frame size or on where a request starts or stops: `n` below is the room a
request asks for, and every rule gives the same stream for any sequence of
`n`.

- The **buffer**: what the child delivered and `cross` has neither delivered
  nor handed to a transition, in order, with marks and metadata at their
  positions and with the places where the child stated a track end
  ([child-clock.md §2](child-clock.md#2-a-tick-of-the-child-clock)). It
  holds at most two tracks:
  - the **current part**: the rest of the **current track**, of length `r`;
  - the **next part**: the start of the **next track**, of length `La`,
    present only once the end of the current track was **found**: the child
    stated it and `cross` read up to it.

  The next track **has begun** when the next part holds at least one sample.
  Its end may be found too; `cross` never reads the child past it.

- `phase`:

  | Phase        | `cross` delivers                               |
  | ------------ | ---------------------------------------------- |
  | `idle`       | Nothing. There is no current track.            |
  | `track`      | The current part.                              |
  | `transition` | The **transition source** ([§6](#6-crossing)). |

- Per track: its cross duration ([§3](#3-cross-duration)), its metadata
  ([§9](#9-metadata)) and its level ([§7](#7-level-analysis)).
- A pending abort ([§10](#10-abort)).

The phase and the buffer change only while a request is served, when an
abort is carried out, and at wake and sleep.

## 3. Cross duration

Each track of the child has its own cross duration. It governs how far ahead
that track is read, and the crossing at **that track's end**
([§5](#5-the-end-of-a-track)). `D` is the duration of the track being
buffered: the next track once it has begun, the current one otherwise. `E`
is the duration of the current track once its end was found, as it was at
that moment.

**Resolving** a value in seconds: converted to samples; a negative value
resolves to zero and SHOULD be logged. Zero is a valid duration: the tracks
do not overlap, and the transition function is still called
([§6](#6-crossing)).

A track's duration is, in order:

1. the last **override** found in the track's own metadata as it is
   buffered: a metadata holding the key `override_duration` with a value
   that parses as a float. It applies from the sample that carries it. A
   value that does not parse is ignored;
2. with `persist_override`, the last override found in an earlier track
   since `cross` was woken;
3. `duration`, read when the first sample of the track is buffered. A
   function is called once per track, at that moment.

- An override MUST be read from the child's data as it is buffered, once,
  and from nowhere else: not from what `cross` delivers.
- A track's override MUST NOT govern the end of any other track, unless
  `persist_override`.
- Without any override in the stream, `persist_override` changes nothing.

Rationale: one number serves three uses: how far to read, how much of the
ending track to hand over, how much of the starting one. Tying it to a track
and freezing it as `E` when that track's end is found keeps the three
apart.

## 4. Reading the child

`cross` reads its child only while serving a request
([child-clock.md §4](child-clock.md#4-reading)). A probe never does. Before
it delivers anything for a request of room `n`, `cross` **tops up**:

1. While the end of the current track is not found: read until the current
   part holds `D + n`, in `track` and `idle`, or `D`, in `transition`.
2. Once it is found, while the end of the next track is not: read until the
   next part holds the **starting window**, `W = max(E, fade-in window)`,
   and at least one sample.
3. Reading stops when the child is not available. That is not an end.

The **fade-in window** is `liq_fade_in_delay + liq_fade_in`, read from the
next track's metadata ([§9](#9-metadata)) as buffered so far, each counting
as zero when missing or not a float. It is evaluated again as data arrives.

The next part is **complete** when it holds `W`, when the end of the next
track was found, or when the child is exhausted.

- The end of a track is where the child stated `track end`, and nowhere
  else. A segment of the child that is short, empty or ends `late` or
  `pause` MUST NOT be taken for one.
- A track of the child that holds no sample is not a track for `cross`: its
  two boundaries are one.
- The work of one request is bounded by `D + n + W`. It MUST NOT depend on
  how long `cross` was left unread.

**How far ahead.** The child is never read further than `D + n` past the
position `cross` delivered in the current track, or, once that track's end
is found, than the starting window of the next. The child is the consumer's
side of [source-contract.md §1b](../switch/source-contract.md#1b-delivery):
its track and metadata events, the selections, counts and script calls of
any selector below it, and the resolution of its requests all happen when
`cross` reads, that far before the listener hears the data. In particular
the next track is entered by the child when the end of the current one is
found. All of them MAY run ahead. What follows from it:

- events at the listener's time are those of `cross` itself
  ([§8](#8-delivery));
- a skip applies to the track being heard ([§10](#10-abort)).

## 5. The end of a track

Once the end of the current track is found, `cross` plays that track up to
the **crossing point** and no further. With `r` the length of the current
part:

- the **head** is `h = r − min(E, La)`, or zero when that is negative: what
  of the current track is played before the crossing point;
- `cross` delivers the head, in as many requests as it takes, topping up
  the next part at each;
- when the head is zero:
  - the next track has begun: the **crossing** takes place
    ([§6](#6-crossing)), with whatever the two parts hold;
  - it has not: there is no crossing. The current track is over, the phase
    is `idle`, and a track arriving later is entered as from `idle`. The
    transition function MUST NOT be called.

So, while the next part is not complete, the current track keeps playing as
long as it has more left than was buffered of the next one; and a track
that ends with nothing after it, the last of the stream included, plays to
its last sample and ends.

In both cases the segment that reaches the crossing point ends with
`track end`. It MAY be empty.

## 6. Crossing

A crossing is decided where the head is zero and the next track has begun.
It is committed when the reader requests past that point
([frame-assembly.md §3](../switch/frame-assembly.md#3-commit)), and only
then:

1. `X` ← `min(E, r, La)`: the **overlap**. At this point `X = r`.
2. `S` ← `min(La, max(X, fade-in window))`: what the transition gets of the
   next track.
3. The **ending source** is the current part, whole. The **starting
   source** is the first `S` of the next part. Both are finite sources as
   in [transition.md §1](transition.md#1-arguments).
4. The levels and the adjusted metadata of both sides are computed
   ([§7](#7-level-analysis),
   [transition.md §2](transition.md#2-metadata-adjustments)).
5. The next track becomes the current track: the current part is what the
   next part held past `S`, its end found or not as it was; the per-track
   state of the ending track is dropped.
6. The transition function is called. Its result is the **transition
   source**; it is woken in the clock of `cross`
   ([§13](#13-lifetime)).
7. If the transition source answers `nothing` to its first probe, it is
   released and replaced by the ending source followed by the starting
   source, the latter beginning a track. SHOULD be logged.
8. The phase is `transition`. The first sample delivered is entered with
   `track`.

`X` and `S` MAY be zero: a cross duration of zero, or a track the previous
transition used whole. The function is called all the same, with empty
sources.

Rationale for step 7: `cross` stated a track end and answered ready before
script code ran. Whatever the function returns, both tracks are still heard
and the stream does not stop while the child has data.

**While the transition plays**, `cross` keeps topping up
([§4](#4-reading-the-child)), so the current track is one duration ahead
when the transition is over, and a current track whose end is found has its
next part ready.

**The transition is over** as stated in
[transition.md §4](transition.md#4-end-of-the-transition). `cross` then
releases it and the phase is:

- `track` when the current part is not empty, or the end of the current
  track is not found;
- otherwise the end of a track at a head of zero
  ([§5](#5-the-end-of-a-track)): the next crossing, or `idle`.

## 7. Level analysis

Two levels are handed to the transition function:

- the **ending level**: over the last `width` seconds of the ending track
  before its end, or over all that `cross` read of it when that is less;
- the **starting level**: over the first `width` seconds of the next part,
  or over all of it when it is shorter.

A level is `20·log10(sqrt(q / (count · channels)))`, with `q` the sum of the
squared samples of all channels over the window and `count` its length in
samples. Over an empty window it is minus infinity.

- The ending window reaches back into data already delivered: an
  implementation keeps what it needs as it reads.
- The result MUST NOT depend on how the data was cut when read. An
  implementation MAY approximate the edges of a window by less than the
  size of one read, as long as two runs of the same stream agree.

## 8. Delivery

`cross` is a user of the seam. Segments, entries and end statements are
those of [frame-assembly.md](../switch/frame-assembly.md).

**In `idle`.** A request tops up and begins the current track: the phase is
`track`, the first sample is entered with `track`.

**In `track`.** A request tops up, then delivers from the current part: up
to `n` samples, and never past the crossing point.

| The segment stops because                                | It ends with |
| -------------------------------------------------------- | ------------ |
| `n` samples were delivered                               | `full`       |
| the crossing point, or the end of the track, was reached | `track end`  |
| the current part ran out and its end is not found        | `late`       |

Reaching the crossing point exactly at `n` samples is `track end`.

**In `transition`.** A request tops up, then relays one segment of the
transition source. When that segment ends, whatever its reason, `cross`
looks at whether the source is now exhausted:

| The transition source's segment               | `cross` states                                     |
| --------------------------------------------- | -------------------------------------------------- |
| ends `full`, `pause` or `late`; not exhausted | The same.                                          |
| ends `track end`; not exhausted               | `track end`. What follows is entered with `track`. |
| ends for any reason; the source is exhausted  | As when the transition is over, below.             |

**When the transition is over:**

| What follows                                             | Statement   | Entry                     |
| -------------------------------------------------------- | ----------- | ------------------------- |
| The current track goes on, now or after a wait           | `pause`     | `continue`                |
| The current track has nothing left and its end was found | `track end` | `track`, for what follows |

A transition that is passed over at the start of a cycle
([transition.md §4](transition.md#4-end-of-the-transition)) made no
statement where it stopped: what follows is entered as in this table, and
the probe answers accordingly ([§11](#11-readiness)).

An entry `track` that is owed and was not applied, because nothing was
delivered since, stays owed: it is applied to the first sample delivered,
whatever was stated meanwhile.

So a crossing puts **one mark, on the first sample delivered after the
crossing point**: the first sample of the transition source, or, when that
delivers nothing, of the track that follows. The head carries none, and
there is none where the transition gives way to the track. Marks the
transition source carries after its first sample are its own tracks and
pass: a transition that wants a crossing to be one track carries none. A
transition source that states a track end and then has nothing more,
without declaring exhaustion, has stated that a track begins where it gives
way.

The track event of a crossing carries the metadata found on its first
sample, or none
([frame-assembly.md §6](../switch/frame-assembly.md#6-joins)).

## 9. Metadata

Metadata stays in the buffer at its position and is delivered with the
data. `cross` adds none, except by the announcement rule below.

**A track's metadata** is the union, key by key, of every metadata `cross`
buffered of that track since its first sample, the latest value winning. It
starts empty with each track. It is what the transition function receives,
after adjustment of a copy
([transition.md §2](transition.md#2-metadata-adjustments)); the adjusted
copy is used for nothing else.

Rationale: a metadata inserted in the middle of a track, a title update for
instance, does not erase the fade keys the track opened with.

**Announcement.** The starting source holds the metadata of the start of
the next track, and script code decides whether it is played. When the
transition is over:

- if the starting source held metadata, and
- the transition source delivered none of it, and
- the track goes on after the transition,

then `cross` MUST ask for that track's metadata, as accumulated over the
starting source, to be inserted on the first sample it delivers after the
transition
([source-contract.md §4](../switch/source-contract.md#4-metadata)).
Otherwise it inserts nothing.

- Whether a metadata was delivered MUST be told from its passage through
  the transition source, never by comparing values: two tracks with equal
  metadata are two announcements.
- A child that carries no metadata gets none announced.
- Whatever mechanism tells the passage MUST leave no trace in what `cross`
  delivers.

So each track is announced once, no earlier than its audio, whatever the
transition keeps of its two sources.

## 10. Abort

An abort is requested, then carried out when the running cycle is over
([source-contract.md §6](../switch/source-contract.md#6-abort)). Nothing
changes before that, and nothing of it runs on the thread that asked. When
it is carried out:

1. A transition source being played is released, with its callbacks
   ([§13](#13-lifetime)), and with what it had left of both tracks.
2. The current part is discarded, with the per-track state of the current
   track.
3. If the end of the current track was not found, the abort is forwarded to
   the child, at once, and what the intake holds is discarded. If it was
   found, the child is already on another track: the abort MUST NOT be
   forwarded, and the next part is kept.
4. The phase is `idle`. A next part that was kept becomes the current part
   at the next request.

- Nothing of the aborted track is delivered afterwards, in any phase.
- The track that follows is entered with `track`, with its own mark and
  metadata. There is no crossing out of an aborted track.
- An abort during a transition aborts the track that was starting: for the
  listener it began at the crossing's mark.
- In `idle` an abort changes nothing and is not forwarded.

## 11. Readiness

`cross` is always fallible. Its probe answers, without reading the child
and without running script code:

| State                                                         | Answer                                          |
| ------------------------------------------------------------- | ----------------------------------------------- |
| `idle`, the buffer holds data or the child is available       | `new track`                                     |
| `idle` otherwise                                              | `nothing`                                       |
| `track`, a crossing is decided ([§6](#6-crossing))            | `new track`                                     |
| `track`, the current part is not empty                        | `more`                                          |
| `track`, the current part is empty, the child is available    | `more`                                          |
| `track` otherwise                                             | `nothing`                                       |
| `transition`, the transition source does not answer `nothing` | Its answer.                                     |
| `transition`, it answers `nothing`                            | As in `track`, for what follows the transition. |

The child is **available** as in
[child-clock.md §4](child-clock.md#4-reading).

Each answer is backed by something `cross` holds or has probed in this
cycle, so that ready means delivering now:

- a non-empty current part is delivered at once;
- a decided crossing delivers from the transition source, or from the two
  sources `cross` holds ([§6](#6-crossing), step 7), or from the track that
  follows when both are empty;
- an available child delivers when read
  ([source-contract.md §2](../switch/source-contract.md#2-probes));
- a transition source that has nothing while the track cannot go on leaves
  `cross` not ready and the transition in place: nothing is committed by a
  probe.

Where `cross` answered `more` and the top-up finds that the current track
ends before any sample, it delivers an empty segment ending `track end`, as
[source-contract.md §2](../switch/source-contract.md#2-probes) allows.

## 12. Other properties

- **Remaining.** The time to the next mark `cross` will put, or unknown:
  - `idle`: the child's;
  - `track`, end found: the head;
  - `track`, end not found: `r` plus the child's, less `D`, not below zero;
    unknown when the child's is;
  - `transition`: the transition source's plus what `track` would answer;
    unknown when either is.
- **Effective source.** `cross` itself, in every phase.
- **Composition.** `inherit`. By the resolution rule of
  [composition.md §1](../switch/composition.md#1-tags) it resolves to its
  child's tag in every phase: the tracks of `cross` are those of its child.
- **Sync source.** None in `idle` and `track`: the child's is the child
  clock's affair ([child-clock.md §7](child-clock.md#7-sync-source)). In
  `transition`, that of the transition source. `cross` tells its clock at
  the commit of a crossing and when a transition is over, by the rule of
  [frame-assembly.md §9](../switch/frame-assembly.md#9-contract-clauses).
- **Exhaustion.** Declared when the child is exhausted, the buffer and the
  intake are empty and no transition is being played.

## 13. Lifetime

- **Clocks.** The child, and nothing else, is in the child clock. Every
  source `cross` builds, and the transition source, MUST be in the clock of
  `cross` before it is woken.
- The transition source is woken at the commit of its crossing and released
  when `cross` leaves it: over, replaced (step 7), aborted, or at sleep.
  The ending and starting sources are woken by whatever reads them; `cross`
  drops its own hold on them with the transition.
- Callbacks registered by the transition function are released with the
  transition ([transition.md §5](transition.md#5-callbacks)).
- **Wake.** The phase is `idle`, the buffer empty, no per-track state, no
  persisted override. The child is woken through the child clock
  ([child-clock.md §5](child-clock.md#5-lifetime)).
- **Sleep.** A transition source being played is released with its
  callbacks; the buffer and the intake are discarded; the child is
  released. Nothing `cross` built stays awake.

## 14. Seam answers

In the order of
[frame-assembly.md §9](../switch/frame-assembly.md#9-contract-clauses).

- **C1.** In `idle`: the current track, when [§11](#11-readiness) answers
  ready; the commit tops up and enters `track`. At a crossing point: the
  crossing ([§6](#6-crossing)); the commit runs the transition function. In
  `transition`, when the transition source is over: the current track or
  the next crossing; the commit releases the transition.
- **C2.** None.
- **C3.** [§8](#8-delivery).
- **C4.** `track` from `idle`. After a wait inside a track the stream
  continues where it stopped: no entry is applied.
- **C5.** [§13](#13-lifetime).
- **C6.** [§10](#10-abort), [§12](#12-other-properties).
- **C7.** Always fallible. Exhaustion: [§12](#12-other-properties).

## 15. Worked example

Derived from the rules above. `duration` = 1 s; requests of 0.02 s; a child
playing three tones of exactly 3 s, `a`, `b`, `c`, each titled on its first
sample, then exhausted; the transition adds its two sources. Times are
positions in the output of `cross`.

| Time | Event                                                                                                                                                                                                |
| ---- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 0.00 | Request in `idle`: 1.02 s of `a` read. Track `a` begins, with its title.                                                                                                                             |
| …    | Each request reads what it delivers: the current part holds 1.00 s after it.                                                                                                                         |
| 1.98 | The top-up reaches the end of `a`: found, `E` = 1 s, `r` = 1.02 s. 1.00 s of `b` read: `La` = 1 s, complete. Head: 0.02 s, delivered. The segment ends `track end`.                                  |
| 2.00 | Crossing: `X` = `S` = 1 s. The function is called; both sources report 1 s remaining. A track begins on the first sample of the transition, with the title of `b`. The top-up reads 1 s more of `b`. |
| 3.00 | The transition source is exhausted: `pause`. `b` goes on from its second second, with no mark.                                                                                                       |
| 4.00 | Second crossing, same figures, `b` to `c`.                                                                                                                                                           |
| 5.98 | The end of `c` is found; the child is exhausted: the next part is complete and empty. Head: 1.02 s.                                                                                                  |
| 7.00 | `c` played to its last sample: `track end`, no transition. `cross` is exhausted.                                                                                                                     |

The output is 7 s long: 9 s less two overlaps of 1 s. It is the same with
tones of 3.01 s (7.03 s), with requests of any size, and with tracks that do
not end where a request does.
