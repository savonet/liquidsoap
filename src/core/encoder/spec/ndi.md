# `%ndi` and `output.ndi`

Part A. `%ndi` is a format that no writer answers: it gives `output.ndi` its
frame type, and `output.ndi` sends media frames, not bytes, to an NDI sender
([encoder.md §1](encoder.md#1-registry), item 7). This file owns `%ndi`'s
parameter specification and `output.ndi`. It answers the sections of
[encoder.md §10](encoder.md#10-implementation-files): §1, and "Nothing."
elsewhere.

It relies on: format values ([formats.md](formats.md)); the output base
([output.md](../../outputs/spec/output.md)); sync sources and pacing
([pacing.md](../../clock/spec/pacing.md)); the fault classes
([faults.md](../../../spec/faults.md)); concurrency
([concurrency.md](../../../spec/concurrency.md)).

## 1. Format and options

`%ndi(...)` takes only sub-encoders, without parameters except the channel
parameters of `%audio` ([formats.md §2](formats.md#2-names-and-kinds)):

| Parameter                                                       | Effect                                                           |
| --------------------------------------------------------------- | ---------------------------------------------------------------- |
| `%audio`, `%audio(channels=n)` and the other channel parameters | audio track on, with that channel count (literal, type-deciding) |
| `%audio.none`                                                   | audio track off                                                  |
| `%video`                                                        | video track on                                                   |
| `%video.none`                                                   | video track off                                                  |

- Both tracks are on by default; a sub-encoder that names one track leaves
  the other at its default. All four are type-deciding.
- The type has an `audio` PCM track when audio is on, of the channel count
  `%audio` gives, 2 by default ([formats.md §4](formats.md#4-typing)), and a
  `video` track of the default video content type when video is on.
- Errors, at type checking, with the position of the parameter or of the
  format ([formats.md §11](formats.md#11-errors)):
  - both tracks off: "`%ndi` needs an audio or a video track";
  - any other parameter, or a sub-encoder given a parameter other than a
    channel parameter of `%audio`: an error naming it and listing those
    accepted;
  - a channel count NDI cannot carry: a type error at its position.
- Helpers: extension, MIME, `bitrate` and `codecs` are none.
- Printed as `%ndi(%audio,%video)`, `%ndi(%audio,%video.none)`,
  `%ndi(%audio.none,%video)`, with `%audio(channels=n)` when the count is
  not 2.
- Every output other than `output.ndi` refuses `%ndi` at construction, with
  an invalid-value error on its format argument that names `output.ndi`.
- **Availability.** `%ndi` is available (it types, and `%ifencoder(%ndi)`
  takes its first branch) exactly when `output.ndi` is in the build, although
  no writer answers it. Reason: availability is otherwise defined by a
  writer answering ([encoder.md §1](encoder.md#1-registry)); without this
  rule `%ndi` would be an unavailable format and every script using it
  would fail type checking.

The audio channel count is 2 by default, the shared default. Migration: a
script that set `settings.frame.audio.channels` to another count and fed
`%ndi` a source of that count gets a type error; it writes
`%ndi(%audio(channels=n), …)` with that count.

## 2. Declarations

Nothing.

## 3. Creation

Nothing.

## 4. Input

Nothing.

## 5. Bytes and markers

Nothing.

## 6. Header forms and preambles

Nothing.

## 7. Metadata

Nothing.

## 8. Delay and padding

Nothing.

## 9. End

Nothing.

## 10. Properties

Nothing.

## 11. Live latency

Nothing.

## 12. Segmenting face

Nothing.

## 13. Delegated destination

Nothing.

## 14. Concurrency

Nothing.

## 15. Faults

Nothing.

## 16. Tests

Nothing. The checks of `%ndi`'s format are in `output.ndi`'s (§N8).

---

## `output.ndi`

`output.ndi` exists only in a build with the NDI binding. It drives no
writer, so it answers the caller numbering of
[encoder.md §11](encoder.md#11-caller-files) only where the seam reaches it:
the format check (§N1). The rest is its own.

### N1. Parameters and checks at evaluation

`output.ndi(library_file, ?name, ?groups, ?self_sync=false, format, source)`
with the common parameters ([output.md §1](../../outputs/spec/output.md#1-common-parameters-methods-and-callbacks)).

- `format` MUST be an `%ndi` value; any other format is an invalid-value
  error on `format` that names `%ndi`.
- `library_file` MUST name a readable NDI library, loaded at evaluation. A
  file that is absent, unreadable or not an NDI library is an invalid-value
  error on `library_file`. The library is process-wide: a second
  `output.ndi` naming another file than the one loaded is an invalid-value
  error naming both.

### N2. The sender

- A run has one NDI sender, created at the output's start with `name`,
  `groups` and the clocking of §N4, and destroyed at its stop. A restart, a
  reset included, creates a new sender, whose position starts at 0.
- A sender that cannot be created is an output fault (§N6).

### N3. What is sent

For each piece of each streaming cycle
([output.md §3](../../outputs/spec/output.md#3-a-streaming-cycle)):

- **Audio**, when on: the piece's samples, every channel, as one planar
  32-bit float audio frame at `frame.audio.samplerate`, channel stride the
  sample count × 4 bytes.
- **Video**, when on: each image of the piece, in order, as one I420 frame
  at the frame's video size and frame rate, progressive. An image of another
  size is adapted the way a writer adapts it
  ([encoder.md §3.3](encoder.md#33-frames-in)).
- **Timecodes**, in units of 100 ns: content at position `p` main ticks
  since the sender was created has timecode `⌊p × 10 000 000 / main_rate⌋`,
  computed from exact integers for every frame. Reason: a per-tick factor
  rounded once makes timecodes drift from the media's own time, by 0.33 %
  at a main rate of 44 100.
- Metadata and track marks are not sent.

### N4. Pacing

| `self_sync` | Tracks     | The sender is clocked on | Sync source of the output       |
| ----------- | ---------- | ------------------------ | ------------------------------- |
| `false`     | any        | nothing                  | none: its source's              |
| `true`      | with video | video                    | a self-paced sync source, `ndi` |
| `true`      | audio only | audio                    | a self-paced sync source, `ndi` |

With `self_sync=true`, the clocked send's wait is the pacing of the clock
([pacing.md §2](../../clock/spec/pacing.md#2-sync-source)), the one wait the
streaming thread allows
([concurrency.md §2](../../../spec/concurrency.md#2-the-streaming-thread)).
The output declares the sync source while its sender exists, and withdraws
it at stop.

### N5. Concurrency

Sends run on the streaming thread. With `self_sync=false`, a send never
waits. Each send hands the SDK a frame it can read until the send returns;
the output keeps no frame across cycles.

### N6. Faults and restart policy

| Fault                                                       | Class               | Outcome                                                                                                                                                                                                          |
| ----------------------------------------------------------- | ------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| a format or library refused (§N1)                           | configuration error | the script fails before it starts                                                                                                                                                                                |
| the sender cannot be created; a send the SDK reports failed | output fault        | logged at `severe`; the sender is destroyed, frames are dropped, and a new sender is created no sooner than `fault_retry_delay` ([faults.md §3](../../../spec/faults.md#3-source-faults-reach-the-script)) later |

The clock and the other outputs go on.

### N7. Lifecycle callbacks

`on_start` and `on_stop` follow the sender's runs
([output.md §2](../../outputs/spec/output.md#2-states)).

### N8. Tests

With an SDK double recording frames, timecodes and the clocking flags:

- `%ndi(%audio.none, %video.none)` fails `--check` with the message of §1,
  with a position; `%ndi(foo)` and `%ndi(%audio(x=1))` likewise.
- `%ndi(%audio(channels=6), %video.none)` types with a 6-channel audio
  track and sends 6 channels.
- `output.file(%ndi, …)` is refused at evaluation, naming `output.ndi`.
- One hour at a main rate of 44 100 with 25 fps: the last frame's timecode
  equals its media time to within 100 ns.
- Each `self_sync` and track row of §N4: the clocking flag and the sync
  source declared.
- A sender double that fails creation: the output logs, retries after
  `fault_retry_delay`, and the clock goes on.
- An image of another size than the frame's is sent at the frame's size.
