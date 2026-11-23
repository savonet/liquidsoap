# `output.external`, the `pipe` operator and the language hooks

Part A. Two operators that put encoded bytes into a child process, and the
hooks through which the language evaluates formats:

- `output.external`: an output whose sink is a process's standard input
  (Part 1). It answers the caller numbering of
  [encoder.md §11](encoder.md#11-caller-files) and states only where it
  departs from [output-file.md](output-file.md).
- the `pipe` operator: a source that sends its input's audio through a
  process, as a WAV stream, and reads audio back (Part 2). It drives a WAV
  writer into a process sink, and answers the same numbering.
- the language hooks (Part 3), owned by [formats.md](formats.md).

It relies on: [output-file.md](output-file.md) (reopen triggers, restart
policy, the bounds of a sink); the encoded-output base
([output-encoded.md](output-encoded.md)); the writer and sinks
([encoder.md](encoder.md), [sink.md](sink.md)); the WAV writer
([wav.md](wav.md)); the output base ([output.md](../../outputs/spec/output.md));
pacing ([pacing.md §2](../../clock/spec/pacing.md#2-sync-source)); faults
([faults.md](../../../spec/faults.md)).

## The process sink

Both operators write into a **process sink**: the standard input of a child
process they start. Its rules, which Parts 1 and 2 use:

1. Capabilities: `fresh`, never `seekable` ([sink.md §1](sink.md#1-capabilities));
   nothing to confirm. The writer writes its streaming form, and by default
   no preamble and no tags; one the script requested explicitly is written
   ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles), items 5
   and 7).
2. Starting: the command is run by the system shell, on work allowed to
   block. The writer writes from its first piece while the process starts;
   the sink holds those bytes within `open_hold` and delivers them, from the
   first, once the process runs
   ([sink.md §3](sink.md#3-never-blocking-the-streaming-thread)).
3. Writing: queued, delivered on work allowed to block or on a non-blocking
   descriptor, within `sink_backlog`; `open_hold`, `sink_backlog` and
   `write_buffer` as [output-file.md §2.4](output-file.md#24-bounds-and-writes).
   A write never blocks the streaming thread, and a process that stops
   reading reaches `sink_backlog`.
4. Watching: the sink observes the process's exit. An exit before the sink's
   end is a sink fault, reported when observed, at the latest at the next
   write; the fault names the command and the exit status or signal.
5. Ending: after the final bytes, the sink closes the process's standard
   input and waits for its exit, on work allowed to block, up to
   `process_exit_bound`; then it asks the process to terminate, and after
   `process_exit_bound` again forces it. An exit status other than 0 at this
   point, or a forced end, is logged at `severe` naming the command, and is
   not escalated ([sink.md §7](sink.md#7-faults)).

   | Parameter            | Recommended | Protects                                                                                             |
   | -------------------- | ----------- | ---------------------------------------------------------------------------------------------------- |
   | `process_exit_bound` | 5 s         | The next run and shutdown, against a process that does not exit or a child that keeps its pipes open |

   This file owns `process_exit_bound`; the `%external` writer's own
   processes use it too ([external.md §3](external.md#3-creation)). The
   delivery of the final bytes before the input is closed is bounded by
   `final_delivery_bound` ([sink.md §4](sink.md#4-markers-and-the-end)).

6. A process sink never starts a process while the previous process of the
   same operator still runs: the new run starts once the previous one has
   exited or was forced to. Reason: two runs of one command commonly write
   to one resource (a server mount, a file).
7. The process's standard output and standard error are Liquidsoap's own
   for `output.external`; Part 2 states its own.

## Part 1. `output.external`

### 1. Formats and declarations

Parameters: those of [output-file.md §1.1](output-file.md#11-parameters)
except `append`, `perm`, `dir_perm` and `on_close`, with
`export_cover_metadata` defaulting to `false`; plus `self_sync` (bool,
default `false`). The second unlabelled argument is the command (string
getter). The default `id` is `output.external`.

Declarations: `fresh`; no readers mid-stream. The checks of
[encoder.md §5.3](encoder.md#53-the-check) refuse a configuration that needs
a seekable destination or needs no byte sink. An `%ffmpeg` `truncate` option
or an option of FFmpeg's file protocol, set explicitly, is refused: the
output owns its destination.

### 2. The sink

A process sink (above). The command getter is called exactly once per start
attempt, by the work that starts the process, never at construction. The
command is used as the getter returns it: no `$(key)` interpolation. Reason:
a metadata value placed in a shell command can end the shell word it sits in
and run anything; a script that wants metadata in its command builds it in
the getter, quoting each value for the shell.

`flush` as [output-file.md §2.4](output-file.md#24-bounds-and-writes).

**Pacing.** With `self_sync` false the output is not a sync source, and its
self-sync is its source's
([output.md §5](../../outputs/spec/output.md#5-sync-and-pacing)). With
`self_sync` true it is a self-paced sync source
([pacing.md §2](../../clock/spec/pacing.md#2-sync-source)): its cycle waits,
inside the tick, while the sink holds more than `pacing_backlog` of stream
time, so that a process consuming at its own rate (a sound player) paces the
clock. The wait is bounded by `sink_backlog`, past which it is a sink fault.

| Parameter        | Recommended               | Protects                                            |
| ---------------- | ------------------------- | --------------------------------------------------- |
| `pacing_backlog` | two frames of stream time | The latency between the stream and a pacing process |

### 3. Writer lifetime

One writer per process run. A run starts at the output's first piece after a
start, a reopen or a retry, as a file opens in
[output-file.md §3](output-file.md#3-writer-lifetime): no piece, no process.
Initial metadata:
[output-encoded.md §3.2](output-encoded.md#32-initial-metadata).

### 4. Pieces and metadata

As [output-file.md §4](output-file.md#4-pieces-and-metadata), with
`in_band_metadata` and `reopen_on_metadata`. Out of band: none.

### 5. Markers and readers

The process reads from byte 0: the sink ignores stream starts and access
points, and acts on `end`.

### 6. End

As [output-file.md §6.1–§6.2](output-file.md#6-end), the process sink ending
as its rule 5 states, its final bytes delivered within
`final_delivery_bound` (2 s for a process,
[sink.md §4](sink.md#4-markers-and-the-end)). There is no `on_close`.

### 7. Reopen, reconnect, append

The triggers, `on_reopen` and the reset of
[output-file.md §7.1–§7.3](output-file.md#7-reopen-reconnect-append): every
reopen and every retry is a new process run with a new writer, started once
the previous process has exited (rule 6). No append.

### 8. Faults and restart policy

| Fault                                                                                             | Class         | Outcome                                        |
| ------------------------------------------------------------------------------------------------- | ------------- | ---------------------------------------------- |
| the command getter raises; the process cannot be started                                          | output fault  | the restart policy                             |
| the process exits while the output streams; a write fails; `open_hold` or `sink_backlog` exceeded | output fault  | the writer ended abandoned; the restart policy |
| a writer fault                                                                                    | its class     | [encoder.md §8](encoder.md#8-faults)           |
| a non-zero exit or a forced end after the final bytes                                             | not escalated | logged at `severe` (rule 5)                    |

The restart policy is `reopen_on_error`, as
[output-file.md §8.2](output-file.md#82-the-restart-policy). The error's
kind is `system` for a system error or a process that exited, its message
naming the command.

### 9. Concurrency

As [output-file.md §9](output-file.md#9-concurrency), the command getter,
the process's start, its writes, its exit and the wait for it on work
allowed to block. The only wait on the streaming thread is the pacing wait
of `self_sync` true, which is a sync source's own.

### 10. Tests

Doubles: a process that records its standard input, one that exits early,
one that never reads, one that ignores the end of its input.

- The recording process receives, per run, a complete stream of the format
  from its first byte, the final bytes included; one run per reopen.
- A process exiting early is reported within one cycle of its exit, naming
  the command, and the retry delay is honoured.
- A process that never reads: the clock's latency stays bounded and the
  fault reaches the policy within `sink_backlog`.
- A process that ignores the end of its input is forced to end after
  `2 × process_exit_bound`, logged; the next run starts only then.
- Logs, the clock's name and the server commands say `output.external`.
- `%mp3` through `output.external`: the process receives no ID3v2 tag;
  `%mp3(id3v2=true)` gives one.
- With `self_sync` true and a process reading at half the real-time rate,
  the clock runs at that rate; with `self_sync` false, the output reports its
  source's self-sync.

## Part 2. The `pipe` operator

A source that sends its input's audio through a process and delivers the
audio the process returns, with its input's metadata and track marks put
back on it.

### 1. Formats and declarations

| Parameter          | Type   | Default | Use                                                                       |
| ------------------ | ------ | ------- | ------------------------------------------------------------------------- |
| `process`          | string |         | the command                                                               |
| `replay_delay`     | float? | `null`  | how metadata and track marks come back (§4)                               |
| `data_length`      | int?   | `null`  | the data length the WAV header declares; `null`: unknown (streaming form) |
| `buffer`           | float  | `1.`    | seconds of returned audio held before the operator is ready               |
| `max`              | float  | `10.`   | most seconds of returned audio held                                       |
| `restart`          | bool   | `true`  | start a new run when a run ends on its own                                |
| `restart_on_error` | bool   | `true`  | start a new run when a run fails                                          |
| (unlabelled)       | source |         | audio only                                                                |

No format: the operator writes through the WAV writer ([wav.md](wav.md)),
16-bit integer samples, its input's channel count, the internal audio rate.
A non-null `data_length` is an explicit header length: the header declares
it, and the operator does not enforce it. The operator is fallible.

### 2. The sink

A process sink (above), whose standard input receives the WAV stream. Its
standard output is read on work allowed to block, as a WAV stream: a header
of PCM audio, then samples. A header whose channel count differs from the
operator's, or that is not PCM, is a fault (§8). The audio returned is
converted to the internal audio rate from the rate its header declares. Its
standard error is logged at `debug`.

The rate the operator writes into the header it sends is always the rate of
the samples it sends; the rate the process declares in its own header is
read for conversion only, and never written.

### 3. Writer lifetime

One writer per process run. A run starts when the operator wakes up, and at
each restart (§7); it ends at a boundary (§4, replay mode off), at sleep,
and when the process ends.

### 4. Pieces and metadata

- The operator reads its input on its clock's streaming work, one frame per
  cycle, while a run is up and the returned-audio buffer holds less than
  `max`. Each frame is cut into pieces as
  [output-encoded.md §4.1](output-encoded.md#41-cutting-a-frame-into-pieces)
  and written, positions converted from main ticks to samples by the writer.
  The operator is not an output: unlike outputs, it also cuts at track marks,
  since each boundary below ends a run.
- A **boundary** is a position of the input carrying a metadata, a track
  mark, or both. Metadata and track marks are not sent to the process; the
  operator puts them back on the returned audio:
  - `replay_delay` null: at a boundary, the operator ends the writer clean,
    closes the process's input, and reads its output to the end; the
    boundary's metadata and track mark are placed together on the returned
    audio right after that output's last sample; a new run takes the input
    from the boundary on. One boundary costs one restart, whatever it
    carries. A boundary at a run's first sample costs none: it is placed at
    once, after the audio already returned.
  - `replay_delay = d`: no restart; the boundary is placed on the returned
    audio `d` seconds of returned audio after the position at which the
    input before it ended.
- The operator is ready once it holds `buffer` seconds of returned audio,
  and stays ready until it holds none.

### 5. Markers and readers

The process reads from byte 0: markers are ignored.

### 6. End

A run ending at a boundary or at sleep is a clean end: the final bytes reach
the process, which is waited for as the process sink's rule 5 states. At
sleep the returned audio still held is discarded, as a source going down
discards what it holds
([source-contract.md §5](../../source/spec/source-contract.md#5-consumption)).

### 7. Reopen, reconnect, append

| A run ended               | Next run                             |
| ------------------------- | ------------------------------------ |
| at a boundary (§4)        | at once, once the process has exited |
| on its own, exit status 0 | if `restart`                         |
| on a fault (§8)           | if `restart_on_error`                |
| at sleep                  | none                                 |

A run not started for a boundary starts at least `fault_retry_delay`
([faults.md §3](../../../spec/faults.md#3-source-faults-reach-the-script))
after the previous run started. Reason: a command that exits at once would
otherwise be restarted on every cycle. While no run is up and no audio is
held, the operator is not ready.

### 8. Faults and restart policy

| Fault                                                                          | Class        | Outcome                                                              |
| ------------------------------------------------------------------------------ | ------------ | -------------------------------------------------------------------- |
| the process cannot start, exits with a status other than 0, or its sink faults | source fault | the operator ends its track at the audio returned so far; §7         |
| the returned stream's header is not PCM WAV, or has another channel count      | source fault | the same; the message names the command and what the header declares |

Each is logged at `severe` with the command and the cause
([faults.md §3](../../../spec/faults.md#3-source-faults-reach-the-script)).

### 9. Concurrency

Reading the input and the writer's calls run on the operator's clock's
streaming work; the process's start, writes, output reading and exit run on
work allowed to block. The returned-audio buffer and the pending boundaries
are shared between them under
[concurrency.md §3](../../../spec/concurrency.md#3-shared-state). The input
is never read from any other thread.

### 10. Tests

- An identity process (`cat`) with `replay_delay` null: the returned audio
  equals the input, sample count exact, and every metadata and track mark
  comes back at its input position.
- A process with a fixed delay and `replay_delay` set to it: metadata come
  back within one frame of their input position, without restart.
- A metadata and a track mark at one mid-frame position restart the process
  once and come back together; with a video rate that makes main ticks finer
  than samples, cut positions stay exact.
- A resampling process (44.1 kHz in, 48 kHz out): every header the process
  receives, at every run, declares 44.1 kHz.
- A metadata at the input's first sample starts no empty run.
- A command that exits at once is restarted at most once per
  `fault_retry_delay`.
- A process returning another channel count ends the track, and a
  `fallback` above the operator switches away.

## Part 3. Language hooks

The language evaluates `%name(...)` and decides `%ifencoder` through hooks
the core installs. Their rules are owned by [formats.md](formats.md):
building and the lookup that follows it
([formats.md §5](formats.md#5-building)), the phase of each check
([formats.md §7](formats.md#7-when-a-format-is-validated)), and availability
with `%ifencoder` and `%ifnencoder`
([formats.md §8](formats.md#8-availability-and-ifencoder)).
