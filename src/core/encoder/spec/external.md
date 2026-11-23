# `%external`

Part A. The external writer: it runs a command as a child process, sends it
the stream as WAV (audio only) or AVI (with video) on its standard input, and
writes what the process prints on its standard output into the sink. This
file owns `%external`'s parameter specification and the child process's
lifecycle: start, input, output, restarts, end of output, stop and failure.
It answers every section of
[encoder.md §10](encoder.md#10-implementation-files) under the same number.

It relies on: the seam ([encoder.md](encoder.md)); format values and their
phases ([formats.md](formats.md)); sinks ([sink.md](sink.md)); the WAV and
AVI streaming forms the process receives ([wav.md §6](wav.md#6-header-forms-and-preambles),
[avi.md §6](avi.md#6-header-forms-and-preambles)); the fault classes
([faults.md](../../../spec/faults.md)); concurrency
([concurrency.md](../../../spec/concurrency.md)).

**The process is a black box.** The writer does not know the format, the
framing, the header or the latency of what the process prints. Every rule
below follows from that.

## 1. Format and options

`%external(...)`. Parameters, folded in order, last one wins
([formats.md §3](formats.md#3-the-parameter-specification)):

| Parameter                                            | Value            | Default                                             | Effect                                                              |
| ---------------------------------------------------- | ---------------- | --------------------------------------------------- | ------------------------------------------------------------------- |
| `process="cmd"`, or an anonymous string              | non-empty string | required                                            | the command line, run by the system shell                           |
| `mono`, `stereo`, `mono=b`, `stereo=b`, `channels=n` | literal          | 2 channels                                          | channel count, type-deciding ([formats.md §4](formats.md#4-typing)) |
| `samplerate=n`                                       | int, 1 to 2³²−1  | `frame.audio.samplerate`, deferred                  | the rate sent to the process                                        |
| `video=b`                                            | literal bool     | `false`                                             | with `true`, a video track and AVI input; type-deciding             |
| `width=n`, `height=n`                                | int ≥ 1          | `frame.video.width`, `frame.video.height`, deferred | the picture size sent; only with `video=true`                       |
| `header=b`                                           | bool             | `true`                                              | `false`: raw PCM input, no WAV header; only without video           |
| `restart_on_crash=b`                                 | bool             | `false`                                             | a new process when the process ends unasked (§3)                    |
| `restart_on_metadata` (anonymous)                    | —                | off                                                 | a new process at each metadata offer (§7)                           |
| `streamable=b`                                       | bool             | `false`                                             | `true` declares the process's output streamable (§2, §5)            |

- `restart_after_delay` is not a parameter: it is refused, naming it.
  Migration: restart on a schedule from the output, with its
  `reopen_when` predicate (a new writer, hence a new process, per reopen),
  or use `restart_on_metadata`.
- The type is `format(audio=pcm(<layout>))`, with `video=<default>` added
  when `video=true`.
- Errors, each at the phase of
  [formats.md §7](formats.md#7-when-a-format-is-validated), with a position
  ([formats.md §11](formats.md#11-errors)):
  - no `process`, or an empty one: "`%external` needs a `process`";
  - `width` or `height` without `video=true`, `header` with `video=true`:
    the parameter has no effect in this configuration, and is refused
    ([formats.md §3](formats.md#3-the-parameter-specification));
  - a value out of range or of the wrong type, naming the parameter.
- **No adaptable property.**
- Helpers ([formats.md §9](formats.md#9-helpers)): extension, MIME, `codecs`
  and `bitrate` are none. An output that needs a content type takes it from
  its own parameter.
- Printed with every option set, `restart_on_metadata` and `streamable`
  only when set.

Migration: `%external(video=false, …)` is accepted.
`%external(width=…, …)` without `video=true`, which failed at start-up,
fails `--check`. `%external(video=true, header=false, …)` is refused: remove
`header`. `%external(restart_after_delay=…, …)` is refused (above).

## 2. Declarations

| Declaration        | Value                                           |
| ------------------ | ----------------------------------------------- |
| streamable         | no; yes with an explicit `streamable=true`      |
| needs seekable     | no                                              |
| can continue       | yes: the process's output is appended unchanged |
| in-band metadata   | yes with `restart_on_metadata`, else no (§7)    |
| segmentable        | no                                              |
| needs no byte sink | no                                              |
| accepts delegated  | no                                              |

- Not streamable by default: the writer cannot name a position where a
  reader starts, nor the header such a reader needs. A shared mount or an
  SRT listener refuses `%external` at evaluation
  ([encoder.md §5.3](encoder.md#53-the-check)); the message names the
  dedicated mode and `streamable=true`. Migration: use a dedicated
  encoder, a native or `%ffmpeg` format, or declare `streamable=true`.
- `streamable=true` is the script's claim that a reader can start at any
  byte boundary of the process's output with no entry header: true of a
  self-synchronising format (MPEG audio, ADTS, MPEG-TS), whose decoders
  find the next frame by its sync word. The writer then marks the access
  points of §5, and the claim is never checked.
- On a continuing sink the writer appends what the process prints; whether
  the concatenation is valid depends on the process's format, which the
  script chose. The output logs at `info` that it appends a process's
  output.
- No capability changes what the writer sends the process: its input is
  always the streaming form of §4, whatever the sink.

## 3. Creation

The process lifecycle, from the writer's creation to its end.

`process_exit_bound` is the process sink's
([output-pipe.md](output-pipe.md#the-process-sink), rule 5): it bounds the
wait for each process this writer starts as it bounds that sink's.

**Start.** Creation, after every check of the configuration:

1. starts the command with the system shell (`/bin/sh -c` on POSIX), the
   output's environment and working directory, with its standard input,
   output and error connected to the writer;
2. queues the process's input header (§4) as its first input;
3. starts the writer's own work (§14).

Creation does not wait for the process. A process that cannot be started for
lack of a resource (no process slot, no descriptor) is an output fault
([encoder.md §8](encoder.md#8-faults)). A process that ends with the shell's
"not found" or "not executable" status (127, 126) before it has printed
anything is a configuration error, a hard error naming the command
([faults.md §2](../../../spec/faults.md#2-classes)).

**Process stream.** Each process the writer starts is one stream of the
writer (§5). A **restart** ends the current process as a stop does (below),
without ending the writer, and starts a new one as above, with its own input
header. Restarts happen:

| Cause                                         | When                                    | Input lost                                                                                     |
| --------------------------------------------- | --------------------------------------- | ---------------------------------------------------------------------------------------------- |
| planned: a metadata offer (§7)                | at the next piece, before it is encoded | none                                                                                           |
| `restart_on_crash`: the process ended unasked | as soon as the writer sees the end      | what the ended process received and did not answer; logged at `important` with the stream time |

- On a planned restart, the new process is started at once and receives
  every piece from the restart on; the old one gets its input closed and
  ends as at a stop. Both MAY run at once. The expected minimal
  implementation runs them one after the other in effect: the new process
  is started at once and its input queued, and its output is held until the
  old one's has ended, as the next item requires.
- Output order: the new process's output is written into the sink only after
  the old one's output has ended (EOF, or the bound below). It is held
  meanwhile.
- After an unasked end, the new process is started no sooner than
  `process_restart_delay` after the previous start. Pieces handed meanwhile
  are queued for it, within `process_input_backlog`.
- A process that ends unasked without having printed anything
  `process_failed_starts` times in a row is an output fault, whatever
  `restart_on_crash` says.

**End of a process.** A process has ended when it has exited and been
reaped. Its output has ended at end of file on its standard output, or
`process_exit_bound` after it exited, whichever comes first. Reason: a child
the process started can keep the output pipe open after the process exits.
End of output, exit and "asked to stop" are three distinct events; the
writer never takes one for another.

**Stop of a process.** The writer closes the process's input, after every
byte queued for it. Closing the input is the end-of-stream signal: the
process is expected to print its tail and exit. If it has not exited
`process_exit_bound` later, it is sent a termination request; if it still has
not exited `process_exit_bound` after that, it is killed. Every byte it
printed before its output ended is kept.

**Unasked end.** A process that exits, or closes its standard input, while
the writer did not ask it to stop:

- every byte it printed is written into the sink, in order, before anything
  else happens;
- with `restart_on_crash`, it is a restart (above);
- otherwise it is an output fault: the call that sees it writes the
  process's remaining output, then reports the fault
  ([encoder.md §8](encoder.md#8-faults)).

A write to a process that closed its input (EPIPE) is how the writer may
first see an unasked end: it never fails the program and never escapes as
its own error.

| Parameter               | Recommended                                                        | Protects                                                                  |
| ----------------------- | ------------------------------------------------------------------ | ------------------------------------------------------------------------- |
| `process_input_backlog` | the output's write timeout, in stream time; 10 s where it has none | Memory, against a process that does not read its input                    |
| `process_restart_delay` | 1 s                                                                | The CPU and the log, against a process that ends at once in a loop        |
| `process_failed_starts` | 3                                                                  | The output's restart policy, against restarting a process that cannot run |

## 4. Input

- Without video: the piece's audio, converted to `samplerate` and to signed
  16-bit PCM, in the WAV streaming form
  ([wav.md §6](wav.md#6-header-forms-and-preambles)) with
  `samplesize=16`: the WAV header first unless `header=false`, then sample
  blocks.
- With video: the AVI streaming form
  ([avi.md §6](avi.md#6-header-forms-and-preambles)) at `width × height`,
  `channels` and `samplerate`, chunks as [avi.md §4–§5](avi.md#4-input).
- Each process receives its own complete input: its header, then the pieces
  from its start. The input never depends on piece boundaries.
- Input is written by the writer's own work (§14), behind a queue bounded by
  `process_input_backlog`. A process that does not read its input past the
  bound is an output fault. No call of the writer waits for the process.

## 5. Bytes and markers

- The bytes are those the process prints on its standard output, unchanged,
  process after process, in order. The writer adds no byte.
- `stream_start(unknown, t)` precedes the first byte of each process's
  output, `t` being the media time of the first piece that process
  received: the writer does not know the header's length
  ([encoder.md §3.4](encoder.md#34-bytes-and-markers-out), item 8).
- Without `streamable=true`, no access point is marked. With it, an
  `access_point(t)` precedes the bytes each call writes, `t` being the
  media time of the first piece the process had received when it printed
  them; the entry header is empty.
- The bytes a call writes are those the process printed since the previous
  call. They are not the encoding of the piece the call was given, and the
  writer makes no claim about which piece they encode.
- Bytes the process prints on its standard error are not part of the
  stream: each line is logged at `debug` under the output's name.

## 6. Header forms and preambles

Nothing: the writer writes no header and no preamble of its own, on any
sink, and rewrites nothing at its end. The process's input always has the
streaming form (§4), since a process's input cannot seek.

## 7. Metadata

- Initial metadata: not used.
- **Metadata never reaches the process.** No value of a metadata is sent to
  it, in its input or otherwise.
- In-band offer: with `restart_on_metadata`, the writer offers `metadata`
  in the sense of [encoder.md §9](encoder.md#9-in-band-metadata), item 1:
  `metadata(m)` records the offer, and the next piece goes to a new
  process, whose output starts a new stream; `m`'s values never reach the
  process. An offer that starts no new stream under encoder.md §9, item 3,
  starts no process. Without `restart_on_metadata`, the writer does not
  offer `metadata`.

## 8. Delay and padding

Nothing: unknown. `delay` and `padding` are none.

## 9. End

`end(clean)` runs as work allowed to block
([encoder.md §3.6](encoder.md#36-draining-and-final-bytes)):

1. drains the rate converter, and queues its samples for the process;
2. stops the current process (§3), and any process still ending from a
   planned restart;
3. writes into the sink every byte each process printed until its output
   ended, in stream order;
4. writes `end`.

It returns within `2 × process_exit_bound` plus the time to write the bytes,
whatever the process does. A process that had already ended contributes the
bytes it printed, and the end goes on.

`end(abandoned)` closes the input, asks every process to terminate, kills
it `process_exit_bound` later if it has not exited, discards its output and
writes nothing.

A writer ended without having been given a piece writes nothing, even if a
process printed something.

## 10. Properties

`bitrate`, `delay` and `padding` are none.

## 11. Live latency

The process's own latency is not counted, as a codec's look-ahead is not
([encoder.md §3.9](encoder.md#39-live-latency-bound)). What the process has
printed is written by the writer's next call. Output of a new process held
during a planned restart is held at most until the old process's output
ends (§3).

## 12. Segmenting face

Nothing. `%external` is not segmentable (§2).

## 13. Delegated destination

Nothing.

## 14. Concurrency

The writer's own work, all of it work allowed to block or fast I/O on
non-blocking descriptors ([concurrency.md §1](../../../spec/concurrency.md#1-work)):

- writing each process's input queue;
- reading each process's standard output into the writer's collected
  output, and its standard error into the log;
- watching each process's exit, and reaping it.

- Writing input and reading output are independent: neither waits for the
  other, and the output is read whatever the state of the input. Reason: a
  process that stops reading its input until its output is read would
  otherwise stop both.
- The collected output, the process states and the restart in progress are
  shared between this work and the writer's calls, under
  [concurrency.md §3](../../../spec/concurrency.md#3-shared-state). The work
  never writes into the sink ([encoder.md §7](encoder.md#7-concurrency),
  item 4).
- Every pipe and every process is released exactly once, a process ending
  during a restart included; no process is left unreaped.
- Nothing is shared with other writers.

## 15. Faults

| Fault                                                                  | Class                                                                         |
| ---------------------------------------------------------------------- | ----------------------------------------------------------------------------- |
| a parameter rejected (§1)                                              | configuration error                                                           |
| the command not found or not executable (status 127 or 126, no output) | configuration error, a hard error (§3)                                        |
| a process cannot be started for lack of a resource                     | output fault                                                                  |
| an unasked end without `restart_on_crash`                              | output fault, after its output is written                                     |
| `process_failed_starts` unasked ends without output in a row           | output fault                                                                  |
| the input backlog past `process_input_backlog`                         | output fault                                                                  |
| an unasked end with `restart_on_crash`                                 | none: a restart, logged at `important`                                        |
| a process exiting non-zero or on a signal                              | logged at `severe` with the command and the status; classed by the rows above |
| a process that does not exit at a stop within its bounds               | not escalated: terminated, then killed, logged at `severe`                    |

## 16. Tests

With process doubles:

- `cat`: the bytes written equal the WAV header and every sample, the tail
  included after a clean stop.
- A process that prints N bytes and exits, no restart: all N bytes reach the
  sink, then the output's restart policy runs once; the clock goes on.
- The same with `restart_on_crash`, repeated at a high rate: no fault
  escapes, EPIPE included; each process's output is a stream start; the
  stream time lost is logged.
- A process that exits at once without output: after
  `process_failed_starts` attempts, one output fault. A missing command: a
  hard error naming it.
- `restart_on_metadata` with a metadata every 0.5 s, through `cat`: every
  input sample appears in exactly one process's output, and the output holds
  one stream start per process, in order. Equal metadata twice: no restart.
- `%external(restart_after_delay=1., process="cat")` fails `--check`,
  naming the parameter.
- `streamable=true` with a process printing ADTS, on a shared harbor mount:
  accepted; a listener starting after 3 s decodes from its first byte.
- A process that ignores its closed input: the stop returns within its
  bound, and the process is gone. A process whose child keeps its output
  open: the stop returns within `process_exit_bound`.
- A process that stops reading its input until its output is read: no
  stall, and the streaming thread's latency stays bounded.
- A process that never reads: one output fault within
  `process_input_backlog`.
- `video=true`: the process receives a valid AVI stream; `%external(width=
320, process="cat")` fails `--check`; `%external` without `process` fails
  `--check` with a position.
- A shared harbor mount given `%external` is refused at evaluation, naming
  `streamable=true` and the dedicated mode.
- `output.file(append=true)` given `%external`: accepted; the process's
  output is appended unchanged.
