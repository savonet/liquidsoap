# `output.external`, the `pipe` operator and the language hooks (as built)

Three things that put encoded bytes into a process or decide which encoders
exist:

- `output.external`: the pipe output of [output-file.md](output-file.md)
  with a process's standard input in place of a file. Only its departures are
  stated here.
- the `pipe` operator: a source that sends its input's audio to a process and
  reads audio back. It does **not** use the encoder seam: it writes its own
  WAV stream.
- the language hooks that evaluate `%name(...)` and answer `%ifencoder`.

It relies on: [output-file.md](output-file.md) (pipe outputs: encoder slot,
reopen triggers, error-retry, call sequence), the encoded base
([output-encoded.md](output-encoded.md)), the seam
([encoder.md](encoder.md)) and the WAV header writer shared with `%wav`
([wav.md §10](wav.md#10-bytes-produced)).

## Part 1. `output.external`

### 1. Formats accepted and how the factory is obtained

Every format value, including `%ffmpeg`: there is no ffmpeg variant here, so an
`%ffmpeg` format is used as written. Its instance returns bytes only when its
format has no URL destination; the stream-face rules of the FFmpeg encoder
apply ([encoder.md](../../optionals/ffmpeg/spec/encoder.md)). The factory is
looked up at each instance creation, as for the channel variant of
`output.file` ([output-file.md §1](output-file.md#1-formats-accepted-and-how-the-factory-is-obtained)).

Parameters: those of `output.file` minus `append`, `perm`, `dir_perm`,
`on_close`; plus `self_sync` (bool, default `false`). The second unlabelled
argument is the command (string getter).

`self_sync`: `true` makes the output a sync source of its own; `false` makes
it **not** a sync source and, unlike `output.file`, does not report its
source's sync either.

The stdlib (`extra/telnet.liq`) replaces `output.external` by a wrapper that,
once woken, registers a telnet command `<id>.reopen` calling `reopen()`.

### 2. Instance lifecycle

As [output-file.md §2](output-file.md#2-instance-lifecycle), with "file"
read as "process": one process and one instance per open. Every reopen and
every error retry starts a new process, and the new instance writes a new
stream from its first byte (checked: `reopen_on_metadata` always true over two
tracks gave three process runs, the first fed nothing, and two complete WAV
streams in the sink).

### 3. Call sequence

As [output-file.md §3](output-file.md#3-call-sequence).

### 4. The bytes

- Open: the command getter is called, interpolated as filenames are (§5.1 of
  output-file.md: every `$(key)` is empty) without the `/` substitution, and
  run with `/bin/sh -c`. The output holds a buffered channel to the process's
  standard input. Its standard output and error are liquidsoap's.
- Writes and `flush`: as the channel variant of `output.file` (64 KiB channel
  buffer; flush after each string when `flush` is true). The process sees
  the bytes in bursts of the channel buffer unless `flush` is set. Writes
  block when the pipe is full: a slow process slows the clock.
- Close: close the channel (flushing it), then **wait for the process to
  exit**, on the clock's thread. A system error during that close is
  swallowed, including the broken pipe of the final flush; the exit status
  is ignored. There is no `on_close`.
- No seek, no rewrite: the process receives exactly the concatenation of the
  instance's `encode` outputs then its `stop` output.

### 5. Reopen, reconnect, append, file naming

As output-file.md §5.2–§5.3. There is no append and no naming. The command
getter is asked at every open.

### 6. Failures

- A process that exits while the output writes: the next write that reaches
  the pipe fails with `EPIPE`, raised as a runtime error of kind `system`,
  message "Broken pipe" (the process is not killed by `SIGPIPE`; checked with
  `head -c 100`: the error surfaced about 1 s into a 44.1 kHz mono stream,
  once the channel and pipe buffers filled). It then follows the error-retry
  path of [output-file.md §6](output-file.md#6-failures).
- A command that cannot be started fails at open the same way.
- The output's id defaults to `output.file`, not `output.external`: logs,
  the clock's name and the telnet namespace say `output.file` unless the
  script sets `id` (checked, finding).

### 7. Threads and locks

As output-file.md §7. The wait for the process at close runs on the clock's
thread.

### 8. Tests

None targets `output.external` (`tests/regression/external-encoder.liq` tests
the `%external` encoder, a different thing). A harness needs a process double
that records its standard input, one that exits early (broken pipe), and one
that never reads (blocking).

## Part 2. The `pipe` operator

### 1. Formats accepted and how the factory is obtained

Nothing: no format, no factory, no instance. The operator writes WAV itself
with the same header writer as `%wav`.

Parameters: `process` (string, required), `replay_delay` (float?, `null`),
`data_length` (int?, `null`), `buffer` (float, 1.), `max` (float, 10.),
`restart` (bool, true), `restart_on_error` (bool, true), the source (audio
only).

### 2. Lifecycle

When the operator wakes up, it starts the process (threaded process handler).
At sleep it stops it. The process is restarted after it exits, as decided by
§5.

### 3. Call sequence

Per process run:

1. on start: write the WAV header (§4);
2. whenever the process can take input and nothing is queued: pull one frame
   from the input source, convert its audio to bytes, queue it as chunks cut
   at metadata (§4);
3. write the head chunk, at most `Utils.buflen` (1024 bytes, overridable by the
   `LIQ_BUFLEN` environment variable) per write;
4. when a chunk ending at a metadata or a track mark is fully written:
   - `replay_delay = null`: remember it and **stop the process** (close its
     input). The metadata or track mark is added to the output when the
     process has exited, then the process is restarted;
   - `replay_delay = d`: queue it to be replayed `d` seconds of output audio
     later, and go on.
5. on the process's output: read a WAV header first (checks the channel
   count, reads sample size and rate), then raw samples, converted, resampled
   to the internal rate and buffered.

### 4. The bytes

What the process receives on its standard input, per run:

- a 44-byte WAV header: `RIFF`, RIFF length, `WAVE`, `fmt `, 16, format 1,
  channels = the operator's channel count, sample rate, byte rate, block
  align, bits = 16, `data`, data length;
  - lengths: `data_length = null` (the default, and any negative value): the
    "unknown" pair `0xEFFFFFFF` / `0xEFFFFFDB`; else `data_length + 36` and
    `data_length`;
  - sample rate: the operator's current input-rate field. It starts at the
    internal audio rate and is **overwritten by the rate the process
    announces** in its own output header; the next run's header carries that
    rate while the samples are still at the internal rate (finding);
- then interleaved signed 16-bit little-endian samples of each pulled frame.

Chunking of one frame of `n` samples and `c` channels (`2·n·c` bytes): one
chunk per metadata, ending at `2·c·p` bytes for a metadata at position `p`,
then a last chunk to the end. The position `p` is in main ticks and used as a
sample count (equal only when the main rate equals the audio rate). Track
marks: only the first is considered; a metadata chunk ending at it is a
"break and metadata" only when its byte offset equals the track mark's
main-tick position, which holds only at position 0 (finding); otherwise the
track mark is attached to the frame's last chunk.

Checked (8 kHz mono, 3 s sine with a metadata at 0 and a track end, process
`tee`): the process input holds three headers. The first run receives only the
header (the chunk before the metadata at 0 is empty, and its completion stops
the process), the second the 3 s of samples, the third only a header.

### 5. Restart

When a process run ends, by exit status:

| Run ended with                | Restart?              | Then |
| ----------------------------- | --------------------- | ---- |
| exit 0, or stopped by us      | yes                   |      |
| non-zero status, exception    | if `restart_on_error` |      |
| (any) and the operator sleeps | no                    |      |

When restarting, the pending stop reason is applied to the output buffer:
metadata, track mark, or both; with no pending reason, the process is
restarted only if `restart` is true.

### 6. Failures

A process output with another channel count fails with "Invalid channels from
pipe process!". Process errors go to the restart table above. The process's
standard error is logged at debug level.

### 7. Threads and locks

The process handler runs the input, output and error callbacks on its own
threads; a mutex protects the replay list, the stop reason and the format
fields read from the process header. The input source is pulled from the
process handler's input callback.

### 8. Tests

None.

## Part 3. Language hooks

The core installs two hooks the language calls:

- **Evaluating `%name(params)`**: find the format builder registered for
  `name` and build the format value; then look its factory up. An unknown name
  or a format no plug-in answers fails with "unsupported format" at the
  encoder's position. So a script that evaluates a format the binary cannot
  encode fails at evaluation, before any output exists.
- **`%ifencoder %name` / `%ifnencoder %name`**: build `%name` with **no
  parameters**, then look its factory up. Any exception, including a format
  whose defaults are invalid, reads as "absent". An unknown name takes the
  else branch for both forms.

The seam owns the registry these consult
([encoder.md §2](encoder.md#2-registry-and-factories), §9).
