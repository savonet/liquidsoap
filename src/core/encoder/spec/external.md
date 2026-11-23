# `%external` encoder (as built)

What the external-process encoder does today: it spawns a shell command,
writes raw PCM (with a WAV header) or raw AVI to its stdin, collects
whatever the process writes to stdout on a scheduler thread, and hands
those bytes back on the next `encode`. The shared seam is
[encoder.md](encoder.md). The WAV and AVI bytes it writes to the process
are owned by [wav.md](wav.md) and [avi.md](avi.md). The process runner it
uses (spawn, pipes, control channel, restarts) is described here only as far
as this encoder depends on it.

## 1. Format and options

Syntax: `%external(...)`. Parameters, last one wins:

| Parameter                                            | Value          | Default                               | Effect                                 |
| ---------------------------------------------------- | -------------- | ------------------------------------- | -------------------------------------- |
| `process="cmd"` or anonymous string                  | string         | — (required)                          | shell command line                     |
| `channels=n`, `mono`, `stereo`, `mono=b`, `stereo=b` | static         | 2 (hard-coded, issue #933)            | channels sent                          |
| `samplerate=n`                                       | int            | `frame.audio.samplerate`, read lazily | rate sent                              |
| `video=true`                                         | literal `true` | off                                   | send AVI with video                    |
| `width=n`, `height=n`                                | int            | `frame.video.width/height`            | video size; **also turns video on**    |
| `header=b`                                           | bool           | true                                  | WAV header to the process (audio only) |
| `restart_on_crash=b`                                 | bool           | false                                 | respawn when the process ends          |
| `restart_on_metadata` (anonymous)                    | —              |                                       | respawn on each `encode_metadata`      |
| `restart_after_delay=n`                              | int            |                                       | parsed and printed, **no effect**      |

`restart_on_metadata` and `restart_after_delay` share one slot: the last
given wins.

Errors:

- no process: the exception `No_process` escapes as "Error -1: Exception
  raised: External_encoder_format.No_process" (no position, no kind);
- `video=false` (or any non-`true` value): "Invalid value false for value
  mode. Only `true` or `false is allowed." at type computation, kind
`encoder`; `video=false` is therefore unusable;
- unknown parameters: "unknown parameter name … / Unknown encoder
  parameter: …", kind `encoder`.

Type: computed from the script's literal parameters: audio PCM with
_channels_ channels, plus a default `video` track iff `video=true` is
literally present. `width=`/`height=` without `video=true` give a
script-level audio-only type but a value with video: the script passes
`--check` and fails at start-up with a type error at an unknown position
("a source without a `video` track is used where a source with one is
expected").

Printed as `%external(channels=2,samplerate=44100,video=false,header=true,restart_on_crash=false,,process=cat)`
(the restart slot prints empty when unset, leaving `,,`).
Extension: none (raises "not found"); MIME `application/octet-stream`; no
bitrate; Icecast: no content type (the output must be given `format=`),
info reports samplerate and channels.

## 2. Factory and instance creation

The factory takes the output's name (used as the logger name, e.g.
`output.file`) and ignores the initial metadata. Creating an instance:

1. computes the **input header** sent to each process: an AVI header
   ([avi.md §6](avi.md#6-header)) with the format's size, channels and rate
   if video is on (regardless of `header`); else a WAV header
   ([wav.md §6](wav.md#6-header), 16 bits, placeholder sizes) if
   `header=true`; else nothing;
2. **spawns the process** at once (`/bin/sh -c cmd` with the current
   environment; stdin, stdout and stderr are pipes), logs "Starting
   process" at `important`, writes the input header to its stdin on the
   caller's thread (blocking until fully written), then registers the
   process task with the scheduler. On a respawn the header is written by
   the scheduler thread that respawns;
3. creates a rate converter (audio-only path) and an empty output buffer.

A spawn failure raises out of the factory.

## 3. `encode`

1. Converts the frame: with video, exactly as `%avi` encodes a frame
   ([avi.md §3](avi.md#3-encode): `00db` chunk then `01wb` chunk; the AVI
   header is not repeated here); otherwise resampled, interleaved
   S16LE PCM of the frame's audio.
2. Under the encoder's internal lock, writes all those bytes to the current
   process's stdin, blocking until written.
   - No process (it ended and was not restarted, or it ended and its
     respawn has not run yet), or its stdin already closed for a restart:
     the runner raises "finished". If `restart_on_crash` is set or a
     metadata restart is still flagged, the frame's bytes are **dropped
     silently**; otherwise "finished" escapes `encode`. The metadata flag
     is cleared when the old process's end is handled, before the respawn
     runs, so a frame that falls in that window escapes with "finished"
     even under `restart_on_metadata`.
   - A write error (e.g. EPIPE because the process exited and has not yet
     been reaped) escapes `encode`, restart options or not.
3. Returns **everything collected from stdout since the previous return**,
   and empties the buffer. These bytes are not the encoding of this frame:
   they are whatever the process has emitted so far, possibly nothing.

On an escape from `encode`, the collected stdout bytes stay in the buffer:
they are returned by a later `encode`/`stop` if any, else lost.

## 4. `encode_into`

Nothing.

## 5. `encode_metadata`

The metadata content is ignored; it never reaches the process. With
`restart_on_metadata`, the call marks "metadata restart" and asks the
runner to stop the process (§9 describes the stop sequence); the respawn is
§11. Without it, nothing.

## 6. `header`

Always empty. The encoder does not know the process's output format, so a
late joiner gets no header.

## 7. `restart_offset`

Always "none", including after a process restart, although the bytes of a
new process start a new stream with its own header.

## 8. `bitrate`

"none".

## 9. `stop`

Under the internal lock: mark "stopping", ask the runner to stop the
process, then wait on a condition (single wait, no predicate loop) until
either the stdout reader reads end-of-file while stopping, or the process
end is reported. Then return and empty the output buffer.

The runner's stop sequence: a "stop" byte on its control pipe; the
scheduler task reading it closes the process's stdin ("Closing process's
stdin", `important`); the process is expected to flush and exit; a waiter
thread reaps it and signals "done"; the scheduler drains stdout and stderr
to end-of-file through the normal callbacks, closes the pipes, logs "Process
exited with code N" (`important`) and calls the restart decision, which
answers "no restart" because "stopping" is set ("Cleaning up process").

If the process ignores the closed stdin and never exits or closes stdout,
`stop` never returns. If there is no process any more (ended without
restart), the runner raises "finished" out of `stop`. A second `stop` does
the same.

## 10. Bytes produced

The concatenation of the stdout bytes of every process spawned by the
instance, in order, as collected. Nothing is added by the encoder.

Observed (real binary):

- `process="cat"`: the output is the input: a 44-byte WAV header with
  placeholder sizes, then PCM. 2.02 s of stereo 44.1 kHz gave 356 372 bytes
  = 44 + 101 frames × 3528: no byte lost at stop.
- `restart_on_metadata`, metadata every 0.5 s, `sh -c 'printf START; exec
cat'`: four `START` + WAV header + PCM segments in one file (one per
  process). Each restart loses about 80 ms of audio (frames encoded while
  the old stdin is closed and before the new process exists are dropped):
  1.8 s of PCM for 2.06 s of input.
- `restart_on_crash=true`, `head -c 50000`: one 50 005-byte segment per
  process, frames in between dropped; after a few restarts a write hit
  EPIPE before the exit was seen, the error escaped `encode`, the clock
  failed and the script exited with status 1.
- `head -c 10000`, no restart: the process exits; the next `encode` raises
  "finished", the clock fails (script exit 1), and only 2048 of the 10 000
  bytes reach the file: the rest stayed in the buffer.

## 11. Concurrency and failures

Threads:

- **Caller's thread** (the clock): `encode`, `encode_metadata`, `stop`;
  `encode` blocks on stdin writes; `stop` blocks until the process ends.
- **Scheduler threads**: the process task: writes the input header on
  (re)start, reads stdout (up to 1024 bytes per wake-up, `LIQ_BUFLEN`),
  reads stderr (each read of up to 1024 bytes logged as is at `debug`,
  "stderr: …"), handles control
  bytes, drains pipes at exit, decides restarts.
- **One waiter thread per process**, reaping it.

Locks: the seam's per-instance lock around every call; the encoder's
internal lock around the stdin write in `encode`, around each stdout read
and buffer append, around `encode_metadata`, and around `stop` (released
while waiting). The stdin write and the stdout reader therefore exclude each
other: while `encode` is blocked writing, no stdout is collected. A process
that stops reading stdin until its stdout is drained would deadlock both;
not observed with `cat` or a 20× expanding process at real-time rate.

Restart decision when a process ends (exit, signal, or an exception in the
runner): stopping → none; metadata restart pending → respawn after 0 s and
clear the flag; else `restart_on_crash` → respawn after 0 s; else none.
Exit status ≠ 0 and signals are logged at `important` (twice: once by the
encoder, once by the runner). A respawn writes the input header again, so
each process gets a full header.

An exception raised by one of the encoder's callbacks inside the runner is
logged ("Error while running process") and kills the process without
restart.

## 12. Segmenting (HLS) face

Nothing (see [wav.md §12](wav.md#12-segmenting-hls-face)).

## 13. Where the bytes go

- **Two destinations.** The encoder writes I/O itself: to the process's
  stdin, blocking the caller's thread. Its output bytes come from another
  thread and are returned with a lag of one call: the bytes a caller gets
  from `encode` are not the encoding of the frame it gave.
- **The process input is unseekable**: placeholder sizes in the WAV/AVI
  header it sends ([wav.md §13](wav.md#13-where-the-bytes-go)).
- **One output stream per process**, concatenated. Restarts produce
  chained streams (each with its own header) inside one byte stream, with
  no `restart_offset` to tell the caller where; the caller cannot start a
  new file there, and a late joiner gets no header.
- **The tail exists only at `stop`.** Whatever the process flushes after
  its stdin is closed comes back from `stop`, which blocks for it.
- The encoder knows nothing about the output format, so MIME and
  extension are unknown and the caller must be told.

## 14. Tests

- **An external encoder can be created and its output starts.**
  (`tests/regression/external-encoder.liq`, `process="cat"`.) Binding: the
  output reaches "started" with an external process. Incidental: `cat`,
  the file name. It does not look at bytes, stop, or restarts.
- **Untested:** bytes round-trip through the process; the tail returned by
  `stop`; restart on metadata (and the frames it drops); restart on crash;
  process exiting early; `video=true`; stderr logging. A harness needs a
  scriptable process double (echo, expand, exit after N bytes, ignore EOF,
  exit non-zero) and a way to observe blocking of `encode`/`stop`.
