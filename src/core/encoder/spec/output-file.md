# `output.file`

Part A. How `output.file` drives a container writer: the file sink it
provides, how a file is named and opened, when a file ends and the next
begins, what a closed file holds, and the one restart policy every failure
reaches. It answers the caller numbering of
[encoder.md §11](encoder.md#11-caller-files). `output.external`
([output-pipe.md](output-pipe.md)) shares the reopen triggers (§7) and the
restart policy (§8), and states only its departures.

It relies on: the output base ([output.md](../../outputs/spec/output.md));
the encoded-output base: pieces, current metadata, exported form, order
([output-encoded.md](output-encoded.md)); the writer
([encoder.md](encoder.md)); sinks ([sink.md](sink.md)); faults
([faults.md](../../../spec/faults.md)); concurrency
([concurrency.md](../../../spec/concurrency.md)). `output.file.hls` is a
different operator ([HLS tree](../../outputs/hls/spec/)).

## 1. Formats and declarations

### 1.1 Parameters

Beyond those of [output.md §1](../../outputs/spec/output.md#1-common-parameters-methods-and-callbacks):

| Parameter               | Type                             | Default                     | Use                                                                                                      |
| ----------------------- | -------------------------------- | --------------------------- | -------------------------------------------------------------------------------------------------------- |
| `append`                | bool                             | `false`                     | extend an existing file instead of replacing it (§2.3, §7.4)                                             |
| `perm`                  | int                              | `0o666`                     | mode of a created file, under the process's umask                                                        |
| `dir_perm`              | int                              | `0o777`                     | mode of each created directory, under the umask                                                          |
| `flush`                 | bool                             | `false`                     | hand every write to the system at once (§2.4)                                                            |
| `on_close`              | `(string.{clean: bool}) -> unit` | does nothing                | called with the path after each file is closed; its `clean` method says whether its end was clean (§6.3) |
| `reopen_on_error`       | `(error?) -> float?`             | returns `fault_retry_delay` | the restart policy (§8)                                                                                  |
| `reopen_on_metadata`    | `(metadata) -> bool`             | returns `false`             | reopen trigger (§7.1)                                                                                    |
| `reopen_when`           | `() -> bool`                     | returns `false`             | reopen trigger (§7.1)                                                                                    |
| `reopen_delay`          | float getter                     | `120.`                      | minimum age of a file, in stream time, before `reopen_when` is asked (§7.1)                              |
| `export_cover_metadata` | bool                             | `true`                      | [output-encoded.md §4.2](output-encoded.md#42-the-exported-form)                                         |
| `in_band_metadata`      | bool                             | `true`                      | offer metadata to a writer that carries it in band (§4)                                                  |
| (1st unlabelled)        | format                           |                             |                                                                                                          |
| (2nd unlabelled)        | string getter                    |                             | the path (§2.2)                                                                                          |
| (3rd unlabelled)        | source                           |                             |                                                                                                          |

Method `reopen()`: requests a reopen (§7.1). Callback `on_reopen`
([source-contract.md §10](../../source/spec/source-contract.md#10-callbacks)).

### 1.2 Lookup

The format value was looked up when it was evaluated
([formats.md §5](formats.md#5-building)): an output never meets a format no
writer answers. `%ndi` is refused at construction
([formats.md §2](formats.md#2-names-and-kinds)).

### 1.3 Declarations

From its parameters alone ([encoder.md §5.1](encoder.md#51-what-an-output-declares)):

| Configuration            | `append` | Capabilities             |
| ------------------------ | -------- | ------------------------ |
| writes bytes into a sink | `false`  | `seekable`, `fresh`      |
| writes bytes into a sink | `true`   | `seekable`, `continuing` |
| needs no byte sink       | `false`  | `delegated(path)`        |
| needs no byte sink       | `true`   | refused (below)          |

Readers mid-stream: none. A configuration that needs no byte sink (an FFmpeg
muxer that writes its own files, `format="dash"`) takes the path as its
delegated destination ([encoder.md §6](encoder.md#6-the-delegated-destination));
the output keeps the naming, the directories, the reopen triggers, `on_close`
and the restart policy.

### 1.4 Checks at evaluation

Beyond the rows of [encoder.md §5.3](encoder.md#53-the-check), each an
invalid-value error on the argument named:

| Case                                                                                                    | Argument       | Message names                                                                  |
| ------------------------------------------------------------------------------------------------------- | -------------- | ------------------------------------------------------------------------------ |
| `append=true` with a configuration that cannot continue, or that needs no byte sink                     | format         | the format, and that it cannot be appended to                                  |
| `append=true` with a preamble set explicitly                                                            | format         | the parameter, and that a preamble cannot be appended mid-file                 |
| an `%ffmpeg` `truncate` option, or an option of FFmpeg's file protocol, set explicitly                  | format         | the option, and that `output.file` owns the file (use `append`)                |
| `perm` other than its default, or `flush=true`, with a configuration that needs no byte sink            | that parameter | that the muxer creates its own files                                           |
| an `%ffmpeg` format without `format=`, with a path that is not a constant string                        | format         | that the muxer cannot be guessed from a name known only at open; set `format=` |
| an `%ffmpeg` format without `format=`, with a constant path whose extension names no muxer of the build | format         | the extension, and that no muxer is known for it                               |

An `%ffmpeg` format without `format=` given a constant path takes the muxer
FFmpeg guesses from that path's extension, at construction
([ffmpeg.md §1](ffmpeg.md#1-format-and-options), item 5). Every check of
this file and of [encoder.md §5.3](encoder.md#53-the-check) then runs with
that muxer.

Reason for the last row: the muxer opens its files itself, so these options
would parse and do nothing
([formats.md §3](formats.md#3-the-parameter-specification)).

## 2. The sink

### 2.1 What it is

A file sink ([sink.md](sink.md)) serves one file, from the open to the close,
to one reader from byte 0. It owns the name, the directories, the
permissions, truncation and append, the descriptor and its retries
([sink.md §6](sink.md#6-what-a-sink-owns)). Opening, writing, seeking,
reading back and closing run as work allowed to block; the streaming thread
never waits for any of them
([sink.md §3](sink.md#3-never-blocking-the-streaming-thread)).

### 2.2 Naming

1. A file is opened when the output has its first piece for it (§3). The
   path getter is called then, exactly once per open attempt, by the work
   that opens. It is never called at construction, and a retry after a
   failed open calls it again. Reason: a getter is a function of time; a
   second call near a boundary answers another name, and a name kept from a
   failed attempt retries an unusable path for ever.
2. A leading `~` or `~/` is expanded with the user's home directory.
3. `$(key)` and `$(if $(key),"then","else")` are interpolated against the
   full current metadata at the file's first piece
   ([output-encoded.md §3.1](output-encoded.md#31-current-metadata)): a
   reopen on a metadata names the new file after that metadata. In an
   interpolated value, `/` and the platform's path separator become `-`.
   `\$(` is kept literally.
4. Each missing directory of the path is created, top down, with `dir_perm`.

### 2.3 Confirmation and opening

Before the writer is built, the sink reads the kind and size of the path,
without opening it, and confirms its capabilities
([sink.md §1](sink.md#1-capabilities)):

| Declared                 | Path                                  | Confirmed                | Opened as                                   |
| ------------------------ | ------------------------------------- | ------------------------ | ------------------------------------------- |
| `seekable`, `fresh`      | absent, or a regular file             | `seekable`, `fresh`      | created with `perm`, or truncated           |
| `seekable`, `continuing` | a regular file of size > 0            | `seekable`, `continuing` | for writing at its end                      |
| `seekable`, `continuing` | absent, or a regular file of size 0   | `seekable`, `fresh`      | created with `perm`                         |
| either                   | a FIFO, a character device            | `fresh`, not seekable    | for writing, neither truncated nor appended |
| either                   | anything else (a directory, a socket) | —                        | the open fails (§8)                         |

- A FIFO or a device takes the streaming form, logged once at `info` naming
  the path ([encoder.md §5.4](encoder.md#54-at-open)). A configuration that
  needs a seekable destination there is a hard configuration error naming
  the output, the path and the need.
- The open itself can wait (a FIFO waits for its reader, a slow mount): it
  runs on the opening work. The writer is built on confirmation and writes
  from its first piece; the sink holds those bytes and delivers them, from
  the first, once open ([sink.md §3](sink.md#3-never-blocking-the-streaming-thread)).
- A file whose name equals that of a file this output closed in the same run
  is replaced when `append` is false. The output logs that at `important`,
  once per open. Reason: an unchanged name with a reopen trigger silently
  keeps only the last file.

### 2.4 Bounds and writes

The sink's bounds ([sink.md §3](sink.md#3-never-blocking-the-streaming-thread)
owns `open_hold` and `sink_backlog`):

| Parameter      | Value for this output | Protects                                                     |
| -------------- | --------------------- | ------------------------------------------------------------ |
| `open_hold`    | 10 s of stream time   | Memory, while a file is being opened (a FIFO without reader) |
| `sink_backlog` | 10 s of stream time   | Memory, and the age of what a slow disk or reader receives   |
| `write_buffer` | 64 KiB (recommended)  | System calls; the delay a reader of the growing file sees    |

- Past `open_hold` the open has failed; past `sink_backlog` the destination
  cannot keep up. Each is a sink fault (§8).
- With `flush` true, the sink's work hands every byte to the system as
  soon as it runs, without holding up to `write_buffer`. Reason: handing
  them before the output's next cycle would make the streaming thread wait
  for the disk, which §2.1 forbids. With `flush` false, the sink MAY hold up to
  `write_buffer` bytes before handing them on. Both apply to every
  configuration that writes into a sink, `%ffmpeg` included.
- A FIFO or a device never paces the stream: its writes do not block the
  clock, and a reader slower than the stream reaches `sink_backlog`.

### 2.5 Seeking

On a sink confirmed `seekable`, the writer seeks, asks the size and reads
back as [sink.md §2](sink.md#2-operations) allows: a clean end finalises
the file form or updates the existing header of a continued file
([encoder.md §3.5](encoder.md#35-header-forms-and-preambles)).

## 3. Writer lifetime

- One writer per file ([encoder.md §2.1](encoder.md#21-one-writer-per-destination-stream)).
  It is built when the file's first piece is ready and the sink has
  confirmed. Pieces gathered before confirmation are held by the output,
  within `open_hold`, and handed to the writer in order.
- Initial metadata: [output-encoded.md §3.2](output-encoded.md#32-initial-metadata).
- **No piece, no file.** A file is opened only for a piece. A started output
  whose source produces nothing, or whose reopen trigger fires before any
  piece of the current file, leaves no file and calls no `on_close` for it.
  Reason: a file opened at start and closed empty at the first metadata is a
  0-byte file nobody asked for.

## 4. Pieces and metadata

- Pieces and their order: [output-encoded.md §4](output-encoded.md#4-pieces-and-metadata).
- **In band.** When the writer offers in-band metadata (native `%ogg`: a new
  link) and `in_band_metadata` is true, each metadata is offered. With
  `in_band_metadata` false, a file carries only its initial metadata.
  Reason: a chained file is valid but poorly read by players; a script that
  splits per track gets its titles through `reopen_on_metadata` instead.
- `reopen_on_metadata` runs at step 1 of
  [output-encoded.md §4.3](output-encoded.md#43-order) with the piece's full
  metadata, every key included. When it reopens, that metadata becomes the
  new file's initial metadata, and is not offered in band.
- Out of band: none.

## 5. Markers and readers

The sink serves one reader from byte 0: it ignores stream starts and access
points, and acts on `end` (§6).

## 6. End

### 6.1 Clean and abandoned ends

| Cause                                                        | End         |
| ------------------------------------------------------------ | ----------- |
| the output stops (on request, its source ends, a final stop) | `clean`     |
| a reopen trigger (§7.1)                                      | `clean`     |
| a fault of the writer or of the sink (§8)                    | `abandoned` |

### 6.2 Final bytes

- A clean end drains the writer, writes its final bytes, finalises the
  header on a seekable sink, then closes the descriptor
  ([encoder.md §3.6](encoder.md#36-draining-and-final-bytes),
  [sink.md §4](sink.md#4-markers-and-the-end)). Every final byte reaches the
  file. This runs as work allowed to block; the streaming thread hands it
  over and goes on.
- At a final stop the process waits for every pending close of the output,
  up to `final_delivery_bound`, before it exits; one still pending then is
  logged at `severe` naming the path. For a file sink that bound is
  `sink_backlog`, for a FIFO or a device 2 s
  ([sink.md §4](sink.md#4-markers-and-the-end)).
- An abandoned end writes no final byte. The bytes the writer wrote before
  the fault are delivered where the descriptor still takes them
  ([sink.md §4](sink.md#4-markers-and-the-end)), then the descriptor is
  closed. The file keeps what was delivered; a writer that wrote its streaming form first
  leaves a valid header ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles)).
- A failure while finalising or closing is logged at `severe` with the path
  and the cause; it is not escalated, and `on_close` still runs
  ([sink.md §7](sink.md#7-faults)).

### 6.3 `on_close`

- Called with the path once for every file that was opened, after its close
  completed: every byte delivered, the header finalised, the descriptor
  closed. From then on the output never touches that file: the callback MAY
  move, rename, delete or upload it.
- The path carries a `clean` method, `true` after a clean end (§6.1) and
  `false` after an abandoned one, whose file holds what was delivered
  before the fault and its streaming-form header. A callback that uses the
  path alone, as a string, is accepted. Reason: a callback taking only the
  path does not unify with a type that has an optional argument; a string
  with a method keeps such a callback valid.
- It runs as work allowed to block. Calls of one output run one at a time,
  in the order the files were closed. The next file's open does not wait for
  them, except when it names the same path, which it opens only after that
  file's `on_close` returned (its bytes held within `open_hold`). Reason: a
  callback that moves the file must not move its successor.
- A callback that raises is an output fault of the output
  ([faults.md §2](../../../spec/faults.md#2-classes)): logged at `severe`
  with its backtrace, it reaches the restart policy (§8.2) once, as any
  output fault does, the file it was called for staying as the callback
  left it. Reason: `on_close` is where the script takes the file over (a
  move, an upload); a failure there is a file the script did not handle,
  which the script's policy decides about, not a hook that does nothing
  ([faults.md §0](../../../spec/faults.md#0-the-rule) states the general
  case). It still runs on work allowed to block, never on the streaming
  thread (§9).

## 7. Reopen, reconnect, append

### 7.1 Reopen triggers

| Trigger                      | Asked                                                                                               | Takes effect                       | `on_reopen` |
| ---------------------------- | --------------------------------------------------------------------------------------------------- | ---------------------------------- | ----------- |
| `reopen_on_metadata(m)` true | for every piece metadata (§4)                                                                       | at that metadata's position        | yes         |
| `reopen_when()` true         | once per streaming cycle, before its first piece, while the file's age is at least `reopen_delay()` | at the cycle's first piece         | yes         |
| `reopen()`                   | a request, from any thread                                                                          | before the next piece              | yes         |
| a retry after a fault (§8)   | —                                                                                                   | at the first piece after the delay | no          |

- A reopen ends the current file clean (§6) and the next piece opens a new
  one (§2). When no file is open, a trigger closes nothing, and `on_reopen`
  does not run.
- A file's age is the stream time of the pieces written to it. `reopen_delay`
  is read when `reopen_when` would be asked. Reason: a polled condition such
  as "minute 0" stays true for a whole minute; the minimum age makes it one
  split, and stream time keeps the split where the stream is, whatever the
  clock's lateness.
- A predicate that raises is logged at `severe` and counts as false.
- `on_reopen` runs after the new file is requested, for the three script
  triggers only: a retry is not a reopen the script asked for.

### 7.2 Where a split lands

A split by metadata lands at the metadata's position; by `reopen_when` or
`reopen()`, at a frame boundary, within one frame of the stream time it was
asked at. Each file is a complete stream that decodes alone and signals its
own delay and padding
([encoder.md §2.1](encoder.md#21-one-writer-per-destination-stream),
[§3.7](encoder.md#37-delay-and-padding)): the files of a split, decoded with
their signalled values honoured, together hold the samples written, sample
count exact. Gapless splits are the writer's option.

### 7.3 Stop, start and reset

- A stop ends the file clean; the next start opens a new one at its first
  piece, by the same naming.
- A reset ([output.md §4](../../outputs/spec/output.md#4-reset)) does
  nothing here: the file and its writer go on. Reason: a file has no
  real-time reader, and closing it would split a recording at every
  lateness recovery of the clock.

### 7.4 Append

With `append` true, every file confirmed `continuing` is extended by its new
writer as one stream ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles)),
across reopens and across runs. A file absent or empty gets a complete
stream with its header.

## 8. Faults and restart policy

### 8.1 Faults

| Fault                                                               | Class               | Outcome                                                   |
| ------------------------------------------------------------------- | ------------------- | --------------------------------------------------------- |
| a configuration refused at evaluation (§1.4)                        | configuration error | the script does not start                                 |
| a file that needs seeking found to be a FIFO or a device (§2.3)     | configuration error | hard error when it shows                                  |
| the path getter raises                                              | output fault        | the restart policy                                        |
| a directory cannot be created; the open fails; `open_hold` exceeded | output fault        | the writer, if built, ended abandoned; the restart policy |
| a write or a seek fails; `sink_backlog` exceeded                    | output fault        | the same                                                  |
| a writer fault ([encoder.md §8](encoder.md#8-faults))               | its class           | an output fault reaches the restart policy                |
| finalising or closing fails                                         | not escalated       | §6.2                                                      |
| `on_close` raises                                                   | output fault        | the restart policy (§6.3)                                 |
| a reopen predicate raises                                           | not escalated       | §7.1                                                      |

The error a fault hands the policy carries a kind: `system` for a system
error, its message naming the path; `encoder` for a writer fault; `output`
otherwise.

### 8.2 The restart policy

`reopen_on_error(error)` is the output's one restart policy. Every output
fault reaches it exactly once, whatever failed: the first open after a
start, an open after a reopen or a retry, a write, the writer.

- By default it returns `fault_retry_delay`
  ([faults.md §3](../../../spec/faults.md#3-source-faults-reach-the-script)):
  the output retries after that delay. Reason: a full disk or an
  unmounted directory commonly clears, and an output that stops for good
  on the first failure loses the recording.
- It returns `d ≥ 0`: the current file is ended abandoned and closed;
  `on_close` runs for it if it was opened. For `d` seconds the output reads
  nothing from its source, so the source's content waits for it where the
  source can hold it. Then the next piece opens a new file, the getter asked
  again. The output stays `started`: no `on_stop`, no `on_start`.
- It returns `null` or `d < 0`: the same end, then the output stops as by
  `stop()`, logged at `severe` with the error. The clock and every other
  output go on ([faults.md §2](../../../spec/faults.md#2-classes)).
- It raises: logged at `severe`, and taken as `fault_retry_delay`
  ([faults.md §3](../../../spec/faults.md#3-source-faults-reach-the-script)).
- During a delay no metadata is read, so no reopen trigger fires.

## 9. Concurrency

| Work                                                                                                              | Runs on                     |
| ----------------------------------------------------------------------------------------------------------------- | --------------------------- |
| cutting pieces, writer calls, `reopen_on_metadata`, `reopen_when`, `reopen_delay`, `reopen_on_error`, `on_reopen` | the output's streaming work |
| the path getter, directories, confirmation, open, writes, seeks, reading back, finalisation, close, `on_close`    | work allowed to block       |

- `reopen()`, `start()` and `stop()` MAY be called from any thread: each posts
  a request carried out between pieces
  ([encoder.md §7](encoder.md#7-concurrency)). Nothing writes to a file
  after its close: the sink holds the descriptor and closes it once
  ([concurrency.md §4](../../../spec/concurrency.md#4-resources)).
- The streaming thread never waits for an open, a write, a close or
  `on_close`.

## 10. Tests

- **Each file is complete.** For WAV, MP3, FLAC, Ogg Vorbis and an `%ffmpeg`
  format, and each end (stop, each reopen trigger, a retry): every file
  starts with its header, decodes alone, and the samples of all files equal
  the samples written. In `on_close` the file is complete, finalised (WAV
  sizes, FLAC total and MD5, MP3 Info frame) and can be moved without
  affecting the output.
- **Final bytes reach the file at a final stop.** An Ogg file ends with its
  end-of-stream pages.
- **Names.** With `"$(title).ogg"` and `reopen_on_metadata` true, tracks A
  and B give `A.ogg` and `B.ogg`; with `%mp3`, by default, each carries
  its title in an ID3v2 tag, and a cover given as metadata. After a `reopen_when` split, the new file carries the current title.
  The getter is called once per open and once per retry; a getter that
  answers an unwritable path then a good one is followed.
- **No empty file.** A fallible output on a source that never produces, and
  a reopen on the first metadata, leave no 0-byte file and no `on_close`.
- **Retry delay on every path.** For a failing directory creation, open and
  write, at the first start, after a reopen and after a previous error: with
  a policy returning 1 s, attempts in 3 s are between 2 and 4; the policy
  sees kind `system`; a `null` policy stops the output and the clock and
  another output go on; with the default policy, the output retries after
  `fault_retry_delay`.
- **`on_close`.** After a stop its path's `clean` method is `true`,
  after a write fault `false`; a callback that uses the path as a plain
  string type-checks and is called too. A callback
  that raises reaches the policy once; with the default policy the output
  goes on with the next file.
- **Triggers.** `reopen_when={0m}` with defaults gives one file per hour;
  `on_reopen` runs for the three script triggers and not for a retry.
- **Append.** One minute of `%wav`, `%mp3`, `%flac` and `%ogg(%vorbis)`
  appended to one minute of the same: the result decodes as two minutes;
  `%ffmpeg(format="mp4")` with `append=true` is refused at evaluation.
- **FIFO.** A FIFO path with `%wav`: logs the streaming form once, the
  reader decodes; a reader opening 2 s late gets the stream from its first
  byte; the clock keeps its rate meanwhile. A non-fragmented MP4 to a FIFO
  is a hard error naming the path.
- **Never blocking.** A FIFO whose reader never reads: the clock's latency
  stays bounded, and the fault reaches the policy within `sink_backlog`.
- **Options apply to every format.** `perm`, `dir_perm`, `flush` and
  directory creation give the same outcome with `%wav` and with
  `%ffmpeg(format="wav")`; the script's format value prints the same before
  and after. `append=true` is accepted with `%wav` and refused with
  `%ffmpeg(format="wav")` ([ffmpeg.md §2](ffmpeg.md#2-declarations)).
- **Muxer from the name.** `%ffmpeg(%audio(codec="libmp3lame"))` to
  `output.file` on `"x.mp3"` writes MP3; on `{"#{time()}.mp3"}` it is
  refused at evaluation, asking for `format=`.
- **Concurrent requests.** `start()`, `stop()` and `reopen()` from a script
  thread every few milliseconds over thousands of cycles: no crash, no write
  after a close, every closed file complete.
