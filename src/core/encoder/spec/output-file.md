# `output.file` (as built)

How `output.file` drives an encoder and what it does with the bytes. It owns
the **pipe output** machinery shared with `output.external`: the open/closed
pipe, the encoder slot, the reopen triggers and the error-retry path.
[output-pipe.md](output-pipe.md) states only where `output.external` departs.

It relies on: the generic output, its states and its streaming cycle
([output.md](../../outputs/spec/output.md)); the piece cutting and metadata
export of the encoded base ([output-encoded.md](output-encoded.md)); the
instance record ([encoder.md](encoder.md)); the FFmpeg encoder for what a
`%ffmpeg` instance does with its file
([encoder.md](../../optionals/ffmpeg/spec/encoder.md)). `output.file.hls` is a
different operator ([HLS tree](../../outputs/hls/spec/)).

## 1. Formats accepted and how the factory is obtained

Any format value. The operator picks one of two variants at construction from
the format's kind:

| Variant         | Formats               | Who writes the file                                       |
| --------------- | --------------------- | --------------------------------------------------------- |
| **channel**     | every kind but FFmpeg | the output, from the bytes the instance returns           |
| **ffmpeg-file** | `%ffmpeg(...)`        | the instance itself; the output only creates and names it |

**Channel variant.** The factory is looked up (`get_factory`) each time an
instance is created (§2), not at construction. A format no plug-in answers
fails with an invalid-value error "Unsupported encoding format" on the format
argument. In practice this cannot happen: evaluating `%name(...)` already
looked the factory up and failed with "unsupported format"
([encoder.md §2](encoder.md#2-registry-and-factories)).

**ffmpeg-file variant.** At each instance creation the output builds a new
format from the script's one, with two fields replaced:

- destination: the URL `file:<current filename>`;
- container option `truncate`: `0` when `append` is true, else `1`. Any
  `truncate` the script gave is overwritten.

It then looks the factory up for that format. If no file is open at that
moment it fails with "output.file: no file is open" (not reachable: the slot
is only armed after an open). `truncate = 0` on a `file:` URL is how the FFmpeg
encoder learns it must append (§4.3).

**Parameters** (beyond the generic ones of
[output.md §1](../../outputs/spec/output.md#1-common-parameters-methods-and-callbacks)):

| Parameter               | Type                 | Default         | Use                                                                                         |
| ----------------------- | -------------------- | --------------- | ------------------------------------------------------------------------------------------- |
| `append`                | bool                 | `false`         | open for appending instead of truncating (§5.4)                                             |
| `perm`                  | int                  | `0o666`         | mode of a created file, under the umask                                                     |
| `dir_perm`              | int                  | `0o777`         | mode of each created directory, under the umask                                             |
| `on_close`              | `(string) -> unit`   | does nothing    | called with the filename after each close (§4.4)                                            |
| `flush`                 | bool                 | `false`         | flush the channel after each write (§4.1)                                                   |
| `reopen_on_error`       | `(error?) -> float?` | returns `null`  | error-retry policy (§6)                                                                     |
| `reopen_on_metadata`    | `(metadata) -> bool` | returns `false` | reopen trigger (§5.2)                                                                       |
| `reopen_when`           | `() -> bool`         | returns `false` | reopen trigger (§5.2)                                                                       |
| `reopen_delay`          | float getter         | `120.`          | minimum age of the file before `reopen_when` is asked                                       |
| `export_cover_metadata` | bool                 | `true`          | [output-encoded.md §3.2](output-encoded.md#32-which-metadata-is-handed-on-and-in-what-form) |
| (1st unlabelled)        | format               |                 |                                                                                             |
| (2nd unlabelled)        | string getter        |                 | filename (§5.1)                                                                             |
| (3rd unlabelled)        | source               |                 |                                                                                             |

Method `reopen()`: asks for a reopen at the next piece (§5.2). Callback
`on_reopen`. Telnet: only the generic output commands; no `reopen` command.

## 2. Instance lifecycle

The output has a **pipe** (open or closed), an **open date**, a **reopen
request** flag, a **current filename** (set or not) and an **encoder slot**:
`none`, `pending` or `created(instance)`.

**Open** (the output's `start`, a reopen, or the error-retry path):

1. open the file (§4), which sets the current filename;
2. open date := now;
3. clear the reopen request;
4. slot := `pending`.

No instance is created at open. A failure in step 1 leaves the pipe closed and
the slot unchanged (§6).

**Instance creation** happens on the first of these while the slot is
`pending`, and turns the slot into `created`:

- a call to the output's `encode_metadata` with a non-empty exported
  metadata;
- a call to the output's `encode` with a piece of positive length.

The factory receives the output's id as name and **empty initial metadata**,
always. The pipe outputs keep a "current metadata" for this purpose and for
filename interpolation, but nothing has written it since 2023 (finding).

**Close** (the output's `stop`, a reopen, the error path), only when the pipe
is open:

1. if the slot is `created`, call the instance's `stop`; if it raises, its
   result is taken as no bytes; slot := `none` (also from `pending`);
2. send the stop bytes as any bytes (§4); an exception from steps 1–2 is
   swallowed;
3. close the file (§4.4), then `on_close(filename)`, then clear the current
   filename.

So: **one instance per opened file**, created lazily, stopped at most once,
never reused. A file that never received a non-empty piece or an exported
metadata gets no instance, and no bytes: it is left empty (checked: with
`reopen_on_metadata` returning true, the file opened at start is reopened at
the first track's metadata before any byte and stays 0 bytes long).

| Event                                                       | Close | Open                                      | `on_reopen` | `on_start`/`on_stop` |
| ----------------------------------------------------------- | ----- | ----------------------------------------- | ----------- | -------------------- |
| output started                                              |       | yes                                       |             | `on_start`           |
| output stopped, or source ended (fallible), or put to sleep | yes   |                                           |             | `on_stop`            |
| reopen trigger (§5.2)                                       | yes   | yes                                       | yes         |                      |
| error (§6)                                                  | yes   | later, at the first piece after the delay | no          | no                   |
| clock reset after latency                                   | no    | no                                        |             |                      |

The pipe outputs replace the generic reset by a no-op: a reset is not a
discontinuity for them, and the file stays open with the same instance.

## 3. Call sequence

The base cuts the frame and calls, per piece, `encode_metadata` (when a
metadata is found) then `encode`, then `send`
([output-encoded.md §3](output-encoded.md#3-call-sequence)). The pipe output
wraps them:

**`encode_metadata(m)`** (`m` already reduced to exported keys, possibly
empty):

1. call `reopen_on_metadata(m)`; if true, reopen (§5.2), whatever the pipe's
   state;
2. if `m` is non-empty and the slot is `pending` or `created`: create the
   instance if needed, then `instance.encode_metadata(m)`.

**`encode(piece)`**, before encoding, first match of:

| Pipe   | Condition                                                     | Action |
| ------ | ------------------------------------------------------------- | ------ |
| closed | now ≥ open date                                               | open   |
| open   | reopen requested (and now ≥ open date, always true when open) | reopen |
| open   | now ≥ open date + `reopen_delay()` **and** `reopen_when()`    | reopen |

`reopen_delay` is read, and `reopen_when` asked, only when the pipe is open
and no reopen is requested; `reopen_when` only once the delay has elapsed.
Both are asked once **per piece**, not per frame. Then: if the piece has
positive length and the slot is not `none`, create the instance if needed and
return `instance.encode(piece)`; else return no bytes.

**`send(bytes)`**: when the pipe is open, write the bytes (§4); otherwise
drop them.

The pipe outputs never call `header`, `encode_into`, `restart_offset` or
`bitrate`. `stop` is called only from close.

Sequence for one file: open → [`encode_metadata`]\* interleaved with
`encode` per piece → `stop` → close. Example, `reopen_on_metadata` always
true, two tracks `A`, `B`, same filename getter giving `tag1`, `tag2`:

| Step                          | Calls                                                                                                                                                                                          |
| ----------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| start                         | open `tag0` (slot pending)                                                                                                                                                                     |
| piece 1, metadata `{title=A}` | `reopen_on_metadata` → true → close `tag0` (no instance: nothing written, `on_close(tag0)`) → open `tag1` → create instance (initial metadata empty) → `encode_metadata({title=A})` → `encode` |
| …                             | `encode` per piece into `tag1`                                                                                                                                                                 |
| metadata `{title=B}`          | close `tag1` (`stop`, write stop bytes, `on_close`) → open `tag2` → new instance → `encode_metadata({title=B})` → `encode`                                                                     |
| end                           | close `tag2`                                                                                                                                                                                   |

Checked with `%ogg(%vorbis)`: `tag1` carries `title=A`, `tag2` `title=B`,
`tag0` is 0 bytes.

## 4. The bytes

### 4.1 Channel variant: writing

- The file is opened with write-only, create, and either append or truncate,
  mode `perm`, as a buffered channel in binary mode. The channel buffer is
  the runtime's (64 KiB): without `flush`, bytes reach the file when the
  buffer fills or at close; with `flush`, the channel is flushed after every
  string of the returned rope.
- The output writes, in order, each string of each returned rope. It never
  seeks, never rewrites, never reads back.
- **The file's content is exactly the concatenation of the bytes returned by
  the instance's `encode` calls, then of its `stop`.** `header()` is never
  asked: an encoder whose format needs a header puts it in the bytes of its
  first `encode` (WAV, MP3's ID3v2, Ogg's header pages, FLAC's `fLaC` and
  STREAMINFO). An encoder that would need to patch its header at the end
  (sizes, seek tables) cannot: the output gives it no way back.

### 4.2 Worked examples (channel variant)

`%wav(mono)`, 8 kHz, a 0.1 s sine, one run: 1644 bytes = 44 + 800 samples ×
2 bytes; nothing after the last sample (WAV `stop` returns nothing):

```
00000000: 5249 4646 ffff ffef 5741 5645 666d 7420  RIFF....WAVEfmt
00000010: 1000 0000 0100 0100 401f 0000 803e 0000  ........@....>..
00000020: 0200 1000 6461 7461 dbff ffef 0000 5b2b  ....data......[+
```

The RIFF and data lengths are the WAV encoder's "unknown" values
(`0xEFFFFFFF`, `0xEFFFFFDB`, [wav.md §10](wav.md#10-bytes-produced)) and stay
so: the output never patches them.

`%mp3(mono, id3v2=true)`: the file starts with an empty ID3v2.3 tag, 10
bytes, because the instance is created with empty metadata (§2) and LAME's
`encode_metadata` does nothing:

```
00000000: 4944 3303 0000 0000 0000 fffb 90c4 0000  ID3.............
```

### 4.3 ffmpeg-file variant

- At open the output creates the file if missing (write-only, create, mode
  `perm`, **no truncation**) and closes the descriptor at once. Its channel is
  the filename itself; writing, flushing and closing it do nothing. `flush`
  has no effect; the FFmpeg `flush_packets` option is what flushes.
- The instance created next opens `file:<path>` itself:
  - `append = false`: FFmpeg's file protocol with `truncate = 1`. The output
    is seekable; the muxer finalises the container at `stop` (checked with
    `%ffmpeg(format="wav")`: the RIFF size `0x000022ba` and data size
    `0x00002274` are patched, and FFmpeg adds a `LIST/INFO/ISFT Lavf…`
    chunk);
  - `append = true`: the FFmpeg encoder writes through a descriptor it opens
    for appending (mode `0o644` if it had to create it, which it never has
    to). The output is not seekable: sizes are left at `0xFFFFFFFF` (checked).
- `encode` returns no bytes; `send` writes nothing. The stop of the instance
  writes the trailer and closes the file before the output's close runs
  `on_close` (§4.4).
- A muxer that writes several files (`format="dash"`, used by the stdlib's
  `output.file.dash`) writes them next to the named file; the output knows
  only the named one.

### 4.4 Close and `on_close`

Close order: instance `stop` → its bytes written → channel closed (flushes) →
`on_close(filename)` → current filename cleared. `on_close` therefore sees a
complete file, for both variants; the media tests rely on this (§8). An
exception from `on_close` propagates and leaves the current filename set
(§5.1).

### 4.5 Non-regular files

The open is a plain blocking open. On a FIFO with no reader it blocks the
clock's thread until a reader opens it (checked: the output's start returned
when the reader opened, 1.3 s late, and the clock logged "Latency is too
high"). Writes to a FIFO or a slow device block the same way. Append and
truncate flags are passed as given; on a FIFO they do nothing.

## 5. Reopen, reconnect, append, file naming

### 5.1 File naming

At each open, if no current filename is set:

1. call the filename getter;
2. expand a leading `~` or `~/` with `$HOME`;
3. interpolate `$(key)` and `$(if $(key),"then","else")` against the current
   metadata, replacing `/` in values by `-`. **The current metadata is never
   set**, so every `$(key)` becomes the empty string and every `$(if …)`
   takes its else branch (checked: `meta-$(title)-$(foo).wav` gives
   `meta--.wav`). `\$(` is kept literally;
4. create each missing directory of the path, top-down, with `dir_perm`
   (an existing non-directory component fails with `ENOTDIR`);
5. record the result as the current filename.

If the current filename is already set, it is reused without calling the
getter. It is set at step 5, before the file is opened, and cleared only after
a successful close. So when the open itself fails (permission denied), the
next retry opens the **same** name even if the getter now answers another
(checked, finding). A failure at step 4 does not set it.

Time-based names are written by the script (`{time.string("%H%M.flac")}`):
the getter is asked at every open.

### 5.2 Reopen triggers

| Trigger                      | When it is acted on               | Delay applies                                                                                        | `on_reopen` |
| ---------------------------- | --------------------------------- | ---------------------------------------------------------------------------------------------------- | ----------- |
| `reopen()` method            | next piece, pipe open             | no                                                                                                   | yes         |
| `reopen_when()` true         | asked per piece, pipe open        | `reopen_delay`, from the last open                                                                   | yes         |
| `reopen_on_metadata(m)` true | at once, inside `encode_metadata` | no; does not look at the pipe's state (unreachable during an error delay, when the cycle is skipped) | yes         |
| error retry (§6)             | first piece after the error delay | the returned delay                                                                                   | no          |

A reopen logs "Re-opening output pipe." (important), closes (§2), opens (§2),
then runs the `on_reopen` callbacks.

`reopen_on_metadata` receives only the exported keys
([output-encoded.md §3.2](output-encoded.md#32-which-metadata-is-handed-on-and-in-what-form)):
a metadata `[title=T, foo=bar/baz]` reaches it as `[title=T]` (checked). It is
called for every metadata handed on, including empty ones.

### 5.3 What a reopen does to the bytes

- Every opened file gets a new instance: its own header, its own stream.
  Nothing carries over between instances.
- When reopening on metadata, that metadata is then handed to the new
  instance by `encode_metadata`, so the new file carries it in-stream where the
  encoder supports it (Ogg comments: yes; LAME: no). When reopening by
  `reopen_when`, `reopen()`, an error retry, or a stop/start, the new instance
  receives no metadata until the next one in the stream.
- The same filename with `append = false` is truncated at each open: only the
  last file's bytes remain (checked: three 0.1 s tracks reopened on metadata
  into `meta--.wav` leave one 0.1 s file).
- A fallible output whose source ends and comes back is closed and reopened
  the same way (no `on_reopen`, but `on_stop` then `on_start`).

### 5.4 Append

`append = true` opens for appending. Each instance writes a complete
stream from its first byte, so an appended file is the **byte concatenation
of complete encoded files**, one per open. Checked, two runs into one file:

| Format                    | Result                                                                                                                  |
| ------------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| `%wav(mono)`              | two 44-byte headers; the second at offset 1644, read as audio by a decoder that trusts the first header's length        |
| `%ogg(%vorbis)`           | a chained Ogg file (two links); `ffprobe` reports the first link's 0.5 s                                                |
| `%mp3`                    | two MP3 streams back to back (8359 + 8359 bytes); with `id3v2=true`, by construction, a second (empty) ID3 tag mid-file |
| `%flac`                   | a second `fLaC` marker and STREAMINFO mid-file; `ffprobe` reports no duration                                           |
| `%ffmpeg(format="s16le")` | raw samples appended (8000 + 1600 bytes)                                                                                |
| `%ffmpeg(format="wav")`   | appended WAVs with sizes `0xFFFFFFFF` (non-seekable, §4.3)                                                              |

### 5.5 Stdlib and builtin callers

| Caller                                                            | Uses                                                                                                                                  |
| ----------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| `request.dump`                                                    | the operator with every default, `fallible=true`, id `request.drop`                                                                   |
| `synth:` protocol (`protocols.liq`)                               | `%wav` to a temporary file on a passive clock ticked by hand until `on_stop`                                                          |
| `output.file.dash` (`extra/audio.liq`)                            | `%ffmpeg(format="dash", ...)`; FFmpeg writes the manifest and segments                                                                |
| icecast server mount dump (`extra/icecast_server/mount_dump.liq`) | `flush=true`, the feeder's format (copy encoder with `flush_packets=1`), `reopen_on_error` that stops the dump and returns `1000000.` |

## 6. Failures

| Where                                        | What is raised                                                                                                                                   |
| -------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| directory creation, open, write, close       | runtime error of kind `system`, message from the system (`<path>: Permission denied`, `Broken pipe`)                                             |
| factory, `encode`, `encode_metadata`, `stop` | the encoder's exception, unchanged ([encoder.md §3.8](encoder.md#38-failure-behaviour-common-to-all-operations)); `stop`'s is swallowed at close |
| script callbacks                             | their errors, unchanged                                                                                                                          |

An I/O error surfaces at the write that flushes the channel, possibly frames
after its cause.

**Error-retry path.** The pipe output wraps its whole streaming cycle,
including the start transition that runs inside it. On any exception:

1. close, ignoring every error of the close;
2. call `reopen_on_error(error)` with the error as a script error (kind
   `output` for non-script exceptions);
3. if it returns `d ≥ 0`: log "Error while streaming: <error>, will re-open in
   <d>s" (important); open date := now + `d`;
4. if it returns `null` or a negative number: re-raise; the clock's source
   error rule applies ([clock.md §11](../../clock/spec/clock.md#11-failure)).

While the pipe is closed and now < open date, the output's cycle is skipped
**entirely**: the output does not pull its source, so nothing is lost, the
stream is held (checked: a 2 s source with 1 s of failing opens took 3.05 s
of wall time and the file holds 2 s). When the date passes, the cycle runs:
an output whose start had failed is started (open, `on_start`); a started one
opens at its next piece. The output stays `started` through errors: no
`on_stop`, no `on_start`.

The remaining pieces of the frame being encoded when the error hit are lost.
The current instance is stopped and its stop bytes are written if the pipe
still takes them.

Exceptions outside the cycle (the output's `stop` on sleep or on request) are
not caught here: they go to the generic output's failure rule.

## 7. Threads and locks

Everything runs on the thread animating the output's clock: opens (which can
block, §4.5), writes (blocking), closes, the instance's calls, and every script
callback (filename getter, `reopen_when`, `reopen_delay`, `reopen_on_metadata`,
`reopen_on_error`, `on_close`, `on_reopen`). `reopen()` may be called from any
thread: it sets an atomic flag. The current filename is an atomic cell. No
lock is taken by the output; the instance's own lock is the seam's
([encoder.md §4](encoder.md#4-serialisation-protect)).

## 8. Tests

- **A closed file is complete and decodable.** `tests/media/*.liq` and the
  generated `encoder_*.liq.in` write with `output.file`, then `ffprobe` the
  file from `on_close`. Binding: `on_close` runs after the instance's stop
  bytes are on disk; probe succeeds with the expected stream parameters.
  Incidental: durations, the probe tool.
- **Reopen triggers fire in order and errors reach the right policy.**
  `tests/regression/GH2756.liq`: metadata reopen, then `reopen_when`, then an
  open failure retried after 2 s (callstack `metadata, reload, error`).
  `GH2756-2.liq`: `reopen_on_error` returning `null` sends the error to the
  clock's `on_error` (callstack `metadata, error, streaming error`). Binding:
  the order and that a `null` policy escalates. Simulated failure: a path
  whose parent is a regular file. Doubles: a filename getter switchable at
  run time, a clock error handler.
- **The error delay is honoured.** `tests/regression/output_reopen_on_error_delay.liq`:
  an always-failing open with a 1 s delay is retried a handful of times in
  3 s, not once per cycle. Binding: attempts > 0 and ≤ 10.
- **`reopen_when` with a time-based name gives several complete files.**
  `tests/regression/GH3316.liq` (`%flac`, `reopen_delay=2.`): three
  `on_close` calls, each file probes as FLAC.
- **Append across output lifetimes.** `tests/regression/GH4819.liq`: outputs
  with `append=true` shut down and recreated; binding is only that the new
  output starts. Nothing checks the appended bytes.
- Not covered: filename interpolation, `perm`/`dir_perm`, FIFO destinations,
  `flush`, the ffmpeg-file variant's append, exported-key filtering at
  `reopen_on_metadata`.
