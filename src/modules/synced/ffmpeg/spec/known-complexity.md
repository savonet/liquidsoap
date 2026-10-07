# Known complexity

Pitfalls the history of these bindings has already paid for, stated without
reference to the code that hit them. Each entry is a warning to someone
writing the same bindings from the specification.

## Measurement

- **Commits**: 826 subjects scanned (777 in the upstream repository, 49 more
  in the embedded tree); about 140 diffs read.
- **Fix commits**: 109 upstream subjects mention fix, leak, crash, segfault or
  revert; 14 of the 49 embedded commits are fixes, one of them an audit that
  fixed nineteen defects at once.
- **Issues and pull requests**: 31 upstream issues listed, 18 read in full; 87
  upstream pull requests listed, 23 read; 8 liquidsoap issues and 4
  liquidsoap pull requests read where a binding fix named them.
- **Reverts**: 20 upstream subjects say "revert". Nine withdrawn or reshaped
  designs are reconstructed in the entries below. One revert, forcing symbolic
  linking of the stubs on Linux (`bdf0c6f`, undone in `a66297d`), carries no
  recorded reason and is not an entry.

Each entry ends with a **Count** of the separate fixes or reports it stands
for, a **Sources** trace, and a **Rule** line naming the rule of the
specification that answers it. `#N` is an upstream issue or pull request, `liquidsoap#N` one
in liquidsoap's tracker, `liq:` prefixes a commit of the embedded tree.

## Lifetimes and garbage collection

### OCaml values held across an allocation

- **Situation** — A C function builds a compound result (a tuple, a list, a
  record of planes) or calls back into OCaml while it still holds a parameter
  or an intermediate value in a C local. Any allocation in between may run a
  collection that moves the value.
- **What went wrong** — Segmentation faults with no pattern, under dynamically
  created inputs and after weeks of uptime; a collector that tried to allocate
  terabytes after reading a corrupt header; a process that restarted silently
  several times a day.
- **Why it is hard** — The window is one allocation wide and only opens when a
  collection lands inside it. Parameters that are immediates today become
  blocks when a type changes. A helper that returns a fresh value is safe on
  its own and unsafe as the argument of the next allocating call. A value
  passed to a closure invoked after the runtime lock was released and retaken
  is in the same position.
- **How to tell** — Run every operation that returns a compound value, and
  every operation that takes a closure, under a runtime that collects and
  compacts at each allocation, and compare with a reference run. A small minor
  heap is not such a check: the runtime clamps it, and large results skip it.
- **Count** — 7 fixes, 4 reports. **Sources** — `923d137`, `6d9b6f2`,
  `4ae9631`, `b661d57`, `6e01868`, `2b8a876`, liq:`ece26c0b7`,
  liq:`f69ea58b3`; #64, #80, liquidsoap#1941, liquidsoap#4064,
  liquidsoap#4065, liquidsoap#4708, liquidsoap#5334.
- **Rule:** [binding-contract.md](binding-contract.md) §2.3 B1; [tests.md](tests.md) §12, collection at every allocation.

### C data and OCaml values confused at the boundary

- **Situation** — A structure of small C integers is returned as an OCaml
  block; an enumeration value already converted to a variant is converted
  again; an OCaml handle is passed where FFmpeg expects the native object it
  wraps.
- **What went wrong** — A video encoder died inside the major collector about
  twice an hour, because even integers stored untagged were followed as
  pointers. Encoder capabilities came back as values no constructor could
  match, so a capability test was always false. Ten of eleven option readers
  read garbage.
- **Why it is hard** — Each mistake type-checks in C. The untagged integer
  only matters when a collection scans the half-built block. The double
  conversion yields a well-formed value of the wrong identity.
- **How to tell** — For every returned integer field, read it back from OCaml
  and compare with the value FFmpeg reports. For every enumeration result,
  match it against a literal constructor. For every reader of a native
  object, set the value through FFmpeg and read it through the binding.
- **Count** — 3 fixes. **Sources** — liq:`531973cbc`, liq:`58e3fb3de`; #114.
- **Rule:** [binding-contract.md](binding-contract.md) §2.3 B2; [build.md](build.md) §3.6 G11; [tests.md](tests.md) 1.3, 2.4.

### Native memory the collector cannot see

- **Situation** — A frame or a packet is a few words on the OCaml heap and
  megabytes on the C heap. The collector paces itself on what it can see.
- **What went wrong** — Memory grew far beyond the live set while decoding,
  because handles were collected at the pace of small objects. A duplicated
  packet was accounted as empty.
- **Why it is hard** — Every way of producing a handle must report the size,
  including the ones added later (duplication, conversion output).
- **How to tell** — Decode a long video in a loop that drops every frame, and
  check that resident memory stays flat without explicit collections. Repeat
  for each operation that produces a frame or a packet.
- **Count** — 3 fixes. **Sources** — `e2d36e1`, `fc40b33`, liq:`711701f22`.
- **Rule:** [binding-contract.md](binding-contract.md) §2.2 L9, which holds for every operation that produces a frame or a packet.

### What a finaliser is allowed to do

- **Situation** — Releasing a container closes network connections, may log,
  may invoke callbacks, and must drop the roots that kept closures alive. The
  collector runs the release.
- **What went wrong** — Crashes at collection time, and a crash specific to
  OCaml 5 once collections ran concurrently. A redesign that moved the whole
  release into one finaliser was reverted the day it landed.
- **Why it is hard** — The runtime offers two kinds of finaliser with
  different rights. The one tied to the memory block may not release the
  runtime lock, allocate, or touch roots. The one that may do those things
  runs earlier and may run while the value is still reachable from another
  finalised value. A release that blocks needs the first kind's timing and the
  second kind's rights.
- **How to tell** — Open and drop containers, with and without callbacks, in a
  loop with collections forced, on every supported OCaml version, under an
  address sanitiser. Each release step is classified: needs the lock released, touches
  roots, or frees memory only.
- **Count** — 6 fixes, 1 revert. **Sources** — `2181f29`, `c5613c4`,
  `c8ced45`, `20929df`, `4bbbd57`, `9c80044`, `9e671f8`.
- **Rule:** [binding-contract.md](binding-contract.md) §2.2 L7 and L8; [avformat.md](avformat.md) §2.1, Release. The two kinds of finaliser are described in [language-notes/ocaml-c-interface.md](language-notes/ocaml-c-interface.md).

### Dependent handles that outlive their parent

- **Situation** — A stream names its container, a filter context its graph, a
  custom I/O object is used by its container, plane bigarrays point into a
  frame. The program keeps the dependent value and drops the parent.
- **What went wrong** — A graph was collected while a frame was being pushed
  into one of its filters. A container was collected before its streams under
  OCaml 5. An I/O object was freed while its container still read from it.
- **Why it is hard** — The dependency is invisible to the collector unless the
  dependent value holds the parent, and holding it through a finaliser closure
  proved fragile. A parent released explicitly leaves the dependent value
  valid as an OCaml value.
- **How to tell** — For each dependent kind: create it, drop every other
  reference to the parent, force a full collection, then use it. The operation
  succeeds, or fails with the documented closed error.
- **Count** — 4 fixes. **Sources** — `be34b3b`, `ffb9e27`, `133bc4a`,
  `ea4fcdd`, `9e671f8`.
- **Rule:** [binding-contract.md](binding-contract.md) §2.2 L3 and L4; the dependent handles are listed in [avformat.md](avformat.md) §2.1, [avfilter.md](avfilter.md) §2.2 and [avutil.md](avutil.md) §2.5, §8.2.

### Callback roots on a failed open

- **Situation** — An open is given an interrupt function or I/O callbacks. The
  closures are rooted inside the native record before FFmpeg is asked to open.
  The open fails, for example on an HTTP 404 in a reconnect loop.
- **What went wrong** — "Fatal error: out of memory" with no memory in use,
  and segmentation faults at a later collection: a root still pointed into the
  freed record.
- **Why it is hard** — Every failure branch between rooting and success must
  unroot, in the right order relative to freeing. Open has many such branches
  and each new option adds one.
- **How to tell** — For each open function and each of its failure causes
  (unknown format, rejected option, unreachable URL, failed probe), fail it
  with every callback supplied, then force a major collection.
- **Count** — 5 fixes, 1 report. **Sources** — `2f2e9ef`, `70f4cc9`,
  `1b6ca47`, `d38c2b0`, `ca20131`; #55.
- **Rule:** [binding-contract.md](binding-contract.md) §2.2 L2 and L8, §7.1 C3; [avformat.md](avformat.md) §4.3 and §4.4; [tests.md](tests.md) 3.2.

### Error paths between the C allocation and the OCaml handle

- **Situation** — An operation allocates a native object, then performs steps
  that can fail or raise, then wraps the object in a handle. Or it receives a
  packet or frame it decides not to return.
- **What went wrong** — A leak that grew with every failed connection, every
  skipped packet, every receive that produced nothing, every exception raised
  by an I/O callback during open. A stream freed on a failed creation and
  freed again at close.
- **Why it is hard** — Until the handle exists nothing will release the
  object. Raising an OCaml exception from C skips every cleanup written after
  it. Wrapping first makes the finaliser responsible for half-built objects.
- **How to tell** — Inject a failure at each step of each constructor and each
  read, in a loop, under a leak detector that is first shown to report a
  planted leak.
- **Count** — 10 fixes. **Sources** — `98c8ac0`, `54563e3`, `e129fcb`,
  `09452a6`, `8faf61c`, `2f91f93`, `ea4fcdd`, `cb21b05`, `4153cca`,
  `3d9edbe`, liq:`58e3fb3de`; #76.
- **Rule:** [binding-contract.md](binding-contract.md) §2.2 L1 and L2; [avcodec.md](avcodec.md) §11, last paragraph, for a receive that returns nothing.

### Two allocator families

- **Situation** — The binding allocates its own records and buffers, hands
  some of them to FFmpeg, and frees objects FFmpeg allocated.
- **What went wrong** — Memory obtained from the C library was released by
  FFmpeg and the reverse. A buffer was passed to a release call that expects
  the address of a pointer. A packet was released as plain memory, leaving its
  payload.
- **Why it is hard** — FFmpeg may replace a buffer it was given, so the
  pointer to free is the current one. Each FFmpeg object has its own release
  call, and several accept a pointer of the wrong level without complaint.
- **How to tell** — Every allocation that crosses the boundary uses FFmpeg's
  allocator. Each object kind has one release call. Run the suite with an
  allocator that aborts on a mismatched free.
- **Count** — 3 fixes. **Sources** — `250e4cb`, `2f91f93`, liq:`58e3fb3de`.
- **Rule:** [binding-contract.md](binding-contract.md) §2.2 L1 and L10.

## Runtime lock and threads

### Lock state on every exit path

- **Situation** — An operation releases the runtime lock around an FFmpeg
  call, or a callback takes it. The call fails, or the callback returns early
  with an error.
- **What went wrong** — A deadlock when an I/O callback raised. A livelock in
  scaler creation. An encoder opened with the lock taken twice.
- **Why it is hard** — Release and acquire are separate statements with
  branches between them. Taking the lock twice or releasing it twice does not
  fail at once on every platform.
- **How to tell** — For each operation that releases the lock, force each
  failure it can have while a second OCaml thread counts in a loop: the
  counter keeps advancing during the call and the caller gets its error.
- **Count** — 6 fixes. **Sources** — `ccf8ed9`, `5b4d506`, `e74a92d`,
  `c418da8`, `b05372d`, `55551d8`.
- **Rule:** [binding-contract.md](binding-contract.md) §6.1 M3.

### OCaml heap data used while the lock is released

- **Situation** — An FFmpeg call that may block is given a string, an option
  table or sample data that still lives on the OCaml heap, with the runtime
  lock released.
- **What went wrong** — Crashes at random points of startup and after hours of
  encoding, "invalid argument" from FFmpeg on arbitrary tracks.
- **Why it is hard** — Releasing the lock is recommended for every long call,
  and every input of such a call must first be copied out. The copy is easy to
  forget for small arguments.
- **How to tell** — For each operation that releases the lock, list its
  inputs: each is an immediate, a C copy, or a native object. Stress it with a
  second thread that allocates continuously.
- **Count** — 2 fixes. **Sources** — `f228110`, `9f87180`; #12,
  liquidsoap#1045.
- **Rule:** [binding-contract.md](binding-contract.md) §6.1 M2.

### A call made with the lock held re-enters through a callback

- **Situation** — A container uses custom I/O or an interrupt function. An
  operation thought to be cheap (position query, reopening the output,
  release) makes FFmpeg call the I/O layer, which calls back into OCaml.
- **What went wrong** — The callback waited for a lock its own caller held.
- **Why it is hard** — Which FFmpeg calls reach the I/O layer is not visible
  in their names. Release is reached both explicitly and from the collector,
  with different lock states.
- **How to tell** — With custom read, write and seek functions that record
  their calls, run every container operation: each one that triggers a
  callback returns.
- **Count** — 3 fixes. **Sources** — `2181f29`, liq:`58e3fb3de`; #31.
- **Rule:** [binding-contract.md](binding-contract.md) §6.1 M4; [avformat.md](avformat.md) §6.1 lists `tell`, `flush`, `close` and release by collection among the calls made with the lock released.

### Callbacks arriving on threads the runtime has not seen

- **Situation** — FFmpeg runs an I/O, interrupt or device callback on one of
  its own threads.
- **What went wrong** — The callback entered the runtime from an unregistered
  thread. Registration was added, later removed with a rewrite of the transfer
  buffer, and restored by an audit.
- **Why it is hard** — On the usual path the callback runs on the calling
  OCaml thread and works without registration. Registration must be undone at
  thread exit, not at callback exit.
- **How to tell** — Drive a callback from a thread created in C. The callback
  completes and the thread exits cleanly.
- **Count** — 3 changes. **Sources** — `38f651a`, `fa0ec37`,
  liq:`58e3fb3de`.
- **Rule:** [binding-contract.md](binding-contract.md) §6.3 M10; [avutil.md](avutil.md) §6.3; [tests.md](tests.md) 5.3.

### Codecs and scalers run on one thread unless asked

- **Situation** — A codec is opened, or a scaler created, with FFmpeg's
  defaults.
- **What went wrong** — A 4K decode ran on one core and the pipeline at half
  real time. A thread count given to the encoder looked ignored, because the
  decoder was the bottleneck. The scaler ignored its thread setting entirely.
- **Why it is hard** — libavcodec defaults to one thread and FFmpeg's own
  tools override that before opening. The scaler's slice threads are reachable
  only through its frame-based entry point.
- **How to tell** — Open a decoder with no option and read back its thread
  count; a caller's explicit count still wins. Scale with several threads and
  compare the output byte for byte with one thread.
- **Count** — 1 fix. **Sources** — liq:`1935cbb18`; #117, liquidsoap#5014.
- **Rule:** [avcodec.md](avcodec.md) §4.9; [swscale.md](swscale.md) §4.5; [tests.md](tests.md) 2.5, 7.3.

## Callbacks

### An exception inside a callback run by FFmpeg

- **Situation** — A read, write, seek or interrupt function supplied by the
  program raises while FFmpeg's frames are on the stack.
- **What went wrong** — The exception unwound through FFmpeg, leaving the lock
  and FFmpeg's state inconsistent. The first repair returned an error code
  without restoring the lock state. The error code itself was changed from
  "unknown" to "external", with the exception text sent to FFmpeg's log.
- **Why it is hard** — Four callbacks need the same treatment and two were
  written without it for years. The exception has nowhere to go: FFmpeg
  reports its own error code to the outer operation.
- **How to tell** — Make each callback raise. The enclosing operation raises
  `Avutil.Error`, the exception text is logged, and the container can still be
  closed.
- **Count** — 5 fixes. **Sources** — `54563e3`, `7c945df`, `e74a92d`,
  `ad874cd`, liq:`58e3fb3de`.
- **Rule:** [binding-contract.md](binding-contract.md) §7.1 C1; [avformat.md](avformat.md) §7.1; [tests.md](tests.md) 3.14.

### The log callback

- **Situation** — FFmpeg logs from any thread, at any time, including from
  inside a call the binding made with the runtime lock held, and from threads
  that hold FFmpeg's internal locks.
- **What went wrong** — Calling OCaml from the log callback deadlocked. A
  queue drained by an OCaml thread was added, disabled, re-enabled and
  reverted within three weeks, left off for four years, then rebuilt so that
  the C side touches no OCaml state. That version lost wake-ups: the reader
  slept with messages queued.
- **Why it is hard** — The callback cannot know whether its thread holds the
  runtime lock. Anything it takes may already be taken. The reader must check
  the queue under the same lock the writer signals under, and the writer runs
  on FFmpeg's hot path.
- **How to tell** — Log from several threads at once, at debug level, while
  encoding: every line is delivered, none twice. Log from a thread that holds
  the runtime lock: no hang. Stop logging, then log again.
- **Count** — 9 changes, 2 reverts. **Sources** — `02ecb6e`, `21bf5a3`,
  `e683c3f`, `a940842`, `f4007d2`, `50a3ece`, `a8ce7c9`, `997e797`,
  liq:`e9bb70415`, liq:`9bc0ad0f0`; #20, #49, #119.
- **Rule:** [avutil.md](avutil.md) §7.1 N1–N10; [tests.md](tests.md) 1.14, 1.15.

### The interrupt callback: identity and lifetime

- **Situation** — An interrupt function is installed on a container. FFmpeg
  polls it during any blocking I/O, including the I/O done while the container
  is being released.
- **What went wrong** — FFmpeg was given the closure's address at installation
  time; the collector moved the closure. The function was polled during
  release after its root had been dropped. Outputs registered it differently
  from inputs.
- **Why it is hard** — FFmpeg keeps one opaque pointer for the life of the
  context, and it must stay valid while the closure moves. Release order
  matters: the last blocking call comes before the root is dropped.
- **How to tell** — Install an interrupt function, force compactions, block on
  a silent socket, set the flag: the open returns. Close a container whose
  release blocks: the function is called or skipped, never called after
  release.
- **Count** — 5 fixes. **Sources** — `38f651a`, `837fdf2`, `81cf1f4`,
  `1b6ca47`, `d38c2b0`.
- **Rule:** [binding-contract.md](binding-contract.md) §2.3 B1 and §2.2 L8; [avformat.md](avformat.md) §7.1.4; [tests.md](tests.md) 3.15.

### Custom I/O: how much FFmpeg hands over

- **Situation** — FFmpeg calls the custom write function with a byte count of
  its choosing. The binding moves bytes through a transfer buffer of fixed
  size. A custom read function returns zero bytes at end of input.
- **What went wrong** — With a transfer buffer smaller than FFmpeg's own, each
  large write was cut and reported as complete. HLS output stuttered or was
  silent above a bitrate, with no error. A read returning zero was not turned
  into end of file.
- **Why it is hard** — FFmpeg treats the write result as success or failure,
  not as a count. The size FFmpeg uses depends on the muxer and the bitrate,
  so low-bitrate tests pass.
- **How to tell** — Mux through a custom write function at a bitrate that
  fills FFmpeg's buffer and compare the bytes with the same mux to a file.
  Feed a custom read function that ends: the reader sees end of input.
- **Count** — 4 fixes. **Sources** — `89a5118`, `fa0ec37`, `68d2b86`,
  liq:`480f10bde`; #72.
- **Rule:** [avformat.md](avformat.md) §7.1.1 and §7.1.2; [tests.md](tests.md) 3.12, 3.13.

### Reporting key frames while muxing

- **Situation** — A program segments its output and needs to act exactly
  before each key frame reaches the muxer. Frames go in; packets come out of
  the encoder later, several at a time or none.
- **What went wrong** — A query "was the last write a key frame" was removed
  as unreliable. A flush-on-key-frame option was added to both frame and
  packet writes and removed from packet writes the same day. The surviving
  callback on frame writes was invoked with an unrooted closure after the lock
  had been released; the repair was applied, reverted, and applied again a
  year later.
- **Why it is hard** — The key-frame flag exists only on packets, after the
  encoder's delay. The callback runs in the middle of an operation that has
  released the lock.
- **How to tell** — Encode with a callback that records the muxer position:
  each recorded position is the start of a key packet. Run it with collections
  forced.
- **Count** — 6 changes, 1 revert. **Sources** — `f054d79`, `3f837ba`,
  `bbbc69c`, `7fa2270`, `6e01868`, `2b8a876`, liq:`ece26c0b7`; #71.
- **Rule:** [avformat.md](avformat.md) §4.4 (`write_frame`), §6.2 and §7.2; [tests.md](tests.md) 3.16.

## Containers and streams

### Streams that appear after the input is opened

- **Situation** — A demuxer without a header (MPEG-PS) reports streams as it
  meets them while packets are read. Any per-stream state sized at open is too
  short.
- **What went wrong** — A valid MP3 probed as MPEG-PS played to its end, then
  the process crashed on close with 25 streams against a table sized for
  fewer.
- **Why it is hard** — The stream count is stable for almost every format.
  Every loop over streams and every per-stream table must use the current
  count, on read, seek and release.
- **How to tell** — Read an input with one stream at open and more starting
  later, seek, and close it. [tests.md](tests.md) §11 describes the fixture.
- **Count** — 1 fix. **Sources** — liq:`9cac50e42`; liquidsoap#5412.
- **Rule:** [avformat.md](avformat.md) §2.2; [tests.md](tests.md) 3.8.

### Operations on a closed container

- **Situation** — A program uses a container or one of its streams after
  `Av.close`, closes twice, or the collector releases a container that was
  closed explicitly.
- **What went wrong** — Null dereferences in stream accessors, a second
  release of the same objects, crashes in accessors called on outputs that had
  no stream table yet.
- **Why it is hard** — The check must sit in front of every operation,
  including the ones reached through a stream, and release must be idempotent
  because two paths reach it.
- **How to tell** — Call every container and stream operation on a closed
  container: each raises the closed error. Close twice. Drop a closed
  container and collect.
- **Count** — 5 fixes. **Sources** — `07098d5`, `51799b9`, `262bc70`,
  `a1794c7`, liq:`58e3fb3de`.
- **Rule:** [binding-contract.md](binding-contract.md) §2.2 L5 and §5.3; [avformat.md](avformat.md) §2.1; [tests.md](tests.md) 3.1.

### Muxer calls before the header exists

- **Situation** — An output is flushed or closed before any packet was
  written, so the muxer was never initialised.
- **What went wrong** — A crash when flushing an encoder that had not started;
  a trailer written to a muxer with no header.
- **Why it is hard** — The header is written lazily by the first write. Every
  other muxer call depends on that hidden state.
- **How to tell** — Open an output, add streams, then flush and close without
  writing. Both return.
- **Count** — 2 fixes. **Sources** — `9b20007`, `26eb22d`.
- **Rule:** [avformat.md](avformat.md) §2.1, states of an output container; [tests.md](tests.md) 3.4.

### Text FFmpeg returns as NULL

- **Situation** — A descriptor field documented as a string is absent: a codec
  with no long name, a filter pad with no name. Or the program passes neither
  a URL nor a format to an open.
- **What went wrong** — Crashes when listing codecs, when opening with empty
  arguments, and, in an open report, at module load while enumerating the
  pads of every registered filter.
- **Why it is hard** — Enumeration happens at load time, over whatever the
  installed FFmpeg registers, so one unusual external filter stops every
  program. Each string accessor needs its own guard.
- **How to tell** — For every string the binding copies from FFmpeg, state its
  value when the pointer is null. Load the filter module against a build with
  every optional filter enabled.
- **Count** — 2 fixes, 1 open report. **Sources** — `44275fd`, `f96950a`;
  #120.
- **Rule:** [binding-contract.md](binding-contract.md) §1.3 I1 and §4 A3; [tests.md](tests.md) 4.1.

### Setting metadata replaces

- **Situation** — Metadata is set a second time on a stream, a container or a
  frame.
- **What went wrong** — New entries were merged into the old dictionary, so
  stale tags survived; an earlier version walked a dictionary while deleting
  from it.
- **Why it is hard** — FFmpeg's dictionary call adds or overwrites one key.
  "Set" at the interface means the whole table.
- **How to tell** — Set `{a, b}` then `{b}`: a read returns `{b}`.
- **Count** — 2 fixes. **Sources** — `01c6238`, `09bec7f`.
- **Rule:** [avformat.md](avformat.md) §4.4; [avutil.md](avutil.md) §4.3; [tests.md](tests.md) 1.9, 3.18.

### A copied stream has no frame rate

- **Situation** — A video stream is remuxed: its codec parameters are copied
  to a new output stream.
- **What went wrong** — The output stream carried no average frame rate.
  Three attempts in one day set it in the copy, reverted that, and ended with
  an accessor pair the caller uses.
- **Why it is hard** — The frame rate belongs to the stream, the copied
  parameters to the codec. The muxer does not derive one from the other.
- **How to tell** — Remux a constant-rate video and probe the result: the
  stream reports the source's frame rate.
- **Count** — 3 changes, 1 revert. **Sources** — `9731489`, `f7d4055`,
  `9c4cc61`, `3d7ccd2`; #61.
- **Rule:** [avformat.md](avformat.md) §4.4, stream copies; [tests.md](tests.md) 3.19.

### One operation written once per media kind

- **Situation** — Audio, video and subtitle variants of the same operation
  (create a stream, write a frame, list a capability, convert a container of
  samples) are separate code paths.
- **What went wrong** — An audit found nineteen defects where the copies had
  drifted: a bounds check reversed in one variant, a closed-container check
  present in one of three, an index used before its check, a free on one
  path only.
- **Why it is hard** — Each variant is tested with its own happy path. A
  guard added to one is not missed in the others until a misuse reaches them.
- **How to tell** — For each operation that exists per media kind, run the
  same misuse cases on every kind: closed container, out-of-range stream,
  failed creation, header already written.
- **Count** — 1 audit, 19 defects. **Sources** — liq:`58e3fb3de`; #114,
  liquidsoap#5334.
- **Rule:** [binding-contract.md](binding-contract.md), preamble: each library file answers every clause, so a missing answer is visible; [tests.md](tests.md) §0, misuse is tested like use.

## Encoding and decoding loops

### Frames left in the decoders at end of input

- **Situation** — `Av.read_input` decodes frames. The demuxer reports end of
  file while decoders still hold frames (reordering delay, frame threading).
- **What went wrong** — A single-frame image yielded no frame. A video lost
  its tail: 616 of 625 frames, more with more decoder threads.
- **Why it is hard** — The loss grows with the decoder's delay, which is zero
  for most audio and for intra-only video. Draining is a state of each
  decoder, and a seek back from the end must leave it.
- **How to tell** — Count the frames of a file with B-frames and of a
  single-frame image; seek to the start after end of input and count again.
- **Count** — 1 fix. **Sources** — liq:`d9d649c45`; #118, liquidsoap#5442.
- **Rule:** [avformat.md](avformat.md) §4.3, `read_input` step 6; [tests.md](tests.md) 3.6.

### Frames left in the decoders across a seek

- **Situation** — `Av.seek` repositions the demuxer while decoders hold
  frames decoded before the seek.
- **What went wrong** — The first frames after the seek carried pre-seek
  timestamps. A rate filter downstream bridged the gap by duplicating frames:
  memory grew by hundreds of megabytes per second until the process was
  killed.
- **Why it is hard** — The demuxer and the decoders are separate objects and
  the seek call only knows the first. The stale frames are valid frames.
- **How to tell** — Read some frames, seek far ahead, read one frame: its
  timestamp is at the target. The fixture needs B-frames, or the decoder
  holds nothing.
- **Count** — 1 fix. **Sources** — liq:`797883677`; #116, liquidsoap#5385.
- **Rule:** [avformat.md](avformat.md) §4.3, `seek`; [tests.md](tests.md) 3.7.

### A receive that yields nothing

- **Situation** — `avcodec_receive_frame` or `avcodec_receive_packet` answers
  `AVERROR(EAGAIN)` or `AVERROR_EOF`. The binding had allocated the object to
  receive into. With a hardware decoder, a second frame is needed for the
  download.
- **What went wrong** — One frame or packet leaked per empty receive: a leak
  proportional to running time. The hardware download was attempted before
  there was a frame.
- **Why it is hard** — "Nothing yet" is the most frequent outcome of a
  receive and is reported as an error code. The early decoding loop also
  mixed up "need more input" with "output pending".
- **How to tell** — Decode and encode a long stream under a leak detector.
  Check that a packet producing several frames delivers all of them, and that
  a packet producing none is not an error.
- **Count** — 4 fixes. **Sources** — `7494b06`, `cb21b05`, `3d9edbe`; #29,
  #31, #76.
- **Rule:** [avcodec.md](avcodec.md) §4.10 and §11. The hardware download this entry mentions is not offered: decoders decode in software ([avcodec.md](avcodec.md) §4.9).

### Flushing twice

- **Situation** — An encoder is flushed, then given another frame or flushed
  again.
- **What went wrong** — The second flush reached FFmpeg in a state it does
  not accept.
- **Why it is hard** — FFmpeg's draining state has no public query; the
  binding has to remember it, per encoder and per decoder, and the two do not
  answer a repeated flush the same way.
- **How to tell** — Flush twice, and send after flush, on an encoder and on a
  decoder: each case has one documented outcome.
- **Count** — 1 fix. **Sources** — `1b72beb`.
- **Rule:** [avcodec.md](avcodec.md) §2.4, one state table for decoders and encoders; [tests.md](tests.md) 2.7.

## Timestamps and time bases

### "Unknown" encoded as a number

- **Situation** — FFmpeg marks an unknown duration with `AV_NOPTS_VALUE` and
  an unknown aspect ratio with a zero numerator.
- **What went wrong** — An unknown stream duration was rescaled and returned
  as a huge negative time. An unknown aspect ratio was returned as a ratio.
- **Why it is hard** — The sentinel survives arithmetic. Each field has its
  own sentinel, and some accessors must stay raw for round-tripping.
- **How to tell** — Read the duration of a live stream and the aspect ratio
  of a stream that declares none: both are absent, not numbers.
- **Count** — 3 changes. **Sources** — `ddd4098`, `6f466fd`, `0a81aff`.
- **Rule:** [binding-contract.md](binding-contract.md) §3 E7; [avformat.md](avformat.md) §4.3; [avcodec.md](avcodec.md) §3.2; [avutil.md](avutil.md) §4.13; [tests.md](tests.md) 3.20.

### Setting a frame timestamp leaves the derived one behind

- **Situation** — A program sets the presentation timestamp of a decoded
  frame. FFmpeg components read the frame's best-effort timestamp instead.
- **What went wrong** — The new timestamp was ignored downstream.
- **Why it is hard** — The frame carries several timestamp fields and FFmpeg
  fills the derived one only when decoding.
- **How to tell** — Set a timestamp on a decoded frame and read every
  timestamp accessor: they agree.
- **Count** — 1 fix. **Sources** — `828f254`.
- **Rule:** [avutil.md](avutil.md) §4.3; [tests.md](tests.md) 1.8.

## Audio conversion

### Converter output shared between calls

- **Situation** — A resampler converts repeatedly and returns a container of
  samples each time.
- **What went wrong** — Clipped, distorted audio: the value returned by one
  call was overwritten by the next while the program still read it. Removing
  the shared value was itself reverted once before it held.
- **Why it is hard** — Reusing the output buffer is the obvious optimisation
  and passes any test that consumes each result before the next call.
- **How to tell** — Convert twice with different input, then compare the
  first result with a copy taken before the second call.
- **Count** — 3 changes, 2 reverts. **Sources** — `3a646ff`, `11b9eae`,
  `20929df`; #13.
- **Rule:** [binding-contract.md](binding-contract.md) §4 A5 and §8 S3; [swresample.md](swresample.md) §2.2; [tests.md](tests.md) 6.4.

### NaN samples

- **Situation** — Float samples cross the boundary in either direction and
  one of them is not a number.
- **What went wrong** — A NaN entered a filter or an encoder and poisoned
  everything after it.
- **Why it is hard** — NaN is a valid float on both sides and no conversion
  step fails on it.
- **How to tell** — Convert a buffer containing NaN in each direction: the
  output holds zero at that position.
- **Count** — 1 fix. **Sources** — `da05c0d`.
- **Rule:** [swresample.md](swresample.md) §4.4; [tests.md](tests.md) 6.5.

### Sample storage on each side

- **Situation** — Planar samples are held by FFmpeg as one allocation with a
  pointer per channel. OCaml float arrays are read through the runtime's
  float accessors.
- **What went wrong** — Planes were allocated and freed one by one against an
  allocator that hands out one block. Float arrays were copied as raw
  doubles.
- **Why it is hard** — Both shortcuts work on common platforms and channel
  counts.
- **How to tell** — Convert to and from every container kind with one, two
  and six channels under an address sanitiser, and compare sample values.
  [tests.md](tests.md) 6.1 gives the cases.
- **Count** — 2 fixes. **Sources** — `f228110`, `fa05f89`; #12.
- **Rule:** [binding-contract.md](binding-contract.md) §2.2 L1; [swresample.md](swresample.md) §3.2; [tests.md](tests.md) 6.1.

### Channel layouts are structures with a lifetime

- **Situation** — A channel layout is copied out of a codec, a frame or a
  filter sink, or filled by an FFmpeg call into a temporary.
- **What went wrong** — Silent restarts in production traced to layout
  copies. A handle was created around an uninitialised layout and then
  filled. A default layout was read when parsing had failed and skipped when
  it had succeeded.
- **Why it is hard** — The structure may own heap memory for custom layouts.
  It needs initialising before use and releasing after, including in
  temporaries, and a bitwise copy is wrong.
- **How to tell** — Round-trip a standard and a custom layout through every
  accessor that returns one, under a leak detector and an address sanitiser.
- **Count** — 3 fixes. **Sources** — `6d9b6f2`, `979d282`,
  liq:`58e3fb3de`; #64, #73, liquidsoap#4064, liquidsoap#4065.
- **Rule:** [avutil.md](avutil.md) §2.2; [tests.md](tests.md) 1.13, 6.9.

## Video conversion

### Plane count comes from the pixel format

- **Situation** — The number of planes of an image is needed to expose or
  allocate them.
- **What went wrong** — A grey image converted from YUV reported two planes.
  Counting non-null data pointers and counting non-zero line sizes were both
  tried and both wrong.
- **Why it is hard** — FFmpeg leaves stale pointers and unset line sizes in
  unused slots. Paletted formats have a second buffer that is not a plane.
- **How to tell** — For a set of formats covering grey, planar YUV,
  semi-planar, packed RGB and paletted, the reported count equals
  `av_pix_fmt_count_planes`.
- **Count** — 2 fixes. **Sources** — `8c0f717`, `f204643`; #15.
- **Rule:** [swscale.md](swscale.md) §4.5; [avutil.md](avutil.md) §4.11, §4.13; [tests.md](tests.md) 7.1.

### Plane sizes follow chroma subsampling

- **Situation** — Output planes are allocated, or existing planes exposed, as
  byte arrays.
- **What went wrong** — Every plane was sized as line size times image
  height. Chroma planes came out at twice their length; memory use of video
  pipelines was visibly higher; a copy ran past its destination. A first
  repair computed the sizes by hand in one contiguous block and was reverted
  the next day in favour of FFmpeg's own computation.
- **Why it is hard** — The over-sized plane works. Exposed, it extends past
  the buffer.
- **How to tell** — For 4:2:0 and 4:2:2 output, each plane's length equals
  what `av_image_fill_plane_sizes` reports, plus the stated padding.
- **Count** — 4 fixes, 1 revert. **Sources** — `c5b4e37`, `d53ad21`,
  `f204643`, `2bca90a`, liq:`58e3fb3de`; #66, #67, #68.
- **Rule:** [binding-contract.md](binding-contract.md) §8 S2; [avutil.md](avutil.md) §8.2; [swscale.md](swscale.md) §4.5; [tests.md](tests.md) 1.10, 7.1.

## Filters

### Parsing a description around existing endpoints

- **Situation** — A textual graph description is parsed into a graph that
  already has sources and sinks. The program names which pads the
  description's open ends connect to.
- **What went wrong** — The first design created the endpoint filters itself
  from unattached descriptions and returned attached ones; it did not work
  for non-trivial graphs and the signature was changed. The list of endpoints
  handed to FFmpeg was built by walking to the end and writing through the
  null link.
- **Why it is hard** — FFmpeg's naming is inverted: its "inputs" are pads the
  description's outputs link to. It takes ownership of the endpoint lists and
  frees filters on failure.
- **How to tell** — Parse a description with two labelled inputs and two
  labelled outputs between existing sources and sinks, run frames through,
  and check each sink receives its own stream.
- **Count** — 2 changes. **Sources** — `094ad19`, `480f6d0`; #84.
- **Rule:** [avfilter.md](avfilter.md) §4.10 and §2.1; [tests.md](tests.md) 4.3, 4.4.

## Options

### Option types newer than the bindings

- **Situation** — Listing the options of every filter at load time meets an
  option type the binding has no case for, from a newer FFmpeg.
- **What went wrong** — Every program failed at start with "Invalid option
  type", including a version query.
- **Why it is hard** — FFmpeg adds option types, and a flag bit that turns
  any type into an array, between releases. The listing runs over all
  registered classes.
- **How to tell** — List the options of every codec, format and filter class
  of the newest supported FFmpeg: the call returns, and unknown types are
  omitted.
- **Count** — 3 fixes. **Sources** — `e7fe168`, `1e5cbc3`, `0eeb255`;
  liquidsoap#2392.
- **Rule:** [binding-contract.md](binding-contract.md) §1.3 I1; [avutil.md](avutil.md) §4.15; [tests.md](tests.md) 1.5.

### Telling the caller which options were not used

- **Situation** — An operation takes an option table, adds options it derives
  from typed arguments, hands the lot to FFmpeg, and reports back the keys
  FFmpeg did not consume.
- **What went wrong** — The report was computed against the wrong table, so
  the caller saw derived keys it never set, or lost keys of its own.
- **Why it is hard** — FFmpeg empties one dictionary. The caller's table and
  the derived entries must stay distinguishable through that round trip.
- **How to tell** — Pass a table with one valid and one unknown key to every
  operation taking options: afterwards the table holds the unknown key only.
- **Count** — 3 fixes. **Sources** — `9f87180`, `3649271`, liq:`f69ea58b3`.
- **Rule:** [avutil.md](avutil.md) §9.1; [tests.md](tests.md) 1.7.

### One dictionary, several consumers

- **Situation** — Options given when opening an output may belong to the
  generic muxer context, to the muxer's private options, or to the I/O
  protocol.
- **What went wrong** — Options reached only one of them; the rest were
  reported unused or silently ignored.
- **Why it is hard** — Each consumer removes what it recognises, so order
  matters, and a key valid for a later consumer looks unused until then.
- **How to tell** — Open an output with one option of each kind: all three
  take effect and none is reported unused.
- **Count** — 1 change. **Sources** — `b449d6d`.
- **Rule:** [avformat.md](avformat.md) §4.4 (`open_output`) and §9; [tests.md](tests.md) 3.17.

### Numeric bounds of options

- **Situation** — An option's minimum and maximum are stored by FFmpeg as
  doubles, whatever the option's type.
- **What went wrong** — Integer options reported wrong bounds.
- **Why it is hard** — The extremes of 64-bit integers are not representable
  as doubles, and OCaml's native integer is narrower still.
- **How to tell** — Read the bounds of an option spanning the full integer
  range and of one spanning the full 64-bit range.
- **Count** — 1 fix. **Sources** — `26b9f26`.
- **Rule:** [avutil.md](avutil.md) §3.2; [tests.md](tests.md) 1.6.

### Setters whose result is dropped

- **Situation** — A context is configured through a series of option
  assignments, or a dictionary is filled entry by entry.
- **What went wrong** — A rejected assignment went unnoticed and the context
  ran with a default. Twelve copies of the dictionary loop checked the result;
  a thirteenth did not.
- **Why it is hard** — These calls almost never fail.
- **How to tell** — Give each configurable object one out-of-range value per
  parameter: creation fails with an error.
- **Count** — 3 fixes. **Sources** — `2989235`, `ca20131`,
  liq:`f69ea58b3`.
- **Rule:** [binding-contract.md](binding-contract.md) §4 A7 and §9 O2; [tests.md](tests.md) 2.12, 7.6.

## Enumerations and generated tables

### Tables generated from other headers than the ones compiled against

- **Situation** — The enum tables are generated by reading FFmpeg headers
  found by a search of the generator's own. The C compiler finds its headers
  through the compile flags.
- **What went wrong** — A generated pixel-format table named members the
  compiled header did not declare, and the build failed in generated code.
  When no header was found the build went on with an empty table.
- **Why it is hard** — Two FFmpeg installations on one machine are common
  (system and local, Homebrew versions). pkg-config prints no include path
  for the default location.
- **How to tell** — Install two FFmpeg versions, point pkg-config at one:
  the tables match that one. Remove the headers: the build fails with a
  message naming the header.
- **Count** — 5 fixes, 1 report. **Sources** — `d9b80c4`, `0b50a84`,
  `0dacc4c`, `b964722`, `c2465e4`; #77.
- **Rule:** [build.md](build.md) §3.2 G3 and G7; [tests.md](tests.md) 9.1, 9.2.

### Enum members behind preprocessor conditionals

- **Situation** — FFmpeg headers guard some enum members by version or
  configuration macros.
- **What went wrong** — A table listed a member the build did not have. The
  generator was changed to read preprocessed headers; it then closed the
  preprocessor's output before reading all of it and failed, and was given a
  fallback to the raw header for when the preprocessor cannot run.
- **Why it is hard** — Preprocessing needs the target compiler and its flags
  at generation time, and its output format differs between compilers.
- **How to tell** — Generate against two FFmpeg versions that differ by a
  guarded member; each table matches its headers. A failing preprocessor
  fails the build.
- **Count** — 4 fixes. **Sources** — `d0b8160`, `3d12307`, `c4f3dba`,
  `627500f`.
- **Rule:** [build.md](build.md) §3.2 G4, G5 and G7.

### Values missing from a table

- **Situation** — FFmpeg returns an enum value the generated table of that
  kind does not contain: "none", a wrapped-frame codec, a member declared
  outside the scanned range, a format newer than the list.
- **What went wrong** — Probing a file or listing a codec's capabilities
  raised, although nothing was wrong with the input.
- **Why it is hard** — Codec identifiers are one C enum split into per-kind
  OCaml types by position. Markers and aliases share values. FFmpeg uses
  "none" as an ordinary answer.
- **How to tell** — Convert every value of each C enum, as declared by the
  installed header, to OCaml and back. Probe a stream with an unknown codec.
- **Count** — 4 fixes. **Sources** — `4d5f530`, `b53d9ff`, `9d8c0fd`,
  `9ebabeb`.
- **Rule:** [build.md](build.md) §3.3; [binding-contract.md](binding-contract.md) §3 E3, E4 and E7; [tests.md](tests.md) 9.3.

### Each capability list has its own terminator

- **Situation** — A codec's supported sample formats, layouts, rates, frame
  rates, colour spaces and ranges are C arrays ended by a sentinel.
- **What went wrong** — A list was walked to the wrong sentinel: channel
  layouts to −1 where the array ends with 0; colour spaces to −1 on an
  unsigned enum ended by "unspecified", which never matches.
- **Why it is hard** — The sentinel differs per list and is stated only in
  FFmpeg's documentation. An over-run usually stops on a stray zero.
- **How to tell** — For a codec that declares each list, the binding returns
  exactly the entries `avcodec_get_supported_config` counts.
- **Count** — 2 fixes. **Sources** — `5b50475`, liq:`afa508cf9`.
- **Rule:** [avcodec.md](avcodec.md) §4.3; [tests.md](tests.md) 2.3.

### Hand-written flag sets and tables

- **Situation** — A small flag set or a mapping between two enumerations is
  written by hand next to a generated table.
- **What went wrong** — A flag list was combined with "and" from zero, so
  filter commands always carried no flag, and its one flag had the wrong bit.
  A sample-format table was two entries short of the generated one and
  returned an out-of-range kind for the rest.
- **Why it is hard** — Nothing ties the hand-written table to the header. A
  flag that is never set looks like a flag that has no effect.
- **How to tell** — Check each hand-written constant against the header at
  build time. For each flag, observe an effect that only that flag causes.
- **Count** — 2 fixes. **Sources** — liq:`58e3fb3de`.
- **Rule:** [binding-contract.md](binding-contract.md) §3 E1 and E6; [tests.md](tests.md) 9.4, 4.8.

## Build, detection and cross-compilation

### Build-time programs run on the build machine

- **Situation** — The enum generator and the detector are programs the build
  runs. Under `dune -x windows`, or on an architecture with no native
  compiler, the machine that builds is not the one the libraries are for.
- **What went wrong** — The generator was built with a compiler that did not
  exist on bytecode-only architectures. It read the build machine's headers
  for a cross build. Passing the C compiler down was added, reverted and
  added again.
- **Why it is hard** — The program must be native to the build machine and
  read the target's headers with the target's preprocessor. It learns about
  the target only from its arguments.
- **How to tell** — Cross-build for Windows from a machine whose own FFmpeg
  is a different version: the tables match the target's headers. Build on a
  bytecode-only switch.
- **Count** — 4 fixes, 1 revert. **Sources** — `fdff66f`, `08543b3`,
  `c2465e4`, `422de27`; #21, #27.
- **Rule:** [cross-compilation.md](cross-compilation.md) §1.2 X1–X3 and X15, §2.3 X8; [tests.md](tests.md) 9.7.

### Link flags from pkg-config on Windows

- **Situation** — pkg-config for a static FFmpeg returns linker options
  meant for a C link line.
- **What went wrong** — The OCaml link of the device library failed on
  Windows.
- **Why it is hard** — The flags are valid for the C compiler and rejected,
  or misread, when passed through the OCaml toolchain.
- **How to tell** — Cross-build every library against a static FFmpeg and
  link an executable.
- **Count** — 1 fix. **Sources** — `39adbd5`.
- **Rule:** [build.md](build.md) §2.4 D7; [cross-compilation.md](cross-compilation.md) X10.

### A library that silently drops out of the build

- **Situation** — A binding library is built only when detection succeeds.
  Detection results are cached. A library can also be declared optional to
  the build system.
- **What went wrong** — One library was marked optional on top of its
  detection gate: an unresolved dependency would have removed its stubs from
  the build and left it green. Detection did not rerun after FFmpeg was
  installed or upgraded. A typo once discarded the compile flags.
- **Why it is hard** — "Not available" and "failed" look the same, and the
  build system does not track the environment detection depends on.
- **How to tell** — The build reports which libraries are built. With FFmpeg
  present and one library broken, the build fails. Installing FFmpeg and
  rebuilding enables the libraries.
- **Count** — 3 fixes. **Sources** — `9f41de7`, liq:`2465afdab`,
  liq:`a3e5f35a5`.
- **Rule:** [build.md](build.md) §2.1 D5, §2.5 D9–D11, §5 T2; [tests.md](tests.md) 9.5, 9.6.

### Warning flags

- **Situation** — C warnings are enabled per library. Some are errors.
  Version conditionals leave a variable unused on one side.
- **What went wrong** — Only one of seven libraries was compiled with
  warnings; enabling them everywhere exposed two live bugs. Later, "unused
  variable" as an error broke the build against every FFmpeg older than the
  one developed on.
- **Why it is hard** — A warning promoted to an error is tested only on the
  versions someone builds.
- **How to tell** — Build with the full warning set against the oldest and
  the newest supported FFmpeg.
- **Count** — 2 fixes. **Sources** — liq:`afa508cf9`, liq:`3a3d1d66c`.
- **Rule:** [build.md](build.md) §2.7; [tests.md](tests.md) §8.1.

### C library functions the Windows toolchain lacks

- **Situation** — The stubs duplicate OCaml strings into C strings.
- **What went wrong** — The cross build failed on `strndup`; the Windows
  packages carried a patch for it until the tree switched to FFmpeg's own.
- **Why it is hard** — The function exists on every platform a developer
  builds on.
- **How to tell** — The stubs include only ISO C, the OCaml runtime, FFmpeg
  and POSIX threads. Cross-build to verify.
- **Count** — 2 fixes. **Sources** — `4046247`, `f5371d7`.
- **Rule:** [cross-compilation.md](cross-compilation.md) §2.5 X12 and X13; [tests.md](tests.md) 9.8.

### A library whose only job is registration

- **Situation** — The device library registers input and output devices with
  libavformat. A program opens a device by name through the container
  library and never names a device function.
- **What went wrong** — The linker dropped the library and the devices did
  not exist. Forcing the link and adding an explicit initialisation call were
  both needed.
- **Why it is hard** — Nothing fails at build time. The device list is
  simply shorter.
- **How to tell** — A program that links the device library and only lists
  input formats sees the device formats.
- **Count** — 2 fixes. **Sources** — `b38419e`, `8191c63`.
- **Rule:** [avdevice.md](avdevice.md) §1 and §4.1; [tests.md](tests.md) 5.1.

## FFmpeg version drift

### The channel layout API was replaced

- **Situation** — FFmpeg 5.1 introduced a structure for channel layouts and
  FFmpeg 7 removed the integer masks.
- **What went wrong** — A development FFmpeg broke program start. For a year
  each release of the bindings built against one side only, and users on the
  other side got compile errors in the middle of an install.
- **Why it is hard** — The type appears in frames, codec contexts,
  parameters, filters, the resampler and options. Supporting both sides
  doubles each of those paths.
- **How to tell** — The lower bound is at or above 5.1 and no path uses the
  masks.
- **Count** — 3 changes, 3 reports. **Sources** — `5ea1d0a`, `5406339`,
  `ad53aa5`; #59, #60, #62, liquidsoap#2392.
- **Rule:** [compatibility.md](compatibility.md) §1.1 V1 and §2: the floor is above the replacement and no path uses the masks.

### Symbols removed, renamed or moved at each major release

- **Situation** — Each FFmpeg major release removes what the previous one
  deprecated: registration calls, packet initialisation, child-class
  iteration, stream side data, per-codec capability arrays, pad counts,
  profile constants. Headers move.
- **What went wrong** — The build broke on FFmpeg 4.3, 5.0, 7.0 and 8.0, each
  time after users had upgraded.
- **Why it is hard** — Deprecation warnings appear one major release ahead
  and are not errors. Replacement and removal are in different releases, so
  the conditional has a specific version on each side.
- **How to tell** — Build with deprecation warnings as errors against the
  newest release. Every version conditional names the library version at
  which the replacement appeared.
- **Count** — 9 fixes, 4 reports. **Sources** — `e99156f`, `97c1fc4`,
  `7d5b396`, `1e5cbc3`, `8703012`, `dedf071`, `ce9b97f`, `8e648df`,
  `27bb428`; #43, #54, #77, #78.
- **Rule:** [compatibility.md](compatibility.md) §2 V5 and V7.

### An enum member is not a macro

- **Situation** — A feature is detected with a preprocessor test on a name
  that FFmpeg declares as an enum member, or on a name that does not exist.
- **What went wrong** — Array-typed options were compiled out on every
  version, and one option flag is still never reported.
- **Why it is hard** — The test compiles and is false, which is the same as
  an old FFmpeg.
- **How to tell** — Every feature test compares library versions, or tests a
  name checked to be a macro in the headers.
- **Count** — 2 fixes. **Sources** — `0eeb255`, `ef67e71`.
- **Rule:** [compatibility.md](compatibility.md) §2 V6.

### Declared minimum versions the code does not meet

- **Situation** — Detection accepts an FFmpeg version. The stubs use an API
  that version lacks.
- **What went wrong** — Users on older distributions passed detection and
  got compile errors from the stubs. The declared floor accepted versions the
  code could not compile against; the real floor was found by bisecting
  FFmpeg.
- **Why it is hard** — The floor is seven library versions, none equal to a
  release number, and only the versions in CI are ever built.
- **How to tell** — Build and run the suite against the oldest release that
  passes detection.
- **Count** — 4 changes, 4 reports. **Sources** — `93655ca`, `ae7573f`,
  liq:`58e3fb3de`, liq:`3a3d1d66c`; #56, #63, #69, #70.
- **Rule:** [compatibility.md](compatibility.md) §1.1 V2 and V3; [build.md](build.md) §2.3 D6.

## Tests

### Tests that cannot fail

- **Situation** — A test program prints its verdict, or checks only what it
  finds.
- **What went wrong** — One test printed "FAILED" five times and exited 0.
  Two were run without the arguments their bodies iterate over. One exited 0
  when its input had no subtitle stream. The suite had two real assertions.
- **Why it is hard** — A green run is the expected outcome, so nobody looks
  at it.
- **How to tell** — Each test fails when it made no check. Break each
  behaviour on purpose once and see the test go red.
- **Count** — 1 fix, 3 defects. **Sources** — liq:`2ef0317f5`; #114.
- **Rule:** [tests.md](tests.md) §0 and §10 H1, H2, H4.

### A regression fixture that does not trigger the condition

- **Situation** — A regression depends on a property of the media: decoder
  delay, streams appearing late, a demuxer without a header.
- **What went wrong** — The seek and drain tests cannot fail on a file
  without B-frames; the late-stream test needed a file built for it.
- **Why it is hard** — Any convenient test file passes.
- **How to tell** — Each regression test is run once against the unfixed
  behaviour and seen to fail, and its fixture's defining property is
  recorded.
- **Count** — 3 tests. **Sources** — liq:`797883677`, liq:`9cac50e42`,
  liq:`d9d649c45`.
- **Rule:** [tests.md](tests.md) §11 and §10 H4.

### Steps that depend on the FFmpeg build or the machine

- **Situation** — A test needs a particular encoder, a hardware device, a
  resampling engine, or a network peer.
- **What went wrong** — Flaky runs led to tests being switched to another
  codec, made optional, disabled on one platform, or disabled on CI.
- **Why it is hard** — A skip and a pass print the same result.
- **How to tell** — The suite states what the FFmpeg build must contain, and
  reports skipped steps as skipped.
- **Count** — 4 changes. **Sources** — `67656de`, `7ac2d7c`, `63b6de5`,
  `0c49fde`.
- **Rule:** [tests.md](tests.md) §8.2 and §10 H3.

## Reconciliation

Every entry above ends with a **Rule** line naming the rule of the normative
specification that answers it. No entry is unanswered and none contradicts
the specification.

Where the specification no longer has the mechanism an entry was about, the
entry says what handles the situation now.
