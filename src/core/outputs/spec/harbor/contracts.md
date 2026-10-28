# Contracts relied on

What `output.harbor` requires from the rest of Liquidsoap. These components
are not specified here; each section states what the operator needs of them,
and a component that does not provide it is where the fix belongs.

## 1. Harbor server

A per-port HTTP server shared by `input.harbor`, `harbor.http.*` and this
operator.

- **Ports.** A port is opened by the first registration on it, bound on every
  address of `settings.harbor.bind_addrs`, with the listen backlog
  `settings.harbor.max_connections`; accepted sockets get `TCP_NODELAY`. A
  port records the transport it was opened with; a registration with a
  transport of another name fails. A port is closed when its last route and
  last source are removed. A registration that follows a removal on the same
  port MUST succeed, whether or not the port was closed in between: an
  immediate restart of an output must not fail to bind.
- **Literal routes.** The server MUST offer a route that matches a request
  path, query string removed, by byte equality
  ([connection.md §1](connection.md#1-routing)).
- **Registrations are owned.** Registering returns a handle, and removal
  takes that handle: it removes that registration only. Registrations are
  tried in the order they were made and the first that matches serves, which
  is the order `harbor.http.register` documents. A literal registration is the
  exception: it goes ahead of every earlier registration with the same verb
  and path, so the most recent output on a mount serves it, and removing it
  puts the earlier one back in its place.
- **Dispatch.** The server reads the request head within
  `settings.harbor.timeout`, parses the request line and headers, strips the
  query string, and calls the matching route on work allowed to block. It
  passes the version (`"1.0"`/`"1.1"`), lowercased header names, the
  query-string arguments, the socket and the path. Unmatched requests get the
  server's own 404 or admin handling.
- **Versions.** A request whose version is neither `HTTP/1.0` nor
  `HTTP/1.1` MUST be answered `505 HTTP Version Not Supported` by the server
  and MUST NOT reach a route. Nothing a client sends, and nothing a route
  raises, may end the process.
- **Replies.** A route ends with a reply the server writes and then closes the
  socket, or by taking the socket over, after which the server neither writes
  nor closes. A reply always ends.
- **Credentials check**: [connection.md §3](connection.md#3-authentication).
- **Socket operations** through the port's transport: read, write, close, and
  the underlying descriptor. Every transport's write MUST be non-blocking once
  the socket is: it writes what fits, returns the count accepted, and reports
  "would block" without waiting. An encrypted transport MUST buffer what it
  has encrypted and not yet written, and accept no new bytes until that is
  drained, rather than block.

## 2. Format and MIME derivation

Shared with `output.icecast`. From the encoding format argument:

- the format used for encoding is the given one, except an `%ffmpeg` mp3
  format, which gets `id3v2_version=0` and `write_xing=0` unless set
  ([FFmpeg io.md §12](../../../optionals/ffmpeg/spec/io.md#12-other-outputs-special-casing-ffmpeg)):
  an ID3v2 tag or a Xing frame in the middle of a live stream is garbage to
  a joiner;
- the encoder factory for that format, or the error of
  [operator.md §2](operator.md#2-validation-at-construction);
- the MIME type: the `format` parameter when non-empty; otherwise by encoder:
  `%mp3` and `%shine` → `audio/mpeg`; `%fdkaac` → `audio/aac`; `%flac` →
  `audio/flac`; `%wav` → `audio/wav`; `%avi` → `video/avi`; `%ogg` →
  `application/ogg`; `%ffmpeg` → its `mime_type`, else the MIME of its
  container name, else none; `%external` and `%ndi` → none.

## 3. Output base

- The output's clock and its synchronization mode, read at start
  ([operator.md §7](operator.md#7-lifecycle)); the output's self-sync
  delegated to its source.
- Parameters `start`, `fallible`, `register_telnet`, `id`; methods and
  callbacks of every output; the infallibility check; telnet commands.
- The start/stop state machine: start when the source becomes ready (if
  `start` is true), stop when the source ends or fails (fallible outputs),
  `start` and `stop` methods. The operator's start and stop are called on
  transitions and serialised ([operator.md §7](operator.md#7-lifecycle)).
- Per frame: the frame is cut at the positions of its metadata. For each
  non-empty piece: if a metadata lies in it, the first one is passed to
  _metadata_; then _encode_ the piece, then _send_ the result. Metadata comes
  first, so that the bytes of the piece are in the scope of its metadata.

## 4. Encoder

An encoder instance is created by `factory(name, initial metadata)` and
offers:

- `encode(frame piece) → bytes`;
- `encode_metadata(metadata)`: in-stream metadata, effective in later output;
- `header() → bytes`: what a client joining now, at the start of the next
  output, needs first to decode; after a new logical stream began (an Ogg link
  after a metadata change), the new stream's header. It MUST be complete for
  every format the encoder produces, the container's identification headers
  included;
- `restart_offset() → offset?`: in the bytes the latest `encode` or `stop`
  returned, where a logical stream with its own header pages starts. It MAY
  be reported for the first output of an instance;
- `stop() → bytes`: final bytes.

Every instance serialises its own calls. The FFmpeg encoder's side of this
contract is in [FFmpeg encoder.md](../../../optionals/ffmpeg/spec/encoder.md).

## 5. Scheduler

- Work in classes: **fast I/O** (non-blocking, short, never parks a domain)
  and work **allowed to block** (user code, encoder teardown). The operator
  states which class each piece of work is ([concurrency.md §1](concurrency.md#1-work));
  the scheduler decides domains, thread counts and placement.
- A task waits on readable descriptors, a delay, or an explicit wake-up; a
  wake-up of a task already due is coalesced and never blocks the waker.
- Moving the current computation to the class allowed to block.
- The scheduler's parallelism, from which an implementation MAY derive the
  number of delivery groups ([operator.md §6](operator.md#6-settings)).

## 6. Lifecycle hooks

- A process-wide "before core shutdown" hook, run before the core stops,
  from which the operator marks listeners closed.
- The server-start hook through which route registration is deferred.
- Callback registration with `synchronous` flags, as for every operator.
