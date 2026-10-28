# Contracts relied on

What `output.harbor` takes from the rest of Liquidsoap, stated as far as the
operator depends on it. These components are not specified here.

## 1. Harbor server

A per-port HTTP server shared by `input.harbor`, `harbor.http.*` and this
operator.

- **Ports.** A port is opened by the first registration on it and bound on
  every address of `settings.harbor.bind_addrs` (default `0.0.0.0`), with the
  listen backlog `settings.harbor.max_connections` (default 128). Accepted
  sockets get `TCP_NODELAY`. A port records the transport it was opened with;
  a registration with a transport of another name fails. A port is closed
  when its last handler and last source are removed.
- **Handler registration** for a verb and a pattern
  ([connection.md §1](connection.md#1-routing)): appended to the port's list,
  no duplicate check. It runs immediately when the server has started, and is
  queued until then otherwise. Removal takes the same path and removes every
  entry with the same verb and pattern text.
- **Dispatch.** The server reads the request head with
  `settings.harbor.timeout`, parses the request line and headers, strips the
  query string, tries the port's handlers in order, and calls the first
  match. Before calling it, it moves the request task to the `Threaded` queue.
  The handler gets the version (`"1.0"`/`"1.1"`), lowercased header names,
  the query (named regex groups first, then the query-string arguments), and
  the socket. Unmatched requests get the server's own 404 or admin handling.
  A handler that raises anything other than a reply gets a 500.
- **Replies.** A handler ends by raising one of:
  - _close_ with a producer of strings: the server writes each string the
    producer returns, in order, until it returns the empty string, then closes
    the socket. A producer that never returns the empty string keeps the
    server writing until the write fails;
  - _custom_: the handler owns the socket; the server neither writes nor
    closes.
    The helpers give a _close_ reply that sends one fixed string then stops,
    and the standard error page ([connection.md §3](connection.md#3-authentication)).
- **Credentials check**, given the request headers, the query and an
  authentication function: [connection.md §3](connection.md#3-authentication).
  It answers 401 by raising the _close_ reply itself.
- **Socket operations** through the port's transport: read, write, close, and
  the underlying file descriptor. On the plain TCP transport, write is a
  plain `write` system call. SSL writes are non-blocking and report
  would-block as `EAGAIN`; TLS writes send the whole range before returning,
  within `settings.harbor.timeout`.

## 2. Format and MIME derivation

Shared with `output.icecast`. From the encoding format argument:

- the format used for encoding is the given one, except an `%ffmpeg` mp3
  format, which gets `id3v2_version=0` and `write_xing=0` unless set
  ([FFmpeg io.md §12](../../../optionals/ffmpeg/spec/io.md#12-other-outputs-special-casing-ffmpeg));
- the encoder factory for that format, or the error of
  [operator.md §2](operator.md#2-validation-at-construction);
- the MIME type: the `format` parameter when non-empty; otherwise by encoder:
  `%mp3` and `%shine` → `audio/mpeg`; `%fdkaac` → `audio/aac`; `%flac` →
  `audio/flac`; `%wav` → `audio/wav`; `%avi` → `video/avi`; `%ogg` →
  `application/ogg`; `%ffmpeg` → its `mime_type`, else the MIME of its
  container name, else none; `%external` and `%ndi` → none.

## 3. Output base

- Parameters `start`, `fallible`, `register_telnet`, `id`; methods and
  callbacks of every output; the infallibility check; telnet commands.
- The start/stop state machine: start when the source becomes ready (if
  `start` is true), stop to idle when the source ends or fails (fallible
  outputs), `start` and `stop` methods. The operator's start and stop are
  called on transitions ([operator.md §7](operator.md#7-lifecycle)).
- Per frame: the frame is cut at the positions of its metadata. For each
  non-empty piece `[a, b)`: if a metadata lies in `[a, b)`, the first one is
  exported and passed to _metadata_; then _encode_ the piece, then _send_ the
  result. The operator keeps the default metadata-first order.
- A hook run at the start of every frame, used for the shutdown sweep.

## 4. Encoder

An encoder instance is created by `factory(name, initial metadata)` and
offers:

- `encode(frame piece) → bytes`;
- `encode_metadata(metadata)`: in-stream metadata, effective in later output;
- `header() → bytes`: what a client joining now needs first; after an Ogg
  restart, the new link's header;
- `restart_offset() → offset?`: in the bytes the latest `encode` or `stop`
  returned, where a new stream with its own header pages starts (an Ogg link
  after a metadata change); none when they start none;
- `stop() → bytes`: final bytes.

Every instance from the factory serialises its own calls with a lock. The
FFmpeg encoder's side of this contract is in
[FFmpeg encoder.md](../../../optionals/ffmpeg/spec/encoder.md).

## 5. Scheduler

- Tasks with a priority (`Blocking`, `Threaded`, `Non_blocking`) waiting on
  readable descriptors and delays; a handler returns the tasks to run next.
  `Blocking` tasks keep their domain busy until done; `Threaded` ones may
  block on I/O.
- `reschedule(priority)`: move the current computation to that queue.
- The maximum domain count, the default number of writers
  ([operator.md §6](operator.md#6-settings)).

## 6. Lifecycle hooks

- A process-wide "before core shutdown" hook, used to set the shutdown flag.
- The server-start hook through which handler registration is deferred.
- Callback registration with `synchronous` flags, as for every operator.
