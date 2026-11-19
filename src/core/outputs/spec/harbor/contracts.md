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
  is the order `harbor.http.register` documents. A registration with the verb
  and path of an existing one on the port fails, unless it is a literal one
  that overrides: it then goes ahead of every earlier registration with the
  same verb and path, so the most recent output on a mount serves it, and
  removing it puts the earlier one back in its place.
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
  drained, rather than block. A transport that reports "would block" MAY
  already hold part of the offered range; the next write MUST offer the same
  bytes from the same start, possibly followed by more, as a flush resuming at
  its first unaccepted byte does.

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

The output base of [output.md](../output.md): its parameters, methods and
callbacks (§1), its states and serialised transitions (§2), the per-frame
loop that hands the operator's _metadata_, _encode_ and _send_ each piece,
metadata first (§3), resets (§4) and pacing (§5). What harbor needs in
addition:

- The output's clock and its synchronization mode, read at start
  ([operator.md §7](operator.md#7-lifecycle)).
- A fallible output stops when its source ends or fails, unless
  `keep_listeners` holds it ([operator.md §7](operator.md#7-lifecycle)).
- While the source is unavailable and the output is kept started
  (`keep_listeners`), the base's unavailable-source hook skips encode and send
  for the cycle and does not call _metadata_. When the source is available
  again the base resumes with the next piece, and the encoder follows the
  source as for any switch
  ([encoder-copy.md §12](../../../optionals/ffmpeg/spec/encoder-copy.md#12-source-switching-and-per-source-state)).
  A source that ends for good is treated the same way: the output is held
  until `stop`, `shutdown()` or process shutdown, since the caller owns the
  end in this mode.

## 4. Encoder

The stream face of [encoder.md §3](../../../encoder/spec/encoder.md#3-the-stream-face),
created by its factory with the output's name and the initial metadata, with
an instance per run that serialises its own calls
([encoder.md §2](../../../encoder/spec/encoder.md#2-instances)). Harbor
uses `encode_into` when offered, so that it keeps the bytes without an
allocation, and relies on `header()` and `restart_offset()` for joiners. The
FFmpeg encoder's side of this contract is in
[FFmpeg encoder.md](../../../optionals/ffmpeg/spec/encoder.md).

## 5. Scheduler

The project's work classes, wake-ups and placement are
[concurrency.md §1](../../../../spec/concurrency.md#1-work); the operator
states which class each piece of its work is
([concurrency.md §1](concurrency.md#1-work)). What harbor needs in addition:

- A task waits on readable descriptors, a delay, or an explicit wake-up.
- Moving the current computation to the class allowed to block.
- The scheduler's parallelism, from which an implementation MAY derive the
  number of delivery groups ([operator.md §6](operator.md#6-settings)).

## 6. Lifecycle hooks

- A process-wide "before core shutdown" hook, run before the core stops,
  from which the operator marks listeners closed.
- The server-start hook through which route registration is deferred.
- Callback registration with `synchronous` flags, as for every source
  ([source-contract.md §10](../../../source/spec/source-contract.md#10-callbacks)).
