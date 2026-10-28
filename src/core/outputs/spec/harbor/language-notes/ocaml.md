# OCaml notes

Part B: how the current implementation maps Part A onto OCaml and
Liquidsoap's internal APIs. Advice for an OCaml rebuild, not a design to copy.
Where these notes and Part A differ, Part A governs: the writers below run as
`Blocking` tasks where Part A asks for fast I/O tasks, and the harbor's
handler removal goes by pattern where Part A asks for owned registrations.

## Structure

- One file, `src/core/outputs/harbor_output.ml`, defines a virtual class
  `['a] base` parameterised by the per-listener mode state, and two
  subclasses `shared_output` (`'a = shared_state`) and `dedicated_output`
  (`'a = dedicated_state`). The mode contract of [modes.md](../modes.md) is the
  set of virtual methods: `create_listener`, `connect_listener`,
  `has_pending`, `flush_listener`, `staging_size`, `stop_listener_encoder`,
  plus `encode`, `encode_metadata`, `send`, `start`, `stop` from the output
  base.
- `base` inherits `[Strings.t] Output.encoded`, which supplies the per-frame
  cutting and metadata-first ordering of
  [contracts.md §3](../contracts.md#3-output-base).
- MIME and encoder resolution come from
  `Icecast_utils.Icecast_v`, instantiated with a module whose `content` is the
  MIME string and whose `info` is `unit`: the harbor output uses only
  `encoder_data.factory` and `encoder_data.format`.
- The operator is registered with `Lang.add_operator ~base:Modules.output
"harbor"`. Listener callbacks are `Lang_source` callback descriptors, so
  `on_connect` / `on_disconnect` get the standard `synchronous` handling. The
  `listeners` method scheme is `([], …)`.

## Concurrency primitives

- Listener `closed` flag, `bytes_sent`, shard `members`, shard `wake`,
  `next_shard`, `next_client_id`, the shared mode's `stream_start`,
  `chunk_starts` and the output's last metadata are `Atomic.t`. Member lists
  are replaced with compare-and-set retry loops.
- Fields written by one actor at a time (`metadata_position`,
  `last_sent_metadata`, `last_write_time`, the shared `offset`/`carry`) are
  plain `mutable` record fields; their single-writer discipline is what
  [concurrency.md §2](../concurrency.md#2-ownership-of-a-listener) states.
- The output's start/stop uses `Mutex.protect`. The dedicated per-listener
  encoder lock is a `Mutex.t` in the listener state.
- `Strings.Mutable.t` is the dedicated pending queue. Its `write` runs the
  socket write under the buffer's own lock and drops what was written, so a
  concurrent append cannot move the bytes being written.
- `ByteRing` (`src/core/utils/byteRing.ml`) is the ring of [ring.md](../ring.md),
  built on two atomics: the `{ buf; cap }` record and the tail.
- The listener socket close is guarded by an `Atomic.t` once-flag captured in a
  closure (`close`), separate from the `closed` flag.

## Scheduler idioms

- Writers are `Scheduler.Task` values with `priority = `Blocking`and events`[`Read wake_out; `Delay 1.]`; the handler returns the next task to re-arm,
or `[]` to end.
- The teardown is a one-shot task with `priority = `Threaded`and`events = [`Delay 0.]`.
- `Scheduler.reschedule ~priority:`Threaded ()` moves the request task off
  the server's non-blocking queue before authentication and after
  publication.
- The wake channel is `Unix_utils.socketpair ~cloexec:true ()` with the write
  end set non-blocking; Windows only allows non-blocking mode on sockets.

## Harbor API usage

- `Harbor.add_http_handler ~pos ~transport ~port ~verb:`Get ~uri`and`Harbor.remove_http_handler ~port ~verb:`Get ~uri ()`; both go through
  `Server.on_start`.
- Replies are exceptions: `Harbor.reply f`, `Harbor.simple_reply s` and
  `Harbor.custom ()` all raise `Harbor.Reply`. `Harbor.reply` takes a producer
  called until it returns `""`; `simple_reply` wraps a string in a producer
  that returns it once (`mk_simple`). A constant producer never returns `""`, so
  the server keeps writing it: every refusal reply must be a finite producer
  ([connection.md §7](../connection.md#7-failure-paths)).
- `Harbor.http_auth_check ~query ~meth:"GET" ~uri ~login socket headers`
  takes `login = ("", fun { Harbor.socket; user; password; _ } -> bool)`.
- `Harbor.write` is the transport's `socket#write`. Non-blocking behaviour
  comes from `Unix.set_nonblock` on the descriptor. The SSL transport maps
  `Error_want_read`/`Error_want_write` to `EAGAIN`; the TLS transport
  (`Liq_tls.write`) does a `write_all` with `Harbor.conf_timeout`.

## Process shutdown

`Lifecycle.before_core_shutdown` sets a module-level `Atomic.t`; each output
checks it in an `on_frame (`Before_frame …)` hook and disconnects its
listeners once.
