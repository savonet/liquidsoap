# The operator

`output.harbor` encodes a source and serves the result over HTTP to any
number of listeners, on a port of the harbor server, at one mount.

## 1. Parameters

Shared with every output (generic output base, [contracts.md §3](contracts.md#3-output-base)):

| Name              | Type    | Default | Meaning                                                                     |
| ----------------- | ------- | ------- | --------------------------------------------------------------------------- |
| `id`              | string? | null    | Source id. Also used to name the encoder.                                   |
| `start`           | bool    | `true`  | Start the output as soon as the source is available.                        |
| `fallible`        | bool    | `false` | Accept a fallible source; the output stops while the source is unavailable. |
| `register_telnet` | bool    | `true`  | Register telnet commands for this output.                                   |

Shared with `output.icecast` (MIME derivation, [contracts.md §2](contracts.md#2-format-and-mime-derivation)):

| Name             | Type   | Default | Meaning                                                            |
| ---------------- | ------ | ------- | ------------------------------------------------------------------ |
| `format`         | string | `""`    | MIME type sent as `Content-type`. Empty: derived from the encoder. |
| (unlabeled, 1st) | format | —       | The encoding format.                                               |

Specific to `output.harbor`:

| Name                | Type               | Default                 | Meaning                                                                                                                     |
| ------------------- | ------------------ | ----------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| `mount`             | string             | — (required)            | Mount path. A leading `/` is added when absent. See [connection.md §1](connection.md#1-routing).                            |
| `port`              | int                | `8000`                  | Harbor port.                                                                                                                |
| `transport`         | http_transport     | the plain TCP transport | Transport of the port. An SSL/TLS transport serves HTTPS.                                                                   |
| `timeout`           | float              | `30.`                   | Seconds a listener with pending data may go without accepting a byte ([delivery.md §5](delivery.md#5-no-progress-timeout)). |
| `encoding`          | string             | `""`                    | Charset metadata values are converted to. Empty means UTF-8 ([icy.md §5](icy.md#5-charset)).                                |
| `url`               | string?            | null                    | Sent as `StreamUrl` in ICY blocks.                                                                                          |
| `metaint`           | int                | `8192`                  | ICY metadata interval, in stream bytes.                                                                                     |
| `auth`              | (record → bool)?   | null                    | Authentication function ([connection.md §3](connection.md#3-authentication)). Null: no authentication.                      |
| `buffer`            | int                | `327675` (5 × 65535)    | Per-listener limit, in bytes, of how far a listener may fall behind.                                                        |
| `burst`             | int?               | `65534`                 | Bytes of already-encoded stream a new listener receives first. Null: none. Shared mode only.                                |
| `headers`           | [(string, string)] | `[]`                    | Extra response headers, sent verbatim.                                                                                      |
| `dumpfile`          | string?            | null                    | Path the encoded stream is also written to.                                                                                 |
| `dedicated_encoder` | bool               | `false`                 | Selects the encoder mode ([modes.md](modes.md)).                                                                            |
| (unlabeled, 2nd)    | source             | —                       | The source to encode.                                                                                                       |

The `dedicated_encoder` value picks the mode once, at construction:
`false` gives [shared-encoder.md](shared-encoder.md), `true` gives
[dedicated-encoder.md](dedicated-encoder.md). The two modes accept the same
parameters.

## 2. Validation at construction

In this order, when the operator is evaluated:

1. The encoder factory and the MIME type are resolved
   ([contracts.md §2](contracts.md#2-format-and-mime-derivation)). An
   unsupported format fails with `No encoder found for that format`; no MIME
   type from either `format` or the encoder fails with
   `No format (mime) found, please specify one.` Both are invalid-value
   errors attached to the format argument.
2. When `burst` is not null and `burst > buffer`, the operator fails with an
   invalid-value error on `buffer`: `Maximum buffering inferior to burst
length`. This check runs in both modes.
3. The mount's first character is inspected to decide whether to prepend
   `/`. An empty mount fails here with a runtime error `index out of bounds`.
4. The generic output base checks that the source is infallible unless
   `fallible` is true, and unifies the frame types.

Nothing else is validated: `metaint`, `timeout` and `buffer` are taken as
given; `headers` are not inspected; the `encoding` name is looked up only when
metadata is converted ([icy.md §5](icy.md#5-charset)).

## 3. Methods

The generic output methods (`start`, `stop`, `is_started`, and the source
methods every output has), plus:

- `listeners()` returns the list of listener records
  ([§5](#5-the-listener-record)) of the listeners currently connected: every
  listener published to a writer shard and not yet marked closed. `duration`
  and `bytes_sent` are read at call time. The order is unspecified.

## 4. Callbacks

The generic output callbacks (`on_start`, `on_stop`, `on_output`), plus two
listener callbacks. Both take one listener record. Like every callback, each
registration chooses `synchronous=true` (run in the actor that fires it) or
`synchronous=false` (each handler runs in its own thread, in any order).

- **`on_connect`** fires when a listener has been admitted
  ([connection.md §5](connection.md#5-admission)), before it receives any
  byte. `duration` is the time since the record was created (≈ 0), and
  `bytes_sent` is `0`.
- **`on_disconnect`** fires once for every listener whose `on_connect` fired,
  after it, whatever ended the connection. `duration` is measured to the
  moment the listener was marked closed; `bytes_sent` is the total accepted by
  the socket, read when the callback runs.

A handler that raises is logged with its backtrace (`Listener callback
failed: …`); the other handlers and the listener's teardown still run.
Ordering and threads are in [concurrency.md §3](concurrency.md#3-guarantees).

## 5. The listener record

| Field          | Type               | Value                                                                                                           |
| -------------- | ------------------ | --------------------------------------------------------------------------------------------------------------- |
| `id`           | int                | Per-output counter, starting at 0, incremented for every admitted listener. Never reset, not even by a restart. |
| `ip`           | string             | Peer address without port, numeric. Reverse DNS is never applied here.                                          |
| `uri`          | string             | Request path, query string removed.                                                                             |
| `protocol`     | string             | `"1.0"` or `"1.1"`.                                                                                             |
| `headers`      | [(string, string)] | Request headers, names lowercased, values as received, in request order.                                        |
| `connected_at` | float              | Wall-clock time (Unix epoch seconds) when the record was created.                                               |
| `duration`     | float              | See [§4](#4-callbacks) and [§3](#3-methods).                                                                    |
| `bytes_sent`   | int                | Bytes the socket accepted for this listener, response head included.                                            |

`id` is a counter rather than the address so that the record can be shown
without exposing the peer. The log uses a separate identifier, the peer
address with port (`127.0.0.1:37890`).

## 6. Settings

| Key                              | Default as displayed                   | Effective default when unset                     |
| -------------------------------- | -------------------------------------- | ------------------------------------------------ |
| `settings.harbor.output.writers` | the runtime's recommended domain count | the scheduler's maximum domain count, at least 1 |

It is the number of writer tasks (shards) per output
([delivery.md §1](delivery.md#1-shards)). It is read each time the output
starts. Harbor-wide settings (`settings.harbor.*`: bind addresses, timeouts,
reverse DNS, verbose passwords) belong to the server
([contracts.md §1](contracts.md#1-harbor-server)); `reverse_dns` changes the
`address` passed to `auth`.

## 7. Lifecycle

States follow the generic output: stopped and started. Transitions:

**Start** (under the output's start/stop lock; a start while started does
nothing):

1. Create the encoder state of the mode ([modes.md §7](modes.md#7-start)).
2. Create the writer shards and their tasks
   ([delivery.md §1](delivery.md#1-shards)).
3. Register the HTTP handler for `GET` on the mount
   ([connection.md §1](connection.md#1-routing)). Registration takes effect
   when the server has started; before that it is queued.
4. Open `dumpfile` for writing, truncating it, when set.

**Stop** (same lock; a stop while stopped does nothing):

1. Stop the encoder state of the mode ([modes.md §8](modes.md#8-stop)).
2. Remove the HTTP handler.
3. Mark every listener of every shard closed
   ([delivery.md §6](delivery.md#6-disconnection)).
4. Stop the writer tasks ([delivery.md §7](delivery.md#7-stopping-the-writers)).
5. Close `dumpfile`.

A stop followed by a start is a restart: shards, tasks and the encoder state
are created anew. The listener id counter, the last metadata
([icy.md §4](icy.md#4-cadence-and-change-detection)) and, in shared mode, the
ring and its markers ([shared-encoder.md §7](shared-encoder.md#7-start)) are
kept.

**Process shutdown.** Before the core shuts down, a process-wide flag is set.
At the start of the next frame of each `output.harbor`, the first time it sees
the flag, every listener is marked closed. The output itself is then stopped
by the generic shutdown.

**Fallible source.** When the output is fallible and its source becomes
unavailable, the generic base stops the output, which disconnects every
listener; it starts again when the source is available.
