# The operator

`output.harbor` encodes a source and serves the result over HTTP to any
number of listeners, on a port of the harbor server, at one mount.

## 1. Parameters

The names, types and defaults below are the script-facing API. A rebuild
MUST keep every one of them unchanged; new optional parameters MAY be added.

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

| Name                | Type                      | Default                 | Meaning                                                                                                                       |
| ------------------- | ------------------------- | ----------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| `mount`             | string                    | — (required)            | Mount path, matched literally. A leading `/` is added when absent ([connection.md §1](connection.md#1-routing)).              |
| `port`              | int                       | `8000`                  | Harbor port.                                                                                                                  |
| `transport`         | http_transport            | the plain TCP transport | Transport of the port. An SSL/TLS transport serves HTTPS. A transport value carrying extra methods MUST be accepted.          |
| `timeout`           | float                     | `30.`                   | Seconds a listener owed bytes may go without accepting one ([delivery.md §5](delivery.md#5-no-progress-timeout)).             |
| `encoding`          | string                    | `""`                    | Charset metadata values are converted to. Empty means UTF-8 ([icy.md §5](icy.md#5-charset)).                                  |
| `url`               | string?                   | null                    | Sent as `StreamUrl` in ICY blocks.                                                                                            |
| `metaint`           | int                       | `8192`                  | ICY metadata interval, in stream bytes.                                                                                       |
| `auth`              | (record → bool)?          | null                    | Authentication function ([connection.md §3](connection.md#3-authentication)). Null: no authentication.                        |
| `accept`            | (listener record → bool)? | null                    | Admission predicate ([connection.md §4](connection.md#4-admission-predicate)). Null: every authenticated request is admitted. |
| `buffer`            | int                       | `327675` (5 × 65535)    | How far, in stream bytes, a listener may fall behind the live stream ([modes.md §4](modes.md#4-flush)).                       |
| `burst`             | int?                      | `65534`                 | Bytes of already-encoded stream a new listener receives first. Null: none. Shared mode only.                                  |
| `headers`           | [(string, string)]        | `[]`                    | Extra response headers ([connection.md §5](connection.md#5-the-response-head)).                                               |
| `dedicated_encoder` | bool                      | `false`                 | Selects the encoder mode ([modes.md](modes.md)).                                                                              |
| (unlabeled, 2nd)    | source                    | —                       | The source to encode.                                                                                                         |

`dedicated_encoder` picks the mode once, at construction: `false` gives
[shared-encoder.md](shared-encoder.md), `true` gives
[dedicated-encoder.md](dedicated-encoder.md). Both modes accept the same
parameters.

## 2. Validation at construction

Every check below MUST run when the operator is evaluated, and each failure
MUST be an invalid-value error attached to the argument it names, so that a
script error points at its cause:

1. The encoder factory and the MIME type are resolved
   ([contracts.md §2](contracts.md#2-format-and-mime-derivation)). An
   unsupported format fails with `No encoder found for that format`; no MIME
   type from either `format` or the encoder fails with
   `No format (mime) found, please specify one.` Both on the format argument.
2. `mount` MUST NOT be empty.
3. `metaint` and `buffer` MUST be positive; `timeout` MUST be positive.
4. In shared mode, when `burst` is not null, `burst ≤ buffer`; otherwise the
   error is on `buffer`: `Maximum buffering inferior to burst length`. A burst
   longer than the allowed lag would put a joiner in overflow on its first
   write. In dedicated mode `burst` is ignored and MUST NOT be checked: its
   default is not null, so a check would reject valid dedicated scripts.
5. Every pair of `headers` MUST have a name that is an HTTP token and a value
   with no CR or LF. A line break in a value would end the response head or
   inject headers.
6. A non-empty `encoding` MUST name a charset the converter knows. An unknown
   name would otherwise fail later, on the streaming path, at the first
   metadata.
7. The generic output base checks that the source is infallible unless
   `fallible` is true, and unifies the frame types.

## 3. Methods

The generic output methods (`start`, `stop`, `is_started`, and the source
methods every output has), plus:

- `listeners()` returns the listener records ([§5](#5-the-listener-record)) of
  the listeners connected at call time: every listener whose `on_connect`
  handlers have run and that has not yet been marked closed
  ([delivery.md §6](delivery.md#6-disconnection)). `duration` and
  `bytes_sent` are read at call time. The order is unspecified.

`listeners()` MUST agree with the callbacks: a listener appears from the end
of its synchronous `on_connect` handlers until it is marked closed, so it is
already absent inside its own synchronous `on_disconnect` handlers.

## 4. Callbacks

The generic output callbacks (`on_start`, `on_stop`, `on_output`), plus two
listener callbacks, each taking one listener record. Each registration
chooses `synchronous=true` (run in the actor that fires it) or
`synchronous=false` (each handler runs on its own, in any order).

- **`on_connect`** fires when a listener is admitted
  ([connection.md §6](connection.md#6-admission)), before any byte is written
  to its socket. `duration` is ≈ 0 and `bytes_sent` is `0`.
- **`on_disconnect`** fires exactly once for every listener whose
  `on_connect` fired, after the synchronous `on_connect` handlers, whatever
  ended the connection. It fires once no further byte can be written to the
  listener, so `bytes_sent` is final; `duration` is measured to the moment the
  listener was marked closed.

A handler that raises MUST be logged with its backtrace
(`Listener callback failed: …`) and MUST NOT prevent the other handlers or
the listener's teardown. Accounting built on these callbacks (access logs,
listener counts) depends on this; see
[concurrency.md §3](concurrency.md#3-guarantees).

## 5. The listener record

Every field keeps its name, type and meaning; new fields MAY be added.

| Field          | Type               | Value                                                                                                        |
| -------------- | ------------------ | ------------------------------------------------------------------------------------------------------------ |
| `id`           | int                | Per-output counter, starting at 0, unique and increasing over the operator's life. Never reset by a restart. |
| `ip`           | string             | Peer address without port, numeric. Never a reverse-DNS name.                                                |
| `uri`          | string             | Request path, query string removed.                                                                          |
| `protocol`     | string             | `"1.0"` or `"1.1"`.                                                                                          |
| `headers`      | [(string, string)] | Request headers, names lowercased, values as received, in request order.                                     |
| `connected_at` | float              | Wall-clock time (Unix epoch seconds) when the record was created.                                            |
| `duration`     | float              | See [§3](#3-methods) and [§4](#4-callbacks).                                                                 |
| `bytes_sent`   | int                | Bytes the socket accepted for this listener, response head included.                                         |

The record is created before the admission predicate runs
([connection.md §6](connection.md#6-admission)), so a refused request
consumes an `id`: ids of admitted listeners are increasing, not necessarily
consecutive.

`id` exists so that a record can be shown without exposing the peer address.
Logs MAY use the peer address with port (`127.0.0.1:37890`) as the
listener's identifier. `ip` is numeric because consumers hash it or replace it
with a forwarded address ([icecast-server.md §5](icecast-server.md#5-what-it-reads-from-the-output));
the `auth` record's `address` follows the harbor's reverse-DNS setting
instead ([connection.md §3](connection.md#3-authentication)).

## 6. Settings

| Key                              | Meaning                                                                                                         |
| -------------------------------- | --------------------------------------------------------------------------------------------------------------- |
| `settings.harbor.output.writers` | Hint for how many independent delivery groups an output uses ([delivery.md §1](delivery.md#1-delivery-groups)). |

The key MUST remain accepted. Its displayed default MUST be the value used
when it is unset. An implementation MAY ignore it when the scheduler decides
the parallelism of delivery on its own. Harbor-wide settings
(`settings.harbor.*`) belong to the server
([contracts.md §1](contracts.md#1-harbor-server)).

## 7. Lifecycle

States follow the generic output: stopped and started. Start and stop are
serialised per output; a start while started and a stop while stopped MUST
do nothing.

**Start**:

1. Create the encoder state of the mode ([modes.md §7](modes.md#7-start)).
2. Create the delivery groups ([delivery.md §1](delivery.md#1-delivery-groups)).
3. Register the mount on the port ([connection.md §1](connection.md#1-routing)).
   Registration takes effect when the server has started; before that it is
   queued.

**Stop**:

1. Remove the mount's registration, so no new request reaches the output.
2. Stop the encoder state of the mode and queue its final bytes for the
   listeners ([modes.md §8](modes.md#8-stop), [§11](modes.md#11-final-bytes)).
3. Run one last delivery pass for every group
   ([delivery.md §7](delivery.md#7-stopping)), then mark every listener closed.
4. End the delivery groups.

Stop MUST NOT wait on any listener: the last pass is one non-blocking write
per listener. A request admitted concurrently with a stop MUST end closed,
with both callbacks ([delivery.md §7](delivery.md#7-stopping)).

**Restart.** A stop followed by a start creates new delivery groups and a new
encoder state. The listener id counter and the last metadata
([icy.md §4](icy.md#4-cadence-and-change-detection)) are kept; no byte the
previous encoder produced reaches a listener admitted after the restart
([shared-encoder.md §7](shared-encoder.md#7-start)).

**Process shutdown.** When the core is about to shut down, every listener of
every `output.harbor` MUST be marked closed, from the shutdown hook itself:
waiting for the next frame would leave listeners connected when no frame
comes. The output is then stopped by the generic shutdown.

**Fallible source.** When the output is fallible and its source becomes
unavailable, the generic base stops the output, which disconnects every
listener; it starts again when the source is available.
