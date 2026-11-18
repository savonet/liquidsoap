# The output base (Part A)

What every output shares, whatever it writes to. Each output's tree states
its own `start` and `stop`, and links here for the rest. Outputs are sources
of type output ([clock spec](../../clock/spec/README.md#vocabulary)) and
read their one source under the
[source contract](../../source/spec/source-contract.md).

## 1. Common parameters, methods and callbacks

- Parameters: `id`; `fallible` (default `false`); `start` (default `true`):
  start as soon as the source is available; `register_telnet` (default
  `true`).
- Methods: `start()`, `stop()`, `is_started()`, `shutdown()`.
- Callbacks: `on_start`, `on_stop`, registered as every source callback
  ([source-contract.md §10](../../source/spec/source-contract.md#10-callbacks)).
- At construction, an output that is not `fallible` checks that its source
  is infallible, and fails with an invalid-value error on the source
  otherwise.

## 2. States

| State     | Meaning                                                                |
| --------- | ---------------------------------------------------------------------- |
| `idle`    | Not started, and starts on its own when its source is available again. |
| `started` | Started: the output's own `start` ran, and `stop` has not.             |
| `stopped` | Stopped on request: it starts again only when asked.                   |

- Transitions are requested (`start()`, `stop()`, the source becoming
  available or ending) and carried out on the streaming thread before the
  next streaming cycle, in the order requested. They are serialised: a start
  while started and a stop while stopped do nothing.
- Entering `started` runs the output's `start`, then the `on_start`
  callbacks. Leaving it runs the output's `stop`, then the `on_stop`
  callbacks.
- When woken with `start` true, the output asks to be started. A fallible
  output whose source ends falls back to `idle`; it is started again when
  the source is available.
- Going to sleep stops the output at once, without waiting for a next cycle:
  a sleeping output gets none. This is the **final stop**: the end of the
  process, `shutdown()`, a clock stopped. Every other stop is not final, and
  an output that keeps state across runs keeps it across those.
- `shutdown()` stops the output for good.

## 3. A streaming cycle

On each streaming cycle where it is started and its source is ready, the
output gathers its frame and hands it on: the frame is cut at the positions
of its metadata, and for each non-empty piece the first metadata in it, if
any, is handed on first, then the piece. Metadata comes first, so that the
bytes of a piece are in the scope of its metadata. Cover art and other
binary metadata entries are not handed to encoders.

## 4. Reset

A **reset** is what the clock asks of its outputs after too much latency
([pacing.md §6](../../clock/spec/pacing.md#6-rest-and-lateness)): stop then
start, and only while started. Afterwards the output is started exactly as
after a first start. A reset is not a final stop. An output for which a
reset is a discontinuity of its stream says so in its own document.

## 5. Sync and pacing

An output is not a sync source of its own unless its document says it is
(a sound card). Its self-sync is its source's
([pacing.md §3](../../clock/spec/pacing.md#3-finding-the-sync-source)). An
output whose readers expect data at the real-time rate (a network server)
needs a clock paced in real time, and refuses to start on a clock with no
synchronization or a passive one unless the script allows it.

## 6. Failure

Faults follow [faults.md](../../../spec/faults.md): an output fault stops or
restarts that output under its own restart policy and reports through its
callbacks; the clock and other outputs go on. An exception that escapes an
output's `start`, `stop` or cycle is a failure of that output under the
clock's source error rule
([clock.md §11](../../clock/spec/clock.md#11-failure)). Each output's
document states its restart policy.
