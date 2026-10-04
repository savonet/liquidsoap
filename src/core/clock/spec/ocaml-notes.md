# OCaml notes — as built (Part B)

What is specific to the language and libraries. None of it is a rule of the
clock.

## Handles and unification

A handle is a mutable reference cell from a small union-find module
(`Unifier`): `deref` follows links to the record, `c <-- c'` links one cell to
another. Every public function takes a handle and dereferences it first;
functions prefixed `_` take the dereferenced record or the streaming state.

A started clock's record is captured by its animator's closures, which is why
the stopped clock is the one merged away.

## Concurrency

- OCaml 5, several domains. A clock's tick runs on one thread at a time, but
  handles are read from any thread: script callbacks, the server, other clocks.
- State, id, controller, stack, needs-thread, ticks, pulled and the last
  latency log are `Atomic`.
- The lists are queues from the shared utilities: `Queue` is a lock-free
  snapshot list (iteration reads an immutable snapshot; mutation takes a lock),
  `WeakQueue` the weak variant. Both are wrapped here so that `push` skips an
  element already present by physical equality.
- Iterating the active set goes through a snapshot (`elements`) because the
  weak queue's own iterator would hold its lock while sources stream.
- `current_sync_source`, `sync_source_entries`, `time_implementation`,
  `animator` and `catchup_ticks` are plain mutable fields.
- The memoised sync type of an operator is an `Atomic` holding an option; two
  threads may both compute it, and the result is the same.

## Sync sources

`sync_source` is an extensible variant. A functor (`MkSyncSource`) adds one
constructor per kind and registers a handler record in a global list; printing
and reading a sync source's parameters walks that list. Equality between sync
sources is polymorphic `compare`.

## Time

A time source is a first-class module (`Liq_time.T`). `Liq_time.set_offset`
wraps one. The "posix" implementation is registered by another library;
"ocaml" is the built-in one based on `Unix.gettimeofday` and `Thread.delay`.

## Scheduler

- The task animator is one `Duppy.Task` of priority `` `Clock `` with a
  `` `Delay 0. `` event. Its handler wraps the loop in `Duppy.run`, which
  installs the effect handler `Duppy.reschedule` needs.
- Parking is a call to `Duppy.reschedule` with a delay and the clock priority.
  It raises `Effect.Unhandled` outside a `Duppy.run`, which is how a tick
  driven from elsewhere learns it cannot park.
- The thread animator is created through `Tutils.create`, which names and
  tracks the thread.

## Start-up plumbing

- Clock creation announces itself with an effect (`Clock_created`). The
  collecting scope is an effect handler; outside one the effect is unhandled
  and the clock is retained instead.
- The start pass at application start and the shutdown sequence are hooks in
  the application's lifecycle module (`before_start`, `before_core_shutdown`).

## Errors

Clock errors are exceptions. Their text is produced by printers registered
with the language runtime's error reporter (conflict, loop, controller
conflict) or with `Printexc` (sync error, animator conflict).
