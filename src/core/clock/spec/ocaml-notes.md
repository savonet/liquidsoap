# OCaml notes (Part B)

Notes on the OCaml implementation the specification was extracted from. None
of it is a rule of the clock, and none of it is a design to copy.

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
- State, id, controller, stack, and ticks are `Atomic`.
- The lists are queues from the shared utilities: `Queue` is a lock-free
  snapshot list (iteration reads an immutable snapshot; mutation takes a lock),
  `WeakQueue` the weak variant. Both are wrapped so that `push` skips an
  element already present by physical equality.
- Iterating the active set goes through a snapshot (`elements`) because the
  weak queue's own iterator would hold its lock while sources stream.
- The sets that must not keep a source alive are weak sets.
- The memoised sync type of an operator is an `Atomic` holding an option; two
  threads may both compute it, and the result is the same.
- Threads of one domain run one at a time. Spreading the threads of
  never-resting clocks over the cores means spreading them over domains; the
  scheduler library may be given an entry point for this.

## Sync sources

`sync_source` is an extensible variant. A functor (`MkSyncSource`) adds one
constructor per kind and registers a handler record in a global list; printing
and reading a sync source's parameters walks that list. Polymorphic `compare`
on such a value walks its content and fails on closures: the declared identity
is what to compare.

## Time

A time source is a first-class module (`Liq_time.T`). `Liq_time.set_offset`
wraps one. The "posix" implementation is registered by another library;
"ocaml" is the built-in one based on `Unix.gettimeofday` and `Thread.delay`.

## Scheduler

- A task animator is one `Duppy.Task` of the clock priority, ready at once.
  Its handler wraps the loop in `Duppy.run`, which installs the effect handler
  that `Duppy.reschedule` needs.
- Giving the worker back is a call to `Duppy.reschedule` from inside that
  handler: it suspends the loop where it is and queues the continuation as a
  new task. Outside a `Duppy.run` it raises `Effect.Unhandled`.
- A thread animator is created through `Tutils.create`, which names and tracks
  the thread.

## Start-up plumbing

- Clock creation announces itself with an effect (`Clock_created`). The
  collecting scope is an effect handler; outside one the effect is unhandled.
- The start pass at application start and the shutdown sequence are hooks in
  the application's lifecycle module (`before_start`, `before_core_shutdown`).

## Errors

Clock errors are exceptions. Their text is produced by printers registered
with the language runtime's error reporter or with `Printexc`.
