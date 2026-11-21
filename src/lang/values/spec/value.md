# Values (Part A)

Owns what a finaliser on a value watches and when it runs. Relies on nothing
above the project principles. Partial.

## 1. Copies

The runtime copies a value without the script asking: setting a position
(the result of a builtin call, a default argument, a cast), adding, hiding
or removing methods, and preparing the arguments of an operator, which
rebuilds functions with the operator's types. A copy MUST keep pointing at
the data the value holds: the same string, list, content or function
identity.

The **position** of a value is where it was produced: the expression it
was evaluated from, the call site for the result of a builtin and for a
default argument it used, the cast for a cast value. Error messages about
the value point there. Adding methods does not produce a new value: after
`x = 1223` and `let x.foo = "aabbc"`, `x`'s position is still that of
`1223`.

## 2. Finalisation

`runtime.gc.finalise.last(fn, v)` registers `fn` to run once `v` is
unreachable. `runtime.gc.finalise(fn, v)` does the same and passes `v` to
`fn`. They mirror OCaml's `Gc.finalise_last` and `Gc.finalise`.

1. **What is watched.** The OCaml block every copy of `v` shares (§1),
   with the guarantee `Gc` gives it:
   - a string: the string;
   - a non-empty list or tuple: the list;
   - a function: an identity block created with it, which every copy,
     operators' included, shares;
   - a custom value: its content, e.g. the source itself, however many
     times it is wrapped.

   `fn` MUST NOT run while any copy of `v` is reachable.

2. **Values without data of their own.** Numbers, booleans, `null`, empty
   lists and tuples, and statically allocated data hold no block a
   finaliser can watch: a finaliser on them MUST be ignored, with a
   warning in the log.
3. **Shared data.** Values that share their data are watched together: a
   string literal evaluated twice is one string. A list passed to an
   operator MAY be rebuilt by it, and the rebuilt list is not a copy.
4. **Running `fn`.** With `finalise`, `fn` receives `v` as it was
   registered, with its position and methods, even when those methods refer
   to the data being watched: holding `v` for `fn` MUST NOT keep that data
   alive. With `finalise.last`, `fn` receives nothing. `fn` MUST NOT run
   inside the garbage collector: collection only queues
   it, and it runs later as an ordinary scheduler task, under the same
   error handling as any script callback.
5. **Shutdown.** After the sources are shut down, and again when the
   process exits, a last collection MUST run the queued functions,
   including when the scheduler never started.
6. **Reachability is not scope.** `fn` runs when no live value refers to
   `v`, not when its variable goes out of scope. Top-level bindings stay
   reachable for the life of the script. A closure that refers to `v`
   keeps it alive, including `fn` itself: a `fn` that mentions `v` never
   runs. The documentation of `runtime.gc.finalise` MUST say both.
7. `fn` runs at most once.

Rationale for 4: the collector runs at any allocation, in any thread,
possibly with locks held or in the middle of a streaming cycle. A script
callback run there can deadlock, re-enter the runtime or stall the stream.

Rationale for 1: a finaliser attached to one copy would fire while another
copy is still in use. Watching the data the copies share makes every copy
keep it alive, at no cost for values nobody watches.
