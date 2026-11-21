# Values (Part A)

Owns the split between base and view, value identity and finalisation.
Relies on nothing above the project principles. Partial.

## 1. Base and view

A value is a view on a base. A view MUST carry the value's position and
its methods. A non-custom base MUST NOT carry either.

A custom base MAY carry dynamic methods (the method lookup and hidden
methods of `source.tracks` values), and hiding or stripping them MAY
rebuild it: a custom value's identity is its content's (§2.5).

Rationale: everything a script can change about a value without making it
a different value is a property of the view. Keeping it out of the base is
what lets every view of one value share that base (§2). Dynamic methods
stay out of the view so that every other value does not pay a field for
them.

## 2. Identity

1. The operations that only change a position or methods MUST produce a
   derived view that shares the base of the view they start from. They
   include:
   - setting a position, including on the arguments and result of a builtin
     call and on a cast;
   - adding, replacing, hiding or removing methods: `x.{foo = …}`,
     `let x.foo = …`, method hiding, spreading;
   - stripping methods.
2. Passing a value to a function, a builtin or an operator, storing it (in a
   reference, a list, a closure environment) and returning it MUST keep its
   base, with one exception: preparing an operator's arguments MAY rebuild
   the base of a function, which gets a body typed for that operator, and of
   a list or tuple holding one. A rebuilt base MUST keep the base it was
   rebuilt from reachable for as long as it is itself reachable.

   Rationale: the types of a function body are read at runtime, to give the
   sources it creates the content the operator expects, so each operator
   needs its own copy. Keeping the original reachable is what makes the copy
   count as a view of the same value for §4.

3. A new base is created only by evaluating an expression that builds a new
   value: a literal, a function, a list or tuple constructor, a builtin
   returning a value it built.
4. The base of a non-custom value MUST have a physical identity the runtime
   can rely on: distinct bases are distinct blocks, and a base is never
   duplicated or shared with an unrelated value by the implementation.
5. A custom value's identity is its content's, not its base's. Several bases
   MAY wrap the same content: a builtin that returns a source wraps it anew.

Rationale: §4 attaches behaviour to a base. It is sound only if every view
the script holds of a value reaches the same base, and nothing the runtime
keeps for that value reaches another one.

## 3. Comparison

Not yet decided. Recorded so far:

- Values of custom types that are objects or resources compare by the
  identity of their content.
- Comparing a custom value MUST NOT run a structural comparison over values
  it contains (their positions differ).

## 4. Finalisation

`runtime.gc.finaliser(v, fn)` registers `fn` to run once `v` is unreachable.

1. **What `v` designates.** For a non-custom value, its base (§2): `fn`
   MUST NOT run while any view of the value is reachable, including derived
   views such as the result of `let x.foo = …` and values the runtime holds
   for an operator that received `v`. For a custom value, its content: `fn`
   runs once the wrapped object is unreachable, however many bases wrap it.
   This is only meaningful when the content is the object itself: a track
   wraps a pair built anew on each access, so finalising a track watches
   that pair. Not yet decided: refusing such values.
2. **Refused values.** A value whose base has no identity of its own (ints,
   floats, strings, booleans, `null`) or a custom value whose content is not
   heap-allocated MUST be refused with an error at the call.
3. **Running `fn`.** `fn` has type `() -> unit` and MUST NOT receive `v`.
   It MUST NOT run inside the garbage collector: collection only schedules
   it, and it runs later as an ordinary task, under the same error handling
   as any script callback.
4. **Reachability is not scope.** `fn` runs when no live value refers to
   `v`, not when its variable goes out of scope. Top-level bindings stay
   reachable for the life of the script. A closure that refers to `v` keeps
   it alive, including `fn` itself: a `fn` that mentions `v` never runs. The
   documentation of `runtime.gc.finaliser` MUST say both.
5. `fn` runs at most once.

Rationale for 3: the collector runs at any allocation, in any thread,
possibly with locks held. A script callback run there can deadlock or
re-enter the runtime.
