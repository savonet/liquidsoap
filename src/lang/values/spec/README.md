# Values specification

What a runtime value MUST, SHOULD and MAY be, under the
[conventions](../../../spec/conventions.md) and
[principles](../../../spec/principles.md) of the project. This tree is the
place to collect decisions about values. Partial: it records decisions as
they are taken, not yet the whole model.

**Scope.** The runtime representation of script values: their identity, what
an operation on a value keeps and what it creates, and what the runtime may
attach to a value. Terms, types and typing are out of scope. The objects a
custom value wraps (sources, requests, clocks, …) are their own trees'.

| File                 | Content                                                 |
| -------------------- | ------------------------------------------------------- |
| [value.md](value.md) | Part A. Base and view, identity, finalisation. Partial. |

**Compatibility surface.** `runtime.gc.finaliser` and its semantics
([value.md §4](value.md#4-finalisation)). The representation itself is
internal and changes freely.

## Vocabulary

- **Value**: what a script expression evaluates to.
- **Base**: what a value is, independently of where it was written and of
  its methods: an int, a string, a list of values, a closure, a wrapped OCaml
  object (**custom value**), ….
- **View**: a base seen with a position and a set of methods. A script
  variable holds a view.
- **Derived view**: a view built from another one by changing only its
  position or its methods.
- **Content**: the OCaml object a custom value wraps.
