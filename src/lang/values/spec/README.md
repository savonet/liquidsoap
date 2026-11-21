# Values specification

What a runtime value MUST, SHOULD and MAY be, under the
[conventions](../../../spec/conventions.md) and
[principles](../../../spec/principles.md) of the project. This tree is the
place to collect decisions about values. Partial: it records decisions as
they are taken, not yet the whole model.

**Scope.** The runtime representation of script values: what an operation on
a value keeps and what it creates, and what the runtime may attach to a value.
Terms, types and typing are out of scope. The objects a custom value wraps
(sources, requests, clocks, …) are their own trees'.

| File                 | Content                                           |
| -------------------- | ------------------------------------------------- |
| [value.md](value.md) | Part A. Copies, positions, finalisation. Partial. |

**Compatibility surface.** `runtime.gc.finalise`, `runtime.gc.finalise.last` and their semantics
([value.md §2](value.md#2-finalisation)). The representation itself is
internal and changes freely.

## Vocabulary

- **Value**: what a script expression evaluates to.
- **Copy**: a value the runtime builds from another one, holding the same
  data, e.g. with another position or more methods.
- **Content**: the OCaml object a custom value wraps.
