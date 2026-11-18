# OCaml notes (Part B)

Notes on the implementation this was extracted from. Not normative. They describe
the code as it was before the rebuild: where a note and the specification
disagree, the specification is what to build.

## Where things are

| Spec                                         | Code                                                                                   |
| -------------------------------------------- | -------------------------------------------------------------------------------------- |
| [switch.md](switch.md)                       | `src/core/operators/switch.ml`, class `switch`                                         |
| Proxy                                        | same file, class `insert_initial_track_mark`                                           |
| [frame-assembly.md](frame-assembly.md)       | `src/core/source/source.ml`, class `generate_from_multiple_sources`                    |
| [source-contract.md](source-contract.md)     | `src/core/source/source.ml`, `source.mli`                                              |
| Tags, profiles, per-source methods           | `src/core/source/source.ml` (`resolved_composition`), `src/core/source/lang_source.ml` |
| Default handoff, profile values              | `src/libs/composition.liq`                                                             |
| Policies                                     | `src/libs/switches.liq`                                                                |
| [sequence.md](sequence.md)                   | `src/core/operators/sequence.ml`                                                       |
| [dynamic.md](dynamic.md)                     | `src/core/operators/dyn_op.ml`                                                         |
| [cross-seam.md](cross-seam.md)               | `src/core/operators/cross.ml` (`get_source`, the compound)                             |
| [rotate-merge.md](rotate-merge.md)           | `src/libs/extra/source.liq`                                                            |
| `once`, `prepend`, `append`, delayed fade-in | `src/libs/source.liq`, `src/libs/fades.liq`                                            |

## Mechanisms specific to this implementation

- **Per-cycle values** are `Source.Per_cycle.t`, read through
  `self#per_cycle`. Readiness and the frame of every source are two of them.
- **Cycle hooks**: `on_before_streaming_cycle` runs inside the first
  `is_ready` of a tick; the after-cycle hook is registered on the clock's
  after-tick list at that moment.
- **Unconsumed data**: `consumed` is a per-cycle maximum; the after-cycle hook
  prepends what is past it to the source's cache.
- **The proxy** overrides `consumed` to forward to the wrapped source and
  calls the inherited one from `generate_frame`, so it never caches.
- **Callback collection** during `on_select` uses an effect
  (`Script_callback.collect`), keyed on object ids of the switch's children,
  the proxy and the ending source. Effects do not cross threads.
- **Predicate snapshot** runs inside `Atomic_section.run`.
- **Constant-true predicate** is recognised syntactically: a function whose
  body is the literal `true`.
- **Typing at handoff**: the proxy's and the effective source's frame types
  are unified below the switch's (`Typing.(<:)`); the proxy's content type is
  forced before `on_select` runs so that the script body's narrower static
  type does not escape the runtime checks.
- **Method schemes**: per-source methods are declared optional on the child
  type, with empty quantifier lists, so plain sources and `s.{…}` values both
  typecheck and generalise.
- **Profiles** are two global mutable references, replaced by the standard
  library at load.

## Things that bit while reading

- Captured logs are written by a log thread; script `print` is immediate. Do
  not read orderings across the two.
- `source.time` is clock-based, the same for every source of a clock; it does
  not tell which source produced a chunk.
- `dune build @test_<name>` can pull unrelated media tests in a tree where an
  encoder is missing. Run a regression test directly:
  `cd tests/regression && liquidsoap ../test.liq ./<name>.liq`.
