# OCaml notes (Part B)

Notes on the implementation. Not normative: where a note and the
specification disagree, the specification is what to build. The first
section describes the rebuild; the rest describes the code it started from.

## The rebuild

The rebuild kept the existing classes and cut them down. How it realises
the contract:

| Spec                                  | Code                                                                                            |
| ------------------------------------- | ----------------------------------------------------------------------------------------------- |
| Cycles of a tick                      | `Clock.request_segment`, `Clock.after_segment`; `continue_segments` in `src/core/clock/life.ml` |
| An output gathers a frame over cycles | `do_output` in `src/core/outputs/output.ml`                                                     |
| Static cycle                          | `Source.Per_cycle`, released by `after_streaming_cycle`, which runs after each cycle            |
| Reason a segment ended                | `source#segment_end`, computed by the private `frame_end`                                       |
| Exhausted                             | `source#exhausted`                                                                              |
| Seam                                  | `generate_from_multiple_sources` in `src/core/source/source.ml`                                 |
| Decision, then commit                 | `choose_next_source ~boundary`, then `enter_chosen_source`                                      |
| End statement and entry               | `next_entry`, asked when the track of the source being read ends                                |
| A user moving on from a source        | `leave_source`, called before the user forgets the source                                       |
| Switch decisions                      | `decide_mid_track`, `decide_at_boundary`, `handoff` in `src/core/operators/switch.ml`           |
| Presented source                      | `insert_initial_track_mark`: `leave`, `track_ended`                                             |

Where the code stops short of the specification:

- **Probes answer readiness only.** No source tells `more` from
  `new track`. A source that starts a track without having declared the end
  of the previous one costs its reader one empty cycle
  ([source-contract.md §2](source-contract.md#2-probes)). Outputs follow at
  most `max_empty_segments` empty cycles in a row.
- **`sequence`, `source.dynamic` and `cross` still commit when probed.** Only
  `switch` separates the decision from the handoff.
- **A request source takes its next request when probed**, not when read.
  It no longer does so while producing.
- **An operator over several sources declares `pause`, never `track end`**,
  when one of them stopped: it cannot tell whose track is its own.
- **`cross` builds its transition out of buffers that declare exhaustion**;
  the rest of it is as before
  ([cross-seam.md §4](cross-seam.md#4-open)).
- **Aborts take effect at once**, not at the end of the cycle.

## Where things were

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
