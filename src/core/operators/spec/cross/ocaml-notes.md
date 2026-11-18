# OCaml notes (Part B)

Where the as-built text lives in the code, and what is specific to this
implementation. See also [../switch/ocaml-notes.md](../switch/ocaml-notes.md)
for the seam.

## Where things are

| Spec                               | Code                                                                                         |
| ---------------------------------- | -------------------------------------------------------------------------------------------- |
| [cross.md](cross.md)               | `src/core/operators/cross.ml`, class `cross`                                                 |
| Buffer source, fed or not          | same file, class `consumer` (`?fed`)                                                         |
| Phase                              | `status`: `` `Idle ``, `` `Before ``, `` `After ``, each with an activation                  |
| Stores, levels, kept metadata      | `gen_before`, `gen_after`, `rms_*`, `mem_before`, `before_metadata`, `after_metadata`        |
| Resolve, reset, override           | `set_main_duration`, `reset_duration`, `process_override_metadata`                           |
| `take`                             | `child_get ~is_first`, over `split_frame`                                                    |
| Probe                              | `get_source`, called from the seam's `choose_next_source`                                    |
| Start buffering, top up            | `prepare_before`, `buffer_before`                                                            |
| The ending track is over           | `analyze_after`                                                                              |
| Build                              | `create_after`                                                                               |
| Designate, release                 | `set_status`, `prepare_source`, `release_transition`                                         |
| Entry                              | `next_entry`: `` `Track `` in `After`, `` `As_is `` otherwise                                |
| Abort                              | `abort_track`, `pending_abort_track`, hook on `on_before_streaming_cycle`                    |
| [child-clock.md](child-clock.md)   | `src/core/operators/child_support.ml`: `base`, `child_output`, `child_get_frame`             |
| Fallback announcement              | `src/core/operators/replay_metadata.ml`                                                      |
| Sequences of the compound          | `src/core/operators/sequence.ml`, with `~new_track_on_source_switch:false`                   |
| Callback collection                | `Lang_source.collect_callback_releases`, an effect keyed on object ids                       |
| [script-layer.md](script-layer.md) | `src/libs/fades.liq`; plots in `src/libs/extra/fades.liq`; autocue in `src/libs/autocue.liq` |

## Specific to this implementation

- **Everything hangs off readiness.** `get_source` runs inside the
  per-cycle readiness value, so the probe's work happens once per cycle.
  `enter_chosen_source` is the seam's default: it returns what the probe
  found.
- **Three entries.** The seam here knows `` `Track ``, `` `Continue `` and
  `` `As_is ``. The specification of the seam only has the first two.
- **A mark at the end of a frame** is how a source states `track end`.
  Appended to a store it becomes a mark on the first sample of what
  follows.
- **A source built at run time is in its own clock** until unified:
  `prepare_source` unifies before waking.
- **The child is woken twice**: by the child output and by `cross`
  directly.
- **The cross class has no declared children**: the generic rules for
  exhaustion and for a segment's end see none.
- **`cross` caches like any source**: what its reader did not use of a
  frame is kept by `cross`, though the seam's specification says a user
  holds no data.
- **Floats** are parsed with `float_of_string` and printed with
  `string_of_float`.
- **The level** uses the sum of squares over all channels of a frame,
  divided by the sample count and the channel count.

## Things that bit while reading

- The frame is 0.02 s in the tests' default configuration, not 0.04 s.
- `source.time` is clock time: inside a transition function it reads the
  time of the crossing, not a position in either track.
- The wrapper `./liquidsoap` rebuilds before running. A script that dies at
  start with logs off prints nothing: turn the log back on before
  concluding.
- Two instances in one script interleave their prints; sort by label.
