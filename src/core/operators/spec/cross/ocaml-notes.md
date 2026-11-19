# OCaml notes (Part B)

Where the as-built text lives in the code, and what is specific to this
implementation. See also [../switch/ocaml-notes.md](../switch/ocaml-notes.md)
for the seam.

## Where things are

| Spec                                                  | Code                                                                                         |
| ----------------------------------------------------- | -------------------------------------------------------------------------------------------- |
| [cross.md](cross.md)                                  | `src/core/operators/cross.ml`, class `cross`                                                 |
| Ending and starting sources                           | same file, class `consumer`, built by `finite_source`                                        |
| Phase                                                 | `phase`: `` `Idle ``, `` `Track ``, `` `Transition `` with its source and activation         |
| Buffer                                                | `tracks`: one `track` per track of the child, with its data, duration, metadata, `recent`    |
| Duration of a track                                   | `resolve_duration`, `cross_duration`, `persisted_duration`                                   |
| Reading the child, top up                             | `take_intake`, `buffer_track_data`, `wants_more`, `top_up`                                   |
| Levels                                                | `level`, `remember`, `ending_level`, `starting_level`                                        |
| Crossing                                              | `commit_crossing`                                                                            |
| Delivery                                              | `deliver`, `deliver_track`, `deliver_transition`, `enter`, `frame_end`                       |
| Announcement by passage                               | `announce`, the hidden key `passage_key`                                                     |
| End of a transition                                   | `leave_transition`, `release_transition`                                                     |
| Readiness                                             | `can_generate_frame`, `track_can_deliver`                                                    |
| Abort                                                 | `abort_track`, carried out by the base class after the cycle                                 |
| [child-clock.md](../../../source/spec/child-clock.md) | `src/core/operators/child_support.ml`: `base`, `child_output` (`set_on_track_end`)           |
| Replacement of an empty transition                    | `src/core/operators/sequence.ml`                                                             |
| Callback collection                                   | `Lang_source.collect_callback_releases`, an effect keyed on object ids                       |
| [script-layer.md](script-layer.md)                    | `src/libs/fades.liq`; plots in `src/libs/extra/fades.liq`; autocue in `src/libs/autocue.liq` |

## Specific to this implementation

- **`cross` does not use the seam.** It reads one child through the child
  clock and one transition source in its own clock, and assembles its
  frames itself.
- **A source built at run time is in its own clock** until unified:
  `wake` unifies before waking.
- **What the reader did not use of a frame** is kept by the base class;
  an abort drops it with `discard_held`.
- **Floats** are parsed with `float_of_string` and printed with
  `string_of_float`.
- **The level** uses the sum of squares over all channels, divided by the
  sample count and the channel count.

## Things that bit while reading

- The frame is 0.02 s in the tests' default configuration, not 0.04 s.
- `source.time` is clock time: inside a transition function it reads the
  time of the crossing, not a position in either track.
- The wrapper `./liquidsoap` rebuilds before running. A script that dies at
  start with logs off prints nothing: turn the log back on before
  concluding.
- Two instances in one script interleave their prints; sort by label.
