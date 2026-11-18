# Cross rebuild: pass 3 handoff

For the session that rebuilds `cross` from its specification. Written at the
end of the switch rebuild, from what that work established.

## Input

- Repo `/Users/toots/sources/savonet/liquidsoap`, on top of branch
  `switch-rewrite` (draft PR 5656).
- The specification of `cross`, in this directory, once passes 1 and 2 have
  produced it. [brief.md](brief.md) holds the framing.
- The contract `cross` has to meet: [../switch/](../switch/README.md).
  `ocaml-notes.md` there maps it to the code and lists where the code stops
  short.

## Approach

Decided by the maintainer for the switch rebuild, and kept here. Read the
existing code. Keep its backbone. First cut what the specification does not
need, then fix what is left until it matches. Do not rebuild blind.

When the specification is wrong or silent, fix it first, then implement.

## The contract surface, in code

All in `src/core/source/source.ml` unless stated.

| What                            | Where                                                      |
| ------------------------------- | ---------------------------------------------------------- |
| Why a source's data stopped     | `source#segment_end`, computed by the private `frame_end`  |
| Will never deliver again        | `source#exhausted`                                         |
| Never ends a track              | `source#endless`; such a source composes as live           |
| The seam                        | class `generate_from_multiple_sources`                     |
| Decide at the probe             | `choose_next_source ~boundary`                             |
| Commit at the read              | `enter_chosen_source`                                      |
| How the next data is entered    | `next_entry`, asked when the track being read ends         |
| The user moved on from a source | `leave_source`, called before the user forgets it          |
| The user reads another source   | `Clock.signal_sync_change`, raised by the seam             |
| Another cycle in the tick       | `Clock.request_segment`, asked by outputs only             |
| A skip                          | `source#request_abort`, carried out once the cycle is over |
| Profile of an operator          | `resolved_composition`                                     |

`cross` today: `get_source` does everything at the probe. Its `next_entry`
returns `` `Track `` in the `After` phase and `` `As_is `` otherwise. Its
buffers are the class `consumer` in `src/core/operators/cross.ml`.

## Rules learned the hard way

Each cost a failed run during the switch rebuild.

- **A source built at run time sits in its own clock** until it is unified
  with its reader's, and is never ready there. Whoever wakes it unifies it.
- **Profiles follow what a source plays.** An operator that has selected
  nothing yet must resolve from what it is about to play, never default to
  live: a live candidate cuts into a file track.
- **The profile that governs a leave is the one in force when the child was
  left.**
- **A switch nobody reads cannot finish a transition.** Any rule that makes
  an operator not ready until something it holds has been read out can
  deadlock. Give it a way to move on.
- **An operator over several sources cannot say whose track is its own.**
  The base class makes it declare `pause`, never `track end`. A transition
  built with `add` therefore never declares a track end by itself.
- **Opening a file's decoder detects video dimensions**, and only if nothing
  asked for the dimensions before. It has to happen at the probe.
- **Sources created during a cycle close it after their reader.** Nothing may
  depend on children closing before parents.
- **Finite tones now last exactly their duration.** Tests that relied on the
  extra frame need it stated.

## Verifying

- Build: opam switch 5.5.2, `unset PKG_CONFIG_LIBDIR`, homebrew appended to
  `PKG_CONFIG_PATH`; `dune build`.
- Run single tests only, never `@citest` or `@mediatest`.
- `dune build @test_<name>` replays a cached success. Add `--force` to
  really run it.
- A test that needs no generated file can be run directly, with a timeout:
  `liquidsoap --stdlib src/libs/stdlib.liq ../test.liq ./<name>.liq` from
  `tests/regression`.
- Test assets: clone `git@github.com:savonet/liquidsoap-test-assets.git`
  next to `test.liq`, as `tests/liquidsoap-test-assets`.
- Before calling a local failure environmental, run the same test on the
  build the work started from. `GH4828` was dismissed that way and was real.
- A check counts only if it was seen to fail: against the starting build, or
  with the fix taken out.
- Compare audio, not frames. Tests that record one item per frame (a
  checksum per frame, a plot row per frame) move whenever frame boundaries
  move. Write both builds to a WAV file and compare the samples.

## Checks that guard `cross` today

Streams: `cross`, `crossfade`, `crossfade2`, `cross_on_track`,
`cross-sequence-transition`, `cross-transition-metadata`,
`cross-transition-drops-incoming`, `cross-no-empty-metadata`,
`cross-transition-arguments`, `cross-override`, `cross-persist-override`,
`fades-overrides`, `fades-persistent-override`, `crossfade-plot`,
`autocue-plot`.

Regression: `GH4828`, `GH3239-2`, `GH5148`, `GH5581`, `GH5581-2`, `AC5109`,
`callback_counts`.

The plot images are compared under a separate alias: `dune build @genplot`.

## Project rules

- Ask before committing. Confirm the exact command before every push.
- No attribution lines on commits or on the PR.
- Never amend, never hard reset.
- Infallibility: keep, and add where missing, hard failures on contract
  violations by sources that promised it.
