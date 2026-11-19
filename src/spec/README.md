# Liquidsoap specifications

What Liquidsoap MUST, SHOULD and MAY do, written to be implemented from.
The specifications live next to the code they describe and are read from
the most general to the most specific: a rule is stated once, at the most
general level where it holds, and every level below links to it.

## Project

| File                             | Content                                                               |
| -------------------------------- | --------------------------------------------------------------------- |
| [conventions.md](conventions.md) | How specifications are written: Part A/B, owners, documents, logs.    |
| [principles.md](principles.md)   | What holds everywhere, each principle with its owner.                 |
| [faults.md](faults.md)           | Keep streaming unless the error is major: fault classes and outcomes. |
| [concurrency.md](concurrency.md) | Work classes, the streaming thread, shared state, resources.          |
| [testing.md](testing.md)         | What every conformance suite does.                                    |

## Streaming model and clocks

| Tree                                              | Content                                                                                                 |
| ------------------------------------------------- | ------------------------------------------------------------------------------------------------------- |
| [core/source/spec](../core/source/spec/README.md) | Sources: the streaming cycle, the source contract, frame assembly, composition, child clocks, draining. |
| [core/clock/spec](../core/clock/spec/README.md)   | Clocks: ticks, unification, pacing and sync sources, failure, observability.                            |

## Generic seams

| Tree                                                | Content                                                              |
| --------------------------------------------------- | -------------------------------------------------------------------- |
| [core/outputs/spec](../core/outputs/spec/README.md) | The output base: states, start and stop, the per-frame loop, resets. |
| [core/encoder/spec](../core/encoder/spec/README.md) | The encoder interface every output drives.                           |

## Components

| Tree                                                                          | Content                                                        |
| ----------------------------------------------------------------------------- | -------------------------------------------------------------- |
| [core/operators/spec/switch](../core/operators/spec/switch/README.md)         | `switch`, its policies, `sequence`, `source.dynamic`, helpers. |
| [core/operators/spec/cross](../core/operators/spec/cross/README.md)           | `cross`, transitions, `cross.plan`, `crossfade`.               |
| [core/outputs/hls/spec](../core/outputs/hls/spec/README.md)                   | `output.file.hls` and its wrappers.                            |
| [core/outputs/spec/harbor](../core/outputs/spec/harbor/README.md)             | `output.harbor`.                                               |
| [core/optionals/ffmpeg/spec](../core/optionals/ffmpeg/spec/README.md)         | The FFmpeg layer: encoders, decoders, content, filters, I/O.   |
| [core/optionals/srt/spec](../core/optionals/srt/spec/README.md)               | `input.srt`, `output.srt`, `srt.socket`.                       |
| [modules/synced/ffmpeg/spec](../modules/synced/ffmpeg/spec/README.md)         | The OCaml FFmpeg bindings.                                     |
| [libs/extra/spec/icecast-server](../libs/extra/spec/icecast-server/README.md) | `icecast.server`.                                              |

A new specification tree follows [conventions.md](conventions.md), links up
for every rule of a level above, and is added to this index.

The specifications of vendored bindings under `src/modules/synced/` are
mirrored into standalone repositories with their module, so they are
self-contained and link nowhere outside it
([conventions.md §2](conventions.md#2-one-owner-per-rule)).
