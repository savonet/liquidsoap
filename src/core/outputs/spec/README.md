# Outputs specification

What every output MUST, SHOULD and MAY do, under the
[conventions](../../../spec/conventions.md) and
[principles](../../../spec/principles.md) of the project. Each output has its
own tree and links here for what all outputs share.

| File / tree                                  | Content                                                                |
| -------------------------------------------- | ---------------------------------------------------------------------- |
| [output.md](output.md)                       | Part A. The output base: parameters, states, the cycle, reset, pacing. |
| [encoder spec](../../encoder/spec/README.md) | The encoder interface outputs drive.                                   |
| [shared-stream.md](shared-stream.md)         | Part A. Serving one writer's stream to readers that start mid-stream.  |
| [harbor/](harbor/README.md)                  | `output.harbor`.                                                       |
| [HLS](../hls/spec/README.md)                 | `output.file.hls` and its wrappers.                                    |
| [SRT](../../optionals/srt/spec/README.md)    | `output.srt`, with `input.srt`.                                        |

**Compatibility surface.** The common parameters, methods and callbacks of
[output.md §1](output.md#1-common-parameters-methods-and-callbacks).

## Vocabulary

The vocabulary of the [streaming model](../../source/spec/README.md#vocabulary),
plus:

- **Run**: from a start to the next stop.
- **Final stop**: the stop of going to sleep
  ([output.md §2](output.md#2-states)).
- **Piece**: the part of a frame between two metadata positions
  ([output.md §3](output.md#3-a-streaming-cycle)).
- **Reset**: stop then start, asked by the clock
  ([output.md §4](output.md#4-reset)).
