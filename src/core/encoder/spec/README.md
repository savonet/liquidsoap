# Encoders specification

What every encoder MUST, SHOULD and MAY offer the outputs that drive it,
under the [conventions](../../../spec/conventions.md) and
[principles](../../../spec/principles.md) of the project.

| File                                                           | Content                                                  |
| -------------------------------------------------------------- | -------------------------------------------------------- |
| [encoder.md](encoder.md)                                       | Part A. Registry, factories, the stream face, instances. |
| [HLS encoder seam](../../outputs/hls/spec/encoder-contract.md) | The segmenting face the HLS output drives.               |
| [FFmpeg encoder](../../optionals/ffmpeg/spec/encoder.md)       | `%ffmpeg`, answering both faces.                         |

Draining, which every encoder does at its end, is
[draining.md](../../source/spec/draining.md).

**Compatibility surface.** The `%format(...)` syntax of each encoder is its
own tree's. The interfaces here are internal and change freely.
