## Introduction

You can use an external program to create a source that will read data coming out
of the standard output (`stdout`) of this program. Contrary to the external file decoders,
data will be buffered and played when a sufficient amount was accumulated.

The program should output data in signed 16 bits little endian PCM (s16le). Number of
channels and samplerate can be specified. There is no need of any wav header in the data,
though it should work too.

## Basic operator

The basic operators for creating an external stream are `input.external.wav`,
`input.external.rawaudio`, `input.external.avi` and `input.external.rawvideo`
(depending on the format of the data produced by the external program). The
parameters for the two first are

- `buffer`: Duration of the pre-buffered data.
- `max`: Maximum duration of the buffered data.
- `channels`: Number of channels.
- `samplerate`: Sample rate.
- `restart`: Restart the process when it has exited normally.
- `restart_on_error`: Restart the process when it has exited with error.

The last parameter is unlabeled. It is a string containing the command that will be executed to
run the external program.

## Wrappers

A wrapper, `input.mplayer`, is defined to use mplayer as the external decoder.
Its code is:

```{.liquidsoap include="content/liq/input.mplayer.liq"}

```
