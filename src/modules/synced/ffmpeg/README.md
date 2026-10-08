# ocaml-ffmpeg

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/ffmpeg/` and will be mirrored here automatically.

![GitHub](https://img.shields.io/github/license/savonet/ocaml-ffmpeg)
![CI](https://github.com/savonet/ocaml-ffmpeg/workflows/CI/badge.svg)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/savonet/ocaml-ffmpeg)

ocaml-ffmpeg is an OCaml interface for the [FFmpeg](http://ffmpeg.org/) Multimedia framework.

Currently, it requires FFmpeg 7.1 or later to compile. Only the last two major releases are intended to be supported; support for older versions may be dropped at any time.

The modules currently available are :

`Avutil` : base module containing the share types and utilities

`Avcodec` : the module containing decoders and encoders for audio, video and subtitle codecs.

`Av` : the module containing demuxers and muxers for reading and writing multimedia container formats.

`Avdevice` : the module containing input and output devices for grabbing from and rendering to many common multimedia input/output software frameworks.

`Avfilter` : the module containing audio and video filters.

`Swresample` : the module performing audio resampling, rematrixing and sample format conversion operations.

`Swscale` : the module performing image scaling and color space/pixel format conversion operations.

Please read the COPYING file before using this software.

# Documentation:

The [API documentation is available here](http://www.liquidsoap.info/ocaml-ffmpeg/).

# Prerequisites:

- ocaml
- FFmpeg
- dune
- findlib

See [dune-project](dune-project) file for versions.

# Installation:

The preferred installation method is via [opam](http://opam.ocaml.org/):

```
opam install ffmpeg
```

This will install the latest release of all ffmpeg-related modules. You can also
install individual modules, for instance:

```
opam install ffmpeg-avcodec ffmpeg-avfilter
```

If you wish to install the latest code from this repository, you can do:

```
opam install .
```

From within this repository.

# Compilation:

```
dune build
```

# Specification and tests:

The behaviour of the bindings is specified in [spec](spec/README.md). The test suite checks the bindings against it:

```
dune build @ffmpeg_citest
```

# Author:

This author of this software may be contacted by electronic mail
at the following address: contact@liquidsoap.info.
