# ocaml-ffmpeg

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/ffmpeg/` and will be mirrored here automatically.

![GitHub](https://img.shields.io/github/license/savonet/ocaml-ffmpeg)
![CI](https://github.com/savonet/ocaml-ffmpeg/workflows/CI/badge.svg)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/savonet/ocaml-ffmpeg)

ocaml-ffmpeg is an OCaml interface for the [FFmpeg](http://ffmpeg.org/) Multimedia framework.

Currently, it requires FFmpeg 7.0 or later to compile.

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

# Examples:

The [audio_decoding](https://github.com/savonet/ocaml-ffmpeg/blob/master/examples/audio_decoding.ml) example shows how to read frames from an audio file and convert them into bytes.

The [audio_device](https://github.com/savonet/ocaml-ffmpeg/blob/master/examples/audio_device.ml) example shows how to read 500 audio frames from an input audio device or an URL and write them into an output audio device or a file.

The [decode_audio](https://github.com/savonet/ocaml-ffmpeg/blob/master/examples/decode_audio.ml) example shows how to parse packets from a mapped file, decode them and write the resulting frames into a file.

The [demuxing_decoding](https://github.com/savonet/ocaml-ffmpeg/blob/master/examples/demuxing_decoding.ml) example shows how to demuxing and decoding audio, video and subtitle frames from a file, converts them into bytes and write them in raw files.

The [encode_audio](https://github.com/savonet/ocaml-ffmpeg/blob/master/examples/encode_audio.ml) example shows how to convert a float array into stereo frames and encode them into packets.

The [encode_video](https://github.com/savonet/ocaml-ffmpeg/blob/master/examples/encode_video.ml) example shows how to create video frames and write them encoded into a file.

The [encoding](https://github.com/savonet/ocaml-ffmpeg/blob/master/examples/encoding.ml) example shows how to create a multimedia file with audio and video streams.

The [player](https://github.com/savonet/ocaml-ffmpeg/blob/master/examples/player.ml) example shows how to read a multimedia file and write audio and video frames to output devices.

The [remuxing](https://github.com/savonet/ocaml-ffmpeg/blob/master/examples/remuxing.ml) example shows how to remuxing multimedia file packets without decoding them.

The [transcode_aac](https://github.com/savonet/ocaml-ffmpeg/blob/master/examples/transcode_aac.ml) example shows how to transcode an audio file into an AAC audio file.

The [transcoding](https://github.com/savonet/ocaml-ffmpeg/blob/master/examples/transcoding.ml) example shows how to transcode audio streams into the AAC codec, video streams into the H264 codec and write them to an output file.

# Author:

This author of this software may be contacted by electronic mail
at the following address: contact@liquidsoap.info.
