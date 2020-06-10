FFmpeg filters
==============

[FFmpeg filters](https://ffmpeg.org/ffmpeg-filters.html) provide audio and video filters that can be used
to transform content using the ffmpeg library. They are enabled in liquidsoap when compiled with the 
optional [ffmpeg-avfilter](https://github.com/savonet/ocaml-ffmpeg).

If enabled, the filters should appear as operators, prefixed with `ffmpeg.filter`. For instance:

```
% liquidsoap -h ffmpeg.filter.aecho

Ffmpeg filter: Add echoing to the audio.

Type: (?int_args : [string * int],
 ?float_args : [string * float],
 ?string_args : [string * string],
 ?rational_args : [string * (int * int)],
 ?flag_args : [string], ffmpeg.filter.graph,
 ffmpeg.filter.audio) -> ffmpeg.filter.audio

Category: Liquidsoap

Parameters:

 * int_args : [string * int] (default: [])

 * float_args : [string * float] (default: [])

 * string_args : [string * string] (default: [])

 * rational_args : [string * (int * int)] (default: [])

 * flag_args : [string] (default: [])

 * (unlabeled) : ffmpeg.filter.graph (default: None)

 * (unlabeled) : ffmpeg.filter.audio (default: None)
```

Each filters accept several typed argument arrays:

* `int_args` for integer parameters, e.g. `int_args=[("foo",123), ("bla", 456), ...]`
* `float_args` for floating point parameters, e.g. `int_args=[("foo",123.23), ("bla", 456.56), ...]`
* `string_args` for string parameters, e.g. `string_args=[("foo","gni"), ("bla","blo"), ...]`
* `rational_args` for rational (fraction) parameters, e.g. `rational_args=[("foo", (1,25)), ("bla", (23, 34)), ...]`
* `flag_args` for fla  parameters, e.g `flag_args=["+foo", "bla", ...]`

Filters input and output are abstract values of type `ffmpeg.filter.audio` and `ffmpeg.filter.video`. They can be created
using `ffmpeg.filter.audio.input`, `ffmpeg.filter.video.input`. Conversely, sources can be created from them using
`ffmpeg.filter.audio.output` and `ffmpeg.filter.video.output`.

Filters are configured within the closure of a function. Here's an example:

```liquidsoap
def flanger_highpass(s) =
  def mkfilter(graph) =
    s = ffmpeg.filter.audio.input(graph, s)
    s = ffmpeg.filter.flanger(graph, s, int_args=[("delay", 10)])
    s = ffmpeg.filter.highpass(graph, s, int_args=[("frequency", 4000)])
    ffmpeg.filter.audio.output(graph, s)
  end

  ffmpeg.filter.create(mkfilter)
end
```

This filter receives an audio input, creates a `ffmpeg.filter.audio.input` with it that can be passed
to filters, applies a flanger effect and then a high pass effect, creates an audio output from it and returns it.

Here's another example for video:
```liquidsoap
def hflip(s) =
  def mkfilter(graph) =
    s = ffmpeg.filter.video.input(graph, s)
    s = ffmpeg.filter.hflip(graph, s)
    ffmpeg.filter.video.output(graph, s)
  end

  ffmpeg.filter.create(mkfilter)
end
```

This filter receives a video input, creates a `ffmpeg.filter.video.input` with it that can be passed to filters,
applies a `hflip` filter (flips the video vertically), creates a video output from it and returns it.

FFmpeg filters are very powerful, they can also convert audio to video, for instance displaying information about the 
stream, and they can combined into powerful graph processing filters.

Please note that not all ffmpeg filters work with liquidsoap. For instance, frame or sample rate conversions should be avoided.
Finally, support for ffmpeg filters is fairly new. If you see any issue or missing feature with them, feel free to let us know!
