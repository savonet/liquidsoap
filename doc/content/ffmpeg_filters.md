FFmpeg filters
==============

[FFmpeg filters](https://ffmpeg.org/ffmpeg-filters.html) provide audio and video filters that can be used
to transform content using the ffmpeg library. They are enabled in liquidsoap when compiled with the 
optional [ffmpeg-avfilter](https://github.com/savonet/ocaml-ffmpeg).

If enabled, the filters should appear as operators, prefixed with `ffmpeg.filter`. For instance:

```
Ffmpeg filter: Add echoing to the audio.

Type: (?in_gain : float?, ?out_gain : float?,
 ?delays : string?, ?decays : string?,
 ffmpeg.filter.graph, ffmpeg.filter.audio) ->
ffmpeg.filter.audio

Category: Liquidsoap

Parameters:

 * in_gain : float? (default: null)
     set signal input gain. (default: 0.6)

 * out_gain : float? (default: null)
     set signal output gain. (default: 0.3)

 * delays : string? (default: null)
     set list of signal delays. (default: "1000")

 * decays : string? (default: null)
     set list of signal decays. (default: "0.5")

 * (unlabeled) : ffmpeg.filter.graph (default: None)

 * (unlabeled) : ffmpeg.filter.audio (default: None)
```

Filters input and output are abstract values of type `ffmpeg.filter.audio` and `ffmpeg.filter.video`. They can be created
using `ffmpeg.filter.audio.input`, `ffmpeg.filter.video.input`. Conversely, sources can be created from them using
`ffmpeg.filter.audio.output` and `ffmpeg.filter.video.output`.

Filters are configured within the closure of a function. Here's an example:

```liquidsoap
def flanger_highpass(s) =
  def mkfilter(graph) =
    s = ffmpeg.filter.audio.input(graph, s)
    s = ffmpeg.filter.flanger(graph, s, delay=10.)
    s = ffmpeg.filter.highpass(graph, s, frequency=4000.)
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
