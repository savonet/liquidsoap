# FFmpeg filters

[FFmpeg filters](https://ffmpeg.org/ffmpeg-filters.html) provide audio and video filters that can be used
to transform content using the ffmpeg library. They are enabled in liquidsoap when compiled with the
optional [ffmpeg-avfilter](https://github.com/savonet/ocaml-ffmpeg).

## Filter as operators

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
using `ffmpeg.filter.audio.input`, `ffmpeg.filter.video.input`. These operators take [media tracks](multitrack.html) as input.

Conversely, tracks can be created from them using `ffmpeg.filter.audio.output` and `ffmpeg.filter.video.output`.

Filters are configured within the closure of a function. Here's an example:

```liquidsoap
def flanger_highpass(audio_track) =
  def mkfilter(graph) =
    audio_track = ffmpeg.filter.audio.input(graph, audio_track)
    audio_track = ffmpeg.filter.flanger(graph, audio_track, delay=10.)
    audio_track = ffmpeg.filter.highpass(graph, audio_track, frequency=4000.)
    ffmpeg.filter.audio.output(graph, audio_track)
  end

  ffmpeg.filter.create(mkfilter)
end
```

This filter receives an audio input, creates a `ffmpeg.filter.audio.input` with it that can be passed
to filters, applies a flanger effect and then a high pass effect, creates an audio output from it and returns it.

Here's another example for video:

```liquidsoap
def hflip(video_track) =
  def mkfilter(graph) =
    video_track = ffmpeg.filter.video.input(graph, video_track)
    video_track = ffmpeg.filter.hflip(graph, video_track)
    ffmpeg.filter.video.output(graph, video_track)
  end

  ffmpeg.filter.create(mkfilter)
end
```

This filter receives a video input, creates a `ffmpeg.filter.video.input` with it that can be passed to filters,
applies a `hflip` filter (flips the video vertically), creates a video output from it and returns it.

## Applying filters to a source

When applying a filter, the input is placed in a clock that is driven by the output. This means that you cannot share other tracks from the
input to the output. This can be an annoying source of confusion.

Thus, when applying FFMpeg filters to sources with audio and video tracks, it is recommended to pass all the tracks through the filter, even
if they are simply copied.

Here's an example with the previous filter:

```liquidsoap
def hflip(s) =
  def mkfilter(graph) =
    let { audio = audio_track, video = video_track} = source.tracks(s)

    video_track = ffmpeg.filter.video.input(graph, video_track)
    video_track = ffmpeg.filter.hflip(graph, video_track)

    audio_track = ffmpeg.filter.audio.input(graph, audio_track)
    audio_track = ffmpeg.filter.acopy(graph, audio)

    video_track = ffmpeg.filter.video.output(graph, video_track)
    audio_track = ffmpeg.filter.audio.output(graph, audio_track)

    source({
      audio = audio_track,
      video = video_track,
      metadata = track.metadata(audio_track),
      track_marks = track.track_marks(audio_track)
   })
  end

  ffmpeg.filter.create(mkfilter)
end
```

FFmpeg filters are very powerful, they can also convert audio to video, for instance displaying information about the
stream, and they can combined into powerful graph processing filters.

## Filter commands

Some filters support [changing options at runtime](https://ffmpeg.org/ffmpeg-filters.html#Changing-options-at-runtime-with-a-command) with a command. These are also
supported in liquidsoap.

In order to do so, you have to use a slightly different API:

```liquidsoap
def dynamic_volume(s) =
  def mkfilter(graph) =
    filter = ffmpeg.filter.volume.create(graph)

    def set_volume(v) =
      ignore(filter.process_command("volume", "#{v}"))
    end

    let {audio = audio_track} = source.tracks(s)

    audio_track = ffmpeg.filter.audio.input(graph, audio_track)
    filter.set_input(audio_track)
    audio_track = filter.output
    audio_track = ffmpeg.filter.audio.output(graph, audio_track)

    s = source({
      audio = audio_track,
      metadata = track.metadata(audio_track),
      track_marks = track.track_marks(audio_track)
    }

    (s, set_volume)
  end

  ffmpeg.filter.create(mkfilter)
end

let (s, set_volume) = dynamic_volume(s)
```

First, we instantiate a volume filter via `ffmpeg.filter.volume.create`. The filter instance has a `process_command`, which we use to create the `set_volume` function. Then,
we apply the expected input to the filter and return the pair `(s, set_volume)` of source and function.

The `ffmpeg.filter.<filter>.create` API is intended for advanced use if you want to use filter commands. Otherwise, `ffmpeg.filter.<filter>` provides a more straight forward
API to filters.

## Filters with dynamic inputs or outputs

Filters with dynamic inputs or outputs can have multiple inputs or outputs, decided at run-time. Typically, `ffmpeg.filter.split`
splits a video stream into multiple streams and `ffmpeg.filter.merge` merges multiple video streams into a single one.

For these filters, the operators' signature is a little different. Here's an example for dynamic outputs:

```
% liquidsoap -h ffmpeg.filter.asplit

Ffmpeg filter: Pass on the audio input to N audio outputs. This filter has
dynamic outputs: returned value is a tuple of audio and video outputs. Total
number of outputs is determined at runtime.

Type: (?outputs : int?, ffmpeg.filter.graph,
 ffmpeg.filter.audio) ->
[ffmpeg.filter.audio] * [ffmpeg.filter.video]

Category: Liquidsoap
Flag: extra

Parameters:

 * outputs : int? (default: null)
     set number of outputs. (default: 2)

 * (unlabeled) : ffmpeg.filter.graph (default: None)

 * (unlabeled) : ffmpeg.filter.audio (default: None)
```

This filter returns a tuple `(audio, video)` of possible dynamic outputs.

Likewise, with dynamic inputs:

```
% liquidsoap -h ffmpeg.filter.amerge

Ffmpeg filter: Merge two or more audio streams into a single multi-channel
stream. This filter has dynamic inputs: last two arguments are lists of audio
and video inputs. Total number of inputs is determined at runtime.

Type: (?inputs : int?, ffmpeg.filter.graph,
 [ffmpeg.filter.audio], [ffmpeg.filter.video]) ->
ffmpeg.filter.audio

Category: Liquidsoap
Flag: extra

Parameters:

 * inputs : int? (default: null)
     specify the number of inputs. (default: 2)

 * (unlabeled) : ffmpeg.filter.graph (default: None)

 * (unlabeled) : [ffmpeg.filter.audio] (default: None)

 * (unlabeled) : [ffmpeg.filter.video] (default: None)
```

This filter receives an array of possible `audio` inputs as well as an array of possible `video` inputs.

Put together, this can be used as such:

```liquidsoap
def parallel_flanger_highpass(s) =
  def mkfilter(graph) =
    audio_track = ffmpeg.filter.audio.input(graph, audio_track)

    let (audio, _) = ffmpeg.filter.asplit(outputs=2, graph, audio_track)

    let [a1, a2] = audio

    a1 = ffmpeg.filter.flanger(graph, a1, delay=10.)
    a2 = ffmpeg.filter.highpass(graph, a2, frequency=4000.)

    # For some reason, we need to enforce the format here.
    a1 = ffmpeg.filter.aformat(sample_fmts="s16", sample_rates="44100", channel_layouts="stereo", graph, a1)
    a2 = ffmpeg.filter.aformat(sample_fmts="s16", sample_rates="44100", channel_layouts="stereo", graph, a2)

    audio_track = ffmpeg.filter.amerge(inputs=2, graph, [a1, a2], [])

    ffmpeg.filter.audio.output(graph, audio_track)
  end

  ffmpeg.filter.create(mkfilter)
end
```
