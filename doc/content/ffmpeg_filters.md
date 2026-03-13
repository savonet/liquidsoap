# FFmpeg filters

[FFmpeg filters](https://ffmpeg.org/ffmpeg-filters.html) provide audio and video processing using the ffmpeg library. They are available in liquidsoap when compiled with the optional [ffmpeg-avfilter](https://github.com/savonet/ocaml-ffmpeg) package.

## Filter as operators

When enabled, filters appear as operators prefixed with `ffmpeg.filter`. For example:

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

Filter inputs and outputs are abstract values of type `ffmpeg.filter.audio` and `ffmpeg.filter.video`. Create them with `ffmpeg.filter.audio.input` and `ffmpeg.filter.video.input`, which take [media tracks](multitrack.html) as input. Convert them back to tracks with `ffmpeg.filter.audio.output` and `ffmpeg.filter.video.output`.

Filters are configured inside a function closure. Here is an example:

```{.liquidsoap include="ffmpeg-filter-flanger-highpass.liq"}

```

This filter takes an audio input, wraps it in a `ffmpeg.filter.audio.input`, applies a flanger and then a high-pass effect, and returns the result as an audio output.

Here is another example for video:

```{.liquidsoap include="ffmpeg-filter-hflip.liq"}

```

This filter takes a video input, wraps it in a `ffmpeg.filter.video.input`, applies an `hflip` filter (horizontal flip), and returns the result as a video output.

## Applying filters to a source

When applying a filter, the input is placed in a clock driven by the output. This means other tracks from the input cannot be shared with the output, which can be a source of confusion.

When applying FFmpeg filters to sources with both audio and video, pass all tracks through the filter even if some are simply copied.

Here is an example:

```{.liquidsoap include="ffmpeg-filter-hflip2.liq"}

```

FFmpeg filters are powerful: they can convert audio to video (for example, to display stream information) and can be combined into complex graph-based processing pipelines.

## Filter commands

Some filters support [changing options at runtime](https://ffmpeg.org/ffmpeg-filters.html#Changing-options-at-runtime-with-a-command) via commands. This is supported in liquidsoap using a slightly different API:

```{.liquidsoap include="ffmpeg-filter-dynamic-volume.liq" to="END"}

```

A volume filter is instantiated via `ffmpeg.filter.volume.create`. The filter instance exposes `process_command`, which is used to build the `set_volume` function. The input is then applied to the filter and the pair `(s, set_volume)` is returned.

The `ffmpeg.filter.<filter>.create` API is for advanced use with filter commands. For standard use, `ffmpeg.filter.<filter>` provides a simpler interface.

## Filters with dynamic inputs or outputs

Filters with dynamic inputs or outputs determine the number of inputs or outputs at runtime. For example, `ffmpeg.filter.split` splits a video stream and `ffmpeg.filter.merge` merges multiple streams into one.

These filters have a different operator signature. Here is an example for dynamic outputs:

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

This filter returns a tuple `(audio, video)` of dynamic outputs.

For dynamic inputs:

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

This filter takes arrays of `audio` and `video` inputs.

Combined, these can be used as follows:

```{.liquidsoap include="ffmpeg-filter-parallel-flanger-highpass.liq"}

```
