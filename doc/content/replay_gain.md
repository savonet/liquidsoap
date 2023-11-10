# Normalization and ReplayGain

## Normalization

If you want to have a constant average volume on an audio stream, you can use the `normalize` operator. However, this operator cannot guess the volume of the whole stream, and can be "surprised" by rapid changes of the volume. This can lead to a volume that is too low, too high, oscillates. In some cases, dynamic normalization also creates saturation.

To tweak the normalization, several parameters are available. These are listed and explained in the [reference](reference.html) and also visible by executing `liquidsoap -h normalize`. However, if the stream you want to normalize consist of audio files, using the replay gain technology might be a better choice.

## Replay gain

[ReplayGain](https://en.wikipedia.org/wiki/ReplayGain) is a proposed standard that is (more or less) respected by many open-source tools. It provides a way to obtain an overall uniform perceived loudness over a track or a set of tracks. The computation of the loudness is based on how the human ear actually perceives each range of frequency. Having computed the average perceived loudness on a track or an album, it is easy to renormalize the tracks when playing, ensuring a comfortable, consistent listening experience.

Because it is track-based, replay gain does not suffer from the typical problems of stream-based, dynamic approaches. Namely, these distort the initial audio, since they constantly adapt the amplification factor. Sometimes it oscillates too quickly in a weird audible way. Sometimes it does not adapt quickly enough, leading to under or over-amplified sections.

### Computing or retrieving replay gain information

The first step in order to use replay gain is to fetch or compute the appropriate normalization level for a given file.

Replay gain information can be found in various metadata fields depending on the audio format and the replay gain computation tool.
Liquidsoap provides a script for extracting the replay gain value which requires the `ffmpeg` binary.

There are two ways to use our replain gain script, one that works for _all_ files and one that can be enabled on a
per-file basis, if you need finer grained control over replay gain.

#### Using the replay gain metadata resolver

The metadata solution is uniform: without changing anything, _all_ your
files will have a new `replaygain_track_gain` metadata when the computation succeeded.

However, keep in mind that this computation can be costly and will be done each time a remote file is
downloaded to be prepared for streaming unless it already has the information pre-computed. For this
reason, it is recommended to pre-compute replay gain information as much as possible, specially
if you intent to stream large audio files.

The replay gain metadata resolver is not enabled by default. You can do it
by adding the following code to your script:

```liquidsoap
enable_replaygain_metadata()
```

#### Using the `replaygain:` protocol

The `replaygain:` protocol triggers replay gain retrieval or computation on
a per-file bases. To use it, you prefix your request URIs with it.

For instance, replacing `/path/to/file.mp3` with `replaygain:/path/to/file.mp3`.

When resolving such a request, a call to our script will be issued,
resulting in your file having the extra `replaygain_track_gain` metadata.

Prepending `replaygain:` is easy if you are using a script
behind some `request.dynamic.list` operator. If you are using the
`playlist` operator, you can use its `prefix` parameter.

Protocols can be chained, for instance:

```
annotate:foo="bar":replaygain:/path/to/file.mp3
```

### Applying replay gain information

After fetching or computing the replay gain information, the next step is to use it to correct the source's volume.

The `amplify()` operator is used for that. This operator can be made to behave according to a given metadata, here the `replaygain` metadata. This is
done using the `override` parameter.

For replay gain implementation, the `amplify` operator would typically be added immediately on top of the basic tracks source, before transitions or other audio processing operators. Typically:

```{.liquidsoap include="content/liq/replaygain-metadata.liq" to=-1}

```

For convenience, we added the `replaygain` operator which performs the
amplification on the right metadata so that this can further be simplified to

```liquidsoap
enable_replaygain_metadata()

s = replaygain(playlist("~/playlist"))
```
