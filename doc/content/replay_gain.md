Normalization and replay gain
=============================
Normalization
-------------
If you want to have a constant average volume on an audio stream, you can use the `normalize` operator. However, this operator cannot guess the volume of the whole stream, and can be ``surprised'' by rapide changes of the volume. This can lead to a volume that is too low, too high, oscillates. In some cases, dynamic normalization also creates saturation.

To tweak the normalization, several parameters are available. These are listed and explained in the [reference](reference.html) and also visible by executing `liquidsoap -h normalize`. However, if the stream you want to normalize consist of audio files, using the replay gain technology might be a better choice.

Replay gain
-----------
[Replay gain](http://www.replaygain.org) is a proposed standard that is (more or less) respected by many open-source tools. It provides a way to obtain an overall uniform perceived loudness over a track or a set of tracks. The computation of the loudness is based on how the human ear actually perceives each range of frequency. Having computed the average perceived loudness on a track or an album, it is easy to renormalize the tracks when playing, ensuring a comfortable, consistent listening experience.

Because it is track-based, replay gain does not suffer from the typical problems of stream-based, dynamic approaches. Namely, these distort the initial audio, since they constantly adapt the amplification factor. Sometimes it oscillates too quickly in a weird audible way. Sometimes it does not adapt quickly enough, leading to under or over-amplified sections.

On the other hand, replay gain has its drawbacks. First, it requires an initial computation that is a bit costly. This computation can be done once for all for local files -- subsequent calls can then retrieve the result from the metadata. Although not impossible in theory, there is no recipe for liquidsoap to offer the same feature on remote files.

### How to use replay gain in Liquidsoap

One easy way to enable replay gain is to use the `ffmpeg` protocol:
```liquidsoap
set("protocol.ffmpeg.replaygain",true)

s = single("ffmpeg:/path/to/file.mp3")

...
```

With the `ffmpeg` protocol, files are entirely decoded to `WAV` format using the `ffmpeg` binary. When the `"protocol.ffmpeg.replaygain"` setting is set to `true`, it will also apply
the replay gain amplification while decoding.

The protocol requires `ffmpeg` in the path, which can be set via
```liquidsoap
set("protocol.ffmpeg.path","...")
```

The `ffmpeg` protocol is handy but it is also limited. For instance, it decodes and analyzes whole files, which can be a problem if you are using
large audio files. Aditionally, you might want to exclude certain files, e.g. jingles from this processing.

If you need more finer-grained control or do not wish to use the `ffmpeg` protocol, you can see the method below.

### Renormalizing according to some metadata field

The `amplify()` operator can behave according to metadata. Its `override` parameter indicates a metadata field that, when present and well-formed, overrides the amplification factor. Well formed fields are floats (e.g. `2` or `0.7`) for linear amplification factors and floats postfixed with `dB` (e.g. `-2 dB`) for logarithmic ones.

For replay gain implementation, the `amplify` operator would typically be added immediately on top of the basic tracks source, before transitions or other audio processing operators. We follow these lines in the next example, where the `replay_gain` field is used to carry the information:
```liquidsoap
list    = playlist("~/playlist")
default = single("~/default.ogg")

s = fallback([list,default])
s = amplify(1.,override="replay_gain",s)

# Here: other effects, and finally the output...
```

You may also take care of not losing the information brought by the metadata. This may be the case for instance if you
use `crossfade` before applying normalization. Hence, normalization should be done as soon as possible 
in the script, if possible just after the initial source.

### Computing and retrieving the data

In practice, the replay gain information can be found in various fields depending on the audio format and the replay gain computation tool.

Liquidsoap provides a script for extracting the replay gain value which requires `ffmpeg`.

There are at least two ways to use it in your liquidsoap script:

using the replay gain metadata resolver, or the `replay_gain` protocol.

The metadata solution is uniform: without changing anything, *all* your
files will have a new `replay_gain` metadata when the computation suceeded. However, this can be problematic,
for example, for jingles,
or if you have large files that would take a very long time
to be analyzed by replaygain tools.
The protocol solution gives you more control on when the replaygain analysis
is performed, but requires that you change some `uri` into `replay_gain:uri`.
We briefly discuss below how to do it conveniently in some typical cases.

Note that our replaygain support for remote files can be problematic.
As such, it would analyze the file after each download, which
may be uselessly costly. One should instead make sure that the file has
been analyzed on the remote machine, so that the local analysis only retrieves
the precomputed value. In any case, remote files can only be treated through
the addition of a metadata resolver, and cannot work with the `replay_gain`
protocol technique (`replaygain:ftp://host/file.ogg` will call
the script using the `ftp://host/file.ogg` as the URI parameter, and
it will fail).

The replay gain metadata resolver is not enabled by default. You can do it 
by adding the following code in your script:
```liquidsoap
enable_replaygain_metadata()
```

The `replay_gain` protocol is enabled by default.
In this case, everytime you need replaygain information about a file,
access it through your new protocol: for example,
replace `/path/to/file.mp3`
by `replay_gain:/path/to/file.mp3`.
The resolving of the protocol will trigger a call to our script,
which will return an annotated request, finally resulting in your file
with the extra `replay_gain` metadata.

Prepending `replay_gain:` is easy if you are using a script
behind some `request.dynamic.list` operator. If you are using the
`playlist` operator, you can use its `prefix` parameter.


