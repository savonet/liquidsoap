# Loudness Normalization

## Normalization

If you want to have a constant average volume on any audio stream, you can use the `normalize` operator. However, this operator cannot guess the volume of the whole stream, and can be "surprised" by rapid changes of the volume. This can lead to a volume that is too low, too high, oscillates. In some cases, dynamic normalization also creates saturation.

To tweak the normalization, several parameters are available. These are listed and explained in the [reference](reference.html) and also visible by executing `liquidsoap -h normalize`. However, if the stream you want to normalize consist of audio files, using the replay gain technology might be a better choice.

## Computing track loudness normalization

Instead of using the `normalize` operator, which can have jumps, it is possible to pre-compute loudness normalization per-track. This can be done using _integrated LUFS_ or _ReplayGain_. Both mechanism work the same way.

### LUFS

[LUFS (Loudness Units relative to Full Scale)](https://en.m.wikipedia.org/wiki/LUFS) is a standard for measuring perceived loudness in audio, designed to reflect how loud a track actually feels to the human ear, rather than just its peak or average levels. It's widely used to ensure consistent loudness across different media, making it especially valuable for streaming platforms, broadcast, and post-production.

LUFS loudness correction in liquidsoap is based on a track's integrated LUFS which is the average LUFS over the track. Give a track integrated LUFS, we compare it to the value defined by `settings.lufs.track_gain_target` and compute its loudness correction accordingly.

Typically, if the track's integrated LUFS is `-23 dB` and `settings.lufs.track_gain_target` is `-16 dB`, we request an amplification of `7 dB`.

LUFS is the preferred method to compute track loudness correction in liquidsoap. However, because there is no standard metadata field to store its value, unless you careful prepare your files for broadcast, the value will have to be computed on the fly, which can generate CPU spikes.

When looking for a track integrated LUFS, we first look if the metadata key defined by `settings.lufs.integrated_metadata` is available and compute it otherwise.

With the default value of `"liq_integrated_lufs"` for `settings.lufs.integrated_metadata`, this means that we look for a metadata of the form: `("liq_integrated_lufs", "-23 dB")` and, if not present, compute the value.

You may thus want to preemptively tag your files to add this metadata, typically using `ffmpeg`.

### Replay gain

[ReplayGain](https://en.wikipedia.org/wiki/ReplayGain) is a proposed standard that is (more or less) respected by many open-source tools. It provides a way to obtain an overall uniform perceived loudness over a track or a set of tracks. The computation of the loudness is based on how the human ear actually perceives each range of frequency. Having computed the average perceived loudness on a track or an album, it is easy to renormalize the tracks when playing, ensuring a comfortable, consistent listening experience.

Unlike LUFS, which is a formal loudness standard used in professional audio and broadcasting, ReplayGain is more of a consumer-level solution, primarily used in music libraries and media players. The key difference is that LUFS is based on precise loudness models defined by international standards and is required by many streaming platforms, while ReplayGain is simpler, less standardized, and not always accurate across all genres or playback systems.

However, ReplayGain has support for standardized metadata fields and can be easily pre-computed using existing tools.

### Computing or retrieving loudness correction information

The first step in order to normalize track loudness is to fetch or compute the appropriate normalization level for a given file.

There are two ways to get this information, one that works for _all_ files and one that can be enabled on a
per-file basis, if you need finer grained control over replay gain.

#### Using metadata resolvers

The metadata solution is uniform: without changing anything, _all_ your
files will have a new track gain metadata when the computation succeeds.

However, keep in mind that this computation can be costly and will be done each time a remote file is
downloaded to be prepared for streaming unless it already has the information pre-computed. For this
reason, it is recommended to pre-compute replay gain information as much as possible, specially
if you intent to stream large audio files.

We have two metadata resolvers:

- A metadata resolver using integrated LUFS, the average LUFS over the whole track
- A metadata resolver using ReplayGain data.

The LUFS metadata resolver is recommended over replaygain.

None of the two metadata resolver are enabled by default. You can do it
by adding the following code to your script:

```liquidsoap
# If you want to use lufs;
enable_lufs_track_gain_metadata()

# If you want to use replaygain
enable_replaygain_metadata()
```

#### Using protocol resolvers

If you want to control on which track you want to compute loudness correction, you can
use protocol resolvers instead.

Just as with metadata decoders, we have two protocol resolvers:

- `lufs_track_gain:uri` will compute the LUFS loudness correction for this `uri`
- `replaygain:uri` will compute the ReplayGain loudness correction for this `uri`

These protocols triggers loudness correction computation on a a per-file bases.
To use it, you prefix your request URIs with it.

For instance, replacing `/path/to/file.mp3` with `lufs_track_gain:/path/to/file.mp3`.

Prepending `lufs_track_gain:` is easy if you are using a script behind some
`request.dynamic.list` operator. If you are using the `playlist` operator,
you can use its `prefix` parameter.

Protocols can be chained, for instance:

```
annotate:foo="bar":lufs_track_gain:/path/to/file.mp3
```

### Applying loudness correction

After fetching or computing the replay gain information, the next step is to use it to correct the source's volume.

The `normalize_track_gain()` operator is used for that. This operator is a simple wrapper around the `amplify` operator
that uses the metadata defined by `settings.normalize_track_gain_metadata` to apply volume correction.

Here's a full example using integrated LUFS as metadata resolver:

```{.liquidsoap include="loudness-correction.liq" to="END"}

```
