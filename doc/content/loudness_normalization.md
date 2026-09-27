# Loudness Normalization

## Normalization

If you want to have a constant average volume on any audio stream, you can use the `normalize` operator. However, this operator cannot guess the volume of the whole stream, and can be "surprised" by rapid changes of the volume. This can lead to a volume that is too low, too high, or oscillates. In some cases, dynamic normalization also creates saturation.

To tweak the normalization, several parameters are available. These are listed and explained in the [reference](./reference.md) and also visible by executing `liquidsoap -h normalize`. However, if the stream you want to normalize consists of audio files, using the replay gain technology might be a better choice.

## Computing track loudness normalization

Instead of using the `normalize` operator, which can have jumps, it is possible to pre-compute loudness normalization per-track. This can be done using _integrated LUFS_ or _ReplayGain_. Both mechanisms work the same way.

### LUFS

[LUFS (Loudness Units relative to Full Scale)](https://en.m.wikipedia.org/wiki/LUFS) is a standard for measuring perceived loudness in audio. It measures how loud a track sounds to the human ear, which can differ from its peak or average signal level. Broadcasters and streaming platforms use it to keep a consistent loudness across programs.

LUFS loudness correction in liquidsoap is based on a track's integrated LUFS which is the average LUFS over the track. Given a track's integrated LUFS, we compare it to the value defined by `settings.lufs.track_gain_target` and compute its loudness correction accordingly.

Typically, if the track's integrated LUFS is `-23 dB` and `settings.lufs.track_gain_target` is `-16 dB`, we request an amplification of `7 dB`.

LUFS is the preferred method to compute track loudness correction in liquidsoap. However, because there is no standard metadata field to store its value, unless you carefully prepare your files for broadcast, the value will have to be computed on the fly, which can generate CPU spikes.

When looking for a track's integrated LUFS, we first look if the metadata key defined by `settings.lufs.integrated_metadata` is available and compute it otherwise.

With the default value of `"liq_integrated_lufs"` for `settings.lufs.integrated_metadata`, this means that we look for a metadata of the form: `("liq_integrated_lufs", "-23 dB")` and, if not present, compute the value.

You may thus want to preemptively tag your files to add this metadata, typically using `ffmpeg`.

### Replay gain

[ReplayGain](https://en.wikipedia.org/wiki/ReplayGain) is a proposed standard that is (more or less) respected by many open-source tools. It provides a way to obtain an overall uniform perceived loudness over a track or a set of tracks. The computation of the loudness is based on how the human ear perceives each range of frequency. Once the average perceived loudness of a track or an album is computed, the player adjusts the volume of each track so that all tracks play at the same loudness.

ReplayGain is mostly used by music libraries and media players. LUFS is defined by international broadcasting standards (EBU R 128, ITU-R BS.1770) and is the measure required by many streaming platforms.

ReplayGain values are stored in standard metadata fields, and many existing tools can pre-compute them.

### Computing or retrieving loudness correction information

The first step in order to normalize track loudness is to fetch or compute the appropriate normalization level for a given file.

There are two ways to get this information, one that works for _all_ files and one that can be enabled on a
per-file basis, if you need finer grained control over replay gain.

#### Using metadata resolvers

The metadata solution is uniform: without changing anything, _all_ your
files will have a new track gain metadata when the computation succeeds.

However, keep in mind that this computation can be costly and will be done each time a remote file is
downloaded to be prepared for streaming unless it already has the information pre-computed. For this
reason, it is recommended to pre-compute replay gain information as much as possible, especially
if you intend to stream large audio files.

We have two metadata resolvers:

- A metadata resolver using integrated LUFS, the average LUFS over the whole track
- A metadata resolver using ReplayGain data.

The LUFS metadata resolver is recommended over replaygain.

Neither of the two metadata resolvers is enabled by default. You can do it
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

Just as with metadata resolvers, we have two protocol resolvers:

- `lufs_track_gain:uri` will compute the LUFS loudness correction for this `uri`
- `replaygain:uri` will compute the ReplayGain loudness correction for this `uri`

These protocols trigger loudness correction computation on a per-file basis.
To use them, prefix your request URIs with the protocol name.

For instance, replacing `/path/to/file.mp3` with `lufs_track_gain:/path/to/file.mp3`.

Prepending `lufs_track_gain:` is easy if you are using a script behind some
`request.dynamic` operator. If you are using the `playlist` operator,
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
