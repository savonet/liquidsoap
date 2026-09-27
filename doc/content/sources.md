# Understanding Sources in Liquidsoap

A Liquidsoap script describes a streaming system. The basic element of this
system is the **source**.

## What is a source?

A **source** produces a stream of media. Each source emits:

- **Frames**: small chunks of media samples.
- **Metadata**: information like artist, title, etc.
- **Track marks**: marks indicating where a track ends and the next one starts.

When Liquidsoap needs more data, it asks a source for the next frame. The source
fills the frame with audio (or video) and adds any metadata and track marks
it has.

You can combine sources, modify them, filter them or choose between them in your
script.

## Building streams from sources

The Liquidsoap language gives you **functions and operators** to build sources:

- Some functions produce **elementary sources**, for instance by reading a file
  or a microphone.
- Other operators **combine or transform** sources, for instance `fallback`,
  `random` or `crossfade`.

You build complex behaviors from simple ones. For example:

```liquidsoap
radio =
  output.icecast(
    %vorbis, mount="test.ogg",
    random([
      jingle,
      fallback([playlist1, playlist2, playlist3])
    ])
  )
```

In this script:

1. `output.icecast` sends audio to an Icecast server.
2. `output.icecast` gets its audio from a `random` source.
3. `random` picks between `jingle` and a `fallback` source.
4. `fallback` plays the first available source among the three playlists.

Each time the output needs audio, it asks the `random` source for a frame, which
asks one of its sources, and so on.

Operators like `random`, `fallback`, `switch` and `rotate` decide _when_ to hand
over from one source to another, and how the transition sounds. Each source
carries its own transition settings, so you rarely have to configure the operator
itself. See [source composition](./composition.md).

## Fallible sources

A playlist can run out of tracks, and a file can fail to load. Liquidsoap
distinguishes two kinds of sources:

- **Infallible** sources always produce data.
- **Fallible** sources may fail to produce data at some point.

Outputs expect an **infallible** source, so that your stream keeps running. When
a source is fallible, add a fallback plan to your script. For example:

- Add a static file with `single()` at the end of a `fallback()`.
- Use `mksafe()` to play silence when the source fails.

Liquidsoap checks your sources at startup and reports an error when an output
has a fallible source.

To allow failures, pass `fallible=true` to the output. The output then stops when
its source fails and starts again when the source is available.

## How streaming happens

Once your script defines its sources and outputs, a **clock** drives the data
flow.

Each source is assigned to a clock. During each clock tick (i.e. iteration),
Liquidsoap:

1. Asks each output to send a frame of data.
2. The output asks its source for the frame.
3. That source may ask other sources.
4. This chain continues until elementary sources produce the data.

This is the **streaming loop**, which runs Liquidsoap. See [clocks](./clocks.md)
for more details.

## Active sources

Most sources are passive: they compute data only when another source or an
output asks them. Some sources are **active**: Liquidsoap animates them at each
clock tick, even when no output uses their data.

For example:

- `input.harbor` receives live streams from the network.
- `input.alsa` reads from a sound card.

These sources receive data continuously, so they must consume it at each tick to
keep their buffers from overflowing.

## Keeping the streaming loop fast

The streaming loop must produce each frame in time. Liquidsoap runs expensive
tasks in background threads:

- Downloading remote files
- Reloading playlists
- Resolving requests and their metadata

Remote files, for instance files accessed through `http:` URIs, are downloaded to
a temporary file before playback begins. Local files are read directly by the
streaming loop, so files stored on a slow network file system such as NFS can
delay the loop and cause glitches.

## Next steps

- Explore the [scripting API reference](./reference.md)
- Learn about [clocks](./clocks.md)
- See how [source callbacks](./callbacks.md) hook into what a source is doing
- Experiment with your own source graphs
