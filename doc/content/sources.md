# Understanding Sources in Liquidsoap 🎧

When you write a Liquidsoap script, you're not just stringing commands together — you're **building a streaming system**.

At the heart of that system is a powerful and abstract concept: the **source**.

## What Is a Source?

A **source** is more than just a stream of audio or video — it’s the fundamental unit of streaming in Liquidsoap. Think of it like a little engine that knows how to produce media, frame by frame.

Each source emits:

- **Frames**: Small chunks of media samples.
- **Metadata**: Information like artist, title, etc.
- **Track marks**: Signals when a track starts or ends.

At any moment, Liquidsoap can ask a source: _"Give me the next frame."_ The source responds with a small packet of sound (or video), plus any metadata if available.

This model lets you combine sources, decorate them, filter them, or choose between them — all in a script.

## Building Streams from Sources 🧱

The Liquidsoap language gives you **functions and operators** to build sources:

- Some functions produce **elementary sources** (e.g. reading from a file or microphone).
- Others **combine or transform** sources (e.g. playlists, fallbacks, crossfades, etc.).

You can build complex behaviors from simple building blocks. For example:

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

Here’s what’s happening:

1. The `output.icecast` sends audio to an Icecast server.
2. It gets that audio from a `random` source.
3. `random` picks between `jingle` or a `fallback` playlist group.
4. `fallback` plays from the first available playlist in order.

Every time the system needs audio, this little pipeline wakes up and produces a frame of data.

Operators like `random`, `fallback`, `switch` and `rotate` have to decide _when_ to
hand over from one source to another, and what that handoff sounds like. Each source
carries its own answer, so you rarely have to configure the operator itself — see
[source composition](./composition.md).

## Sources Are Not Always Reliable (And That’s Okay) ⚠️

What happens if a playlist runs out of tracks? Or a file fails to load?

In Liquidsoap, we say that a source is either:

- **Infallible**: Guaranteed to always produce data.
- **Fallible**: Might fail at some point.

To keep your stream running smoothly, your output expects an **infallible** source. That means somewhere in your source graph, you need a fallback plan.

For example:

- Add a static file with `single()` at the end of a `fallback()`.
- Use `mksafe()` to replace failures with silence.

Liquidsoap can **check the liveness** of your source graph at startup and warn you if it detects possible failure. That’s a safety net to ensure your stream doesn’t unexpectedly stop.

Want to allow failures? You can pass `fallible=true` to most output operators — but do so only if you’re okay with your stream pausing and restarting when necessary.

## How Streaming Actually Happens 🔁

Once your script defines a set of sources and outputs, how does Liquidsoap keep the data flowing?

It all comes down to a **clock**. ⏰

Each source is assigned to a clock. During each clock tick (i.e. iteration), Liquidsoap:

1. Asks the output to send a frame of data.
2. The output asks its underlying source.
3. That source might ask other sources.
4. This chain continues until some elementary sources produce real data.

This forms a **streaming loop**, and it's central to how Liquidsoap runs.

## Active Sources 🔌

Most sources are passive: they only do work when asked. But some are **active** — they need to run even if no one is listening.

For example:

- `input.harbor`: Accepts live streams from the network.
- `input.alsa`: Listens to a microphone.

These sources are **always receiving data**, so they must process it continually or risk overflowing. Even if you don’t route them to an output, Liquidsoap keeps them alive.

## Don’t Block the Stream 🛑

The streaming loop must stay fast and responsive. So, **expensive tasks are offloaded to background threads**:

- Downloading remote files
- Reloading playlists
- Checking metadata
- Polling URLs

This means:

- Don’t rely on remote files that stream directly from NFS or the web.
- All remote files are pre-downloaded into a temp file before playback begins.

Keep the streaming loop light and snappy — it’s the heartbeat of your system.

## What’s Next?

Now that you understand what sources are and how they work, you’ve unlocked the foundation of Liquidsoap. 🎉

Want to go deeper?

- Explore the [scripting API reference](reference.html)
- Learn about [clocks](./clocks.md)
- Experiment with your own custom source graphs
