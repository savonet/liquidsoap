# Understanding Requests and Protocols in Liquidsoap

When you ask Liquidsoap to play something, it’s easy to imagine that the system just grabs your file or stream and starts playing it. In reality, there’s a small but important process that happens first — one that makes Liquidsoap flexible and able to handle a wide variety of media sources.

This page explains how **requests** work, how Liquidsoap resolves them, and how the **protocol system** fits in. The goal is to give you a clear picture of what’s going on without overwhelming you, so you can better understand what happens behind the scenes.

## What’s a Request? 🎫

A **request** in Liquidsoap is simply a way of saying:

> “Here’s something to play — figure out where it is and how to handle it.”

You create a request with [`request.create`](reference.html#request.create), passing a **URI**.

A URI can be:

- A file path: `"/music/song.mp3"` 📁
- A URL: `"https://example.com/song.ogg"` 🌐
- A custom protocol: `"media:12345"` 🛠

A request on its own doesn’t play anything — it needs to be used by a **request-based source** such as `playlist`, `request.dynamic`, or `single`. That source will try to resolve and decode it before playback.

## The Request Lifecycle 🔄

Here’s the general path from URI to playback:

1. **Request creation** — you pass a URI to Liquidsoap.
2. **Protocol resolution** — Liquidsoap figures out how to handle that URI.
3. **Chained resolution** — some protocols return another URI, which might trigger more resolutions.
4. **Local file reached** — the resolution process ends with a file on disk.
5. **Decoder selection** — the file is matched with a decoder that can read its contents.
6. **Playback** — the decoded stream is sent to the source for output. 🔊

💡 **Note:** A request can resolve to a local file but still be unplayable — for example, if the file’s format doesn’t match what the source expects.

## Protocols: How URIs Are Resolved 🗂

A **protocol** in Liquidsoap is a handler for a particular kind of URI.

The general format is:

```
protocol_name:arguments
```

When a request’s URI matches a known protocol name:

- The protocol runs and returns either another URI or a local file.
- If it returns another URI, resolution continues.
- This process can repeat several times until a local file is found.

**Example:**

```
annotate:title="My Song":http://example.com/song.mp3
```

Here’s what happens:

1. The `annotate` protocol adds metadata 🏷.
2. The `http` protocol downloads the file 🌐.
3. The system now has a local file and can look for a decoder.

## The Role of External Decoders 📦

Once a local file is available, Liquidsoap checks for an **external decoder** based on its MIME type or file extension. These decoders can be optionally configured in your script to extend Liquidsoap's supported file formats.

If an external decoder is found, it’s used to read the file and produce the final, playable file.

## Creating Your Own Protocols 🛠

Liquidsoap lets you define custom protocol handlers.

For example, you could create a `media:` protocol like:

```
media:123
```

that looks up a database record and returns the corresponding file path. This can make working with large or structured media collections much easier.

## The `annotate:` Protocol 🏷

The built-in `annotate:` protocol lets you attach metadata directly to a request without changing the file.

The syntax is:

```
annotate:key1="value1",key2="value2":next_uri
```

**Example:**

```
annotate:artist="Liquid Artist",cue_in="3.",cue_out="23.":/music/song.mp3
```

### Cue Points ⏩

Two useful metadata keys are:

- `cue_in` — skip the first _N_ seconds of playback.
- `cue_out` — stop after _N_ seconds from the start (including any skipped portion).

In the example above:

- `cue_in="3."` skips the first 3 seconds.
- `cue_out="23."` stops playback at second 23, so only 20 seconds are heard.

This is particularly useful with pre-processed tracks where intros or outros have been identified.

## Other Built-in Protocols

- **`autocue`** 🎯 — Automatically detects cue points and fades for smooth transitions. See the [autocue](autocue.html) documentation for details.
- **`http` / `https`** 🌐 — Handles web-based media by downloading it before playback, just like any other protocol.
- **`say`** 🗣 — Generates speech from text when configured, useful for automated announcements or DJ-style breaks.

## Why It’s Useful 💡

Understanding requests and protocols isn’t just theoretical — it gives you practical tools to:

- Work with many different kinds of media sources.
- Build custom workflows for databases, APIs, or generated content.
- Control playback behavior with metadata, without touching the original files.

It’s one of the features that makes Liquidsoap adaptable to many streaming setups.

## Next Steps 📚

To explore further:

- Read the [Protocol API documentation](protocols-presentation.html) to learn how to implement your own.
- Experiment with `annotate:` in your playlists.
- Try chaining protocols to create more complex behavior.
