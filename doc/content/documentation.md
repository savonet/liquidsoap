# Documentation index

**How to use**: Start with the [quickstart](./quick_start.md) and make sure you
learn [how to find help](./help.md). Then it's as you like: go for another
[general tutorial](#general-tutorials), or a [specific example](#specific-tutorials), pick a [basic
notion](#core), or some examples from the [cookbook](./cookbook.md). If you've
understood all you need, just browse the [reference](./reference.md) and compose
your dream stream.

To install liquidsoap, see the [installation instructions](./install.md). If
you downloaded a source tarball of liquidsoap, you may first read the
[build instructions](./build.md).

If you are migrating from a previous version, you might want to check out
[this page](./migrating.md).

## General tutorials

- [The book](./book.md): The Liquidsoap book
- [Video presentations](./presentations.md): some presentations we did about liquidsoap
- [How to find help](./help.md) about operators, settings, server commands, etc.
- [Frequently Asked Questions, Troubleshooting](./faq.md)
- [Quickstart](./quick_start.md): where anyone should start.
- [Complete case analysis](./complete_case.md): an example that is not a toy.
- [Cookbook](./cookbook.md): contains lots of idiomatic examples.
- [FFmpeg cookbook](./ffmpeg_cookbook.md): examples specific to the FFmpeg support.

## Reference

- [Script language](./language.md): A more detailed presentation.
- [Core API](./reference.md): The core liquidsoap API
- [Extra API](./reference-extras.md): Extra functions and libraries.
- [Protocols](./protocols.md): List of protocols supported by liquidsoap.
- [Settings](./settings.md): The list of available settings for liquidsoap.
- [FFmpeg](./ffmpeg.md): FFmpeg support documentation.
- [FFmpeg encoder](./ffmpeg_encoder.md) and [FFmpeg filters](./ffmpeg_filters.md): encoding and filtering with FFmpeg.
- [Encoding formats](./encoding_formats.md): The available formats for encoding outputs.
- [Video streams](./video.md): Use `liquidsoap` for video streams
- [JSON import/export](./json.md): Importing and exporting language values in JSON.
- [YAML import/export](./yaml.md) and [XML import/export](./xml.md): Importing and exporting language values in YAML and XML.
- [Strings encoding](./strings_encoding.md): How liquidsoap converts strings to UTF-8.
- [Playlist parsers](./playlist_parsers.md): Supported playlist formats.
- [LADSPA plugins](./ladspa.md): Using LADSPA plugins.
- [Database](./database.md): Support for SQL databases.

## Core

- Basic concepts: [sources](./sources.md), [clocks](./clocks.md) and [requests](./requests.md).
- [How scripts run](./script_lifecycle.md): script loading, typechecking and caching, execution phases and the streaming loop.
- [Source composition](./composition.md): how `fallback`, `switch`, `rotate` and `random` hand over from one source to another.
- [Source callbacks](./callbacks.md): how long `on_track`, `on_metadata` and friends stay attached, and how to release them.
- [Stream contents](./stream_content.md): what kind of streams are supported, and how.
- [Multitrack](./multitrack.md): streams with several audio or video tracks.
- [Memory usage](./memory.md): what uses memory in liquidsoap and how to reduce it.

## Specific tutorials

- [Blank detection](./blank.md)
- [Customize metadata](./metadata.md)
- [Dynamic source creation](./dynamic_sources.md): dynamically create sources using server requests.
- [External programs](./external_programs.md): decode files and encode streams with external programs.
- [HLS output](./hls_output.md): output your stream as HTTP Live Stream.
- [JACK audio](./jack.md): low-latency audio I/O with the JACK audio server.
- [Streaming to Icecast and Shoutcast](./icecast.md): send your stream to a server, and update its ICY metadata.
- [Icecast server emulation](./icecast_server.md): serve streams to listeners directly from liquidsoap.
- [Harbor input and output](./harbor.md): receive streams from icecast and shoutcast source clients, serve them, and relay remote streams.
- [Interaction with the Harbor](./harbor_http.md): interact with a running Liquidsoap using the Harbor server.
- [Interaction with the server](./server.md): interact with a running Liquidsoap instance using the telnet server.
- [Loudness Normalization](./loudness_normalization.md): normalize audio data using LUFS or ReplayGain.
- [Performance and monitoring](./performance.md): latency, profiling and Prometheus metrics.
- [Scheduling tasks](./scheduling.md): run functions at given times or intervals.
- [Seek and cue support](./seek.md): seek and set cue-in and cue-out points in sources.
- [Smart crossfading](./crossfade.md): define custom crossfade transitions.
- [Stereo Tool](./stereotool.md): process audio with Stereo Tool.
- [Subtitles](./subtitles.md): handle subtitle tracks.
- [Using in production](./in_production.md): integrate liquidsoap scripts in a production environment.

## User scripts

- [Beets](./beets.md): an example of a music database integration.
- [Split a CUE sheet](./split-cue.md)

## Behind the curtains

- [Some presentations and publications](./publications.md) explaining the theory underlying Liquidsoap
- [OCaml API documentation](pathname:///liquidsoap/index.html) for Liquidsoap's internals and the libraries it is built on
