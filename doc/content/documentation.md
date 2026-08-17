# Documentation index

**How to use**: Start with the [quickstart](./quick_start.md) and make sure you
learn [how to find help](./help.md). Then it's as you like: go for another
[general tutorial](#general), or a [specific example](#specific), pick a [basic
notion](#core), or some examples from the [cookbook](./cookbook.md). If you've
understood all you need, just browse the [reference](reference.html) and compose
your dream stream.

If you downloaded a source tarball of liquidsoap, you may first read the
[build instructions](./build.md).

If you are migrating from a previous version, you might want to checkout
[this page](./migrating.md).

## General tutorials

- [The book](./book.md): The Liquidsoap book
- [Video presentations](./presentations.md): some presentations we did about liquidsoap
- [How to find help](./help.md) about operators, settings, server commands, etc.
- [Frequently Asked Questions, Troubleshooting](./faq.md)
- [Quickstart](./quick_start.md): where anyone should start.
- [Complete case analysis](./complete_case.md): an example that is not a toy.
- [Cookbook](./cookbook.md): contains lots of idiomatic examples.

## Reference

- [Script language](./language.md): A more detailed presentation.
- [Core API](reference.html): The core liquidsoap API
- [Extra API](reference-extras.html): Extra functions and libraries.
- [Protocols](protocols.html): List of protocols supported by liquidsoap.
- [Settings](settings.html): The list of available settings for liquidsoap.
- [FFmpeg](./ffmpeg.md): FFmpeg support documentation.
- [Encoding formats](./encoding_formats.md): The available formats for encoding outputs.
- [Videos streams](./video.md): Use `liquidsoap` for video streams
- [JSON import/export](./json.md): Importing and exporting language values in JSON.
- [Playlist parsers](./playlist_parsers.md): Supported playlist formats.
- [LADSPA plugins](./ladspa.md): Using LADSPA plugins.
- [Database](./database.md): Support for SQL databases.

## Core

- Basic concepts: [sources](./sources.md), [clocks](./clocks.md) and [requests](./requests.md).
- [Source composition](./composition.md): how `fallback`, `switch`, `rotate` and `random` hand over from one source to another.
- [Source callbacks](./callbacks.md): how long `on_track`, `on_metadata` and friends stay attached, and how to release them.
- [Stream contents](./stream_content.md): what kind of streams are supported, and how.
- [Script loading](./script_loading.md): load several scripts, learn about the script library.
- [Execution phases](./phases.md)

## Specific tutorials

- [Blank detection](./blank.md)
- [Customize metadata](./metadata.md)
- [Dynamic source creation](./dynamic_sources.md): dynamically create sources using server requests.
- [External decoders](./external_decoders.md): use an external program for decoding audio files.
- [External encoders](./external_encoders.md): use an external audio encoding program.
- [External streams](./external_streams.md): use an external program for streaming audio data.
- [HLS output](./hls_output.md): output your stream as HTTP Live Stream.
- [JACK audio](./jack.md): low-latency audio I/O with the JACK audio server.
- [HTTP input](./http_input.md): relay external streams.
- [Harbor input](./harbor.md): receive streams from icecast and shoutcast source clients.
- [ICY metadata update](./icy_metadata.md): manipulate and configure metadata update in Icecast.
- [Interaction with the Harbor](./harbor_http.md): interact with a running Liquidsoap using the Harbor server.
- [Interaction with the server](./server.md) interact with a running Liquidsoap instance using the telnet server.
- [Loudness Normalization](./loudness_normalization.md): normalize audio data using LUFS or ReplayGain.
- [Profiling](./profiling.md): profiling your scripts.
- [Prometheus reporting](./prometheus.md): metrics reporting via prometheus.
- [Requests-based sources](./request_sources.md): create advanced sources using requests.
- [Seek and cue support](./seek.md): seek and set cue-in and cue-out points in sources.
- [Shoutcast output](./shoutcast.md): output to shoutcast.
- [Smart crossfading](./crossfade.md): define custom crossfade transitions.
- [Using in production](./in_production.md): integrate liquidsoap scripts in a production environment.

## User scripts

- [Beets](./beets.md): an example of a music database integration.
- [Geekradio](./geekradio.md)
- [RadioPi](./radiopi.md)
- [Frequence3](./frequence3.md)
- [Video with a single static image](./video-static.md)
- [Split a CUE sheet](./split-cue.md)

## Behind the curtains

- [Some presentations and publications](publications.md) explaining the theory underlying Liquidsoap
- [OCaml API documentation](pathname:///liquidsoap/index.html) for Liquidsoap's internals and the libraries it is built on
