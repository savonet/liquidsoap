# Documentation index

**How to use**: Start with the [quickstart](quick_start.html) and make sure you
learn [how to find help](help.html). Then it's as you like: go for another
[general tutorial](#general), or a [specific example](#specific), pick a [basic
notion](#core), or some examples from the [cookbook](cookbook.html). If you've
understood all you need, just browse the [reference](reference.html) and compose
your dream stream.

If you downloaded a source tarball of liquidsoap, you may first read the
[build instructions](build.html).

If you are migrating from a previous version, you might want to checkout
[this page](migrating.html).

## General tutorials

- [The book](bool.html): The Liquidsoap book
- [Video presentations](presentations.html): some presentations we did about liquidsoap
- [How to find help](help.html) about operators, settings, server commands, etc.
- [Frequently Asked Questions, Troubleshooting](faq.html)
- [Quickstart](quick_start.html): where anyone should start.
- [Complete case analysis](complete_case.html): an example that is not a toy.
- [Cookbook](cookbook.html): contains lots of idiomatic examples.

## Reference

- [Script language](language.html): A more detailed presentation.
- [Core API](reference.html): The core liquidsoap API
- [Extra API](reference-extra.html): Extra functions and libraries.
- [Protocols](protocols.html): List of protocols supported by liquidsoap.
- [Settings](settings.html): The list of available settings for liquidsoap.
- [FFmpeg](ffmpeg.html): FFmpeg support documentation.
- [Encoding formats](encoding_formats.html): The available formats for encoding outputs.
- [Videos streams](video.html): Use `liquidsoap` for video streams
- [JSON import/export](json.html): Importing and exporting language values in JSON.
- [Playlist parsers](playlist_parsers.html): Supported playlist formats.
- [LADSPA plugins](ladspa.html): Using LADSPA plugins.
- [Database](database.html): Support for SQL databases.

## Core

- Basic concepts: [sources](sources.html), [clocks](clocks.html) and [requests](requests.html).
- [Stream contents](stream_content.html): what kind of streams are supported, and how.
- [Script loading](script_loading.html): load several scripts, learn about the script library.
- [Execution phases](phases.html)

## Specific tutorials

- [Blank detection](blank.html)
- [Customize metadata](metadata.html)
- [Dynamic source creation](dynamic_sources.html): dynamically create sources using server requests.
- [External decoders](external_decoders.html): use an external program for decoding audio files.
- [External encoders](external_encoders.html): use an external audio encoding program.
- [External streams](external_streams.html): use an external program for streaming audio data.
- [HLS output](hls_output.html): output your stream as HTTP Live Stream.
- [HTTP input](http_input.html): relay external streams.
- [Harbor input](harbor.html): receive streams from icecast and shoutcast source clients.
- [ICY metadata update](icy_metadata.html): manipulate and configure metadata update in Icecast.
- [Interaction with the Harbor](harbor_http.html): interact with a running Liquidsoap using the Harbor server.
- [Interaction with the server](server.html) interact with a running Liquidsoap instance using the telnet server.
- [Loudness Normalization](loudness_normalization.html): normalize audio data using LUFS or ReplayGain.
- [Profiling](profiling.html): profiling your scripts.
- [Prometheus reporting](prometheus.html): metrics reporting via prometheus.
- [Requests-based sources](request_sources.html): create advanced sources using requests.
- [Seek and cue support](seek.html): seek and set cue-in and cue-out points in sources.
- [Shoutcast output](shoutcast.html): output to shoutcast.
- [Smart crossfading](smartcrossfade.html): define custom crossfade transitions.
- [Using in production](in_production.html): integrate liquidsoap scripts in a production environment.

## User scripts

- [Beets](beets.html): an example of a music database integration.
- [Geekradio](geekradio.html)
- [RadioPi](radiopi.html)
- [Frequence3](frequence3.html)
- [Video with a single static image](video-static.html)
- [Split a CUE sheet](split-cue.html)

## Code snippets

- [Code example index](scripts/index.html)

## Behind the curtains

- [Some presentations and publications](../publications.html) explaining the theory underlying Liquidsoap
- [OCaml libraries](../modules.html) used in Liquidsoap, that can be reused in other projects
- [Documentation of some internals](../modules/liquidsoap/index.html) of Liquidsoap
- [Documentation for previous versions](../previously.html)
