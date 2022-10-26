Liquidsoap
==========
Liquidsoap is a powerful tool for building complex audio and video stream generators,
typically targeting internet radios and webtvs. It consists of a simple script language,
which has a first-class notion of source (basically a *stream*) and
provides elementary source constructors and source compositions
from which you can build the stream generator you want.
This design makes liquidsoap flexible and easily extensible.

We believe that liquidsoap is easy to use. For basic purposes, the scripts
consist of the definition of a tree of sources.
You will quickly [learn](quick_start.html)
how natural it is to use liquidsoap in such cases. The good thing is that
when you will want to make your stream more complex,
you will be able to stay in the same framework and keep a maintainable
configuration.
Of course, using some complex features might require a deeper
understanding of the concepts of [source](sources.html) and
[request](requests.html) and of our [scripting language](language.html).

We discuss below what liquidsoap is and what it is not.
If you already know that and want to get started with liquidsoap,
just jump to the [documentation index](documentation.html).
It guides you through these pages,
starting with the [quickstart tour](quick_start.html).

Liquidsoap is an open-source software
from the [Savonet](http://liquidsoap.info) project.

Features
--------
Here are a few things you can easily achieve using Liquidsoap:

* Playing from files, playlists, directories or script playlists (plays the file chosen by an external program).
* [Video streams](video.html) generation.
* Decoding/encoding using any media format supported by FFmpeg.
* Transcoding of media stream, relay of encoded media stream without re-encoding, sharing encoding to avoid encoding multiple times.
* Transparent remote file access; easy addition of file resolution protocols.
* Scheduling of many sources, depending on time, priorities, quotas, etc.
* Mixing sources on top others.
* Queuing of user requests; editable queues.
* Sound processing: compression, normalization, echo, soundtouch, etc.
* Speech and sound synthesis.
* [Metadata](metadata.html) rewriting and insertion.
* Arbitrary transitions: cross-fade, jingle insertion, custom, etc. The behaviour of the transition can be programmed to depend on metadata and average volume.
* [Input of other streams](http_input.html): useful for switching to a live show. Liquidsoap can relay an HTTP stream but also host it.
* [Blank detection](blank.html).
* Definable event handlers on new tracks, new metadata and excessive blank.
* Multiple outputs in the same instance: you can have several quality settings, use several media or even broadcast several contents from the same instance.
* Output to HLS/Icecast/Shoutcast (MP3/Ogg) or a local file (WAV/MP3/Ogg/AAC).
* Input/output via Jack, ALSA, OSS and PortAudio. Output via `libao`.
* [Interactive control](advanced.html) of many operators via Telnet or UNIX domain socket, and indirectly using scripts, graphical/web/IRC interfaces.

If you need something else, it's highly possible that you can have it -- at least by programming new sources/operators. Send us a request, we'll be happy to discuss these questions.

Non-Features
------------
Liquidsoap is a flexible tool for processing audio and video streams, that's all. We have used it for several internet radio projects, and we know that this flexibility is useful. However, an internet radio usually requires more than just an audio stream, and the other components cannot easily be built from basic primitives as we do in liquidsoap for streams. We don't have any magic solution for these, although we sometimes have some nice tools which could be adapted to various uses.

Liquidsoap itself doesn't have a nice GUI or any graphical programming environment. You'll have to write the script by hand, and the only possible interaction with a running liquidsoap is the telnet server.

Liquidsoap doesn't do any database or website stuff. It won't index your audio files, it won't allow your users to score songs on the web, etc. However, liquidsoap makes the interfacing with other tools easy, since it can call an external application (reading from the database) to get audio tracks, another one (updating last-played information) to notify that some file has been successfully played. An example of this is [Beets](beets.html), RadioPi also has a more complex system of its own along these lines.
