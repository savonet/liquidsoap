# Cookbook

The recipes show how to build a source with a particular feature. You can try short snippets by wrapping the code in an `output(..)` operator and passing it directly to liquidsoap:

```liquidsoap
liquidsoap -v 'output(recipe)'
```

For longer recipes, you might want to create a short script:

```liquidsoap
#!/usr/bin/liquidsoap -v

recipe = # <fill this>
output(recipe)
```

See the [quickstart guide](quick_start.html) for more information on how to run [Liquidsoap](index.html), on what is this `output(..)` operator, etc.

See also the [ffmpeg cookbook](ffmpeg_cookbook.html) for examples specific to the ffmpeg support.

## Contents

- [Sources](#sources)
  - [Single file](#single-file)
  - [Playlists](#playlists)
  - [Generating playlists from a media library](#generating-playlists-from-a-media-library)
  - [Dynamic requests](#dynamic-requests)
  - [Dynamic input with harbor](#dynamic-input-with-harbor)
- [Scheduling](#scheduling)
  - [Fallback and scheduling](#fallback-and-scheduling)
  - [Periodic playlists](#periodic-playlists)
  - [Play a jingle at a fixed time](#play-a-jingle-at-a-fixed-time)
  - [Mixing and switching](#mixing-and-switching)
- [Transitions](#transitions)
- [Encoding and streaming](#encoding-and-streaming)
  - [Transcoding](#transcoding)
  - [Re-encoding a file](#re-encoding-a-file)
  - [RTMP server](#rtmp-server)
  - [Transmitting signal](#transmitting-signal)
- [Recording](#recording)
  - [Generating CUE files](#generating-cue-files)
  - [Dumping a stream into segmented files](#dumping-a-stream-into-segmented-files)
- [Hardware](#hardware)
  - [ALSA output delay](#alsa-output-delay)

## Sources

### Single file

A source which infinitely repeats the same URI:

```{.liquidsoap include="single.liq"}

```

### Playlists

A source which plays a playlist of requests -- a playlist is a file with a URI per line.

```{.liquidsoap include="playlists.liq" to="END"}

```

When building your stream, you'll often need to make it infallible. Usually, you achieve that using a fallback switch (see [Scheduling](#scheduling)) with a branch made of a safe `single`. Roughly, a single is safe when it is given a valid local audio file.

### Generating playlists from a media library

In order to store all the metadata of the files in a given directory and use
those to generate playlists, you can use the `medialib` operator which takes as
argument the directory to index. On first run, it will index all the files of
the given folder, which can take some time (you are advised to use the
`persistency` parameter in order to specify a file where metadata will be
stored to avoid reindexing at each run). The resulting object can then
be queried with the `find` method in order to return all files matching the given
conditions and thus generate a playlist:

```{.liquidsoap include="medialib.liq"}

```

The parameters of the `find` method follow this convention:

- `artist="XXX"` looks for files where the artist tag is exactly the given one
- `artist_contains="XXX"` looks for files where the artist tag contains the
  given string as substring
- `artist_matches="XXX"` looks for files where the artist tag matches the given
  regular expression (for instance `artist_matches="(a)+.*(b)+"` looks for files
  where the artist contains an `a` followed by a `b`).

The tags for which such parameters are provided are: `artist`, `title`, `album`
and `filename`.

Some numeric tags are also supported:

- `year=1999` looks for files where the year is exactly the given one
- `year_ge=1999` looks for files where the year is at least the given one
- `year_lt=1999` looks for files where the year is at most the given one

The following numeric tags are supported: `bpm`, `year`.

If multiple arguments are passed, the function finds files with tags matching
the conjunction of the corresponding conditions.

Finally, if you need more exotic search functions, the argument `predicate` can
be used. It takes as argument a _predicate_ which is a function taking the
metadata of a file and returning whether the file should be selected. For
instance, the following looks for files where the name of the artist is of
length 5:

```{.liquidsoap include="medialib-predicate.liq" from="BEGIN" to="END"}

```

The default implementation of `medialib` uses standard Liquidsoap functions and
can be pretty expensive in terms of memory. A more efficient implementation is
available if you compiled with support for sqlite3 databases. In this case, you
can use the `medialib.sqlite` operator as follows:

```{.liquidsoap include="medialib.sqlite.liq"}

```

(we also support more advanced uses of [databases](database.html)).

### Dynamic requests

Liquidsoap can create a source that uses files provided by the result of the execution of any arbitrary function of your own.
This is explained in the documentation for [request-based sources](request_sources.html).

For instance, the following snippet defines a source which repeatedly plays the first valid URI in the playlist:

```{.liquidsoap include="request.dynamic.liq" to="END"}

```

Of course a more interesting behaviour is obtained with a more interesting program than `cat`, see [Beets](beet.html) for example.

Another way of using an external program is to define a new protocol which uses it to resolve URIs. `protocol.add` takes a protocol name, a function to be used for resolving URIs using that protocol. The function will be given the URI parameter part and the time left for resolving -- though nothing really bad happens if you don't respect it. It usually passes the parameter to an external program; it is another way to integrate [Beets](beet.html), for example:

```{.liquidsoap include="beets-protocol-short.liq"}

```

When resolving the URI `beets:David Bowie`, liquidsoap will call the function, which will call `beet random -f '$path' David Bowie` which will output the path to a David Bowie song.

### Dynamic input with harbor

The operator `input.harbor` allows you to receive a source stream directly inside a running liquidsoap.

It starts a listening server where any Icecast2-compatible source client can connect. When a source connects, its input is fed to the corresponding source in the script, which becomes available.

This can be very useful to relay a live stream without polling the Icecast server for it.

```{.liquidsoap include="harbor-dynamic.liq"}

```

This script, when launched, will start a local server bound to `"0.0.0.0"`, meaning it will listen on all available network interfaces. The server will wait for a source stream to connect on mount point `/live`. If you start a source client streaming to your server on port 8080 with password `"hackme"`, the live source will become available and the radio will stream it immediately.

If the live connection is unstable — for instance when streaming through a roaming phone device — it can be useful to add a short silence when transitioning out of the live input to give it a chance to reconnect:

```{.liquidsoap include="append-silence.liq" to="END"}

```

## Scheduling

### Fallback and scheduling

```{.liquidsoap include="fallback.liq" to="END"}

```

```{.liquidsoap include="scheduling.liq" from="BEGIN" to="END"}

```

### Periodic playlists

It can be useful to have a special playlist that is played at least every 20 minutes (3 times per hour), such as a promotional playlist:

```{.liquidsoap include="regular.liq" from="BEGIN" to="END"}

```

where `promotions` is a source selecting the file to be promoted.

### Play a jingle at a fixed time

Suppose that we have a playlist `jingles` of jingles and we want to play one
within the 5 first minutes of every hour, without interrupting the current
song. We can think of doing something like

```{.liquidsoap include="fixed-time1.liq" from="BEGIN" to="END"}

```

but the problem is that it is likely to play many jingles. In order to play
exactly one jingle, we can use the function `predicate.activates` which detects
when a predicate (here `{ 0m-5m }`) becomes true:

```{.liquidsoap include="fixed-time2.liq" from="BEGIN" to="END"}

```

### Mixing and switching

Add a jingle to your normal source at the beginning of every hour:

```{.liquidsoap include="jingle-hour.liq" from="BEGIN" to="END"}

```

Switch to a live show as soon as one is available. Make the show unavailable when it is silent, and skip tracks from the normal source if they contain too much silence.

```{.liquidsoap include="switch-show.liq" from="BEGIN" to="END"}

```

Without `track_sensitive=false` the fallback would wait for the end of a track before switching to the live. When using the blank detection operators, make sure to fine-tune their `threshold` and `length` (float) parameters.

## Transitions

Crossfade-based transitions buffer source data in advance to compute a transition where the ending and starting tracks potentially overlap. This does not work with all sources — for instance, `input.http` may only receive data at real-time rate and cannot be accelerated to buffer data without risking running out.

The `cross.simple` operator provides a ready-to-use crossfade transition suitable for most cases. You can also create your own custom crossfade transitions — for example, if you want crossfades between tracks of your `music` source but not between a `music` track and jingles. Here's how:

```{.liquidsoap include="cross.custom.liq" from="BEGIN" to="END"}

```

## Encoding and streaming

### Transcoding

[Liquidsoap](index.html) can achieve basic streaming tasks like transcoding with ease. You input any number of "source" streams using `input.http`, and then transcode them to any number of formats / bitrates / etc. The only limitation is your hardware: encoding and decoding are both heavy on CPU. If you want to get the best use of CPUs (multicore, memory footprint etc.) when encoding media with Liquidsoap, we recommend using the `%ffmpeg` encoders.

```{.liquidsoap include="transcoding.liq"}

```

### Re-encoding a file

As a simple example using a fallible output, we shall consider
re-encoding a file.
We start by building a source that plays our file only once.
That source is obviously fallible.
We pass it to a file output, which has to be in fallible mode.
We also disable the `sync` parameter on the source's clock,
to encode the file as quickly as possible.
Finally, we use the `on_stop` handler to shutdown
liquidsoap when streaming is finished.

```{.liquidsoap include="re-encode.liq"}

```

### RTMP server

With our [FFmpeg support](ffmpeg.html), it is possible to create a simple RTMP server with no re-encoding:

```{.liquidsoap include="rtmp.liq"}

```

### Transmitting signal

It is possible to send raw PCM signals between two instances using the [FFmpeg encoder](ffmpeg.html). Here's an example using the SRT transport protocol:

Sender:

```{.liquidsoap include="srt-sender.liq" from="BEGIN"}

```

Receiver:

```{.liquidsoap include="srt-receiver.liq" to="END"}

```

## Recording

### Generating CUE files

When making backups of streams in audio files, it can be useful to generate CUE
files, which store the times where the various tracks occur along with their
metadata (those could then be used later on to split the file for
instance). This can be achieved using the `source.cue` operator:

```{.liquidsoap include="source-cue.liq" from="BEGIN" to="END"}

```

which will generate a CUE file of the following form

```
TITLE "My stream"
PERFORMER "The performer"
FILE "backup.mp3" MP3
  TRACK 01 AUDIO
    TITLE "Title 1"
    PERFORMER "Artist 1"
    INDEX 01 00:00:00
  TRACK 02 AUDIO
    TITLE "Title 2"
    PERFORMER "Artist 2"
    INDEX 01 01:12:67
```

### Dumping a stream into segmented files

It is sometimes useful (or even legally necessary) to keep a backup of an audio
stream. Storing all the stream in one file can be very impractical. In order to
save a file per hour in wav format, the following script can be used:

```{.liquidsoap include="dump-hourly.liq" from="BEGIN"}

```

Here, the function `time.string` generates the file name by replacing `%H` by
the hour, etc. The fact that it is between curly brackets,
i.e. `{time.string(...)}`, ensures that it is re-evaluated each time a new file
is created, changing the file name each time according to the current time.

In the following variant we write a new mp3 file each time new metadata is
coming from `s`:

```{.liquidsoap include="dump-hourly2.liq" from="BEGIN"}

```

In the two examples we use [string interpolation](language.html) and time
literals to generate the output file name.

In order to limit the disk space used by this archive, on unix systems we can
regularly call `find` to clean up the folder; if we want to keep 31 days of
recordings:

```{.liquidsoap include="archive-cleaner.liq"}

```

## Hardware

### ALSA output delay

You can use [Liquidsoap](index.html) to capture and play through ALSA with minimal delay. This is particularly useful when running a live show from your computer, allowing you to capture and play audio through external speakers without audible delay.

This configuration is not trivial since it depends on your hardware. Some hardware allows both recording and playing at the same time, some only one at once, and some none at all. These notes describe what works for us — your mileage may vary.

First launch liquidsoap as a one line program

```
liquidsoap -v --debug 'input.alsa()'
```

Unless you're lucky, the logs are full of lines like the following:

```
Could not set buffer size to 'frame.size' (1920 samples), got 2048.
```

The solution is then to set liquidsoap's internal frame size to this value, which is most likely specific to your hardware. Let's try this script:

```{.liquidsoap include="frame-size.liq"}

```

The setting will be acknowledged in the log as follows:

```
Targeting 'frame.audio.size': 2048 audio samples = 2048 ticks.
```

If everything goes right, you may hear on your output the captured sound without any delay!

If you experience problems it might be a good idea to double the value of the frame size. This increases stability, but also latency.
