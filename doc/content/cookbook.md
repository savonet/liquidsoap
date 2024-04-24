# Cookbook

The recipes show how to build a source with a particular feature. You can try short snippets by wrapping the code in an `output(..)` operator and passing it directly to liquidsoap:

```liquidsoap
liquidsoap -v 'output(recipe)'
```

For longer recipes, you might want to create a short script:

```liquidsoap
#!/usr/bin/liquidsoap -v

log.file.path := "/tmp/<script>.log"
log.stdout := true

recipe = # <fill this>
output(recipe)
```

See the [quickstart guide](quick_start.html) for more information on how to run [Liquidsoap](index.html), on what is this `output(..)` operator, etc.

See also the [ffmpeg cookbook](ffmpeg_cookbook.html) for examples specific to the ffmpeg support.

## Files

A source which infinitely repeats the same URI:

```{.liquidsoap include="single.liq"}

```

A source which plays a playlist of requests -- a playlist is a file with an URI per line.

```{.liquidsoap include="playlists.liq" to="END"}

```

When building your stream, you'll often need to make it unfallible. Usually, you achieve that using a fallback switch (see below) with a branch made of a safe `single`. Roughly, a single is safe when it is given a valid local audio file.

## Transcoding

[Liquidsoap](index.html) can achieve basic streaming tasks like transcoding with ease. You input any number of "source" streams using `input.http`, and then transcode them to any number of formats / bitrates / etc. The only limitation is your hardware: encoding and decoding are both heavy on CPU. If you want to get the best use of CPUs (multicore, memory footprint etc.) when encoding media with Liquidsoap, we recommend using the `%ffmpeg` encoders.

```{.liquidsoap include="transcoding.liq"}

```

## Re-encoding a file

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

## Generating CUE files

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

## RTMP server

With our [FFmpeg support](ffmpeg.html), it is possible to create a simple RTMP server with no re-encoding:

```{.liquidsoap include="rtmp.liq"}

```

## Transmitting signal

It is possible to send raw PCM signal between two instances using the [FFmpeg encoder](ffmpeg.html). Here's an example using
the SRT transport protocol:

Sender:

```{.liquidsoap include="srt-sender.liq" from="BEGIN"}

```

Receiver:

```{.liquidsoap include="srt-receiver.liq" to="END"}

```

## Scheduling

```{.liquidsoap include="fallback.liq" to="END"}

```

```{.liquidsoap include="scheduling.liq" from="BEGIN" to="END"}

```

## Generating playlists from a media library

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

The parameter of the `find` method follow the following convention:

- `artist="XXX"` looks for files where the artist tag is exactly the given one
- `artist_contains="XXX"` looks for files where the artist tag contains the
  given string as substring
- `artist_matches="XXX"` looks for files where the artist tag matches the given
  regular expression (for instance `artist_matches="(a)+.*(b)+"` looks for files
  where the artist contains an `a` followed by a `b`).

The tags for which such parameters are provided are: `artist`, `title`, `album`
and `filename` (feel free to ask if you need more).

Some numeric tags are also supported:

- `year=1999` looks for files where the year is exactly the given one
- `year_ge=1999` looks for files where the year at least the given one
- `year_lt=1999` looks for files where the year at most the given one

The following numeric tags are supported: `bpm`, `year`.

If multiple arguments are passed, the function finds files with tags matching
the conjunction of the corresponding condition.

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

## Force a file/playlist to be played at least every XX minutes

It can be useful to have a special playlist that is played at least every 20 minutes for instance (3 times per hour).
You may think of a promotional playlist for instance.
Here is the recipe:

```{.liquidsoap include="regular.liq" from="BEGIN" to="END"}

```

Where promotions is a source selecting the file to be promoted.

## Play a jingle at a fixed time

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

## Handle special events: mix or switch

Add a jingle to your normal source at the beginning of every hour:

```{.liquidsoap include="jingle-hour.liq" from="BEGIN" to="END"}

```

Switch to a live show as soon as one is available. Make the show unavailable when it is silent, and skip tracks from the normal source if they contain too much silence.

```{.liquidsoap include="switch-show.liq" from="BEGIN" to="END"}

```

Without the `track_sensitive=false` the fallback would wait the end of a track to switch to the live. When using the blank detection operators, make sure to fine-tune their `threshold` and `length` (float) parameters.

## Unix interface, dynamic requests

Liquidsoap can create a source that uses files provided by the result of the execution of any arbitrary function of your own.
This is explained in the documentation for [request-based sources](request_sources.html).

For instance, the following snippet defines a source which repeatedly plays the first valid URI in the playlist:

```{.liquidsoap include="request.dynamic.liq" to="END"}

```

Of course a more interesting behaviour is obtained with a more interesting program than `cat`, see [Beets](beet.html) for example.

Another way of using an external program is to define a new protocol which uses it to resolve URIs. `protocol.add` takes a protocol name, a function to be used for resolving URIs using that protocol. The function will be given the URI parameter part and the time left for resolving -- though nothing really bad happens if you don't respect it. It usually passes the parameter to an external program ; it is another way to integrate [Beets](beet.html), for example:

```{.liquidsoap include="beets-protocol-short.liq"}

```

When resolving the URI `beets:David Bowie`, liquidsoap will call the function, which will call `beet random -f '$path' David Bowie` which will output the path to a David Bowie song.

## Dynamic input with harbor

The operator `input.harbor` allows you to receive a source stream directly inside a running liquidsoap.

It starts a listening server on where any Icecast2-compatible source client can connect. When a source is connected, its input if fed to the corresponding source in the script, which becomes available.

This can be very useful to relay a live stream without polling the Icecast server for it.

An example can be:

```{.liquidsoap include="harbor-dynamic.liq"}

```

This script, when launched, will start a local server, here bound to "0.0.0.0". This means that it will listen on any IP address available on the machine for a connection coming from any IP address. The server will wait for any source stream on mount point "/live" to login.
Then if you start a source client and tell it to stream to your server, on port 8080, with password "hackme", the live source will become available and the radio will stream it immediately.

## Play a short silence when transitioning out of `input.harbor`

If the live connection is unstable, for instance when streaming through a roaming phone device, it can be interesting
to add an extra `5s` of silence when transitioning out of a live `input.harbor` to give the input some chance to reconnect.

This can be done with the `append` operator:

```{.liquidsoap include="append-silence.liq" to="END"}

```

## Dump a stream into segmented files

It is sometimes useful (or even legally necessary) to keep a backup of an audio
stream. Storing all the stream in one file can be very impractical. In order to
save a file per hour in wav format, the following script can be used:

```{.liquidsoap include="dump-hourly.liq" from="BEGIN"}

```

Here, the function `time.string` generates the file name by replacing `%H` by
the hour, etc. The fact that it is between curly brackets,
i.e. `{time.string(...)}`, ensures that it is re-evaluated each time a new file
is created, the changing the file name each time according to the current time.

In the following variant we write a new mp3 file each time new metadata is
coming from `s`:

```{.liquidsoap include="dump-hourly2.liq" from="BEGIN"}

```

In the two examples we use [string interpolation](language.html) and time
literals to generate the output file name.

In order to limit the disk space used by this archive, on unix systems we can
regularly call `find` to cleanup the folder; if we can to keep 31 days of
recording:

```{.liquidsoap include="archive-cleaner.liq"}

```

## Transitions

There are two kinds of transitions. Transitions between two different children of a switch or fallback and transitions between tracks of the same source.

### Switch-based transitions

The switch-based operators (`switch`, `fallback` and `random`) support transitions. For every child, you can specify a transition function computing the output stream when moving from one child to another. This function is given two `source` parameters: the child which is about to be left, and the new selected child. The default transition is `fun (a,b) -> b`, it simply relays the new selected child source.

One limitation of these transitions, however, is that if the transition happen right at the end of a track, which is the default with `track_sensitive=true`, then there is no more data available for the old source, which makes it impossible to fade it out. If that is what you are expecting, you should look at crossfade-based transitions

### Crossfade-based transitions

Crossfade-based transitions are more complex and involve buffering source data in advance to be able to compute a transition where ending and starting track potentially overlap. This does not work with all type of sources since some of them, such as `input.http` may only receive data at real-time rate and cannot be accelerated to buffer their data or else we risk running out of data.

We provide a default operator named `cross.smart` which may be suitable for most usage. But you can also create your own customized crossfade transitions. This is in particular true if you are expecting crossfade transitions between tracks of your `music` source but not between a `music` track and e.g. some jingles. Here's how to do it in this case:

```{.liquidsoap include="cross.smart.liq" from="BEGIN" to="END"}

```

## Alsa output delay

You can use [Liquidsoap](index.html) to capture and play through alsa with a minimal delay. This particularly useful when you want to run a live show from your computer. You can then directly capture and play audio through external speakers without delay for the DJ !

This configuration is not trivial since it relies on your hardware. Some hardware will allow both recording and playing at the same time, some only one at once, and some none at all.. Those note to configure are what works for us, we don't know if they'll fit all hardware.

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
