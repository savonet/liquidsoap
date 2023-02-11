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

```liquidsoap
single("/my/default.ogg")
```

A source which plays a playlist of requests -- a playlist is a file with an URI per line.

```liquidsoap
# Shuffle, play every URI, start over.
playlist("/my/playlist.txt")
# Do not randomize
playlist(mode="normal", "/my/pl.m3u")
# The playlist can come from any URI,
# can be reloaded every 10 minutes.
playlist(reload=600,"http://my/playlist.txt")
```

When building your stream, you'll often need to make it unfallible. Usually, you achieve that using a fallback switch (see below) with a branch made of a safe `single`. Roughly, a single is safe when it is given a valid local audio file.

## Transcoding

[Liquidsoap](index.html) can achieve basic streaming tasks like transcoding with ease. You input any number of "source" streams using `input.http`, and then transcode them to any number of formats / bitrates / etc. The only limitation is your hardware: encoding and decoding are both heavy on CPU. If you want to get the best use of CPUs (multicore, memory footprint etc.) when encoding media with Liquidsoap, we recommend using the `%ffmpeg` encoders.

```liquidsoap
# Input the stream,
# from an Icecast server or any other source
url = "https://icecast.radiofrance.fr/fip-hifi.aac"
input = mksafe(input.http(url))

# First transcoder: MP3 32 kbps
# We also degrade the samplerate, and encode in mono
# Accordingly, a mono conversion is performed on the input stream
output.icecast(
  %mp3(bitrate=32, samplerate=22050, stereo=false),
  mount="/your-stream-32.mp3",
  host="streaming.example.com", port=8000, password="xxx",
  mean(input))

# Second transcoder : MP3 128 kbps using %ffmpeg
output.icecast(
  %ffmpeg(format="mp3", %audio(codec="libmp3lame", b="128k")),
  mount="/your-stream-128.mp3",
  host="streaming.example.com", port=8000, password="xxx",
  input)
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

```liquidsoap
# The input file,
# any format supported by liquidsoap
input = "/tmp/input.mp3"

# The output file
output = "/tmp/output.ogg"

# A source that plays the file once
source = once(single(input))

# We use a clock with disabled synchronization
clock.assign_new(sync="none",[source])

# Finally, we output the source to an
# ogg/vorbis file
output.file(%vorbis, output,fallible=true,
                     on_stop=shutdown,source)
```

## Scheduling

```liquidsoap
# A fallback switch
fallback([playlist("http://my/playlist"),
          single("/my/jingle.ogg")])

# A scheduler,
# assuming you have defined the night and day sources
switch([ ({0h-7h}, night), ({7h-24h}, day) ])
```

## Force a file/playlist to be played at least every XX minutes

It can be useful to have a special playlist that is played at least every 20 minutes for instance (3 times per hour).
You may think of a promotional playlist for instance.
Here is the recipe:

```liquidsoap
# (1200 sec = 20 min)
timed_promotions = delay(1200.,promotions)
main_source = fallback([timed_promotions,other_source])
```

Where promotions is a source selecting the file to be promoted.

## Play a jingle at a fixed time

Suppose that we have a playlist `jingles` of jingles and we want to play one
within the 5 first minutes of every hour, without interrupting the current
song. We can think of doing something like

```liquidsoap
radio = switch([({ 0m-5m }, jingles), ({ true }, playlist)])
```

but the problem is that it is likely to play many jingles. In order to play
exactly one jingle, we can use the function `predicate.activates` which detects
when a predicate (here `{ 0m-5m }`) becomes true:

```liquidsoap
radio = switch([(predicate.activates({ 0m-5m }), jingles), ({ true }, playlist)])
```

## Handle special events: mix or switch

```liquidsoap
# Add a jingle to your normal source
# at the beginning of every hour:
add([normal,switch([({0m0s},jingle)])])
```

Switch to a live show as soon as one is available. Make the show unavailable when it is silent, and skip tracks from the normal source if they contain too much silence.

```liquidsoap
stripped_stream =
  blank.strip(input.http("http://myicecast:8080/live.ogg"))

fallback(track_sensitive=false,
         [stripped_stream,blank.strip(normal)])
```

Without the `track_sensitive=false` the fallback would wait the end of a track to switch to the live. When using the blank detection operators, make sure to fine-tune their `threshold` and `length` (float) parameters.

## Unix interface, dynamic requests

Liquidsoap can create a source that uses files provided by the result of the execution of any arbitrary function of your own.
This is explained in the documentation for [request-based sources](request_sources.html).

For instance, the following snippet defines a source which repeatedly plays the first valid URI in the playlist:

```liquidsoap
request.dynamic.list(
  { [request.create("bar:foo",
      indicators=
        process.read.lines("cat "^quote("playlist.pls")))] })
```

Of course a more interesting behaviour is obtained with a more interesting program than `cat`, see [Beets](beet.html) for example.

Another way of using an external program is to define a new protocol which uses it to resolve URIs. `protocol.add` takes a protocol name, a function to be used for resolving URIs using that protocol. The function will be given the URI parameter part and the time left for resolving -- though nothing really bad happens if you don't respect it. It usually passes the parameter to an external program ; it is another way to integrate [Beets](beet.html), for example:

```liquidsoap
protocol.add("beets", fun(~rlog,~maxtime,arg) ->
  process.read.lines(
    "/home/me/path/to/beet random -f '$path' #{arg}"
  )
)
```

When resolving the URI `beets:David Bowie`, liquidsoap will call the function, which will call `beet random -f '$path' David Bowie` which will output the path to a David Bowie song.

## Dynamic input with harbor

The operator `input.harbor` allows you to receive a source stream directly inside a running liquidsoap.

It starts a listening server on where any Icecast2-compatible source client can connect. When a source is connected, its input if fed to the corresponding source in the script, which becomes available.

This can be very useful to relay a live stream without polling the Icecast server for it.

An example can be:

```liquidsoap
# Serveur settings
settings.harbor.bind_addrs := ["0.0.0.0"]

# An emergency file
emergency = single("/path/to/emergency/single.ogg")

# A playlist
playlist = playlist("/path/to/playlist")

# A live source
live = input.harbor("live",port=8080,password="hackme")

# fallback
radio = fallback(track_sensitive=false,
                 [live,playlist,emergency])

# output it
output.icecast(
  %vorbis,
  mount="test",
  host="host",
  radio)
```

This script, when launched, will start a local server, here bound to "0.0.0.0". This means that it will listen on any IP address available on the machine for a connection coming from any IP address. The server will wait for any source stream on mount point "/live" to login.
Then if you start a source client and tell it to stream to your server, on port 8080, with password "hackme", the live source will become available and the radio will stream it immediately.

## Dump a stream into segmented files

It is sometimes useful (or even legally necessary) to keep a backup of an audio
stream. Storing all the stream in one file can be very impractical. In order to
save a file per hour in wav format, the following script can be used:

```liquidsoap
# A source to dump
# s = ...

# Dump the stream
output.file(%wav, '/archive/%Y-%m-%d/%Y-%m-%d-%H_%M_%S.mp3', s, reopen_when={0m})
```

In the following variant we write a new mp3 file each time new metadata is
coming from `s`:

```liquidsoap
file_name = '/archive/$(if $(title),"$(title)","Unknown archive")-%Y-%m-%d/%Y-%m-%d-%H_%M_%S.mp3'
output.file(%mp3, filename, s, reopen_on_metadata=true)
```

In the two examples we use [string interpolation](language.html) and time
literals to generate the output file name.

In order to limit the disk space used by this archive, on unix systems we can
regularly call `find` to cleanup the folder ; if we can to keep 31 days of
recording :

```liquidsoap
thread.when(every=3600., pred={ true },
    fun () -> list.iter(fun(msg) -> log(msg, label="archive_cleaner"),
        list.append(
            process.read.lines("find /archive/* -type f -mtime +31 -delete"),
            process.read.lines("find /archive/* -type d -empty -delete")
        )
    )
)
```

## Transitions

There are two kinds of transitions. Transitions between two different children of a switch or fallback and transitions between tracks of the same source.

### Switch-based transitions

The switch-based operators (`switch`, `fallback` and `random`) support transitions. For every child, you can specify a transition function computing the output stream when moving from one child to another. This function is given two `source` parameters: the child which is about to be left, and the new selected child. The default transition is `fun (a,b) -> b`, it simply relays the new selected child source.

One limitation of these transitions, however, is that if the transition happen right at the end of a track, which is the default with `track_sensitive=true`, then there is no more data available for the old source, which makes it impossible to fade it out. If that is what you are expecting, you should look at crossfade-based transitions

### Crossfade-based transitions

Crossfade-based transitions are more complex and involve buffering source data in advance to be able to compute a transition where ending and starting track potentially overlap. This does not work with all type of sources since some of them, such as `input.http` may only receive data at real-time rate and cannot be accelerated to buffer their data or else we risk running out of data.

We provide a default operator named `smart_cross` which may be suitable for most usage. But you can also create your own customized crossfade transitions. This is in particular true if you are expecting crossfade transitions between tracks of your `music` source but not between a `music` track and e.g. some jingles. Here's how to do it in this case:

```liquidsoap
# A function to add a source_tag metadata to a source:
def source_tag(s,tag) =
  def f(_)
    [("source_tag",(tag:string))]
  end
  metadata.map(id=tag,insert_missing=true,f,s)
end

# Tag our sources
music = source_tag(..., "music")
jingles = source_tag(..., "jingles")

# Combine them with one jingle every 3 music tracks
radio = rotate(weights = [1,3],[jingles,music])

# Now a custom crossfade transition:
def transition(a,b)
  # If old or new source is not music, no fade
  if a.metadata["source_tag"] != "music" or a.metadata["source_tag"] != "music" then
    sequence([a.source, b.source])
  else
    # Else, apply the standard smart transition
    cross.smart(a, b)
  end
end

# Apply it!
radio = cross(duration=5., transition, radio)
```

## Alsa unbuffered output

You can use [Liquidsoap](index.html) to capture and play through alsa with a minimal delay. This particularly useful when you want to run a live show from your computer. You can then directly capture and play audio through external speakers without delay for the DJ !

This configuration is not trivial since it relies on your hardware. Some hardware will allow both recording and playing at the same time, some only one at once, and some none at all.. Those note to configure are what works for us, we don't know if they'll fit all hardware.

First launch liquidsoap as a one line program

```
liquidsoap -v --debug 'input.alsa(bufferize=false)'
```

Unless you're lucky, the logs are full of lines like the following:

```
Could not set buffer size to 'frame.size' (1920 samples), got 2048.
```

The solution is then to set liquidsoap's internal frame size to this value, which is most likely specific to your hardware. Let's try this script:

```liquidsoap
# Set correct frame size:
# This makes it possible to set any audio frame size.
# Make sure that you do NOT use video in this case!
video.frame.rate := 0

# Now set the audio frame size exactly as required:
settings.frame.audio.size := 2048

input = input.alsa(bufferize=false)
output.alsa(bufferize=false,input)
```

The setting will be acknowledged in the log as follows:

```
Targeting 'frame.audio.size': 2048 audio samples = 2048 ticks.
```

If everything goes right, you may hear on your output the captured sound without any delay! If you want to test the difference, just run the same script with `bufferize=true`.

If you experience problems it might be a good idea to double the value of the frame size. This increases stability, but also latency.
