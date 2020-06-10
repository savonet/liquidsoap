Cookbook
========
The recipes show how to build a source with a particular feature. You can try short snippets by wrapping the code in an `out(..)` operator and passing it directly to liquidsoap:

```liquidsoap
liquidsoap -v 'out(recipe)'
```

For longer recipes, you might want to create a short script:

```liquidsoap
#!/usr/bin/liquidsoap -v

set("log.file.path","/tmp/<script>.log")
set("log.stdout",true)

recipe = # <fill this>
out(recipe)
```

See the [quickstart guide](quick_start.html) for more information on how to run [Liquidsoap](index.html), on what is this `out(..)` operator, etc.

Files
-----
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

When building your stream, you'll often need to make it unfallible. Usually, you achieve that using a fallback switch (see below) with a branch made of a safe `single` or `playlist.safe`. Roughly, a single is safe when it is given a valid local audio file. A `playlist.safe` behaves just like a playlist but will check that all files in the playlist are valid local audio files. This is quite an heavy check, you don't want to have large safe playlists.

Transcoding
-----------
[Liquidsoap](index.html) can achieve basic streaming tasks like transcoding with ease. You input any number of "source" streams using `input.http`, and then transcode them to any number of formats / bitrates / etc. The only limitation is your hardware: encoding and decoding are both heavy on CPU. Also keep in mind a limitation inherent to OCaml: one [Liquidsoap](index.html) instance can only use a single processor or core. You can easily work around this limitation by launching multiple [Liquidsoap](index.html) instances, and thus take advantage of that 8-core Xeon server laying around in the dust in your garage.

```liquidsoap
# Input the stream,
# from an Icecast server or any other source
url = "http://streaming.example.com:8000/your-stream.ogg"
input = mksafe(input.http(url))

# First transcoder: MP3 32 kbps
# We also degrade the samplerate, and encode in mono
# Accordingly, a mono conversion is performed on the input stream
output.icecast(
  %mp3(bitrate=32, samplerate=22050, stereo=false),
  mount="/your-stream-32.mp3",
  host="streaming.example.com", port=8000, password="xxx",
  mean(input))
# Second transcoder : MP3 128 kbps
output.icecast(
  %mp3(bitrate=128), 
  mount="/your-stream-128.mp3",
  host="streaming.example.com", port=8000, password="xxx",
  input)
```

Re-encoding a file
------------------
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

Scheduling
----------
```liquidsoap
# A fallback switch
fallback([playlist("http://my/playlist"),
          single("/my/jingle.ogg")])
# A scheduler,
# assuming you have defined the night and day sources
switch([ ({0h-7h}, night), ({7h-24h}, day) ])
```

Force a file/playlist to be played at least every XX minutes
------------------------------------------------------------
It can be useful to have a special playlist that is played at least every 20 minutes for instance (3 times per hour).
You may think of a promotional playlist for instance.
Here is the recipe:

```liquidsoap
# (1200 sec = 20 min)
timed_promotions = delay(1200.,promotions)
main_source = fallback([timed_promotions,other_source])
```

Where promotions is a source selecting the file to be promoted.

Play a jingle at a fixed time
-----------------------------

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

Handle special events: mix or switch
------------------------------------
```liquidsoap
# Add a jingle to your normal source
# at the beginning of every hour:
add([normal,switch([({0m0s},jingle)])])
```

Switch to a live show as soon as one is available. Make the show unavailable when it is silent, and skip tracks from the normal source if they contain too much silence.

```liquidsoap
stripped_stream = 
  strip_blank(input.http("http://myicecast:8080/live.ogg"))
fallback(track_sensitive=false,
         [stripped_stream,skip_blank(normal)])
```

Without the `track_sensitive=false` the fallback would wait the end of a track to switch to the live. When using the blank detection operators, make sure to fine-tune their `threshold` and `length` (float) parameters.

Unix interface, dynamic requests
--------------------------------
Liquidsoap can create a source that uses files provided by the result of the execution of any arbitrary function of your own.
This is explained in the documentation for [request-based sources](request_sources.html).

For instance, the following snippet defines a source which repeatedly plays the first valid URI in the playlist:

```liquidsoap
request.dynamic.list(
  { [request.create("bar:foo",
      indicators=
        get_process_lines("cat "^quote("playlist.pls")))] })
```

Of course a more interesting behaviour is obtained with a more interesting program than `cat`, see [Beets](beet.html) for example.

Another way of using an external program is to define a new protocol which uses it to resolve URIs. `add_protocol` takes a protocol name, a function to be used for resolving URIs using that protocol. The function will be given the URI parameter part and the time left for resolving -- though nothing really bad happens if you don't respect it. It usually passes the parameter to an external program ; it is another way to integrate [Beets](beet.html), for example:

```liquidsoap
add_protocol("beets", fun(~rlog,~maxtime,arg) ->
  get_process_lines(
    "/home/me/path/to/beet random -f '$path' #{arg}"
  )
)
```

When resolving the URI `beets:David Bowie`, liquidsoap will call the function, which will call `beet random -f '$path' David Bowie` which will output the path to a David Bowie song.

Dynamic input with harbor
-------------------------
The operator `input.harbor` allows you to receive a source stream directly inside a running liquidsoap.

It starts a listening server on where any Icecast2-compatible source client can connect. When a source is connected, its input if fed to the corresponding source in the script, which becomes available.

This can be very useful to relay a live stream without polling the Icecast server for it.

An example can be:

```liquidsoap
# Serveur settings
set("harbor.bind_addr","0.0.0.0")

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
output.icecast(%vorbis, radio,mount="test",host="host")
```

This script, when launched, will start a local server, here bound to "0.0.0.0". This means that it will listen on any IP address available on the machine for a connection coming from any IP address. The server will wait for any source stream on mount point "/live" to login.
Then if you start a source client and tell it to stream to your server, on port 8080, with password "hackme", the live source will become available and the radio will stream it immediately.

Adding new commands
-------------------
You can add more commands to interact with your script through telnet or the server socket.

For instance, the following code, available in the standard API, attaches a `source.skip` command 
to a source. It is useful when the original source do not have a built-in skip command.

```liquidsoap
# Add a skip function to a source
# when it does not have one
# by default
def add_skip_command(s) =
 # A command to skip
 def skip(_) =
   source.skip(s)
   "Done!"
 end
 # Register the command:
 server.register(namespace="#{source.id(s)}",
                 usage="skip",
                 description="Skip the current song.",
                 "skip",skip)
end

# Attach a skip command to the source s:
add_skip_command(s)
```

Dump a stream into segmented files
----------------------------------
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
litterals to generate the output file name.

In order to limit the disk space used by this archive, on unix systems we can
regularly call `find` to cleanup the folder ; if we can to keep 31 days of
recording :

```liquidsoap
exec_at(freq=3600., pred={ true },
    fun () -> list.iter(fun(msg) -> log(msg, label="archive_cleaner"),
        list.append(
            get_process_lines("find /archive/* -type f -mtime +31 -delete"),
            get_process_lines("find /archive/* -type d -empty -delete")
        )
    )
)
```

Manually dump a stream
----------------------
You may want to dump the content of 
a stream. The following code adds 
two server/telnet commands, `dump.start <filename>`
and `dump.stop` to dump the content of source s
into the file given as argument

```liquidsoap
# A source to dump
# s = (...) 

# A function to stop
# the current dump source
stop_f = ref (fun () -> ())
# You should make sure you never
# do a start when another dump
# is running.

# Start to dump
def start_dump(file_name) =
  # We create a new file output
  # source
  s = output.file(%vorbis,
            fallible=true,
            on_start={log("Starting dump with file #{file_name}.ogg")},
            reopen_on_metadata=false,
            "#{file_name}",
            s)
  # We update the stop function
  stop_f := fun () -> source.shutdown(s)
end

# Stop dump
def stop_dump() =
  f = !stop_f
  f ()
end

# Some telnet/server command
server.register(namespace="dump",
                description="Start dumping.",
                usage="dump.start <filename>",
                "start",
                fun (s) ->  begin start_dump(s) "Done!" end)
server.register(namespace="dump",
                description="Stop dumping.",
                usage="dump.stop",
                "stop",
                fun (s) -> begin stop_dump() "Done!" end)
```

Transitions
-----------
There are two kinds of transitions. Transitions between two different children of a switch are not problematic. Transitions between different tracks of the same source are more tricky, since they involve a fast forward computation of the end of a track before feeding it to the transition function: such a thing is only possible when only one operator is using the source, otherwise it'll get out of sync.

### Switch-based transitions
The switch-based operators (`switch`, `fallback` and `random`) support transitions. For every child, you can specify a transition function computing the output stream when moving from one child to another. This function is given two `source` parameters: the child which is about to be left, and the new selected child. The default transition is `fun (a,b) -> b`, it simply relays the new selected child source.

Transitions have limited duration, defined by the `transition_length` parameter. Transition duration can be overriden by passing a metadata. Default field for it is `"liq_transition_length"` but it can also be set to a different value via the `override` parameter. 

Here are some possible transition functions:

```liquidsoap
# A simple (long) cross-fade
# Use metadata override to make sure transition is long enough.
def crossfade(a,b)
  def add_transition_length(_) =
    [("liq_transition_length","15.")]
  end

  transition =
    add(normalize=false,
          [ sequence([ blank(duration=5.),
                       fade.in(duration=10.,b) ]),
            fade.out(duration=10.,a) ])

  # Transition can have multiple tracks so only pass the metadata
  # once.
  map_first_track(map_metadata(add_transition_length),transition)
end

# Partially apply next to give it a jingle source.
# It will fade out the old source, then play the jingle.
# At the same time it fades in the new source.
# Use metadata override to make sure transition is long enough.
def next(j,a,b)
  # This assumes that the jingle is 6 seconds long
  def add_transition_length(_) =
    [("liq_transition_length","15.")]
  end

  transition =
    add(normalize=false,
	  [ sequence(merge=true,
	             [ blank(duration=3.),
	               fade.in(duration=6.,b) ]),
	    sequence([fade.out(duration=9.,a),
	              j,blank()]) ])

  map_first_track(map_metadata(add_transition_length),transition)
end

# A transition, which does a cross-fading from A to B
# No need to override duration as default value (5 seconds)
# is over crossade duration (3 seconds)
def transition(j,a,b)
  add(normalize=false,
	  [ fade.in(duration=3.,b),
	    fade.out(duration=3.,a) ])
end
```

Finally, we build a source which plays a playlist, and switches to the live show as soon as it starts, using the `transition` function as a transition. At the end of the live, the playlist comes back with a cross-fading.

```liquidsoap
fallback(track_sensitive=false,
	     transitions=[ crossfade, transition(jingle) ],
	     [ input.http("http://localhost:8000/live.ogg"),
	       playlist("playlist.pls") ])
```

### Cross-based transitions
The `cross()` operator allows arbitrary transitions between tracks of a same source. Here is how to use it in order to get a cross-fade:

```liquidsoap
def crossfade(~start_next,~fade_in,~fade_out,s)
  fade.in = fade.in(duration=fade_in)
  fade.out = fade.out(duration=fade_out)
  fader = fun (_,_,_,_,a,b) -> add(normalize=false,[fade.in(b),fade.out(a)])
  cross(duration=start_next,fader,s)
end
my_source =
  crossfade(start_next=1.,fade_out=1.,fade_in=1.,my_source)
```

The `crossfade()` function is already in liquidsoap. Unless you need a custom one, you should never have to copy the above example. It is implemented in the scripting language, much like this example. You can find its code in `utils.liq`.

The fade-in and fade-out parameters indicate the duraction of the fading effects. The start-next parameters tells how much overlap there will be between the two tracks. If you want a long cross-fading with a smaller overlap, you should use a sequence to stick some blank section before the beginning of `b` in `fader`.
The three parameters given here are only default values, and will be overriden by values coming from the metadata tags `liq_fade_in`, `liq_fade_out` and `liq_start_next`.

For an advanced crossfading function, you can see the [crossfade documentation](crossfade.html)

Alsa unbuffered output 
-----------------------
You can use [Liquidsoap](index.html) to capture and play through alsa with a minimal delay. This particulary useful when you want to run a live show from your computer. You can then directly capture and play audio through external speakers without delay for the DJ !

This configuration is not trivial since it relies on your hardware. Some hardware will allow both recording and playing at the same time, some only one at once, and some none at all.. Those note to configure are what works for us, we don't know if they'll fit all hardware.

First launch liquidsoap as a one line program

```
liquidsoap -v --debug 'input.alsa(bufferize=false)'
```

Unless you're lucky, the logs are full of lines like the following:

```
Could not set buffer size to 'frame.size' (1920 samples), got 2048.
```

The solution is then to fix the captured frame size to this value, which seems specific to your hardware. Let's try this script:

```liquidsoap
# Set correct frame size:
set("frame.audio.size",2048)

input = input.alsa(bufferize=false)
output.alsa(bufferize=false,input)
```

If everything goes right, you may hear on your output the captured sound without any delay ! If you want to test the difference, just run the same script with `bufferize=true` (or without this parameter since it is the default). The setting will be acknowledged in the log as follows:
```
Targetting 'frame.audio.size': 2048 audio samples = 2048 ticks.
```

If you experience problems it might be a good idea to double the value of the frame size. This increases stability, but also latency.


