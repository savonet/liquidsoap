# Building an advanced stream: file-based sources

The purpose of this part is to document and illustrate the creation of an
advanced stream using static files.

In order to be self-contained, we will use here only pure liquidsoap scripting
functionalities. However, all the parts that use pre-defined functions can be
implemented using external scripts, which is the most common practice, and has
proved to be very convenient in order to integrate your liquidsoap stream into
the framework that you use to manage your radio.

## Preliminaries

In order to make things more clear and modular, we will
separate the code in two parts:

- `radio.liq` is the script that contains the definition of the main stream
- `library.liq` is the script that contains the functions used to build the stream

The scripts here should be tested using the following
command line:

```
liquidsoap /path/to/radio.liq
```

Thus, we do not define here daemonized script. In order to make
things work smoothly, you should put the following lines at the beginning
of `radio.liq`:

```{.liquidsoap include="on2-log.liq"}

```

Finally, we add the following line at the beginning of `radio.liq`,
in order to load our pre-defined functions:

```liquidsoap
%include "/path/to/library.liq"
```

We will use the telnet server to interact with the radio.
Thus, we enable the telnet server by adding the following line in `radio.liq`:

```liquidsoap
settings.server.telnet := true
```

## An initial model

In this part, we describe the initial stream that we want.
We start with a simple stream that contains songs from
a static playlist, with some jingles, with 3 songs for one jingle,
and output the result to an icecast server. This
is described by the following graph:

![Initial stream model](/assets/img/on2_part2_schema1.svg)

This very simple stream is defined by the following content
in `radio.liq`:

```{.liquidsoap include="on2-initial.liq"}

```

For now, `library.liq` does not contain any code so we only do:

```
touch /path/to/library.liq
```

- Write this script to a file, change the default directories and parameters.
- Run it.
- Listen to your initial stream.
- Connect to the telnet server (`telnet localhost 1234`)
- Try the telnet comment: `icecast.skip`
- Check that a jingle is played every 3 songs.
- Sometimes, when skipping, the source does not prepare a new file quickly enough, which breaks the rotation. If this is the case, add the parameter `conservative=true` to each playlist and try again.

Now, we extend this initial stream with some advanced features:

### Notify when a song is played

Once the stream is started, we may want to be able to keep track of the songs that are played,
for instance to display this information on a website.
One nice way to do this is to call a function every time that a new track is passed to the
output, which will inform the user of which tracks are played and when. This can be done
using the `on_metadata` operator.

First, we define a function that is called every time a new metadata is seen in the stream.
This is a function of type `(metadata)->unit`, i.e. a function that receives the metadata
as argument and returns nothing.
The `metadata` type is actually `[(string*string)]`,
i.e. a list of elements of the form `("label","value")`.

Thus, we add the following in `library.liq`:

```{.liquidsoap include="on2-notify.liq" from="BEGIN" to="END"}

```

**Note**: the string `"foo #{bla}"` can also be written `"foo " ^ bla` and is the
string ``foo bar'', if `bla`is the string`"bar"`.

Now, we apply the `on_metadata` operator with this function just
before passing the final source to the output, so we write in `radio.liq`,
before the output line:

```{.liquidsoap include="on2-notify.liq" from="BEGIN2" to="END2"}

```

- Update your scripts.
- Run the new radio.
- Observe the lines printed on new metadata.
- You may prefer to use the `log` function rather than `print`.

Solutions:

- [radio.liq](radio.liq-notify)
- [library.liq](library.liq-notify)

### Custom scheduling

Another issue with the above stream is the fact that jingles have a strict
frequency of one jingle every 3 songs. In a lot of cases, you may want
more flexibility and have full-features scheduling of your songs. The
best approach in this case is to _externalize_ this operation by creating
a scheduler with the language/framework of your choice and integrating it
with liquidsoap using `request.dynamic`.

`request.dynamic` takes a function of type `()->request('a)`,
i.e. a function with no arguments that returns a new request to queue
and create a source with it. Every time that liquidsoap needs to
prepare a new file, it will execute the function and use its result.

Requests in liquidsoap are created with the function `request.create`,
which takes an URIs of the form:

```
protocol:arguments
```

where `protocol:` is optional is `arguments` is the URI of a local file.
For instance, `ftp://server.net/path/to/file.mp3` is a requests using
the _ftp_ protocol, which is resolved using `wget` (if present in the
system).

We are going to use `request.dynamic.list` to merge both the `songs` and
`jingles` sources into one source and let our external scheduler
decides when to play a jingle or a song. However, we will need
later to know if we are currently playing a song or a jingle.

For these reasons, we will be using the `annotate:` protocol.
This protocol can be used to pass additional metadata along
with the metadata of the file. Here, we will pass a metadata
labeled `"type"`, with value `"song"` if the track is a song
or `"jingle"` otherwise.

In the context of this simple presentation, we will write
a dummy script. Thus, we create a file `"/tmp/request"`
that contains a line of the form:

```
annotate:type="song":/path/to/song.mp3
```

And, we add in `library.liq`:

```{.liquidsoap include="on2-scheduling.liq" from="BEGIN" to="END"}

```

Now, we replace the lines defining `songs`, `files` and the line
using the `rotate` operator in `radio.liq` with the following code:

```{.liquidsoap include="on2-scheduling.liq" from="BEGIN2" to="END2"}

```

- Update your scripts.
- Run the new radio.
- Edit `"/tmp/request"` and change its content to:

```liquidsoap
annotate:type="jingle":/path/to/jingle.mp3
```

- Use the command `s.skip` through the telnet server and verify that the new song is being played. This may need more than one skip..
- Use the commands `request.on_air` and `request.metadata` through the telnet server to verify the presence of the `type` metadata
- Extra: think about how to use the previous notification code to interact with this function.

Solutions:

- [radio.liq](radio.liq-request.dynamic.list)
- [library.liq](library.liq-request.dynamic.list)

### Custom metadata

We have just seen how it is possible to use the `annotate:` protocol to
pass custom metadata to any request. Additionally, it is also possible to rewrite
your stream's metadata on the fly, using the `on_metadata` operator.

This operator takes a function of the type `metadata->metadata`, i.e. a function
that takes the current metadata and returns some metadata. Thus, when `metadata.map`
sees a new metadata in the stream, it calls this function and, by default, updates
the metadata with the values returned by the function.

Here, we use this operator to customize the title metadata with the name of our radio.
First, we create a file `"/tmp/metadata"` containing:

```
My Awesome Liquidsoap Radio!
```

Then, in `library.liq`, we add the following function:

```{.liquidsoap include="on2-metadata.liq" from="BEGIN" to="END"}

```

Finally, we apply `metadata.map` to the source, just after the `request.dynamic.list`
definition in `radio.liq`:

```{.liquidsoap include="on2-metadata.liq" from="BEGIN2" to="END2"}

```

Solutions:

- [radio.liq](radio.liq-map_meta)
- [library.liq](library.liq-map_meta)

### Infallible sources

It is reasonable, for a radio, to expect that a stream is
always available for broadcasting. However, problems may happen (and always
do at some point). Thus, we need to offer an alternative for the case
where nothing is available.

Instead of using `mksafe`, which streams blank
audio when this happens, we use a custom sound file. For instance, this
sound file may contain a sentence like ``Hello, this is radio FOO! We are currently
having some technical difficulties but we'll be back soon so stay tuned!''.

We do that here using the `say:` protocol, which creates a speech synthesis
of the given sentence. Otherwise, you may record a (more serious) file and
pass it to the single operator...

First, we add the following in `library.liq`

```{.liquidsoap include="on2-safe.liq" from="BEGIN" to="END"}

```

Then, we add the following line in `radio.liq`, just before
the output line:

```{.liquidsoap include="on2-safe.liq" from="BEGIN2" to="END2"}

```

And we also remove the `fallible=true` from the parameters of `output.icecast`.

- Update your scripts.
- Run and test the new radio.
- To hear the security jingle, you can empty `"/tmp/request"` and use the skip command in telnet or wait for the end of the current song.
- If you put a new valid request in `"/tmp/request"` then the stream comes back to the normal source.

Solutions:

- [radio.liq](radio.liq-safe)
- [library.liq](library.liq-safe)

### Multiple outputs

We may as well output the stream to several targets,
for instance to different icecast mount points with different
formats. Therefore, we define a custom output function
that defines all these outputs.

We add the following in `library.liq`:

```{.liquidsoap include="on2-multiple.liq"}

```

And we replace the output line in `radio.liq` by:

```liquidsoap
outputs(s)
```

- Write your own output function.
- Run and test your new radio.

**Note** liquidsoap may fail with the following error:

```
Connection failed: 403, too many sources connected (HTTP/1.0)!
```

In this case, you should check the maximum number of sources that your
icecast server accepts.

Solutions:

- [radio.liq](radio.liq-outputs)
- [library.liq](library.liq-outputs)

## More advanced functions!

Now that we have a controllable initial radio, we extend our initial
scripts to add advanced features. The following graph illustrates what
we are going to add:

![Advanced stream model](/assets/img/on2_part2_schema2.svg)

\*\* The `Replay gain` node normalizes all the songs using the [Replay Gain](https://en.wikipedia.org/wiki/ReplayGain) technology.

- The `Smart crossfade` node adds crossfading between songs but not jingles.
- The `Smooth_add` node adds the possibility to insert a jingle in the middle of a song, fading out and then back in the initial stream while the jingle is being played.

### Replaygain

The replaygain support is achieved in liquidsoap in two steps:

- apply the `amplify` operator to change a source's volume
- pass a `"replay_gain"` metadata indicating to the `amplify` operator which value to use.

The `"replay_gain"` metadata can be passed manually or computed by liquidsoap.
Liquidsoap comes with a script that can extract the replaygain information
from ogg/vorbis, mp3 and FLAC files. This is a very convenient script but it generate
a high CPU usage which can be bad for real-time streaming.
In some situations, you may compute beforehand this value and pass it manually
using the `annotate` protocol.

If you cannot compute the value beforehand, liquidsoap comes with two ways
to extract the replaygain information

- The `replay_gain:` protocol. All requests of the form `replay_gain:URI` are resolved by passing `URI` to the script provided by liquidsoap. This method allows to select which files should be used with replay gain. However, it will only work if `URI` is a local file.
- The replay gain metadata resolver, enabled by adding a line of the form `enable_replaygain_metadata ()` in your script. In this cases, all requests and not only local files can be processed and you cannot select which one should be used with replaygain.

The most simple solution, in our case, is to change the requests
passed to `request.dynamic.list` to something of the form:

```
annotate:type="song":replay_gain:URI
```

However, in order to illustrate a bit more the functionalities of liquidsoap we present
another solution.
The method we propose here consists in using `metadata.map`, which we have already seen
to update the metadata with a `"replay_gain"` metadata when we see the `"type"` metadata
with the value `"song"`. Thus, we add the following function in `library.liq`:

```{liquidsoap include="on2-replaygain.liq"}

```

And, we add the following line in `radio.liq` after the `request.dynamic.list` line:

```liquidsoap
s = metadata.map(add_replaygain,s)
```

Finally, we add the `amplify` operator. We set the default amplification
to `1.`, i.e. no amplification, and tell the operator to update this value with
the content of the `"replay_gain"` metadata. Thus, only the tracks which have this
metadata will be modified.

We add the following in `radio.liq`, after the line we just inserted:

```liquidsoap
s = amplify(override="replay_gain", 1., s)
```

**Note** we can also apply `amplify` only to `songs`, _before_ the `switch` operator

- Update your scripts.
- Run and test the new radio.
- Change the content of `"/tmp/request"` with something of type `"file"` and skip the current song.
- Use the telnet server to make sure that the new `"replay_gain"` metadata has been added.
- Also, in the logs, you should be able to see that the `replay_gain` information was used to override the amplification factor.
- Can you find a content for `"/tmp/request"` that will enable replay gain on a file which is not of type `"song"`?

**Note** in this case, the `replay_gain` metadata is not added during the request resolution.
Thus, it is not visible in the `request.metadata`. However, you should be able to find another
command that displays it!

Solutions:

- [radio.liq](radio.liq-replay)
- [library.liq](library.liq-replay)

### Smart crossfade

The `smart_crossfade` is a crossfade operator that decides the crossfading to apply depending
on the volume and metadata of the old and new track.

It is defined using a generic `smart_cross`
operator, that takes a function of type `(float, float, metadata, metadata, source, source) -> source`,
i.e. a function that take the volume level (in decibels) of, respectively, the old and new
tracks, the metadata of, resp. the old and new tracks and, finally, the old and new tracks,
and returns the new source with the required transition.

We give here a simple custom implementation of our crossfade. What we do is:

- Crossfade tracks if none of the old and new track are jingle;
- Sequentialize the tracks otherwise.

We identify the type of each track by reading the `"type"` metadata we
have added when creating the `request.dynamic.list` source.

A typical `smart_crossfade` operator is defined in `utils.liq`
but you may do much more things with a little bit of imagination.

Here, we add the following in `library.liq`:

```{.liquidsoap include="on2-crossfade.liq"}

```

Finally, we add the following line in `radio.liq`, just after the
`amplify` operator:

```liquidsoap
s = my_crossfade(s)
```

- Update your scripts.
- Run and test the new radio.
- Modify the custom crossfading to fade out the old song if it is not a jingle, and fade in the new song if it is not a jingle.

Solutions:

- [radio.liq](radio.liq-smart)
- [library.liq](library.liq-smart)

### Smooth_add

Finally, we add another nice feature: a jingle that is played on top of the
current stream. We use the `smooth_add` operator, which is also defined in
`utils.liq`. This operator takes a normal source and a special jingle source.
Every time that a new track is available in the special source, it fades out
the volume of the normal source, plays the track from the special source
on top of the current track of the normal source, and then fades back in
the volume of the normal source when the track is finished.

Typically, you use for the special source a `request.queue` where you push a new
jingle every time you want to use this feature.

We modify `radio.liq` and add the following line just before `my_safe`:

```{.liquidsoap include="on2-smooth-add.liq" from="BEGIN" to="END"}

```

- Update your script.
- Run and test the new radio.
- Use the telnet server to push the request `say:My new radio rocks!` in the special source.
- Listen!

Solutions:

- [radio.liq](radio.liq-smooth)
- [library.liq](library.liq-smooth)

## What about DJs?

We present now another important part of an advanced stream: the addition of a live stream
in order to allow DJs to broadcast their shows.

We are going to add the following features:

- A live input that is played immediately when it is available
- Use different harbor ports, to replace mount points for shoutcast source clients
- A transition jingle that is played when switching between live and files
- Skip the file currently being played when switching to a live source so that the file-based source starts with a fresh song when the live stops
- Define different authentications for the DJs and make sure that a DJ can only broadcast when its show is scheduled

### Live inputs

The live inputs in liquidsoap are of two types:

- Network-based, mostly `input.http` and `input.harbor`.
- Hardware-based, with operators like `input.alsa`, `input.jack`, etc.

We focus here on the first type, and more precisely on `input.harbor`. When using
this operator in your script, the running instance will be able to receive data
coming from icecast and shoutcast source clients. Then, your DJs can broadcast
a live stream using their favorite software. Liquidsoap supports most of the
usual data formats, when enabled as encoder:

- MP3
- Any supported ogg stream
- Aac and Aac+

You may also communicate data between two liquidsoap instance, one using `output.icecast`
to send data and the other one `input.harbor` to receive it. In this case, you want also
use the WAVE or FLAC format to send lossless data.

We add a live source in `radio.liq`, anywhere before the outputs:

```liquidsoap
live = input.harbor("live")
```

**Note** `"live"` is the name of the mountpoint that will be
associated to this source. The default parameters for the port,
user and password are contained in the following settings:

```liquidsoap
settings.harbor.password := "hackme"
settings.harbor.port := 8005
settings.harbor.username := "source"
```

We want the live source to be played as soon as it becomes available. Thus, we
use a `fallback` to combine it with the file-based source, and add the following code
after `my_safe` in `radio.liq`:

```liquidsoap
s = fallback(track_sensitive=false, [live, s])
```

**Note** the `track_sensitive=false` parameter tells liquidsoap to
switch immediately to `live` when it becomes available instead of waiting
for the end of the track currently played by `s`.

- Update your script
- Run the new radio
- Try to connect to the harbor mountpoint. You may use a separate liquidsoap script and `output.icecast`
- Why is the output still infallible ?

Solutions:

- [radio.liq](radio.liq-harbor)
- [library.liq](library.liq-harbor)

### Enabling shoutcast clients

By default, shoutcast source clients are not supported. You can enable them by
adding the following settings:

```liquidsoap
settings.harbor.icy := true
```

**Note** `ICY` is the technical name of the original shoutcast source
protocol.

Additionally, the shoutcast source protocol does not support the notion
of mountpoint: all the sources try to connect to the same `"/"` mountpoint.
However, you can emulate this in liquidsoap by using different harbor sources
on different port.

For instance, if we replace the definition of `live` in `radio.liq` with the
following:

```liquidsoap
live1 = input.harbor(port=9000,"/")
live2 = input.harbor(port=7000,"/")
```

And the `fallback` line with:

```liquidsoap
s = fallback(track_sensitive=false, [live1,live2,s])
```

Then your a DJ should be able to send data using the port `9000`
and another one using the port `7000`, and the one connecting on port
`9000` may be played in priority if the two are connected at the same time.

- Update your script
- Run the new radio
- Connect one source client to port `7000`
- Connect another source client to port `9000`
- Verify that the client on port `9000` is broadcasting

Solutions:

- [radio.liq](radio.liq-shoutcast)
- [library.liq](library.liq-shoutcast)

### A nice transition!

Now that our radio support live shows, we deal with another issue: when switching
to the live show, the current song is cut at the point where it is and the audio
content switches over to the live data without any transition, which is not very nice
for the listeners. Further, when switching back to the file-based source at the
end of the live, the source resumes in the middle of the song that was last played..

In this part, we define a transition for switching from file to live, which fades
the current song out and superposes a jingle before starting the live show.
We use the `transition` parameter of the fallback operators.

This parameter contains functions of the type: `source * source -> source`,
i.e. functions that take two sources as arguments, the old and new source,
and returned a source that is the result of the desired transition. Finally,
when defined as:

```liquidsoap
fallback(transition=[f,g], [s1, s2])
```

`f` is called when switching to `s1` (and `g` when switching to `s2`).

We also use `source.skip`, which skips the file currently being played, in
order to play a fresh file when switching back to the file-based source.

First, we add the following code in `library.liq`:

```{.liquidsoap include="on2-transition.liq" from="BEGIN" to="END"}

```

**Note** `source.skip` may cause troubles if
the file source does not prepare a new track quickly enough.
In this case, you may add `conservative=true` to the
parameters of the `request.dynamic.list` source.

Then, we add the following code in `radio.liq`, where
we defined the `fallback` between the two live sources and
the file-based source:

```{.liquidsoap include="on2-transition.liq" from="BEGIN2" to="END2"}

```

- Update your script
- Run the new radio
- Connect to each harbor and listen to the result

Solutions:

- [radio.liq](radio.liq-transitions)
- [library.liq](library.liq-transitions)

### Custom logins

Another powerful feature of `input.harbor` is the possibility to define a
custom authentication. For instance, imagine that DJ Alice may connect to
the `live1` source only between 20h and 21h, which is the time of her shows,
with the password `"rabbit"`, while DJ Bob may connect to the `live2` source
between 18h and 20h with password `"foo"`.

This can be implemented using the `auth` parameter of `input.harbor`. This
parameter is a function of type: `string * string -> bool`, i.e. a function
that takes a pair `(user,password)` and returns `true` if the connection should
be granted.

You may use this, for instance, with an external script and integrate harbor
and DJ authentication into the framework of your choice. Here we illustrate
this functionality with a custom functions. Thus, we add the following
in `library.liq`:

```{.liquidsoap include="on2-login.liq" from="BEGIN" to="END"}

```

And we use it by replacing the `live1` and `live2` definitions by:

```{.liquidsoap include="on2-login.liq" from="BEGIN2" to="END2"}

```

- Write your custom login function
- Run and test your new radio

No solution here :-)
