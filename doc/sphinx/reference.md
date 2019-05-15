Liquidsoap scripting language reference
=======================================
Categories
----------
The **Source / ...** categories contain all functions that return sources.
The **Input** functions are those which build elementary sources
(playing files, synthesizing sound, etc.).
The **Output** functions are those which take a source and register it
for being streamed to the outside (file, soundcard, audio server, etc.).
The **Visualization** functions are experimental ones that let you 
visualize in real-time some aspects of the audio stream.
The **Sound Processing** functions are those which basically work on the source 
as a continuous audio stream. They would typically be mixers of streams,
audio effects or analysis.
Finally, **Track Processing** functions are basically all 
others, often having a behaviour that depends on or affects the extra 
information that liquidsoap puts in streams: track limits and metadata.

* [Source / Conversions](#Source___Conversions)
* [Source / Input](#Source___Input)
* [Source / Liquidsoap](#Source___Liquidsoap)
* [Source / MIDI Processing](#Source___MIDI_Processing)
* [Source / Output](#Source___Output)
* [Source / Sound Processing](#Source___Sound_Processing)
* [Source / Sound Synthesis](#Source___Sound_Synthesis)
* [Source / Track Processing](#Source___Track_Processing)
* [Source / Video Processing](#Source___Video_Processing)
* [Source / Visualization](#Source___Visualization)
* [Bool](#Bool)
* [Control](#Control)
* [Interaction](#Interaction)
* [Liquidsoap](#Liquidsoap)
* [List](#List)
* [Math](#Math)
* [Pair](#Pair)
* [String](#String)
* [System](#System)

Source / Conversions
--------------------
#### audio_to_stereo
```
(?id:string,source(audio=*+1,video=0,midi=0))->
source(audio=2,video=0,midi=0)```

Convert any kind of audio source into a stereo source.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio=*+1,video=0,midi=0)`)

#### drop_audio
```
(?id:string,source(audio='#a,video='#b,midi='#c))->
source(audio=0,video='#b,midi='#c)```

Drop all audio channels of a stream.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### drop_midi
```
(?id:string,source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi=0)```

Drop all midi channels of a stream.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### drop_video
```
(?id:string,source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video=0,midi='#c)```

Drop all video channels of a stream.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### id
```
(?id:string,source('a))->source('a)```

Does not do anything, simply forwards its input stream.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source('a)`)

#### mean
```
(?id:string,source(audio='#a,video='#b,midi='#c))->
source(audio=1,video='#b,midi='#c)```

Produce mono audio by taking the mean of all audio channels.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### mux_audio
```
(?id:string,audio:source(audio='#a,video=0,midi=0),
 source(audio=0,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Mux an audio stream into an audio-free stream.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `audio` (`source(audio='#a,video=0,midi=0)`)
* `(unlabeled)` (`source(audio=0,video='#b,midi='#c)`)

#### mux_mono
```
(?id:string,mono:source(audio=1,video=0,midi=0),
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a+1,video='#b,midi='#c)```

Mux a mono audio stream into another stream.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `mono` (`source(audio=1,video=0,midi=0)`)
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### mux_stereo
```
(?id:string,stereo:source(audio=2,video=0,midi=0),
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a+2,video='#b,midi='#c)```

Mux a stereo audio stream into another stream.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `stereo` (`source(audio=2,video=0,midi=0)`)
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### mux_video
```
(?id:string,video:source(audio=0,video='#a,midi=0),
 source(audio='#b,video=0,midi='#c))->
source(audio='#b,video='#a,midi='#c)```

Add video channnels to a stream.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `video` (`source(audio=0,video='#a,midi=0)`)
* `(unlabeled)` (`source(audio='#b,video=0,midi='#c)`)

#### stereo.left
```
(source(audio=2,video=0,midi=0))->
source(audio=1,video=0,midi=0)```

Extract the left channel of a stereo source

* `(unlabeled)` (`source(audio=2,video=0,midi=0)`): Source to extract from

#### stereo.right
```
(source(audio=2,video=0,midi=0))->
source(audio=1,video=0,midi=0)```

Extract the right channel of a stereo source

* `(unlabeled)` (`source(audio=2,video=0,midi=0)`): Source to extract from

#### swap
```
(?id:string,source(audio=2,video=0,midi=0))->
source(audio=2,video=0,midi=0)```

Swap two channels of a stereo source.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio=2,video=0,midi=0)`)

Source / Input
--------------
#### blank
```
(?id:string,?duration:float)->source('a)```

Produce silence and blank images.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `duration` (`float` -- defaults to `0.0`): Duration of blank tracks in seconds, default means forever.

#### empty
```
(?id:string)->source(audio='a,video='b,midi='c)```

A source that does not produce anything. No silence, no track at all.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.

#### fail
```
(?id:string)->source('b)```

Creates a source that fails to produce anything.

* `id` (`string` -- defaults to `""`)

#### gstreamer.hls
```
(?id:string,'a)->source(audio=2,video=1,midi=0)```

Play an http live stream.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`'a`): URI of the HLS stream index.

#### in
```
(?id:string,?start:bool,?on_start:(()->unit),
 ?on_stop:(()->unit),?fallible:bool)->
active_source(audio='#a+1,video='#b,midi='#c)```

Create a source from the first available input driver in pulseaudio, portaudio, oss, alsa, blank.

* `id` (`string` -- defaults to `""`)
* `start` (`bool` -- defaults to `true`)
* `on_start` (`()->unit` -- defaults to `{()}`)
* `on_stop` (`()->unit` -- defaults to `{()}`)
* `fallible` (`bool` -- defaults to `false`)

#### input.alsa
```
(?id:string,?bufferize:bool,?clock_safe:bool,
 ?device:string,?fallible:bool,?on_start:(()->unit),
 ?on_stop:(()->unit),?start:bool)->
active_source(audio='#a+1,video=0,midi=0)```

Stream from an ALSA input device.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `bufferize` (`bool` -- defaults to `true`): Bufferize input
* `clock_safe` (`bool` -- defaults to `true`): Force the use of the dedicated ALSA clock
* `device` (`string` -- defaults to `"default"`): Alsa device to use
* `fallible` (`bool` -- defaults to `false`): Allow the input to stop. When false, the source will be infallible but the stop command won't have any effect.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when input starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when input stops.
* `start` (`bool` -- defaults to `true`): Start input as soon as it is created. Disabling it is only taken into account for a fallible input.

#### input.external
```
(?id:string,?buffer:float,?channels:int,?max:float,
 ?restart:bool,?restart_on_error:bool,?samplerate:int,
 string)->source(audio='#a+1,video=0,midi=0)```

Stream data from an external application.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `buffer` (`float` -- defaults to `2.0`): Duration of the pre-buffered data.
* `channels` (`int` -- defaults to `2`): Number of channels.
* `max` (`float` -- defaults to `10.0`): Maximum duration of the buffered data.
* `restart` (`bool` -- defaults to `true`): Restart process when exited.
* `restart_on_error` (`bool` -- defaults to `false`): Restart process when exited with error.
* `samplerate` (`int` -- defaults to `44100`): Samplerate.
* `(unlabeled)` (`string`): Command to execute.

#### input.external.avi
```
(?id:string,?buffer:float,?max:float,?restart:bool,
 ?restart_on_error:bool,string)->
source(audio='#a,video='#b,midi=0)```

WARNING: This is only EXPERIMENTAL!

Stream data from an external application.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `buffer` (`float` -- defaults to `1.0`): Duration of the pre-buffered data.
* `max` (`float` -- defaults to `10.0`): Maximum duration of the buffered data.
* `restart` (`bool` -- defaults to `true`): Restart process when exited.
* `restart_on_error` (`bool` -- defaults to `false`): Restart process when exited with error.
* `(unlabeled)` (`string`): Command to execute.

#### input.external.rawvideo
```
(?id:string,?buffer:float,?max:float,?restart:bool,
 ?restart_on_error:bool,string)->
source(audio=0,video=1,midi=0)```

WARNING: This is only EXPERIMENTAL!

Stream data from an external application.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `buffer` (`float` -- defaults to `1.0`): Duration of the pre-buffered data.
* `max` (`float` -- defaults to `10.0`): Maximum duration of the buffered data.
* `restart` (`bool` -- defaults to `true`): Restart process when exited.
* `restart_on_error` (`bool` -- defaults to `false`): Restart process when exited with error.
* `(unlabeled)` (`string`): Command to execute.

#### input.ffmpeg.video
```
(?id:string,?restart:bool,?restart_on_error:bool,
 ?buffer:float,?max:float,?format:string,string)->
source(audio='#a,video='#b,midi=0)```

No documentation available.

* `id` (`string` -- defaults to `"input.ffmpeg.video"`)
* `restart` (`bool` -- defaults to `true`)
* `restart_on_error` (`bool` -- defaults to `false`)
* `buffer` (`float` -- defaults to `0.2`)
* `max` (`float` -- defaults to `10.0`)
* `format` (`string` -- defaults to `""`)
* `(unlabeled)` (`string`)

#### input.gstreamer.audio
```
(?id:string,?max:float,?on_error:((string)->float),
 ?pipeline:string,?restart:bool)->
source(audio='#a+1,video=0,midi=0)```

Stream audio from a GStreamer pipeline.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `max` (`float` -- defaults to `10.0`): Maximum duration of the buffered data.
* `on_error` (`(string)->float` -- defaults to `fun (_) -> 3.`): Callback executed when an error happens. The callback receives a string representation of the error that occured and returns a float. If returned value is positive, connection will be tried again after this amount of time (in seconds).
* `pipeline` (`string` -- defaults to `"audiotestsrc"`): GStreamer pipeline to input from.
* `restart` (`bool` -- defaults to `true`): Restart input on end of stream event

#### input.gstreamer.audio_video
```
(?id:string,?audio_pipeline:string,?max:float,
 ?on_error:((string)->float),?pipeline:string,
 ?restart:bool,?video_pipeline:string)->
source(audio=2,video=1,midi=0)```

Stream audio+video from a GStreamer pipeline.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `audio_pipeline` (`string` -- defaults to `"audiotestsrc"`): Audio pipeline to input from.
* `max` (`float` -- defaults to `10.0`): Maximum duration of the buffered data.
* `on_error` (`(string)->float` -- defaults to `fun (_) -> 3.`): Callback executed when an error happens. The callback receives a string representation of the error that occured and returns a float. If returned value is positive, connection will be tried again after this amount of time (in seconds).
* `pipeline` (`string` -- defaults to `""`): Main GStreamer pipeline.
* `restart` (`bool` -- defaults to `true`): Restart input on end of stream event
* `video_pipeline` (`string` -- defaults to `"videotestsrc"`): Video pipeline to input from.

#### input.gstreamer.video
```
(?id:string,?max:float,?on_error:((string)->float),
 ?pipeline:string,?restart:bool)->
source(audio=0,video=1,midi=0)```

Stream video from a GStreamer pipeline.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `max` (`float` -- defaults to `10.0`): Maximum duration of the buffered data.
* `on_error` (`(string)->float` -- defaults to `fun (_) -> 3.`): Callback executed when an error happens. The callback receives a string representation of the error that occured and returns a float. If returned value is positive, connection will be tried again after this amount of time (in seconds).
* `pipeline` (`string` -- defaults to `"videotestsrc"`): GStreamer pipeline to input from.
* `restart` (`bool` -- defaults to `true`): Restart input on end of stream event

#### input.harbor
```
(?id:string,?auth:((string,string)->bool),?buffer:float,
 ?debug:bool,?dumpfile:string,?icy:bool,
 ?icy_metadata_charset:string,?logfile:string,?max:float,
 ?metadata_charset:string,
 ?on_connect:(([(string*string)])->unit),
 ?on_disconnect:(()->unit),?password:string,?port:int,
 ?replay_metadata:bool,?timeout:float,?user:string,
 string)->source('a)```

Retrieves the given http stream from the harbor.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `auth` (`(string,string)->bool` -- defaults to `fun (_,_) -> false`): Authentication function. `f(login,password)` returns `true` if the user should be granted access for this login. Override any other method if used.
* `buffer` (`float` -- defaults to `2.0`): Duration of the pre-buffered data.
* `debug` (`bool` -- defaults to `false`): Run in debugging mode by not catching some exceptions.
* `dumpfile` (`string` -- defaults to `""`): Dump stream to file, for debugging purpose. Disabled if empty.
* `icy` (`bool` -- defaults to `false`): Enable ICY (shoutcast) protocol.
* `icy_metadata_charset` (`string` -- defaults to `""`): ICY (shoutcast) metadata charset. Guessed if empty. Default for shoutcast is ISO-8859-1. Set to that value if all your clients send metadata using this charset and automatic detection is not working for you.
* `logfile` (`string` -- defaults to `""`): Log buffer status to file, for debugging purpose. Disabled if empty.
* `max` (`float` -- defaults to `10.0`): Maximum duration of the buffered data.
* `metadata_charset` (`string` -- defaults to `""`): Metadata charset for non-ICY (shoutcast) source protocols. Guessed if empty.
* `on_connect` (`([(string*string)])->unit` -- defaults to `fun (_) -> ()`): Function to execute when a source is connected. Its receives the list of headers, of the form: (<label>,<value>). All labels are lowercase.
* `on_disconnect` (`()->unit` -- defaults to `{()}`): Functions to excecute when a source is disconnected
* `password` (`string` -- defaults to `"hackme"`): Source password.
* `port` (`int` -- defaults to `8005`): Port used to connect to the source.
* `replay_metadata` (`bool` -- defaults to `false`): Replay last known metadata when switching back to this source. This helps when source has dropped due to temporary connection issues.
* `timeout` (`float` -- defaults to `30.0`): Timeout for source connectionn.
* `user` (`string` -- defaults to `"source"`): Source user.
* `(unlabeled)` (`string`): Mountpoint to look for.

#### input.harbor.ssl
```
(?id:string,?auth:((string,string)->bool),?buffer:float,
 ?debug:bool,?dumpfile:string,?icy:bool,
 ?icy_metadata_charset:string,?logfile:string,?max:float,
 ?metadata_charset:string,
 ?on_connect:(([(string*string)])->unit),
 ?on_disconnect:(()->unit),?password:string,?port:int,
 ?replay_metadata:bool,?timeout:float,?user:string,
 string)->source('a)```

Retrieves the given https stream from the harbor.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `auth` (`(string,string)->bool` -- defaults to `fun (_,_) -> false`): Authentication function. `f(login,password)` returns `true` if the user should be granted access for this login. Override any other method if used.
* `buffer` (`float` -- defaults to `2.0`): Duration of the pre-buffered data.
* `debug` (`bool` -- defaults to `false`): Run in debugging mode by not catching some exceptions.
* `dumpfile` (`string` -- defaults to `""`): Dump stream to file, for debugging purpose. Disabled if empty.
* `icy` (`bool` -- defaults to `false`): Enable ICY (shoutcast) protocol.
* `icy_metadata_charset` (`string` -- defaults to `""`): ICY (shoutcast) metadata charset. Guessed if empty. Default for shoutcast is ISO-8859-1. Set to that value if all your clients send metadata using this charset and automatic detection is not working for you.
* `logfile` (`string` -- defaults to `""`): Log buffer status to file, for debugging purpose. Disabled if empty.
* `max` (`float` -- defaults to `10.0`): Maximum duration of the buffered data.
* `metadata_charset` (`string` -- defaults to `""`): Metadata charset for non-ICY (shoutcast) source protocols. Guessed if empty.
* `on_connect` (`([(string*string)])->unit` -- defaults to `fun (_) -> ()`): Function to execute when a source is connected. Its receives the list of headers, of the form: (<label>,<value>). All labels are lowercase.
* `on_disconnect` (`()->unit` -- defaults to `{()}`): Functions to excecute when a source is disconnected
* `password` (`string` -- defaults to `"hackme"`): Source password.
* `port` (`int` -- defaults to `8005`): Port used to connect to the source.
* `replay_metadata` (`bool` -- defaults to `false`): Replay last known metadata when switching back to this source. This helps when source has dropped due to temporary connection issues.
* `timeout` (`float` -- defaults to `30.0`): Timeout for source connectionn.
* `user` (`string` -- defaults to `"source"`): Source user.
* `(unlabeled)` (`string`): Mountpoint to look for.

#### input.hls
```
(?id:string,?reload:float,string)->source('a)```

Play an HLS stream.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `reload` (`float` -- defaults to `10.0`): How often (in seconds) the playlist should be reloaded.
* `(unlabeled)` (`string`): Playlist URI

#### input.http
```
(?id:string,?autostart:bool,?bind_address:string,
 ?buffer:float,?debug:bool,?force_mime:string,
 ?logfile:string,?max:float,?new_track_on_metadata:bool,
 ?on_connect:(([(string*string)])->unit),
 ?on_disconnect:(()->unit),?playlist_mode:string,
 ?poll_delay:float,?timeout:float,?user_agent:string,
 string)->source('a)```

Forwards the given http stream. The relay can be paused/resumed using the start/stop telnet commands.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `autostart` (`bool` -- defaults to `true`): Initially start relaying or not.
* `bind_address` (`string` -- defaults to `""`): Address to bind on the local machine. This option can be useful if your machine is bound to multiple IPs. Empty means no bind address.
* `buffer` (`float` -- defaults to `2.0`): Duration of the pre-buffered data.
* `debug` (`bool` -- defaults to `false`): Run in debugging mode, not catching some exceptions.
* `force_mime` (`string` -- defaults to `""`): Force mime data type. Not used if empty.
* `logfile` (`string` -- defaults to `""`): Log buffer status to file, for debugging purpose. Disabled if empty.
* `max` (`float` -- defaults to `10.0`): Maximum duration of the buffered data.
* `new_track_on_metadata` (`bool` -- defaults to `true`): Treat new metadata as new track.
* `on_connect` (`([(string*string)])->unit` -- defaults to `fun (_) -> ()`): Function to execute when a source is connected. Its receives the list of headers, of the form: (<label>,<value>). All labels are lowercase.
* `on_disconnect` (`()->unit` -- defaults to `{()}`): Function to excecute when a source is disconnected
* `playlist_mode` (`string` -- defaults to `"normal"`): Valid modes are ``normal'', ``random'', ``randomize'' and ``first''. The first ones have the same meaning as for the mode parameter of the playlist operator. The last one discards all entries but the first one.
* `poll_delay` (`float` -- defaults to `2.0`): Polling delay when trying to connect to the stream.
* `timeout` (`float` -- defaults to `30.0`): Timeout for source connectionn.
* `user_agent` (`string` -- defaults to ```
"Liquidsoap/1.4.0+scm (Unix; OCaml 4.07.1)"```
): User agent.
* `(unlabeled)` (`string`): URL of an http stream (default port is 80).

#### input.https
```
(?id:string,?autostart:bool,?bind_address:string,
 ?buffer:float,?debug:bool,?force_mime:string,
 ?logfile:string,?max:float,?new_track_on_metadata:bool,
 ?on_connect:(([(string*string)])->unit),
 ?on_disconnect:(()->unit),?playlist_mode:string,
 ?poll_delay:float,?timeout:float,?user_agent:string,
 string)->source('a)```

Forwards the given https stream. The relay can be paused/resumed using the start/stop telnet commands.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `autostart` (`bool` -- defaults to `true`): Initially start relaying or not.
* `bind_address` (`string` -- defaults to `""`): Address to bind on the local machine. This option can be useful if your machine is bound to multiple IPs. Empty means no bind address.
* `buffer` (`float` -- defaults to `2.0`): Duration of the pre-buffered data.
* `debug` (`bool` -- defaults to `false`): Run in debugging mode, not catching some exceptions.
* `force_mime` (`string` -- defaults to `""`): Force mime data type. Not used if empty.
* `logfile` (`string` -- defaults to `""`): Log buffer status to file, for debugging purpose. Disabled if empty.
* `max` (`float` -- defaults to `10.0`): Maximum duration of the buffered data.
* `new_track_on_metadata` (`bool` -- defaults to `true`): Treat new metadata as new track.
* `on_connect` (`([(string*string)])->unit` -- defaults to `fun (_) -> ()`): Function to execute when a source is connected. Its receives the list of headers, of the form: (<label>,<value>). All labels are lowercase.
* `on_disconnect` (`()->unit` -- defaults to `{()}`): Function to excecute when a source is disconnected
* `playlist_mode` (`string` -- defaults to `"normal"`): Valid modes are ``normal'', ``random'', ``randomize'' and ``first''. The first ones have the same meaning as for the mode parameter of the playlist operator. The last one discards all entries but the first one.
* `poll_delay` (`float` -- defaults to `2.0`): Polling delay when trying to connect to the stream.
* `timeout` (`float` -- defaults to `30.0`): Timeout for source connectionn.
* `user_agent` (`string` -- defaults to ```
"Liquidsoap/1.4.0+scm (Unix; OCaml 4.07.1)"```
): User agent.
* `(unlabeled)` (`string`): URL of an https stream (default port is 80).

#### input.jack
```
(?id:string,?buffer_size:int,?clock_safe:bool,
 ?server:string)->source(audio='#a+1,video=0,midi=0)```

Get stream from jack.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `buffer_size` (`int` -- defaults to `2`): Set buffer size, in frames. Must be >= 1.
* `clock_safe` (`bool` -- defaults to `true`): Force the use of the dedicated bjack clock.
* `server` (`string` -- defaults to `""`): Jack server to connect to.

#### input.keyboard.sdl
```
(?id:string,?velocity:float)->
source(audio='#a,video=0,midi='#b+1)```

WARNING: This is only EXPERIMENTAL!

Play notes from the keyboard.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `velocity` (`float` -- defaults to `0.8`): Velocity of notes.

#### input.mencoder.video
```
(?id:string,?restart:bool,?restart_on_error:bool,
 ?buffer:float,?max:float,string)->
source(audio=0,video=1,midi=0)```

No documentation available.

* `id` (`string` -- defaults to `"input.mencoder.video"`)
* `restart` (`bool` -- defaults to `true`)
* `restart_on_error` (`bool` -- defaults to `false`)
* `buffer` (`float` -- defaults to `0.2`)
* `max` (`float` -- defaults to `10.0`)
* `(unlabeled)` (`string`)

#### input.mplayer
```
(?id:string,?restart:bool,?restart_on_error:bool,
 ?buffer:float,?max:float,string)->
source(audio='#a+1,video=0,midi=0)```

Stream data from mplayer

* `id` (`string` -- defaults to `"input.mplayer"`)
* `restart` (`bool` -- defaults to `true`): restart on exit.
* `restart_on_error` (`bool` -- defaults to `false`): restart on exit with error.
* `buffer` (`float` -- defaults to `0.2`): Duration of the pre-buffered data.
* `max` (`float` -- defaults to `10.0`): Maximum duration of the buffered data.
* `(unlabeled)` (`string`): data URI.

#### input.oss
```
(?id:string,?clock_safe:bool,?device:string,
 ?fallible:bool,?on_start:(()->unit),?on_stop:(()->unit),
 ?start:bool)->active_source(audio='#a+1,video=0,midi=0)```

Stream from an OSS input device.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `clock_safe` (`bool` -- defaults to `true`): Force the use of the dedicated OSS clock.
* `device` (`string` -- defaults to `"/dev/dsp"`): OSS device to use.
* `fallible` (`bool` -- defaults to `false`): Allow the input to stop. When false, the source will be infallible but the stop command won't have any effect.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when input starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when input stops.
* `start` (`bool` -- defaults to `true`): Start input as soon as it is created. Disabling it is only taken into account for a fallible input.

#### input.portaudio
```
(?id:string,?buflen:int,?clock_safe:bool,?fallible:bool,
 ?on_start:(()->unit),?on_stop:(()->unit),?start:bool)->
source(audio='#a+1,video='#b,midi='#c)```

Stream from a portaudio input device.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `buflen` (`int` -- defaults to `256`): Length of a buffer in samples.
* `clock_safe` (`bool` -- defaults to `true`): Force the use of the dedicated Portaudio clock.
* `fallible` (`bool` -- defaults to `false`): Allow the input to stop. When false, the source will be infallible but the stop command won't have any effect.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when input starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when input stops.
* `start` (`bool` -- defaults to `true`): Start input as soon as it is created. Disabling it is only taken into account for a fallible input.

#### input.pulseaudio
```
(?id:string,?client:string,?clock_safe:bool,
 ?device:string,?fallible:bool,?on_start:(()->unit),
 ?on_stop:(()->unit),?start:bool)->
active_source(audio='#a+1,video='#b,midi='#c)```

Stream from a portaudio input device.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `client` (`string` -- defaults to `"liquidsoap"`)
* `clock_safe` (`bool` -- defaults to `true`): Force the use of the dedicated Pulseaudio clock.
* `device` (`string` -- defaults to `""`): Device to use. Uses default if set to ``''.
* `fallible` (`bool` -- defaults to `false`): Allow the input to stop. When false, the source will be infallible but the stop command won't have any effect.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when input starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when input stops.
* `start` (`bool` -- defaults to `true`): Start input as soon as it is created. Disabling it is only taken into account for a fallible input.

#### input.udp
```
(?id:string,?buffer:float,host:string,port:int,string)->
active_source('a)```

WARNING: This is only EXPERIMENTAL!

Input encoded data from UDP, without any control whatsoever.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `buffer` (`float` -- defaults to `1.0`): Duration of buffered data before starting playout.
* `host` (`string`)
* `port` (`int`)
* `(unlabeled)` (`string`): Mime type.

#### input.v4l2
```
(?id:string,?device:string)->
source(audio=0,video=1,midi=0)```

Stream from a video4linux 2 input device, such as a webcam.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `device` (`string` -- defaults to `"/dev/video0"`): V4L2 device to use.

#### input.v4l2_with_audio
```
(?id:string,?device:string)->
source(audio=2,video=1,midi=0)```

Stream from a video4linux 2 input device, such as a webcam.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `device` (`string` -- defaults to `"/dev/video0"`): V4L2 device to use.

#### noise
```
(?id:string,?duration:float)->
source(audio='a,video='b,midi=0)```

Generate (audio and/or video) white noise.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `duration` (`float` -- defaults to `0.0`)

#### playlist
```
(?id:string,?check_next:((request('a))->bool),
 ?conservative:bool,?default_duration:float,
 ?length:float,?mime_type:string,?mode:string,
 ?on_track:((last:bool,int)->bool),?prefix:string,
 ?reload:int,?reload_mode:string,?timeout:float,string)->
source('a)```

Loop on a playlist of URIs.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `check_next` (`(request('a))->bool` -- defaults to <code><fun></code>): Function used to filter next tracks. A candidate track is only validated if the function returns true on it. The function is called before resolution, hence metadata will only be available for requests corresponding to local files. This is typically used to avoid repetitions, but be careful: if the function rejects all attempts, the playlist will enter into a consuming loop and stop playing anything.
* `conservative` (`bool` -- defaults to `false`): If true, estimated remaining time on the current track is not considered when computing queue length.
* `default_duration` (`float` -- defaults to `30.0`): When unknown, assume this duration (in sec.) for files.
* `length` (`float` -- defaults to `10.0`): How much audio (in sec.) should be queued in advance.
* `mime_type` (`string` -- defaults to `""`): Default MIME type for the playlist. Empty string means automatic detection.
* `mode` (`string` -- defaults to `"randomize"`): Play the files in the playlist either in the order (``normal'' mode), or shuffle the playlist each time it is loaded, and play it in this order for a whole round (``randomize'' mode), or pick a random file in the playlist each time (``random'' mode).
* `on_track` (`(last:bool,int)->bool` -- defaults to `fun (_,~last) -> false`): Function to execute when playlist is about to play its next track. Receives track position in the playlist and wether this is the last track. Force a reload by returning `true` in this function. 
* `prefix` (`string` -- defaults to `""`): Add a constant prefix to all requests. Useful for passing extra information using annotate, or for resolution through a particular protocol, such as replaygain.
* `reload` (`int` -- defaults to `0`): Amount of time (in seconds or rounds), when applicable, before which the playlist is reloaded; 0 means never.
* `reload_mode` (`string` -- defaults to `"seconds"`): Unit of the reload parameter, either 'rounds', 'seconds' or 'watch' (reload the file whenever it is changed).
* `timeout` (`float` -- defaults to `20.0`): Timeout (in sec.) for a single download.
* `(unlabeled)` (`string`): URI where to find the playlist.

#### playlist.once
```
(?id:string,?random:bool,?on_done:(()->unit),
 ?reload_mode:string,string)->source('a)```

Custom playlist source written using the script language. It will read directory or playlist, play all files and stop.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `random` (`bool` -- defaults to `false`): Randomize playlist content
* `on_done` (`()->unit` -- defaults to `{()}`): Function to execute when the playlist is finished
* `reload_mode` (`string` -- defaults to `""`): If set to ``watch'', will be reloaded when the playlist is changed
* `(unlabeled)` (`string`): Playlist URI

#### playlist.reloadable
```
(?id:string,?random:bool,?on_done:(()->unit),
 ?filter:(([(string*string)])->bool),string)->
(((?uri:string)->unit)*source('a))```

Custom playlist source written using the script language. Will read directory or playlist, play all files and stop. Returns a pair `(reload,source)` where `reload` is a function of type `(?uri:string)->unit` used to reload the source and `source` is the actual source. The reload function can optionally be called with a new playlist URI. Otherwise, it reloads the previous URI.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `random` (`bool` -- defaults to `false`): Randomize playlist content
* `on_done` (`()->unit` -- defaults to `{()}`): Function to execute when the playlist is finished
* `filter` (`([(string*string)])->bool` -- defaults to `fun (_) -> true`): Filter out some files depending on metadata
* `(unlabeled)` (`string`): Playlist URI

#### playlist.safe
```
(?id:string,?mime_type:string,?mode:string,
 ?on_track:((last:bool,int)->bool),?prefix:string,
 ?reload:int,?reload_mode:string,string)->source('a)```

Loop on a playlist of local files, and never fail. In order to do so, it has to check every file at the loading, so the streamer startup may take a few seconds. To avoid this, use a standard playlist, and put only a few local files in a default safe_playlist in order to ensure the liveness of the streamer.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `mime_type` (`string` -- defaults to `""`): Default MIME type for the playlist. Empty string means automatic detection.
* `mode` (`string` -- defaults to `"randomize"`): Play the files in the playlist either in the order (``normal'' mode), or shuffle the playlist each time it is loaded, and play it in this order for a whole round (``randomize'' mode), or pick a random file in the playlist each time (``random'' mode).
* `on_track` (`(last:bool,int)->bool` -- defaults to `fun (_,~last) -> false`): Function to execute when playlist is about to play its next track. Receives track position in the playlist and wether this is the last track. Force a reload by returning `true` in this function. 
* `prefix` (`string` -- defaults to `""`): Add a constant prefix to all requests. Useful for passing extra information using annotate, or for resolution through a particular protocol, such as replaygain.
* `reload` (`int` -- defaults to `0`): Amount of time (in seconds or rounds), when applicable, before which the playlist is reloaded; 0 means never.
* `reload_mode` (`string` -- defaults to `"seconds"`): Unit of the reload parameter, either 'rounds', 'seconds' or 'watch' (reload the file whenever it is changed).
* `(unlabeled)` (`string`): URI where to find the playlist.

#### request.dynamic
```
(?id:string,?conservative:bool,?default_duration:float,
 ?length:float,?timeout:float,(()->request('a)))->
source('a)```

Play request dynamically created by a given function.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `conservative` (`bool` -- defaults to `false`): If true, estimated remaining time on the current track is not considered when computing queue length.
* `default_duration` (`float` -- defaults to `30.0`): When unknown, assume this duration (in sec.) for files.
* `length` (`float` -- defaults to `10.0`): How much audio (in sec.) should be queued in advance.
* `timeout` (`float` -- defaults to `20.0`): Timeout (in sec.) for a single download.
* `(unlabeled)` (`()->request('a)`)

#### request.equeue
```
(?id:string,?conservative:bool,?default_duration:float,
 ?length:float,?timeout:float)->source('a)```

Receive URIs from users, and play them. Insertion and deletion possible at any position.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `conservative` (`bool` -- defaults to `false`): If true, estimated remaining time on the current track is not considered when computing queue length.
* `default_duration` (`float` -- defaults to `30.0`): When unknown, assume this duration (in sec.) for files.
* `length` (`float` -- defaults to `10.0`): How much audio (in sec.) should be queued in advance.
* `timeout` (`float` -- defaults to `20.0`): Timeout (in sec.) for a single download.

#### request.queue
```
(?id:string,?conservative:bool,?default_duration:float,
 ?interactive:bool,?length:float,?queue:[request('a)],
 ?timeout:float)->source('a)```

Receive URIs from users, and play them.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `conservative` (`bool` -- defaults to `false`): If true, estimated remaining time on the current track is not considered when computing queue length.
* `default_duration` (`float` -- defaults to `30.0`): When unknown, assume this duration (in sec.) for files.
* `interactive` (`bool` -- defaults to `true`): Should the queue be controllable via telnet?
* `length` (`float` -- defaults to `10.0`): How much audio (in sec.) should be queued in advance.
* `queue` (`[request('a)]` -- defaults to `[]`): Initial queue of requests.
* `timeout` (`float` -- defaults to `20.0`): Timeout (in sec.) for a single download.

#### saw
```
(?id:string,?amplitude:float,?duration:float,?float)->
source(audio='#a+1,video=0,midi=0)```

Generate a saw wave.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `amplitude` (`float` -- defaults to `1.0`): Maximal value of the waveform.
* `duration` (`float` -- defaults to `0.0`): Duration in seconds (0. means infinite).
* `(unlabeled)` (`float` -- defaults to `440.0`): Frequency of the saw.

#### sine
```
(?id:string,?amplitude:float,?duration:float,?float)->
source(audio='#a+1,video=0,midi=0)```

Generate a sine wave.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `amplitude` (`float` -- defaults to `1.0`): Maximal value of the waveform.
* `duration` (`float` -- defaults to `0.0`): Duration in seconds (0. means infinite).
* `(unlabeled)` (`float` -- defaults to `440.0`): Frequency of the sine.

#### single
```
(?id:string,?conservative:bool,?default_duration:float,
 ?length:float,?timeout:float,string)->source('a)```

Loop on a request. It never fails if the request is static, meaning that it can be fetched once. Typically, http, ftp, say requests are static, and time is not.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `conservative` (`bool` -- defaults to `false`): If true, estimated remaining time on the current track is not considered when computing queue length.
* `default_duration` (`float` -- defaults to `30.0`): When unknown, assume this duration (in sec.) for files.
* `length` (`float` -- defaults to `10.0`): How much audio (in sec.) should be queued in advance.
* `timeout` (`float` -- defaults to `20.0`): Timeout (in sec.) for a single download.
* `(unlabeled)` (`string`): URI where to find the file

#### square
```
(?id:string,?amplitude:float,?duration:float,?float)->
source(audio='#a+1,video=0,midi=0)```

Generate a square wave.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `amplitude` (`float` -- defaults to `1.0`): Maximal value of the waveform.
* `duration` (`float` -- defaults to `0.0`): Duration in seconds (0. means infinite).
* `(unlabeled)` (`float` -- defaults to `440.0`): Frequency of the square.

Source / Liquidsoap
-------------------
#### buffer
```
(?id:string,?buffer:float,?fallible:bool,?max:float,
 ?on_start:(()->unit),?on_stop:(()->unit),?start:bool,
 source('a))->source('a)```

Create a buffer between two different clocks.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `buffer` (`float` -- defaults to `1.0`): Amount of data to pre-buffer, in seconds.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `max` (`float` -- defaults to `10.0`): Maximum amount of buffered data, in seconds.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`source('a)`)

#### buffer.adaptative
```
(?id:string,?averaging:float,?buffer:float,
 ?fallible:bool,?limit:float,?max:float,
 ?on_start:(()->unit),?on_stop:(()->unit),?reset:bool,
 ?start:bool,source(audio='#a+1,video=0,midi=0))->
source(audio='#a+1,video=0,midi=0)```

WARNING: This is only EXPERIMENTAL!

Create a buffer between two different clocks. The speed of the output is adapted so that no buffer underrun or overrun occurs. This wonderful behavior has a cost: the pitch of the sound might be changed a little.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `averaging` (`float` -- defaults to `30.0`): Half-life for the averaging of the buffer size, in seconds.
* `buffer` (`float` -- defaults to `1.0`): Amount of data to pre-buffer, in seconds.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `limit` (`float` -- defaults to `1.25`): Maximum acceleration or deceleration factor.
* `max` (`float` -- defaults to `10.0`): Maximum amount of buffered data, in seconds.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `reset` (`bool` -- defaults to `false`): Reset speed estimation to 1. when the source becomes available again.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`source(audio='#a+1,video=0,midi=0)`)

Source / MIDI Processing
------------------------
#### midi.chord
```
(?id:string,?metadata:string,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c+1)```

Generate a chord.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `metadata` (`string` -- defaults to `"chord"`): Name of the metadata containing the chords.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### midi.merge_all
```
(?id:string,?track_out:int,
 source(audio='#a,video='#b,midi='#c+1))->
source(audio='#a,video='#b,midi='#c+1)```

Merge all MIDI tracks in one.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `track_out` (`int` -- defaults to `0`): Destination track.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c+1)`)

#### midi.remove
```
(?id:string,[int],source(audio='#a,video='#b,midi='#c+1))->
source(audio='#a,video='#b,midi='#c+1)```

Remove MIDI tracks.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`[int]`): Tracks to remove.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c+1)`)

Source / Output
---------------
#### gstreamer.encode_jpeg_avi
```
('a,source(audio='#b,video='#c+1,midi='#d))->
active_source(audio='#b,video='#c+1,midi='#d)```

Encode jpeg video file using gstreamer

* `(unlabeled)` (`'a`): Encoded file name
* `(unlabeled)` (`source(audio='#b,video='#c+1,midi='#d)`): Source

#### gstreamer.encode_mp3
```
('a,source(audio='#b+1,video='#c,midi='#d))->
active_source(audio='#b+1,video='#c,midi='#d)```

Encode a mp3 file using gstreamer

* `(unlabeled)` (`'a`): Encoded file name
* `(unlabeled)` (`source(audio='#b+1,video='#c,midi='#d)`): Source

#### gstreamer.encode_x264_avi
```
('a,source(audio='#b,video='#c+1,midi='#d))->
active_source(audio='#b,video='#c+1,midi='#d)```

Encode an x264 video file using gstreamer

* `(unlabeled)` (`'a`): Encoded file name
* `(unlabeled)` (`source(audio='#b,video='#c+1,midi='#d)`): Source

#### gstreamer.rtp.mpeg4
```
(?host:string,?port:int,
 source(audio='#a,video='#b+1,midi='#c))->
active_source(audio='#a,video='#b+1,midi='#c)```

Broadcast a video in RTP. In order to play it, save the following in xxx.sdp and use vlc xxx.sdp: v=0 m=video 5000 RTP/AVP 96 c=IN IP4 127.0.0.1 a=rtpmap:96 MP4V-ES/90000

* `host` (`string` -- defaults to `"127.0.0.1"`)
* `port` (`int` -- defaults to `5000`)
* `(unlabeled)` (`source(audio='#a,video='#b+1,midi='#c)`)

#### out
```
(source(audio='#a+1,video='#b,midi='#c))->
active_source(audio='#a+1,video='#b,midi='#c)```

Output a stream using the 'output.prefered' operator. The input source does not need to be infallible, blank will just be played during failures.

* `(unlabeled)` (`source(audio='#a+1,video='#b,midi='#c)`): the source to output

#### output.alsa
```
(?id:string,?bufferize:bool,?clock_safe:bool,
 ?device:string,?fallible:bool,?on_start:(()->unit),
 ?on_stop:(()->unit),?start:bool,
 source(audio='#a+1,video='#b,midi='#c))->
active_source(audio='#a+1,video='#b,midi='#c)```

Output the source's stream to an ALSA output device.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `bufferize` (`bool` -- defaults to `true`): Bufferize output
* `clock_safe` (`bool` -- defaults to `true`): Force the use of the dedicated ALSA clock
* `device` (`string` -- defaults to `"default"`): Alsa device to use
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`source(audio='#a+1,video='#b,midi='#c)`)

#### output.ao
```
(?id:string,?buffer_size:int,?channels_matrix:string,
 ?clock_safe:bool,?driver:string,?fallible:bool,
 ?on_start:(()->unit),?on_stop:(()->unit),
 ?options:[(string*string)],?start:bool,
 source(audio='#a+1,video='#b,midi='#c))->
active_source(audio='#a+1,video='#b,midi='#c)```

Output stream to local sound card using libao.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `buffer_size` (`int` -- defaults to `2`): Set buffer size, in frames.
* `channels_matrix` (`string` -- defaults to `""`): Output channels matrix, ``'' for AO's default.
* `clock_safe` (`bool` -- defaults to `true`): Use the dedicated AO clock.
* `driver` (`string` -- defaults to `""`): Driver to be used, ``'' for AO's default.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `options` (`[(string*string)]` -- defaults to `[]`): List of parameters, depends on the driver.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`source(audio='#a+1,video='#b,midi='#c)`)

#### output.dummy
```
(?id:string,?fallible:bool,?on_start:(()->unit),
 ?on_stop:(()->unit),?start:bool,source('a))->
active_source('a)```

Dummy output for debugging purposes.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`source('a)`)

#### output.external
```
(?id:string,?fallible:bool,?flush:bool,
 ?on_start:(()->unit),?on_stop:(()->unit),
 ?reopen_delay:float,?reopen_on_metadata:bool,
 ?reopen_when:(()->bool),?start:bool,format('a),string,
 source('a))->active_source('a)```

Send the stream to a process' standard input.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `flush` (`bool` -- defaults to `false`): Perform a flush after each write.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `reopen_delay` (`float` -- defaults to `120.0`): Prevent re-opening within that delay, in seconds.
* `reopen_on_metadata` (`bool` -- defaults to `false`): Re-open on every new metadata information.
* `reopen_when` (`()->bool` -- defaults to `{false}`): When should the output be re-opened.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`format('a)`): Encoding format.
* `(unlabeled)` (`string`): Process to pipe data to. Some strftime conversion specifiers are available: `%SMHdmY`. You can also use `$(..)` interpolation notation for metadata.
* `(unlabeled)` (`source('a)`)

#### output.file
```
(?id:string,?append:bool,?dir_perm:int,?fallible:bool,
 ?flush:bool,?on_close:((string)->unit),
 ?on_start:(()->unit),?on_stop:(()->unit),?perm:int,
 ?reopen_delay:float,?reopen_on_metadata:bool,
 ?reopen_when:(()->bool),?start:bool,format('a),string,
 source('a))->active_source('a)```

Output the source stream to a file.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `append` (`bool` -- defaults to `false`): Do not truncate but append in the file if it exists.
* `dir_perm` (`int` -- defaults to `511`): Permission of the directories if some have to be created, up to umask. Although you can enter values in octal notation (0oXXX) they will be displayed in decimal (for instance, 0o777 = 7*8^2 + 7*8 + 7 = 511).
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `flush` (`bool` -- defaults to `false`): Perform a flush after each write.
* `on_close` (`(string)->unit` -- defaults to `fun (_) -> ()`): This function will be called for each file, after that it is finished and closed. The filename will be passed as argument.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `perm` (`int` -- defaults to `438`): Permission of the file if it has to be created, up to umask. You can and should write this number in octal notation: 0oXXX. The default value is however displayed in decimal (0o666 = 6*8^2 + 6*8 + 6 = 438).
* `reopen_delay` (`float` -- defaults to `120.0`): Prevent re-opening within that delay, in seconds.
* `reopen_on_metadata` (`bool` -- defaults to `false`): Re-open on every new metadata information.
* `reopen_when` (`()->bool` -- defaults to `{false}`): When should the output be re-opened.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`format('a)`): Encoding format.
* `(unlabeled)` (`string`): Filename where to output the stream. Some strftime conversion specifiers are available: `%SMHdmY`. You can also use `$(..)` interpolation notation for metadata.
* `(unlabeled)` (`source('a)`)

#### output.file.hls
```
(?id:string,?fallible:bool,?on_start:(()->unit),
 ?on_stop:(()->unit),?perm:int,?playlist:string,
 ?segment_duration:float,?segments:int,?start:bool,
 string,[(string*format('a))],source('a))->
active_source('a)```

Output the source stream to an HTTP live stream served from a local directory.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `perm` (`int` -- defaults to `420`): Permission of the created files, up to umask. You can and should write this number in octal notation: 0oXXX. The default value is however displayed in decimal (0o666 = 6*8^2 + 4*8 + 4 = 412).
* `playlist` (`string` -- defaults to `"stream.m3u8"`): Playlist name (m3u8 extension is recommended).
* `segment_duration` (`float` -- defaults to `10.0`): Segment duration (in seconds).
* `segments` (`int` -- defaults to `10`): Number of segments to keep.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`string`): Directory for generated files.
* `(unlabeled)` (`[(string*format('a))]`): List of specifications for each stream: (name, format).
* `(unlabeled)` (`source('a)`)

#### output.gstreamer.audio
```
(?id:string,?clock_safe:bool,?fallible:bool,
 ?on_error:((string)->float),?on_start:(()->unit),
 ?on_stop:(()->unit),?pipeline:string,?start:bool,
 source(audio='#a+1,video='#b,midi='#c))->
active_source(audio='#a+1,video='#b,midi='#c)```

Output stream to a GStreamer pipeline.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `clock_safe` (`bool` -- defaults to `true`): Use the dedicated GStreamer clock.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `on_error` (`(string)->float` -- defaults to `fun (_) -> 3.`): Callback executed when an error happens. The callback receives a string representation of the error that occured and returns a float. If returned value is positive, connection will be tried again after this amount of time (in seconds).
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `pipeline` (`string` -- defaults to `"autoaudiosink"`): GStreamer pipeline for sink.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`source(audio='#a+1,video='#b,midi='#c)`)

#### output.gstreamer.audio_video
```
(?id:string,?audio_pipeline:string,?blocking:bool,
 ?clock_safe:bool,?fallible:bool,
 ?on_error:((string)->float),?on_start:(()->unit),
 ?on_stop:(()->unit),?pipeline:string,?start:bool,
 ?video_pipeline:string,
 source(audio='#a+1,video='#b+1,midi='#c))->
active_source(audio='#a+1,video='#b+1,midi='#c)```

Output stream to a GStreamer pipeline.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `audio_pipeline` (`string` -- defaults to `"autoaudiosink"`): GStreamer pipeline for audio sink.
* `blocking` (`bool` -- defaults to `true`): Pushing buffers is blocking.
* `clock_safe` (`bool` -- defaults to `true`): Use the dedicated GStreamer clock.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `on_error` (`(string)->float` -- defaults to `fun (_) -> 3.`): Callback executed when an error happens. The callback receives a string representation of the error that occured and returns a float. If returned value is positive, connection will be tried again after this amount of time (in seconds).
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `pipeline` (`string` -- defaults to `""`): GStreamer pipeline for sink.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `video_pipeline` (`string` -- defaults to `"videoconvert ! autovideosink"`): GStreamer pipeline for video sink.
* `(unlabeled)` (```
source(audio='#a+1,video='#b+1,midi='#c)```
)

#### output.gstreamer.video
```
(?id:string,?clock_safe:bool,?fallible:bool,
 ?on_error:((string)->float),?on_start:(()->unit),
 ?on_stop:(()->unit),?pipeline:string,?start:bool,
 source(audio='#a,video='#b+1,midi='#c))->
active_source(audio='#a,video='#b+1,midi='#c)```

Output stream to a GStreamer pipeline.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `clock_safe` (`bool` -- defaults to `true`): Use the dedicated GStreamer clock.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `on_error` (`(string)->float` -- defaults to `fun (_) -> 3.`): Callback executed when an error happens. The callback receives a string representation of the error that occured and returns a float. If returned value is positive, connection will be tried again after this amount of time (in seconds).
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `pipeline` (`string` -- defaults to `"videoconvert ! autovideosink"`): GStreamer pipeline for sink.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`source(audio='#a,video='#b+1,midi='#c)`)

#### output.harbor
```
(?id:string,?auth:((string,string)->bool),?buffer:int,
 ?burst:int,?chunk:int,?dumpfile:string,?encoding:string,
 ?fallible:bool,?format:string,
 ?headers:[(string*string)],?metaint:int,mount:string,
 ?on_connect:((headers:[(string*string)],uri:string,
               protocol:string,string)->unit),
 ?on_disconnect:((string)->unit),?on_start:(()->unit),
 ?on_stop:(()->unit),?password:string,?port:int,
 ?start:bool,?timeout:float,?url:string,?user:string,
 format('a),source('a))->active_source('a)```

Encode and output the stream using the harbor server.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `auth` (`(string,string)->bool` -- defaults to `fun (_,_) -> false`): Authentication function. `f(login,password)` returns `true` if the user should be granted access for this login. Override any other method if used.
* `buffer` (`int` -- defaults to `327675`): Maximun buffer per-client.
* `burst` (`int` -- defaults to `65534`): Initial burst of data sent to the client.
* `chunk` (`int` -- defaults to `1024`): Send data to clients using chunks of at least this length.
* `dumpfile` (`string` -- defaults to `""`): Dump stream to file, for debugging purpose. Disabled if empty.
* `encoding` (`string` -- defaults to `""`): Encoding used to send metadata. If empty, defaults to ``ISO-8859-1'' for non-ogg formats and ``UTF-8'' otherwise.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `format` (`string` -- defaults to `""`): Format, e.g. ``audio/ogg''. When empty, the encoder is used to guess.
* `headers` (`[(string*string)]` -- defaults to `[]`): Additional headers.
* `metaint` (`int` -- defaults to `16000`): Interval used to send ICY metadata
* `mount` (`string`)
* `on_connect` (```
(headers:[(string*string)],uri:string,protocol:string,
 string)->unit```
 -- defaults to `fun (~headers,~uri,~protocol,_) -> ()`): Callback executed when connection is established.
* `on_disconnect` (`(string)->unit` -- defaults to `fun (_) -> ()`): Callback executed when connection stops.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `password` (`string` -- defaults to `"hackme"`)
* `port` (`int` -- defaults to `8000`)
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `timeout` (`float` -- defaults to `30.0`): Timeout for network operations.
* `url` (`string` -- defaults to `""`)
* `user` (`string` -- defaults to `""`): User for client connection, disabled if empty.
* `(unlabeled)` (`format('a)`): Encoding format.
* `(unlabeled)` (`source('a)`)

#### output.harbor.hls
```
(?id:string,?fallible:bool,?on_start:(()->unit),
 ?on_stop:(()->unit),?segment_duration:float,
 ?segments:int,?start:bool,?playlist:string,?port:int,
 ?path:string,[(string*format('a))],source('a))->
active_source('a)```

Output the source stream to an HTTP live stream served from the harbor HTTP server.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `segment_duration` (`float` -- defaults to `10.0`): Segment duration (in seconds).
* `segments` (`int` -- defaults to `10`): Number of segments to keep.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `playlist` (`string` -- defaults to `"stream.m3u8"`): Playlist name (m3u8 extension is recommended).
* `port` (`int` -- defaults to `8000`): Port for incoming harbor (http) connections.
* `path` (`string` -- defaults to `"/"`): Base path for hls URIs.
* `(unlabeled)` (`[(string*format('a))]`): List of specifications for each stream: (name, format).
* `(unlabeled)` (`source('a)`)

#### output.harbor.hls.https
```
(?id:string,?fallible:bool,?on_start:(()->unit),
 ?on_stop:(()->unit),?segment_duration:float,
 ?segments:int,?start:bool,?port:int,?path:string,
 ?playlist:string,[(string*format('a))],source('a))->
active_source('a)```

Output the source stream to an HTTP live stream served from the harbor HTTPS server.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `segment_duration` (`float` -- defaults to `10.0`): Segment duration (in seconds).
* `segments` (`int` -- defaults to `10`): Number of segments to keep.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `port` (`int` -- defaults to `8000`): Port for incoming harbor (https) connections.
* `path` (`string` -- defaults to `"/"`): Base path for hls URIs.
* `playlist` (`string` -- defaults to `"stream.m3u8"`): Playlist name (m3u8 extension is recommended).
* `(unlabeled)` (`[(string*format('a))]`): List of specifications for each stream: (name, format).
* `(unlabeled)` (`source('a)`)

#### output.harbor.ssl
```
(?id:string,?auth:((string,string)->bool),?buffer:int,
 ?burst:int,?chunk:int,?dumpfile:string,?encoding:string,
 ?fallible:bool,?format:string,
 ?headers:[(string*string)],?metaint:int,mount:string,
 ?on_connect:((headers:[(string*string)],uri:string,
               protocol:string,string)->unit),
 ?on_disconnect:((string)->unit),?on_start:(()->unit),
 ?on_stop:(()->unit),?password:string,?port:int,
 ?start:bool,?timeout:float,?url:string,?user:string,
 format('a),source('a))->active_source('a)```

Encode and output the stream using the SSL harbor server.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `auth` (`(string,string)->bool` -- defaults to `fun (_,_) -> false`): Authentication function. `f(login,password)` returns `true` if the user should be granted access for this login. Override any other method if used.
* `buffer` (`int` -- defaults to `327675`): Maximun buffer per-client.
* `burst` (`int` -- defaults to `65534`): Initial burst of data sent to the client.
* `chunk` (`int` -- defaults to `1024`): Send data to clients using chunks of at least this length.
* `dumpfile` (`string` -- defaults to `""`): Dump stream to file, for debugging purpose. Disabled if empty.
* `encoding` (`string` -- defaults to `""`): Encoding used to send metadata. If empty, defaults to ``ISO-8859-1'' for non-ogg formats and ``UTF-8'' otherwise.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `format` (`string` -- defaults to `""`): Format, e.g. ``audio/ogg''. When empty, the encoder is used to guess.
* `headers` (`[(string*string)]` -- defaults to `[]`): Additional headers.
* `metaint` (`int` -- defaults to `16000`): Interval used to send ICY metadata
* `mount` (`string`)
* `on_connect` (```
(headers:[(string*string)],uri:string,protocol:string,
 string)->unit```
 -- defaults to `fun (~headers,~uri,~protocol,_) -> ()`): Callback executed when connection is established.
* `on_disconnect` (`(string)->unit` -- defaults to `fun (_) -> ()`): Callback executed when connection stops.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `password` (`string` -- defaults to `"hackme"`)
* `port` (`int` -- defaults to `8000`)
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `timeout` (`float` -- defaults to `30.0`): Timeout for network operations.
* `url` (`string` -- defaults to `""`)
* `user` (`string` -- defaults to `""`): User for client connection, disabled if empty.
* `(unlabeled)` (`format('a)`): Encoding format.
* `(unlabeled)` (`source('a)`)

#### output.icecast
```
(?id:string,?chunked:bool,?connection_timeout:float,
 ?description:string,?dumpfile:string,?encoding:string,
 ?fallible:bool,?format:string,?genre:string,
 ?headers:[(string*string)],?host:string,?icy_id:int,
 ?icy_metadata:string,?mount:string,?name:string,
 ?on_connect:(()->unit),?on_disconnect:(()->unit),
 ?on_error:((string)->float),?on_start:(()->unit),
 ?on_stop:(()->unit),?password:string,?port:int,
 ?protocol:string,?public:bool,?start:bool,
 ?timeout:float,?url:string,?user:string,?verb:string,
 format('a),source('a))->active_source('a)```

Encode and output the stream to an icecast2 or shoutcast server.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `chunked` (`bool` -- defaults to `false`): Used cunked transfer with the 'http(s)' protocol.
* `connection_timeout` (`float` -- defaults to `5.0`): Timeout for establishing network connections (disabled is negative).
* `description` (`string` -- defaults to `""`)
* `dumpfile` (`string` -- defaults to `""`): Dump stream to file, for debugging purpose. Disabled if empty.
* `encoding` (`string` -- defaults to `""`): Encoding used to send metadata. If empty, defaults to ``UTF-8'' for ``http(s)'' protocol and ``ISO-8859-1'' for ``icy'' protocol.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `format` (`string` -- defaults to `""`): Format, e.g. ``audio/ogg''. When empty, the encoder is used to guess.
* `genre` (`string` -- defaults to `""`)
* `headers` (`[(string*string)]` -- defaults to ```
[("User-Agent","Liquidsoap/1.4.0+scm (Unix; OCaml 4.07.1)")]```
): Additional headers.
* `host` (`string` -- defaults to `"localhost"`)
* `icy_id` (`int` -- defaults to `1`): Shoutcast source ID. Only supported by Shoutcast v2.
* `icy_metadata` (`string` -- defaults to `"guess"`): Send new metadata using the ICY protocol. One of: ``guess'', ``true'', ``false''
* `mount` (`string` -- defaults to `"Use [name]"`): Source mount point. Mandatory when streaming to icecast.
* `name` (`string` -- defaults to `"Use [mount]"`)
* `on_connect` (`()->unit` -- defaults to `{()}`): Callback executed when connection is established.
* `on_disconnect` (`()->unit` -- defaults to `{()}`): Callback executed when connection stops.
* `on_error` (`(string)->float` -- defaults to `fun (_) -> 3.`): Callback executed when an error happens. The callback receives a string representation of the error that occured and returns a float. If returned value is positive, connection will be tried again after this amount of time (in seconds).
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `password` (`string` -- defaults to `"hackme"`)
* `port` (`int` -- defaults to `8000`)
* `protocol` (`string` -- defaults to `"http"`): Protocol of the streaming server: 'http' or 'https' for Icecast, 'icy' for shoutcast.
* `public` (`bool` -- defaults to `true`)
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `timeout` (`float` -- defaults to `30.0`): Timeout for network read and write.
* `url` (`string` -- defaults to `""`)
* `user` (`string` -- defaults to `""`): User for shout source connection. Defaults to ``source'' for icecast connections. Useful only in special cases, like with per-mountpoint users.
* `verb` (`string` -- defaults to `"source"`): Verb to use with the 'http(s)' protocol. One of: 'source', 'put' or 'post'.
* `(unlabeled)` (`format('a)`): Encoding format.
* `(unlabeled)` (`source('a)`)

#### output.jack
```
(?id:string,?buffer_size:int,?clock_safe:bool,
 ?fallible:bool,?on_start:(()->unit),?on_stop:(()->unit),
 ?server:string,?start:bool,
 source(audio='#a+1,video=0,midi=0))->
active_source(audio='#a+1,video=0,midi=0)```

Output stream to jack.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `buffer_size` (`int` -- defaults to `2`): Set buffer size, in frames.
* `clock_safe` (`bool` -- defaults to `true`): Force the use of the dedicated bjack clock.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `server` (`string` -- defaults to `""`): Jack server to connect to.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`source(audio='#a+1,video=0,midi=0)`)

#### output.oss
```
(?id:string,?clock_safe:bool,?device:string,
 ?fallible:bool,?on_start:(()->unit),?on_stop:(()->unit),
 ?start:bool,source(audio='#a+1,video='#b,midi='#c))->
active_source(audio='#a+1,video='#b,midi='#c)```

Output the source's stream to an OSS output device.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `clock_safe` (`bool` -- defaults to `true`): Force the use of the dedicated OSS clock.
* `device` (`string` -- defaults to `"/dev/dsp"`): OSS device to use.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`source(audio='#a+1,video='#b,midi='#c)`)

#### output.portaudio
```
(?id:string,?buflen:int,?clock_safe:bool,?fallible:bool,
 ?on_start:(()->unit),?on_stop:(()->unit),?start:bool,
 source(audio='#a+1,video='#b,midi='#c))->
active_source(audio='#a+1,video='#b,midi='#c)```

Output the source's stream to a portaudio output device.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `buflen` (`int` -- defaults to `256`): Length of a buffer in samples.
* `clock_safe` (`bool` -- defaults to `true`): Force the use of the dedicated Portaudio clock.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`source(audio='#a+1,video='#b,midi='#c)`)

#### output.prefered
```
(?id:string,?fallible:bool,?on_start:(()->unit),
 ?on_stop:(()->unit),?start:bool,
 source(audio='#a+1,video='#b,midi='#c))->
active_source(audio='#a+1,video='#b,midi='#c)```

Output to local audio card using the first available driver in pulseaudio, portaudio, oss, alsa, ao, dummy.

* `id` (`string` -- defaults to `""`)
* `fallible` (`bool` -- defaults to `false`)
* `on_start` (`()->unit` -- defaults to `{()}`)
* `on_stop` (`()->unit` -- defaults to `{()}`)
* `start` (`bool` -- defaults to `true`)
* `(unlabeled)` (`source(audio='#a+1,video='#b,midi='#c)`)

#### output.pulseaudio
```
(?id:string,?client:string,?clock_safe:bool,
 ?device:string,?fallible:bool,?on_start:(()->unit),
 ?on_stop:(()->unit),?start:bool,
 source(audio='#a+1,video='#b,midi='#c))->
active_source(audio='#a+1,video='#b,midi='#c)```

Output the source's stream to a portaudio output device.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `client` (`string` -- defaults to `"liquidsoap"`)
* `clock_safe` (`bool` -- defaults to `true`): Force the use of the dedicated Pulseaudio clock.
* `device` (`string` -- defaults to `""`): Device to use. Uses default if set to ``''.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`source(audio='#a+1,video='#b,midi='#c)`)

#### output.sdl
```
(?id:string,?fallible:bool,?on_start:(()->unit),
 ?on_stop:(()->unit),?start:bool,
 source(audio=0,video=1,midi=0))->
active_source(audio=0,video=1,midi=0)```

Display a video using SDL.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`source(audio=0,video=1,midi=0)`)

#### output.shoutcast
```
(?id:string,?start:bool,?host:string,?port:int,
 ?user:string,?password:string,?genre:string,?url:string,
 ?name:string,?encoding:string,?public:bool,?icy_id:int,
 ?format:string,?dj:(()->string),?dumpfile:string,
 ?icy_metadata:string,?on_connect:(()->unit),
 ?on_disconnect:(()->unit),?aim:string,?icq:string,
 ?irc:string,?icy_reset:bool,?fallible:bool,
 ?on_start:(()->unit),?on_stop:(()->unit),
 ?on_error:((string)->float),format('a),source('a))->
active_source('a)```

Output to shoutcast.

* `id` (`string` -- defaults to `"output.shoutcast"`): Output's ID
* `start` (`bool` -- defaults to `true`): Start output threads on operator initialization.
* `host` (`string` -- defaults to `"localhost"`)
* `port` (`int` -- defaults to `8000`)
* `user` (`string` -- defaults to `""`): User for shout source connection. Useful only in special cases, like with per-mountpoint users.
* `password` (`string` -- defaults to `"hackme"`)
* `genre` (`string` -- defaults to `""`)
* `url` (`string` -- defaults to `""`)
* `name` (`string` -- defaults to `""`)
* `encoding` (`string` -- defaults to `"ISO-8859-1"`): Encoding used to send metadata.
* `public` (`bool` -- defaults to `true`)
* `icy_id` (`int` -- defaults to `1`): Shoutcast source ID. Only supported by Shoutcast v2.
* `format` (`string` -- defaults to `""`): Format, e.g. ``audio/ogg''. When empty, the encoder is used to guess.
* `dj` (`()->string` -- defaults to `{""}`): Callback to set dj name.
* `dumpfile` (`string` -- defaults to `""`): Dump stream to file, for debugging purpose. Disabled if empty.
* `icy_metadata` (`string` -- defaults to `"guess"`): Send new metadata using the ICY protocol. One of: ``guess'', ``true'', ``false''
* `on_connect` (`()->unit` -- defaults to `{()}`): Callback executed when connection starts.
* `on_disconnect` (`()->unit` -- defaults to `{()}`): Callback executed when connection stops.
* `aim` (`string` -- defaults to `""`)
* `icq` (`string` -- defaults to `""`)
* `irc` (`string` -- defaults to `""`)
* `icy_reset` (`bool` -- defaults to `true`): Reset shoutcast source buffer upon connecting (necessary for NSV).
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `on_error` (`(string)->float` -- defaults to `fun (_) -> 3.`): Callback executed when an error happens. If returned value is positive, connection wll be tried again after this amount of time (in seconds).
* `(unlabeled)` (`format('a)`): Encoding format. For shoutcast, should be mp3 or AAC(+).
* `(unlabeled)` (`source('a)`): The source to output

#### output.udp
```
(?id:string,?fallible:bool,host:string,
 ?on_start:(()->unit),?on_stop:(()->unit),port:int,
 ?start:bool,format('a),source('a))->active_source('a)```

WARNING: This is only EXPERIMENTAL!

Output encoded data to UDP, without any control whatsoever.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `fallible` (`bool` -- defaults to `false`): Allow the child source to fail, in which case the output will be (temporarily) stopped.
* `host` (`string`)
* `on_start` (`()->unit` -- defaults to `{()}`): Callback executed when outputting starts.
* `on_stop` (`()->unit` -- defaults to `{()}`): Callback executed when outputting stops.
* `port` (`int`)
* `start` (`bool` -- defaults to `true`): Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
* `(unlabeled)` (`format('a)`): Encoding format.
* `(unlabeled)` (`source('a)`)

#### output.youtube.live
```
(?id:string,?video_bitrate:int,?audio_encoder:string,
 ?audio_bitrate:int,?url:string,key:'a,
 source(audio='#b+1,video='#c+1,midi='#d))->
active_source(audio='#b+1,video='#c+1,midi='#d)```

Stream live on youtube. You need the following Gstreamer plugins: flvmux, rtmpsink, x264enc and a suitable AAC encoder (see `audio_encoder` params).

* `id` (`string` -- defaults to `""`): Source ID
* `video_bitrate` (`int` -- defaults to `2000`): Video bitrate
* `audio_encoder` (`string` -- defaults to `"fdkaacenc"`): Audio encoder. Can be one of: ``fdkaacenc'', ``voaacenc''
* `audio_bitrate` (`int` -- defaults to `128000`): Audio bitrate
* `url` (`string` -- defaults to `"rtmp://a.rtmp.youtube.com/live2"`): Server URL
* `key` (`'a`): Secret key
* `(unlabeled)` (```
source(audio='#b+1,video='#c+1,midi='#d)```
): Source to stream

Source / Sound Processing
-------------------------
#### add
```
(?id:string,?normalize:bool,?weights:[int],
 [source(audio='#a,video='#b,midi=0)])->
source(audio='#a,video='#b,midi=0)```

Mix sources, with optional normalization. Only relay metadata from the first source that is effectively summed.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `normalize` (`bool` -- defaults to `true`)
* `weights` (`[int]` -- defaults to `[]`): Relative weight of the sources in the sum. The empty list stands for the homogeneous distribution.
* `(unlabeled)` (`[source(audio='#a,video='#b,midi=0)]`)

#### amplify
```
(?id:string,?override:string,'a,
 source(audio='#b,video='#c,midi='#d))->
source(audio='#b,video='#c,midi='#d)
where 'a is either float or ()->float```

Multiply the amplitude of the signal.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `override` (`string` -- defaults to `"liq_amplify"`): Specify the name of a metadata field that, when present and well-formed, overrides the amplification factor for the current track. Well-formed values are floats in decimal notation (e.g. '0.7') which are taken as normal/linear multiplicative factors; values can be passed in decibels with the suffix 'dB' (e.g. '-8.2 dB', but the spaces do not matter). Set to empty string ``'' to disable.
* `(unlabeled)` (```
anything that is either float or ()->float```
): Multiplicative factor.
* `(unlabeled)` (`source(audio='#b,video='#c,midi='#d)`)

#### bpm
```
(?id:string,?every:float,((float)->unit),
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Detect the BPM.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `every` (`float` -- defaults to `1.0`): Interval at which BPM is computed (in second).
* `(unlabeled)` (`(float)->unit`): Callback function.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### clip
```
(?id:string,source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Clip sound.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### comb
```
(?id:string,?delay:float,?feedback:'a,
 source(audio='#b,video='#c,midi='#d))->
source(audio='#b,video='#c,midi='#d)
where 'a is either float or ()->float```

Comb filter.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `delay` (`float` -- defaults to `0.001`): Delay in seconds.
* `feedback` (```
anything that is either float or ()->float```
 -- defaults to `-6.0`): Feedback coefficient in dB.
* `(unlabeled)` (`source(audio='#b,video='#c,midi='#d)`)

#### compand
```
(?id:string,?mu:float,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Compand the signal

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `mu` (`float` -- defaults to `1.0`)
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### compress
```
(?id:string,?attack:'a,?gain:'b,?knee:'c,?ratio:'d,
 ?release:'e,?rms_window:float,?threshold:'f,
 source(audio='#g,video='#h,midi='#i))->
source(audio='#g,video='#h,midi='#i)
where 'a, 'b, 'c, 'd, 'e, 'f is either float or ()->float```

Compress the signal.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `attack` (```
anything that is either float or ()->float```
 -- defaults to `100.0`): Attack time (ms).
* `gain` (```
anything that is either float or ()->float```
 -- defaults to `0.0`): Additional gain (dB).
* `knee` (```
anything that is either float or ()->float```
 -- defaults to `1.0`): Knee radius (dB).
* `ratio` (```
anything that is either float or ()->float```
 -- defaults to `2.0`): Gain reduction ratio (n:1).
* `release` (```
anything that is either float or ()->float```
 -- defaults to `400.0`): Release time (ms).
* `rms_window` (`float` -- defaults to `0.1`): Window for computing RMS (in sec).
* `threshold` (```
anything that is either float or ()->float```
 -- defaults to `-10.0`): Threshold level (dB).
* `(unlabeled)` (`source(audio='#g,video='#h,midi='#i)`)

#### compress.exponential
```
(?id:string,?mu:float,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Exponential compressor.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `mu` (`float` -- defaults to `2.0`): Exponential compression factor, typically greater than 1.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### cross
```
(?id:string,?active:bool,?conservative:bool,
 ?duration:float,?minimum:float,
 ?override_duration:string,?width:float,
 ((float,float,[(string*string)],[(string*string)],
   source(audio='#a+1,video=0,midi=0),
   source(audio='#a+1,video=0,midi=0))->
  source(audio='#a+1,video=0,midi=0)),
 source(audio='#a+1,video=0,midi=0))->
source(audio='#a+1,video=0,midi=0)```

Cross operator, allowing the composition of the N last seconds of a track with the beginning of the next track, using a transition function depending on the relative power of the signal before and after the end of track.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `active` (`bool` -- defaults to `false`): The active behavior is to keep ticking the child's clock when the operator is not streaming. Otherwise the child's clock is strictly based on what is streamed off the child source, which results in time-dependent active sources to be frozen when that source is stopped.
* `conservative` (`bool` -- defaults to `true`): Do not trust remaining time estimations, always buffering data in advance. This avoids being tricked by skips, either manual or caused by skip_blank().
* `duration` (`float` -- defaults to `5.0`): Duration in seconds of the crossed end of track.
* `minimum` (`float` -- defaults to `-1.0`): Minimum duration (in sec.) for a cross: If the track ends without any warning (e.g. in case of skip) there may not be enough data for a decent composition. Set to 0. to avoid having transitions after skips, or more to avoid transitions on short tracks. With the negative default, transitions always occur.
* `override_duration` (`string` -- defaults to `"liq_cross_duration"`): Metadata field which, if present and containing a float, overrides the 'duration' parameter for current track.
* `width` (`float` -- defaults to `1.0`): Width of the power computation window.
* `(unlabeled)` (```
(float,float,[(string*string)],[(string*string)],
 source(audio='#a+1,video=0,midi=0),
 source(audio='#a+1,video=0,midi=0))->
source(audio='#a+1,video=0,midi=0)```
): Transition function, composing from the end of a track and the next track. It also takes the power of the signal before and after the transition, and the metadata.
* `(unlabeled)` (`source(audio='#a+1,video=0,midi=0)`)

#### echo
```
(?id:string,?delay:'a,?feedback:'b,?ping_pong:bool,
 source(audio='#c,video='#d,midi='#e))->
source(audio='#c,video='#d,midi='#e)
where 'a, 'b is either float or ()->float```

Add echo.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `delay` (```
anything that is either float or ()->float```
 -- defaults to `0.5`): Delay in seconds.
* `feedback` (```
anything that is either float or ()->float```
 -- defaults to `-6.0`): Feedback coefficient in dB (negative).
* `ping_pong` (`bool` -- defaults to `false`): Use ping-pong delay.
* `(unlabeled)` (`source(audio='#c,video='#d,midi='#e)`)

#### fade.final
```
(?id:string,?duration:float,?override_duration:string,
 ?override_type:string,?type:string,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Fade a stream to silence.

* `id` (`string` -- defaults to `"fade.out"`): Force the value of the source ID.
* `duration` (`float` -- defaults to `3.0`): Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override.
* `override_duration` (`string` -- defaults to `"liq_fade_out"`): Metadata field which, if present and containing a float, overrides the 'duration' parameter for current track.
* `override_type` (`string` -- defaults to `"liq_fade_type"`): Metadata field which, if present and correct, overrides the 'type' parameter for current track.
* `type` (`string` -- defaults to `"lin"`): Fader shape (lin|sin|log|exp): linear, sinusoidal, logarithmic or exponential.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### fade.in
```
(?id:string,?duration:float,?override_duration:string,
 ?override_type:string,?type:string,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Fade the beginning of tracks.

* `id` (`string` -- defaults to `"fade.in"`): Force the value of the source ID.
* `duration` (`float` -- defaults to `3.0`): Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override.
* `override_duration` (`string` -- defaults to `"liq_fade_in"`): Metadata field which, if present and containing a float, overrides the 'duration' parameter for current track.
* `override_type` (`string` -- defaults to `"liq_fade_type"`): Metadata field which, if present and correct, overrides the 'type' parameter for current track.
* `type` (`string` -- defaults to `"lin"`): Fader shape (lin|sin|log|exp): linear, sinusoidal, logarithmic or exponential.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### fade.initial
```
(?id:string,?duration:float,?override_duration:string,
 ?override_type:string,?type:string,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Fade the beginning of a stream.

* `id` (`string` -- defaults to `"fade.in"`): Force the value of the source ID.
* `duration` (`float` -- defaults to `3.0`): Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override.
* `override_duration` (`string` -- defaults to `"liq_fade_initial"`): Metadata field which, if present and containing a float, overrides the 'duration' parameter for current track.
* `override_type` (`string` -- defaults to `"liq_fade_type"`): Metadata field which, if present and correct, overrides the 'type' parameter for current track.
* `type` (`string` -- defaults to `"lin"`): Fader shape (lin|sin|log|exp): linear, sinusoidal, logarithmic or exponential.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### fade.out
```
(?id:string,?duration:float,?override_duration:string,
 ?override_type:string,?type:string,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Fade the end of tracks.

* `id` (`string` -- defaults to `"fade.out"`): Force the value of the source ID.
* `duration` (`float` -- defaults to `3.0`): Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override.
* `override_duration` (`string` -- defaults to `"liq_fade_out"`): Metadata field which, if present and containing a float, overrides the 'duration' parameter for current track.
* `override_type` (`string` -- defaults to `"liq_fade_type"`): Metadata field which, if present and correct, overrides the 'type' parameter for current track.
* `type` (`string` -- defaults to `"lin"`): Fader shape (lin|sin|log|exp): linear, sinusoidal, logarithmic or exponential.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter
```
(?id:string,freq:'a,mode:string,?q:'b,?wetness:'c,
 source(audio='#d,video='#e,midi='#f))->
source(audio='#d,video='#e,midi='#f)
where 'a, 'b, 'c is either float or ()->float```

Perform several kinds of filtering on the signal

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `freq` (```
anything that is either float or ()->float```
)
* `mode` (`string`): Available modes are 'low' (for low-pass filter), 'high' (for high-pass filter), 'band' (for band-pass filter) and 'notch' (for notch / band-stop / band-rejection filter).
* `q` (```
anything that is either float or ()->float```
 -- defaults to `1.0`)
* `wetness` (```
anything that is either float or ()->float```
 -- defaults to `1.0`): How much of the original signal should be added (1. means only filtered and 0. means only original signal).
* `(unlabeled)` (`source(audio='#d,video='#e,midi='#f)`)

#### filter.fir
```
(?id:string,beta:float,?coeffs:int,frequency:float,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Low-pass FIR filter.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `beta` (`float`): Beta should range between 0 and 1.
* `coeffs` (`int` -- defaults to `255`): Number of coefficients
* `frequency` (`float`): Corner frequency in Hz (frequency at which the response is 0.5, that is -6 dB).
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.iir.butterworth.bandpass
```
(?id:string,frequency1:float,frequency2:float,?order:int,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

IIR filter

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `frequency1` (`float`): First corner frequency
* `frequency2` (`float`): Second corner frequency
* `order` (`int` -- defaults to `4`): Filter order
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.iir.butterworth.bandstop
```
(?id:string,frequency1:float,frequency2:float,?order:int,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

IIR filter

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `frequency1` (`float`): First corner frequency
* `frequency2` (`float`): Second corner frequency
* `order` (`int` -- defaults to `4`): Filter order
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.iir.butterworth.high
```
(?id:string,frequency:float,?order:int,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

IIR filter

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `frequency` (`float`): Corner frequency
* `order` (`int` -- defaults to `4`): Filter order
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.iir.butterworth.low
```
(?id:string,frequency:float,?order:int,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

IIR filter

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `frequency` (`float`): Corner frequency
* `order` (`int` -- defaults to `4`): Filter order
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.iir.eq.allpass
```
(?id:string,?bandwidth:float,frequency:float,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

All pass biquad filter.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `bandwidth` (`float` -- defaults to `0.333333333333`): Bandwidth (in octaves)
* `frequency` (`float`): Center frequency
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.iir.eq.bandpass
```
(?id:string,frequency:float,?q:float,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Band pass biquad filter.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `frequency` (`float`): Center frequency
* `q` (`float` -- defaults to `1.0`): Q
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.iir.eq.high
```
(?id:string,frequency:float,?q:float,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

High pass biquad filter.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `frequency` (`float`): Corner frequency
* `q` (`float` -- defaults to `1.0`): Q
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.iir.eq.highshelf
```
(?id:string,frequency:float,?slope:float,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

High shelf biquad filter.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `frequency` (`float`): Center frequency
* `slope` (`float` -- defaults to `1.0`): Shelf slope (in dB/octave)
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.iir.eq.low
```
(?id:string,frequency:float,?q:float,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Low pass biquad filter.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `frequency` (`float`): Corner frequency
* `q` (`float` -- defaults to `1.0`): Q
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.iir.eq.lowshelf
```
(?id:string,frequency:float,?slope:float,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Low shelf biquad filter.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `frequency` (`float`): Corner frequency
* `slope` (`float` -- defaults to `1.0`): Shelf slope (dB/octave)
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.iir.eq.notch
```
(?id:string,frequency:float,?q:float,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Band pass biquad filter.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `frequency` (`float`): Center frequency
* `q` (`float` -- defaults to `1.0`): Q
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.iir.eq.peak
```
(?id:string,frequency:float,?gain:float,?q:float,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Peak EQ biquad filter.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `frequency` (`float`): Center frequency
* `gain` (`float` -- defaults to `1.0`): Gain (in dB)
* `q` (`float` -- defaults to `1.0`): Q
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.iir.resonator.allpass
```
(?id:string,frequency:float,?q:float,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

IIR filter

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `frequency` (`float`): Corner frequency
* `q` (`float` -- defaults to `60.0`): Quality factor
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.iir.resonator.bandpass
```
(?id:string,frequency:float,?q:float,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

IIR filter

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `frequency` (`float`): Corner frequency
* `q` (`float` -- defaults to `60.0`): Quality factor
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.iir.resonator.bandstop
```
(?id:string,frequency:float,?q:float,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

IIR filter

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `frequency` (`float`): Corner frequency
* `q` (`float` -- defaults to `60.0`): Quality factor
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### filter.rc
```
(?id:string,mode:string,rc:'a,?wetness:'b,
 source(audio='#c,video='#d,midi='#e))->
source(audio='#c,video='#d,midi='#e)
where 'a, 'b is either float or ()->float```

First-order filter (RC filter).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `mode` (`string`): Available modes are 'low' (for low-pass filter), 'high' (for high-pass filter).
* `rc` (```
anything that is either float or ()->float```
): Time constant (in seconds).
* `wetness` (```
anything that is either float or ()->float```
 -- defaults to `1.0`): How much of the original signal should be added (1. means only filtered and 0. means only original signal).
* `(unlabeled)` (`source(audio='#c,video='#d,midi='#e)`)

#### flanger
```
(?id:string,?delay:float,?feedback:'a,?freq:'b,?phase:'c,
 source(audio='#d,video='#e,midi='#f))->
source(audio='#d,video='#e,midi='#f)
where 'a, 'b, 'c is either float or ()->float```

Flanger effect.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `delay` (`float` -- defaults to `0.001`): Delay in seconds.
* `feedback` (```
anything that is either float or ()->float```
 -- defaults to `0.0`): Feedback coefficient in dB.
* `freq` (```
anything that is either float or ()->float```
 -- defaults to `0.5`): Frequency in Hz.
* `phase` (```
anything that is either float or ()->float```
 -- defaults to `1.0`): Phase difference between channels in radians.
* `(unlabeled)` (`source(audio='#d,video='#e,midi='#f)`)

#### helium
```
(source(audio='#a+1,video=0,midi=0))->
source(audio='#a+1,video=0,midi=0)```

Increases the pitch, making voices sound like on helium.

* `(unlabeled)` (`source(audio='#a+1,video=0,midi=0)`): The input source.

#### ladspa.amp_mono
```
(?id:string,?gain:'a,
 source(audio='#b,video='#c,midi='#d))->
source(audio='#b,video='#c,midi='#d)
where 'a is either float or ()->float```

Mono Amplifier by Richard Furse (LADSPA example plugins).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `gain` (```
anything that is either float or ()->float```
 -- defaults to `1.0`): Gain (0 <= `gain`).
* `(unlabeled)` (`source(audio='#b,video='#c,midi='#d)`)

#### ladspa.amp_stereo
```
(?id:string,?gain:'a,source(audio=2,video=0,midi=0))->
source(audio=2,video=0,midi=0)
where 'a is either float or ()->float```

Stereo Amplifier by Richard Furse (LADSPA example plugins).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `gain` (```
anything that is either float or ()->float```
 -- defaults to `1.0`): Gain (0 <= `gain`).
* `(unlabeled)` (`source(audio=2,video=0,midi=0)`)

#### ladspa.delay_5s
```
(?id:string,?delay:'a,?dry_wet_balance:'b,
 source(audio='#c,video='#d,midi='#e))->
source(audio='#c,video='#d,midi='#e)
where 'a, 'b is either float or ()->float```

Simple Delay Line by Richard Furse (LADSPA example plugins).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `delay` (```
anything that is either float or ()->float```
 -- defaults to `1.0`): Delay (Seconds) (0 <= `delay` <= 5).
* `dry_wet_balance` (```
anything that is either float or ()->float```
 -- defaults to `0.5`): Dry/Wet Balance (0 <= `dry_wet_balance` <= 1).
* `(unlabeled)` (`source(audio='#c,video='#d,midi='#e)`)

#### ladspa.hpf
```
(?id:string,?cutoff_frequency:'a,
 source(audio='#b,video='#c,midi='#d))->
source(audio='#b,video='#c,midi='#d)
where 'a is either float or ()->float```

Simple High Pass Filter by Richard Furse (LADSPA example plugins).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `cutoff_frequency` (```
anything that is either float or ()->float```
 -- defaults to `440.0`): Cutoff Frequency (Hz) (0 <= `cutoff_frequency` <= 22050).
* `(unlabeled)` (`source(audio='#b,video='#c,midi='#d)`)

#### ladspa.lpf
```
(?id:string,?cutoff_frequency:'a,
 source(audio='#b,video='#c,midi='#d))->
source(audio='#b,video='#c,midi='#d)
where 'a is either float or ()->float```

Simple Low Pass Filter by Richard Furse (LADSPA example plugins).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `cutoff_frequency` (```
anything that is either float or ()->float```
 -- defaults to `440.0`): Cutoff Frequency (Hz) (0 <= `cutoff_frequency` <= 22050).
* `(unlabeled)` (`source(audio='#b,video='#c,midi='#d)`)

#### ladspa.noise_white
```
(?id:string,?amplitude:'a)->
source(audio=1,video=0,midi=0)
where 'a is either float or ()->float```

White Noise Source by Richard Furse (LADSPA example plugins).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `amplitude` (```
anything that is either float or ()->float```
 -- defaults to `1.0`): Amplitude (0 <= `amplitude`).

#### ladspa.sine_faaa
```
(?id:string,source(audio=2,video=0,midi=0))->
source(audio=1,video=0,midi=0)```

Sine Oscillator (Freq:audio, Amp:audio) by Richard Furse (LADSPA example plugins).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio=2,video=0,midi=0)`)

#### ladspa.sine_faac
```
(?id:string,?amplitude:'a,
 source(audio='#b,video='#c,midi='#d))->
source(audio='#b,video='#c,midi='#d)
where 'a is either float or ()->float```

Sine Oscillator (Freq:audio, Amp:control) by Richard Furse (LADSPA example plugins).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `amplitude` (```
anything that is either float or ()->float```
 -- defaults to `1.0`): Amplitude (0 <= `amplitude`).
* `(unlabeled)` (`source(audio='#b,video='#c,midi='#d)`)

#### ladspa.sine_fcaa
```
(?id:string,?frequency:'a,
 source(audio='#b,video='#c,midi='#d))->
source(audio='#b,video='#c,midi='#d)
where 'a is either float or ()->float```

Sine Oscillator (Freq:control, Amp:audio) by Richard Furse (LADSPA example plugins).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `frequency` (```
anything that is either float or ()->float```
 -- defaults to `440.0`): Frequency (Hz) (0 <= `frequency` <= 22050).
* `(unlabeled)` (`source(audio='#b,video='#c,midi='#d)`)

#### ladspa.sine_fcac
```
(?id:string,?amplitude:'a,?frequency:'b)->
source(audio=1,video=0,midi=0)
where 'a, 'b is either float or ()->float```

Sine Oscillator (Freq:control, Amp:control) by Richard Furse (LADSPA example plugins).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `amplitude` (```
anything that is either float or ()->float```
 -- defaults to `1.0`): Amplitude (0 <= `amplitude`).
* `frequency` (```
anything that is either float or ()->float```
 -- defaults to `440.0`): Frequency (Hz) (0 <= `frequency` <= 22050).

#### limit
```
(?id:string,?attack:'a,?gain:'b,?knee:'c,?ratio:'d,
 ?release:'e,?rms_window:float,?threshold:'f,
 source(audio='#g,video='#h,midi='#i))->
source(audio='#g,video='#h,midi='#i)
where 'a, 'b, 'c, 'd, 'e, 'f is either float or ()->float```

Limit the signal.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `attack` (```
anything that is either float or ()->float```
 -- defaults to `100.0`): Attack time (ms).
* `gain` (```
anything that is either float or ()->float```
 -- defaults to `0.0`): Additional gain (dB).
* `knee` (```
anything that is either float or ()->float```
 -- defaults to `1.0`): Knee radius (dB).
* `ratio` (```
anything that is either float or ()->float```
 -- defaults to `20.0`): Gain reduction ratio (n:1).
* `release` (```
anything that is either float or ()->float```
 -- defaults to `400.0`): Release time (ms).
* `rms_window` (`float` -- defaults to `0.1`): Window for computing RMS (in sec).
* `threshold` (```
anything that is either float or ()->float```
 -- defaults to `-10.0`): Threshold level (dB).
* `(unlabeled)` (`source(audio='#g,video='#h,midi='#i)`)

#### mic_filter
```
(source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Remove low frequencies often produced by microphones.

* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`): The input source.

#### mix
```
(?id:string,[source(audio='#a,video='#b,midi='#c)])->
source(audio='#a,video='#b,midi='#c)```

Mixing table controllable via the telnet interface.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`[source(audio='#a,video='#b,midi='#c)]`)

#### normalize
```
(?id:string,?gain_max:'a,?gain_min:'b,?k_down:'c,
 ?k_up:'d,?target:'e,?threshold:'f,?window:float,
 source(audio='#g,video='#h,midi='#i))->
source(audio='#g,video='#h,midi='#i)
where 'a, 'b, 'c, 'd, 'e, 'f is either float or ()->float```

Normalize the signal. Dynamic normalization of the signal is sometimes the only option, and can make a listening experience much nicer. However, its dynamic aspect implies some limitations which can go as far as creating saturation in some extreme cases. If possible, consider using some track-based normalization techniques such as those based on replay gain. See the documentation for more details.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `gain_max` (```
anything that is either float or ()->float```
 -- defaults to `6.0`): Maximal gain value (dB).
* `gain_min` (```
anything that is either float or ()->float```
 -- defaults to `-6.0`): Minimal gain value (dB).
* `k_down` (```
anything that is either float or ()->float```
 -- defaults to `0.1`): Coefficient when the power must go down (between 0 and 1, slowest to fastest).
* `k_up` (```
anything that is either float or ()->float```
 -- defaults to `0.005`): Coefficient when the power must go up (between 0 and 1, slowest to fastest).
* `target` (```
anything that is either float or ()->float```
 -- defaults to `-13.0`): Desired RMS (dB).
* `threshold` (```
anything that is either float or ()->float```
 -- defaults to `-40.0`): Minimal RMS for activaing gain control (dB).
* `window` (`float` -- defaults to `0.1`): Duration of the window used to compute the current RMS power (second).
* `(unlabeled)` (`source(audio='#g,video='#h,midi='#i)`)

#### nrj
```
(source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Compress and normalize, producing a more uniform and ``full'' sound.

* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`): The input source.

#### pipe
```
(?id:string,?buffer:float,?max:float,process:string,
 ?restart:bool,?restart_on_error:bool,
 source(audio='#a+1,video=0,midi=0))->
source(audio='#b+1,video=0,midi=0)```

Process audio signal through a given process stdin/stdout.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `buffer` (`float` -- defaults to `1.0`): Duration of the pre-buffered data.
* `max` (`float` -- defaults to `10.0`): Maximum duration of the buffered data.
* `process` (`string`): Process used to pipe data to.
* `restart` (`bool` -- defaults to `true`): Restart process when exited.
* `restart_on_error` (`bool` -- defaults to `true`): Restart process when exited with error.
* `(unlabeled)` (`source(audio='#a+1,video=0,midi=0)`)

#### sky
```
(source(audio='#a,video='#b,midi=0))->
source(audio='#a,video='#b,midi=0)```

Multiband-compression.

* `(unlabeled)` (`source(audio='#a,video='#b,midi=0)`): The input source.

#### soundtouch
```
(?id:string,?pitch:'a,?rate:'b,?tempo:'c,
 source(audio='#d+1,video=0,midi=0))->
source(audio='#d+1,video=0,midi=0)
where 'a, 'b, 'c is either float or ()->float```

WARNING: This is only EXPERIMENTAL!

Change the rate, the tempo or the pitch of the sound.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `pitch` (```
anything that is either float or ()->float```
 -- defaults to `1.0`)
* `rate` (```
anything that is either float or ()->float```
 -- defaults to `1.0`)
* `tempo` (```
anything that is either float or ()->float```
 -- defaults to `1.0`)
* `(unlabeled)` (`source(audio='#d+1,video=0,midi=0)`)

#### stereo.ms.decode
```
(?id:string,?width:float,source(audio=2,video=0,midi=0))->
source(audio=2,video=0,midi=0)```

Decode mid+side stereo (M/S) to left+right stereo.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `width` (`float` -- defaults to `1.0`): Width of the stereo field.
* `(unlabeled)` (`source(audio=2,video=0,midi=0)`)

#### stereo.ms.encode
```
(?id:string,source(audio=2,video=0,midi=0))->
source(audio=2,video=0,midi=0)```

Encode left+right stereo to mid+side stereo (M/S).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio=2,video=0,midi=0)`)

#### stereo.pan
```
(?id:string,?field:'a,?pan:'b,
 source(audio=2,video=0,midi=0))->
source(audio=2,video=0,midi=0)
where 'a, 'b is either float or ()->float```

Pan a stereo sound.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `field` (```
anything that is either float or ()->float```
 -- defaults to `90.0`): Field width in degrees (between 0 and 90).
* `pan` (```
anything that is either float or ()->float```
 -- defaults to `0.0`): Pan ranges between -1 and 1.
* `(unlabeled)` (`source(audio=2,video=0,midi=0)`)

#### stretch
```
(?id:string,?active:bool,ratio:'a,
 source(audio='#b+1,video=0,midi=0))->
source(audio='#c+1,video=0,midi=0)
where 'a is either float or ()->float```

Slow down or accelerate an audio stream by stretching (sounds lower) or squeezing it (sounds higher).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `active` (`bool` -- defaults to `true`): The active behavior is to keep ticking the child's clock when the operator is not streaming. Otherwise the child's clock is strictly based on what is streamed off the child source, which results in time-dependent active sources to be frozen when that source is stopped.
* `ratio` (```
anything that is either float or ()->float```
): A value higher than 1 means slowing down.
* `(unlabeled)` (`source(audio='#b+1,video=0,midi=0)`)

Source / Sound Synthesis
------------------------
#### dssi.register
```
(string)->unit```

Resgister a DSSI plugin.

* `(unlabeled)` (`string`): Path of the DSSI plugin file.

#### synth.all.saw
```
(?id:string,?attack:float,?decay:float,?envelope:bool,
 ?release:float,?sustain:float,
 source(audio='#a+1,video='#b,midi='#c+16))->
source(audio='#a+1,video='#b,midi='#c+16)```

Saw synthesizer. It creates one synthesizer for each channel.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `attack` (`float` -- defaults to `0.02`): Envelope attack (in seconds).
* `decay` (`float` -- defaults to `0.01`): Envelope decay (in seconds).
* `envelope` (`bool` -- defaults to `true`): Use envelope.
* `release` (`float` -- defaults to `0.01`): Envelope release (in seconds).
* `sustain` (`float` -- defaults to `0.9`): Envelope sustain level.
* `(unlabeled)` (```
source(audio='#a+1,video='#b,midi='#c+16)```
)

#### synth.all.sine
```
(?id:string,?attack:float,?decay:float,?envelope:bool,
 ?release:float,?sustain:float,
 source(audio='#a+1,video='#b,midi='#c+16))->
source(audio='#a+1,video='#b,midi='#c+16)```

Sine synthesizer. It creates one synthesizer for each channel.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `attack` (`float` -- defaults to `0.02`): Envelope attack (in seconds).
* `decay` (`float` -- defaults to `0.01`): Envelope decay (in seconds).
* `envelope` (`bool` -- defaults to `true`): Use envelope.
* `release` (`float` -- defaults to `0.01`): Envelope release (in seconds).
* `sustain` (`float` -- defaults to `0.9`): Envelope sustain level.
* `(unlabeled)` (```
source(audio='#a+1,video='#b,midi='#c+16)```
)

#### synth.all.square
```
(?id:string,?attack:float,?decay:float,?envelope:bool,
 ?release:float,?sustain:float,
 source(audio='#a+1,video='#b,midi='#c+16))->
source(audio='#a+1,video='#b,midi='#c+16)```

Square synthesizer. It creates one synthesizer for each channel.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `attack` (`float` -- defaults to `0.02`): Envelope attack (in seconds).
* `decay` (`float` -- defaults to `0.01`): Envelope decay (in seconds).
* `envelope` (`bool` -- defaults to `true`): Use envelope.
* `release` (`float` -- defaults to `0.01`): Envelope release (in seconds).
* `sustain` (`float` -- defaults to `0.9`): Envelope sustain level.
* `(unlabeled)` (```
source(audio='#a+1,video='#b,midi='#c+16)```
)

#### synth.saw
```
(?id:string,?attack:float,?channel:int,?decay:float,
 ?envelope:bool,?release:float,?sustain:float,
 ?volume:float,source(audio='#a+1,video='#b,midi='#c+1))->
source(audio='#a+1,video='#b,midi='#c+1)```

Saw synthesizer.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `attack` (`float` -- defaults to `0.02`): Envelope attack (in seconds).
* `channel` (`int` -- defaults to `0`): MIDI channel to handle.
* `decay` (`float` -- defaults to `0.01`): Envelope decay (in seconds).
* `envelope` (`bool` -- defaults to `true`): Use envelope.
* `release` (`float` -- defaults to `0.05`): Envelope release (in seconds).
* `sustain` (`float` -- defaults to `0.9`): Envelope sustain level.
* `volume` (`float` -- defaults to `0.3`): Initial volume.
* `(unlabeled)` (```
source(audio='#a+1,video='#b,midi='#c+1)```
)

#### synth.sine
```
(?id:string,?attack:float,?channel:int,?decay:float,
 ?envelope:bool,?release:float,?sustain:float,
 ?volume:float,source(audio='#a+1,video='#b,midi='#c+1))->
source(audio='#a+1,video='#b,midi='#c+1)```

Sine synthesizer.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `attack` (`float` -- defaults to `0.02`): Envelope attack (in seconds).
* `channel` (`int` -- defaults to `0`): MIDI channel to handle.
* `decay` (`float` -- defaults to `0.01`): Envelope decay (in seconds).
* `envelope` (`bool` -- defaults to `true`): Use envelope.
* `release` (`float` -- defaults to `0.05`): Envelope release (in seconds).
* `sustain` (`float` -- defaults to `0.9`): Envelope sustain level.
* `volume` (`float` -- defaults to `0.3`): Initial volume.
* `(unlabeled)` (```
source(audio='#a+1,video='#b,midi='#c+1)```
)

#### synth.square
```
(?id:string,?attack:float,?channel:int,?decay:float,
 ?envelope:bool,?release:float,?sustain:float,
 ?volume:float,source(audio='#a+1,video='#b,midi='#c+1))->
source(audio='#a+1,video='#b,midi='#c+1)```

Square synthesizer.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `attack` (`float` -- defaults to `0.02`): Envelope attack (in seconds).
* `channel` (`int` -- defaults to `0`): MIDI channel to handle.
* `decay` (`float` -- defaults to `0.01`): Envelope decay (in seconds).
* `envelope` (`bool` -- defaults to `true`): Use envelope.
* `release` (`float` -- defaults to `0.05`): Envelope release (in seconds).
* `sustain` (`float` -- defaults to `0.9`): Envelope sustain level.
* `volume` (`float` -- defaults to `0.3`): Initial volume.
* `(unlabeled)` (```
source(audio='#a+1,video='#b,midi='#c+1)```
)

Source / Track Processing
-------------------------
#### append
```
(?id:string,?insert_missing:bool,?merge:bool,source('a),
 (([(string*string)])->source('a)))->source('a)```

Append an extra track to every track. Set the metadata 'liq_append' to 'false' to inhibit appending on one track.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `insert_missing` (`bool` -- defaults to `true`): Treat track beginnings without metadata as having empty one.
* `merge` (`bool` -- defaults to `false`): Merge the track with its appended track.
* `(unlabeled)` (`source('a)`)
* `(unlabeled)` (`([(string*string)])->source('a)`): Given the metadata, build the source producing the track to append. This source is allowed to fail (produce nothing) if no relevant track is to be appended.

#### at
```
((()->bool),source('a))->source('a)```

Restrict a source to play only when a predicate is true.

* `(unlabeled)` (`()->bool`): The predicate, typically a time interval such as `{10h-10h30}`.
* `(unlabeled)` (`source('a)`)

#### crossfade
```
(?start_next:float,?override_duration:string,
 ?fade_in:float,?fade_out:float,
 ?default:((source(audio='#a+1,video=0,midi=0),
            source(audio='#a+1,video=0,midi=0))->
           source(audio='#a+1,video=0,midi=0)),
 ?high:float,?medium:float,?margin:float,?width:float,
 ?conservative:bool,source(audio='#a+1,video=0,midi=0))->
source(audio='#a+1,video=0,midi=0)```

Crossfade between tracks, taking the respective volume levels into account in the choice of the transition.

* `start_next` (`float` -- defaults to `5.0`): Crossing duration, if any.
* `override_duration` (`string` -- defaults to `"liq_cross_duration"`): Metadata field which, if present and containing a float, overrides the 'duration' parameter for current track. 
* `fade_in` (`float` -- defaults to `3.0`): Fade-in duration, if any.
* `fade_out` (`float` -- defaults to `3.0`): Fade-out duration, if any.
* `default` (```
(source(audio='#a+1,video=0,midi=0),
 source(audio='#a+1,video=0,midi=0))->
source(audio='#a+1,video=0,midi=0)```
 -- defaults to <code><fun></code>): Transition used when no rule applies (default: sequence).
* `high` (`float` -- defaults to `-15.0`): Value, in dB, for loud sound level.
* `medium` (`float` -- defaults to `-32.0`): Value, in dB, for medium sound level.
* `margin` (`float` -- defaults to `4.0`): Margin to detect sources that have too different sound level for crossing.
* `width` (`float` -- defaults to `2.0`): Width of the volume analysis window.
* `conservative` (`bool` -- defaults to `true`): Always prepare for a premature end-of-track.
* `(unlabeled)` (`source(audio='#a+1,video=0,midi=0)`): The input source.

#### cue_cut
```
(?id:string,?cue_in_metadata:string,
 ?cue_out_metadata:string,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Start track after a cue in point and stop it at cue out point. The cue points are given as metadata, in seconds from the begining of tracks.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `cue_in_metadata` (`string` -- defaults to `"liq_cue_in"`): Metadata for cue in points.
* `cue_out_metadata` (`string` -- defaults to `"liq_cue_out"`): Metadata for cue out points.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### delay
```
(?id:string,?initial:bool,float,source('a))->source('a)```

Prevents the child from being ready again too fast after a end of track

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `initial` (`bool` -- defaults to `false`): Start in unavailable state, as if a track had just finished.
* `(unlabeled)` (`float`): The source won't be ready less than this amount of seconds after any end of track
* `(unlabeled)` (`source('a)`)

#### drop_metadata
```
(source('a))->source('a)```

Removes all metadata coming from a source.

* `(unlabeled)` (`source('a)`)

#### eat_blank
```
(?id:string,?at_beginning:bool,?max_blank:float,
 ?min_noise:float,?start_blank:bool,?threshold:float,
 ?track_sensitive:bool,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Eat blanks, i.e., drop the contents of the stream until it is not blank anymore.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `at_beginning` (`bool` -- defaults to `false`): Only eat at the beginning of a track.
* `max_blank` (`float` -- defaults to `20.0`): Maximum duration of silence allowed, in seconds.
* `min_noise` (`float` -- defaults to `0.0`): Minimum duration of noise required to end silence, in seconds.
* `start_blank` (`bool` -- defaults to `false`): Start assuming we have blank.
* `threshold` (`float` -- defaults to `-40.0`): Power in decibels under which the stream is considered silent.
* `track_sensitive` (`bool` -- defaults to `true`): Reset blank counter at each track.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### fallback
```
(?id:string,?override:string,?replay_metadata:bool,
 ?track_sensitive:'a,?transition_length:float,
 ?transitions:[(source('b),source('b))->source('b)],
 [source('b)])->source('b)
where 'a is either bool or ()->bool```

At the beginning of each track, select the first ready child.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `override` (`string` -- defaults to `"liq_transition_length"`): Metadata field which, if present and containing a float, overrides the `transition_length` parameter.
* `replay_metadata` (`bool` -- defaults to `true`): Replay the last metadata of a child when switching to it in the middle of a track.
* `track_sensitive` (```
anything that is either bool or ()->bool```
 -- defaults to `true`): Re-select only on end of tracks.
* `transition_length` (`float` -- defaults to `5.0`): Maximun transition duration.
* `transitions` (`[(source('b),source('b))->source('b)]` -- defaults to `[]`): Transition functions, padded with `fun (x,y) -> y` functions.
* `(unlabeled)` (`[source('b)]`): Select the first ready source in this list.

#### fallback.skip
```
(input:source('a),source('a))->source('a)```

Special track insensitive fallback that always skips current song before switching.

* `input` (`source('a)`): The input source
* `(unlabeled)` (`source('a)`): The fallback source

#### insert_metadata
```
(?id:string,source('a))->
((([(string*string)])->unit)*source('a))```

Dynamically insert metadata in a stream. Returns a pair (f,s) where s is a new source and f is a function of type (metadata)->unit, used to insert metadata in s.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source('a)`)

#### map_first_track
```
(?id:string,((source('a))->source('a)),source('a))->
source('a)```

Apply a function to the first track of a source

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`(source('a))->source('a)`): The applied function.
* `(unlabeled)` (`source('a)`): The input source.

#### map_metadata
```
(?id:string,?insert_missing:bool,?strip:bool,
 ?update:bool,(([(string*string)])->[(string*string)]),
 source('a))->source('a)```

Rewrite metadata on the fly using a function.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `insert_missing` (`bool` -- defaults to `true`): Treat track beginnings without metadata as having empty ones. The operational order is: create empty if needed, map and strip if enabled.
* `strip` (`bool` -- defaults to `false`): Completely remove empty metadata. Operates on both empty values and empty metadata chunk.
* `update` (`bool` -- defaults to `true`): Update metadata. If false, existing metadata are cleared and only returned values are set as new metadata.
* `(unlabeled)` (`([(string*string)])->[(string*string)]`): A function that returns new metadata.
* `(unlabeled)` (`source('a)`)

#### max_duration
```
(?id:string,?override:string,float,source('a))->
source('a)```

Limit source duration

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `override` (`string` -- defaults to `"liq_remaining"`): Metadata field which, if present and containing a float, overrides the remaining play time.
* `(unlabeled)` (`float`): Maximum duration
* `(unlabeled)` (`source('a)`)

#### merge_tracks
```
(?id:string,source('a))->source('a)```

Merge consecutive tracks from the input source.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source('a)`)

#### mksafe
```
(?id:string,source('a))->source('a)```

Turn a source into an infaillible source by adding blank when the source is not available.

* `id` (`string` -- defaults to `"mksafe"`)
* `(unlabeled)` (`source('a)`): the source to turn infaillible

#### notify_metadata
```
(?urgency:string,?icon:string,?time:int,
 ?display:(([(string*string)])->string),?title:string,
 source('a))->source('a)```

Use notify to display metadata info.

* `urgency` (`string` -- defaults to `"low"`): Urgency (low|normal|critical).
* `icon` (`string` -- defaults to `"stock_smiley-22"`): Icon filename or stock icon to display.
* `time` (`int` -- defaults to `3000`): Timeout in milliseconds.
* `display` (`([(string*string)])->string` -- defaults to <code><fun></code>): Function used to display a metadata packet.
* `title` (`string` -- defaults to `"Liquidsoap: new track"`): Title of the notification message.
* `(unlabeled)` (`source('a)`)

#### on_blank
```
(?id:string,?max_blank:float,?min_noise:float,
 ?on_noise:(()->unit),?start_blank:bool,?threshold:float,
 ?track_sensitive:bool,(()->unit),
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Calls a given handler when detecting a blank.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `max_blank` (`float` -- defaults to `20.0`): Maximum duration of silence allowed, in seconds.
* `min_noise` (`float` -- defaults to `0.0`): Minimum duration of noise required to end silence, in seconds.
* `on_noise` (`()->unit` -- defaults to `{()}`): Handler called when noise is detected.
* `start_blank` (`bool` -- defaults to `false`): Start assuming we have blank.
* `threshold` (`float` -- defaults to `-40.0`): Power in decibels under which the stream is considered silent.
* `track_sensitive` (`bool` -- defaults to `true`): Reset blank counter at each track.
* `(unlabeled)` (`()->unit`): Handler called when blank is detected.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### on_end
```
(?id:string,?delay:'a,((float,[(string*string)])->unit),
 source('b))->source('b)
where 'a is either float or ()->float```

Call a given handler when there is less than a given amount of time remaining before then end of track.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `delay` (```
anything that is either float or ()->float```
 -- defaults to `5.0`): Execute handler when remaining time is less or equal to this value.
* `(unlabeled)` (`(float,[(string*string)])->unit`): Function to execute. First argument is the remaining time, second is the latest metadata. That function should be fast because it is executed in the main streaming thread.
* `(unlabeled)` (`source('b)`)

#### on_metadata
```
(?id:string,(([(string*string)])->unit),source('a))->
source('a)```

Call a given handler on metadata packets.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`([(string*string)])->unit`): Function called on every metadata packet in the stream. It should be fast because it is executed in the main streaming thread.
* `(unlabeled)` (`source('a)`)

#### on_offset
```
(?id:string,?force:bool,?offset:float,?override:string,
 ((float,[(string*string)])->unit),source('a))->
source('a)```

Call a given handler when position in track is equal or more than a given amount of time.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `force` (`bool` -- defaults to `false`): Force execution of callback if track ends before 'offset' position has been reached.
* `offset` (`float` -- defaults to `-1.0`): Execute handler when position in track is equal or more than to this value.
* `override` (`string` -- defaults to `"liq_on_offset"`): Metadata field which, if present and containing a float, overrides the 'offset' parameter.
* `(unlabeled)` (`(float,[(string*string)])->unit`): Function to execute. First argument is the actual position within the current track, second is the latest metadata. That function should be fast because it is executed in the main streaming thread.
* `(unlabeled)` (`source('a)`)

#### on_track
```
(?id:string,(([(string*string)])->unit),source('a))->
source('a)```

Call a given handler on new tracks.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`([(string*string)])->unit`): Function called on every beginning of track in the stream, with the corresponding metadata as argument. If there is no metadata at the beginning of track, the empty list is passed. That function should be fast because it is executed in the main streaming thread.
* `(unlabeled)` (`source('a)`)

#### once
```
(source('a))->source('a)```

Creates a source that plays only one track of the input source.

* `(unlabeled)` (`source('a)`): The input source.

#### osd_metadata
```
(?color:string,?position:string,?font:string,
 ?display:(([(string*string)])->string),source('a))->
source('a)```

Use X On Screen Display to display metadata info.

* `color` (`string` -- defaults to `"green"`): Color of the text.
* `position` (`string` -- defaults to `"top"`): Position of the text (top|middle|bottom).
* `font` (`string` -- defaults to `"-*-courier-*-r-*-*-*-240-*-*-*-*-*-*"`): Font used (xfontsel is your friend...)
* `display` (`([(string*string)])->string` -- defaults to <code><fun></code>): Function used to display a metadata packet.
* `(unlabeled)` (`source('a)`)

#### overlap_sources
```
(?id:string,?normalize:bool,?start_next:string,
 ?weights:[int],[source(audio='#a,video='#b,midi=0)])->
source(audio='#a,video='#b,midi=0)```

Rotate between overlapping sources. Next track starts according to 'liq_start_next' offset metadata.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `normalize` (`bool` -- defaults to `false`)
* `start_next` (`string` -- defaults to `"liq_start_next"`): Metadata field indicating when the next track should start, relative to current track's time.
* `weights` (`[int]` -- defaults to `[]`): Relative weight of the sources in the sum. The empty list stands for the homogeneous distribution.
* `(unlabeled)` (`[source(audio='#a,video='#b,midi=0)]`): Sources to toggle from

#### playlist.merge
```
(?id:string,?random:bool,string)->source('a)```

Play the whole playlist as one track.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `random` (`bool` -- defaults to `false`): Randomize playlist content
* `(unlabeled)` (`string`): Playlist URI.

#### prepend
```
(?id:string,?merge:bool,
 source(audio='#a,video='#b,midi='#c),
 (([(string*string)])->
  source(audio='#a,video='#b,midi='#c)))->
source(audio='#a,video='#b,midi='#c)```

Prepend an extra track before every track. Set the metadata 'liq_prepend' to 'false' to inhibit prepending on one track.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `merge` (`bool` -- defaults to `false`): Merge the track with its appended track.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)
* `(unlabeled)` (```
([(string*string)])->source(audio='#a,video='#b,midi='#c)```
): Given the metadata, build the source producing the track to prepend. This source is allowed to fail (produce nothing) if no relevant track is to be appended. However, success must be immediate or it will not be taken into account.

#### random
```
(?id:string,?override:string,?replay_metadata:bool,
 ?track_sensitive:'a,?transition_length:float,
 ?transitions:[(source('b),source('b))->source('b)],
 ?weights:[int],[source('b)])->source('b)
where 'a is either bool or ()->bool```

At the beginning of every track, select a random ready child.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `override` (`string` -- defaults to `"liq_transition_length"`): Metadata field which, if present and containing a float, overrides the `transition_length` parameter.
* `replay_metadata` (`bool` -- defaults to `true`): Replay the last metadata of a child when switching to it in the middle of a track.
* `track_sensitive` (```
anything that is either bool or ()->bool```
 -- defaults to `true`): Re-select only on end of tracks.
* `transition_length` (`float` -- defaults to `5.0`): Maximun transition duration.
* `transitions` (`[(source('b),source('b))->source('b)]` -- defaults to `[]`): Transition functions, padded with `fun (x,y) -> y` functions.
* `weights` (`[int]` -- defaults to `[]`): Weights of the children (padded with 1), defining for each child the probability that it is selected.
* `(unlabeled)` (`[source('b)]`)

#### rotate
```
(?id:string,?override:string,?replay_metadata:bool,
 ?track_sensitive:'a,?transition_length:float,
 ?transitions:[(source('b),source('b))->source('b)],
 ?weights:[int],[source('b)])->source('b)
where 'a is either bool or ()->bool```

Rotate between the sources.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `override` (`string` -- defaults to `"liq_transition_length"`): Metadata field which, if present and containing a float, overrides the `transition_length` parameter.
* `replay_metadata` (`bool` -- defaults to `true`): Replay the last metadata of a child when switching to it in the middle of a track.
* `track_sensitive` (```
anything that is either bool or ()->bool```
 -- defaults to `true`): Re-select only on end of tracks.
* `transition_length` (`float` -- defaults to `5.0`): Maximun transition duration.
* `transitions` (`[(source('b),source('b))->source('b)]` -- defaults to `[]`): Transition functions, padded with `fun (x,y) -> y` functions.
* `weights` (`[int]` -- defaults to `[]`): Weights of the children (padded with 1), defining for each child how many tracks are played from it per round, if that many are actually available.
* `(unlabeled)` (`[source('b)]`)

#### rotate.merge
```
(?id:string,?track_sensitive:bool,
 ?transitions:[(source('a),source('a))->source('a)],
 ?weights:[int],[source('a)])->source('a)```

Same operator as rotate but merges tracks from each sources.
For instance, `rotate.merge([intro,main,outro])` creates a source that plays a sequence `[intro,main,outro]` as single track and loops back.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `track_sensitive` (`bool` -- defaults to `true`): Re-select only on end of tracks.
* `transitions` (`[(source('a),source('a))->source('a)]` -- defaults to `[]`): Transition functions, padded with `fun (x,y) -> y` functions.
* `weights` (`[int]` -- defaults to `[]`): Weights of the children (padded with 1), defining for each child how many tracks are played from it per round, if that many are actually available.
* `(unlabeled)` (`[source('a)]`): Sequence of sources to be merged

#### say_metadata
```
(source(?A),?pattern:string)->source(?A)```

Append speech-synthesized tracks reading the metadata.

* `(unlabeled)` (`source(?A)`): The source to use
* `pattern` (`string` -- defaults to ```
"say:$(if $(artist),\"It was $(artist)$(if $(title),\\\", $(title)\\\").\")"```
): Pattern to use

#### sequence
```
(?id:string,?merge:bool,[source('a)])->source('a)```

Play only one track of every successive source, except for the last one which is played as much as available.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `merge` (`bool` -- defaults to `false`): Merge tracks when advancing from one source to the next one. This will NOT merge consecutive tracks from the last source; see merge_tracks() if you need that too.
* `(unlabeled)` (`[source('a)]`)

#### server.insert_metadata
```
(?id:string,source('a))->source('a)```

Register a server/telnet command to update a source's metadata. Returns a new source, which will receive the updated metadata. The command has the following format: insert key1=``val1'',key2=``val2'',...

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source('a)`)

#### skip_blank
```
(?id:string,?threshold:float,?max_blank:float,
 ?min_noise:float,?track_sensitive:bool,
 source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Skip track when detecting a blank.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `threshold` (`float` -- defaults to `-40.0`): Power in decibels under which the stream is considered silent.
* `max_blank` (`float` -- defaults to `20.0`): Maximum silence length allowed, in seconds.
* `min_noise` (`float` -- defaults to `0.0`): Minimum duration of noise required to end silence, in seconds.
* `track_sensitive` (`bool` -- defaults to `true`): Reset blank counter at each track.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### smooth_add
```
(?delay:float,?p:'a,
 normal:source(audio='#b,video='#c,midi=0),
 special:source(audio='#b,video='#c,midi=0))->
source(audio='#b,video='#c,midi=0)
where 'a is either float or ()->float```

Mixes two streams, with faded transitions between the state when only the normal stream is available and when the special stream gets added on top of it.

* `delay` (`float` -- defaults to `0.5`): Delay before starting the special source.
* `p` (```
anything that is either float or ()->float```
 -- defaults to `0.2`): Portion of amplitude of the normal source in the mix.
* `normal` (`source(audio='#b,video='#c,midi=0)`): The normal source, which could be called the carrier too.
* `special` (`source(audio='#b,video='#c,midi=0)`): The special source.

#### source.dynamic
```
(?id:string,(()->[source('a)]))->source('a)```

WARNING: This is only EXPERIMENTAL!

Dynamically change the underlying source.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`()->[source('a)]`)

#### store_metadata
```
(?id:string,?size:int,source('a))->source('a)```

Keep track of the last N metadata packets in the stream, and make the history available via a server command.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `size` (`int` -- defaults to `10`): Size of the history
* `(unlabeled)` (`source('a)`)

#### strip_blank
```
(?id:string,?max_blank:float,?min_noise:float,
 ?start_blank:bool,?threshold:float,
 ?track_sensitive:bool,
 source(audio='#a,video='#b,midi='#c))->
active_source(audio='#a,video='#b,midi='#c)```

Make the source unavailable when it is streaming blank.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `max_blank` (`float` -- defaults to `20.0`): Maximum duration of silence allowed, in seconds.
* `min_noise` (`float` -- defaults to `0.0`): Minimum duration of noise required to end silence, in seconds.
* `start_blank` (`bool` -- defaults to `false`): Start assuming we have blank.
* `threshold` (`float` -- defaults to `-40.0`): Power in decibels under which the stream is considered silent.
* `track_sensitive` (`bool` -- defaults to `true`): Reset blank counter at each track.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### switch
```
(?id:string,?override:string,?replay_metadata:bool,
 ?single:[bool],?track_sensitive:'a,
 ?transition_length:float,
 ?transitions:[(source('b),source('b))->source('b)],
 [((()->bool)*source('b))])->source('b)
where 'a is either bool or ()->bool```

At the beginning of a track, select the first source whose predicate is true.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `override` (`string` -- defaults to `"liq_transition_length"`): Metadata field which, if present and containing a float, overrides the `transition_length` parameter.
* `replay_metadata` (`bool` -- defaults to `true`): Replay the last metadata of a child when switching to it in the middle of a track.
* `single` (`[bool]` -- defaults to `[]`): Forbid the selection of a branch for two tracks in a row. The empty list stands for `[false,...,false]`.
* `track_sensitive` (```
anything that is either bool or ()->bool```
 -- defaults to `true`): Re-select only on end of tracks.
* `transition_length` (`float` -- defaults to `5.0`): Maximun transition duration.
* `transitions` (`[(source('b),source('b))->source('b)]` -- defaults to `[]`): Transition functions, padded with `fun (x,y) -> y` functions.
* `(unlabeled)` (`[((()->bool)*source('b))]`): Sources with the predicate telling when they can be played.

Source / Video Processing
-------------------------
#### video.add_image
```
(?id:string,?width:int,?height:int,?x:int,?y:int,file:'a,
 source(audio='#b,video='#c,midi=0))->
source(audio='#b,video='#c,midi=0)```

Add a static image on the first video channel.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `width` (`int` -- defaults to `0`): Scale to width (zero means frame width, negative means original width).
* `height` (`int` -- defaults to `0`): Scale to height (zero means frame height, negative means original height).
* `x` (`int` -- defaults to `4611686018427387903`): x position.
* `y` (`int` -- defaults to `4611686018427387903`): y position.
* `file` (`'a`): Path to the image file.
* `(unlabeled)` (`source(audio='#b,video='#c,midi=0)`)

#### video.add_text
```
(?id:string,?color:int,?cycle:bool,?font:string,
 ?metadata:string,?size:int,?speed:int,?x:'a,?y:'b,'c,
 source(audio='#d,video='#e+1,midi='#f))->
source(audio='#d,video='#e+1,midi='#f)
where 'a, 'b is either int or ()->int,
  'c is either string or ()->string```

Display a text.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `color` (`int` -- defaults to `16777215`): Text color (in 0xRRGGBB format).
* `cycle` (`bool` -- defaults to `true`): Cycle text when it reaches left boundary.
* `font` (`string` -- defaults to ```
"/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"```
): Path to ttf font file.
* `metadata` (`string` -- defaults to `""`): Change text on a particular metadata (empty string means disabled).
* `size` (`int` -- defaults to `18`): Font size.
* `speed` (`int` -- defaults to `70`): Horizontal speed in pixels per second (0 means no scrolling and update according to x and y in case they are variable).
* `x` (`anything that is either int or ()->int` -- defaults to `10`): x offset.
* `y` (`anything that is either int or ()->int` -- defaults to `10`): y offset.
* `(unlabeled)` (```
anything that is either string or ()->string```
): Text to display.
* `(unlabeled)` (`source(audio='#d,video='#e+1,midi='#f)`)

#### video.add_text.gstreamer
```
(?id:string,?color:int,?cycle:bool,?font:string,
 ?metadata:string,?size:int,?speed:int,?x:'a,?y:'b,'c,
 source(audio='#d,video='#e+1,midi='#f))->
source(audio='#d,video='#e+1,midi='#f)
where 'a, 'b is either int or ()->int,
  'c is either string or ()->string```

Display a text.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `color` (`int` -- defaults to `16777215`): Text color (in 0xRRGGBB format).
* `cycle` (`bool` -- defaults to `true`): Cycle text when it reaches left boundary.
* `font` (`string` -- defaults to ```
"/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"```
): Path to ttf font file.
* `metadata` (`string` -- defaults to `""`): Change text on a particular metadata (empty string means disabled).
* `size` (`int` -- defaults to `18`): Font size.
* `speed` (`int` -- defaults to `70`): Horizontal speed in pixels per second (0 means no scrolling and update according to x and y in case they are variable).
* `x` (`anything that is either int or ()->int` -- defaults to `10`): x offset.
* `y` (`anything that is either int or ()->int` -- defaults to `10`): y offset.
* `(unlabeled)` (```
anything that is either string or ()->string```
): Text to display.
* `(unlabeled)` (`source(audio='#d,video='#e+1,midi='#f)`)

#### video.add_text.sdl
```
(?id:string,?color:int,?cycle:bool,?font:string,
 ?metadata:string,?size:int,?speed:int,?x:'a,?y:'b,'c,
 source(audio='#d,video='#e+1,midi='#f))->
source(audio='#d,video='#e+1,midi='#f)
where 'a, 'b is either int or ()->int,
  'c is either string or ()->string```

Display a text.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `color` (`int` -- defaults to `16777215`): Text color (in 0xRRGGBB format).
* `cycle` (`bool` -- defaults to `true`): Cycle text when it reaches left boundary.
* `font` (`string` -- defaults to ```
"/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"```
): Path to ttf font file.
* `metadata` (`string` -- defaults to `""`): Change text on a particular metadata (empty string means disabled).
* `size` (`int` -- defaults to `18`): Font size.
* `speed` (`int` -- defaults to `70`): Horizontal speed in pixels per second (0 means no scrolling and update according to x and y in case they are variable).
* `x` (`anything that is either int or ()->int` -- defaults to `10`): x offset.
* `y` (`anything that is either int or ()->int` -- defaults to `10`): y offset.
* `(unlabeled)` (```
anything that is either string or ()->string```
): Text to display.
* `(unlabeled)` (`source(audio='#d,video='#e+1,midi='#f)`)

#### video.fade.in
```
(?id:string,?duration:float,?override:string,
 ?transition:string,?type:string,
 source(audio='#a,video='#b+1,midi='#c))->
source(audio='#a,video='#b+1,midi='#c)```

Fade the beginning of tracks. Metadata 'liq_video_fade_in' can be used to set the duration for a specific track (float in seconds).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `duration` (`float` -- defaults to `3.0`): Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override.
* `override` (`string` -- defaults to `"liq_video_fade_in"`): Metadata field which, if present and containing a float, overrides the 'duration' parameter for current track.
* `transition` (`string` -- defaults to `"fade"`): Kind of transition (fade|slide_left|slide_right|slide_up|slide_down|grow|disc|random).
* `type` (`string` -- defaults to `"lin"`): Fader shape (lin|sin|log|exp): linear, sinusoidal, logarithmic or exponential.
* `(unlabeled)` (`source(audio='#a,video='#b+1,midi='#c)`)

#### video.fade.out
```
(?id:string,?duration:float,?override:string,
 ?transition:string,?type:string,
 source(audio='#a,video='#b+1,midi='#c))->
source(audio='#a,video='#b+1,midi='#c)```

Fade the end of tracks. Metadata 'liq_video_fade_out' can be used to set the duration for a specific track (float in seconds).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `duration` (`float` -- defaults to `3.0`): Duration of the fading. This value can be set on a per-file basis using the metadata field passed as override.
* `override` (`string` -- defaults to `"liq_video_fade_out"`): Metadata field which, if present and containing a float, overrides the 'duration' parameter for current track.
* `transition` (`string` -- defaults to `"fade"`): Kind of transition (fade|slide_left|slide_right|slide_up|slide_down|grow|disc|random).
* `type` (`string` -- defaults to `"lin"`): Fader shape (lin|sin|log|exp): linear, sinusoidal, logarithmic or exponential.
* `(unlabeled)` (`source(audio='#a,video='#b+1,midi='#c)`)

#### video.fill
```
(?id:string,?color:int,
 source(audio='#a,video='#b+1,midi='#c))->
source(audio='#a,video='#b+1,midi='#c)```

Fill frame with a color.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `color` (`int` -- defaults to `0`): Color to fill the image with.
* `(unlabeled)` (`source(audio='#a,video='#b+1,midi='#c)`)

#### video.greyscale
```
(?id:string,source(audio='#a,video='#b+1,midi='#c))->
source(audio='#a,video='#b+1,midi='#c)```

Convert video to greyscale.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio='#a,video='#b+1,midi='#c)`)

#### video.invert
```
(?id:string,source(audio='#a,video='#b+1,midi='#c))->
source(audio='#a,video='#b+1,midi='#c)```

Invert video.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio='#a,video='#b+1,midi='#c)`)

#### video.lomo
```
(?id:string,source(audio='#a,video='#b+1,midi='#c))->
source(audio='#a,video='#b+1,midi='#c)```

Emulate the ``Lomo effect''.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio='#a,video='#b+1,midi='#c)`)

#### video.opacity
```
(?id:string,float,source(audio='#a,video='#b+1,midi='#c))->
source(audio='#a,video='#b+1,midi='#c)```

Scale opacity of video.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`float`): Coefficient to scale opacity with.
* `(unlabeled)` (`source(audio='#a,video='#b+1,midi='#c)`)

#### video.opacity.blur
```
(?id:string,source(audio='#a,video='#b+1,midi='#c))->
source(audio='#a,video='#b+1,midi='#c)```

Blur opacity of video.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio='#a,video='#b+1,midi='#c)`)

#### video.rotate
```
(?id:string,?angle:'a,?speed:'b,
 source(audio='#c,video='#d+1,midi='#e))->
source(audio='#c,video='#d+1,midi='#e)
where 'a, 'b is either float or ()->float```

Rotate video.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `angle` (```
anything that is either float or ()->float```
 -- defaults to `0.0`): Initial angle in radians.
* `speed` (```
anything that is either float or ()->float```
 -- defaults to `3.14159265359`): Rotation speed in radians per sec.
* `(unlabeled)` (`source(audio='#c,video='#d+1,midi='#e)`)

#### video.scale
```
(?id:string,?scale:float,?x:int,?xscale:float,?y:int,
 ?yscale:float,source(audio='#a,video='#b+1,midi='#c))->
source(audio='#a,video='#b+1,midi='#c)```

Scale and translate video.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `scale` (`float` -- defaults to `1.0`): Scaling coefficient in both directions.
* `x` (`int` -- defaults to `0`): x offset.
* `xscale` (`float` -- defaults to `1.0`): x scaling.
* `y` (`int` -- defaults to `0`): y offset.
* `yscale` (`float` -- defaults to `1.0`): y scaling.
* `(unlabeled)` (`source(audio='#a,video='#b+1,midi='#c)`)

#### video.sepia
```
(?id:string,source(audio='#a,video='#b+1,midi='#c))->
source(audio='#a,video='#b+1,midi='#c)```

Convert video to sepia.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio='#a,video='#b+1,midi='#c)`)

#### video.tile
```
(?id:string,?normalize:bool,?proportional:bool,
 ?weights:[int],[source(audio='#a,video='#b+1,midi='#c)])->
source(audio='#a,video='#b+1,midi='#c)```

Tile sources (same as add but produces tiles of videos).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `normalize` (`bool` -- defaults to `true`)
* `proportional` (`bool` -- defaults to `true`): Scale preserving the proportions.
* `weights` (`[int]` -- defaults to `[]`): Relative weight of the sources in the sum. The empty list stands for the homogeneous distribution.
* `(unlabeled)` (```
[source(audio='#a,video='#b+1,midi='#c)]```
)

#### video.transparent
```
(?id:string,?color:int,?precision:float,
 source(audio='#a,video='#b+1,midi='#c))->
source(audio='#a,video='#b+1,midi='#c)```

Set a color to be transparent.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `color` (`int` -- defaults to `0`): Color which should be transparent (in 0xRRGGBB format).
* `precision` (`float` -- defaults to `0.0`): Precision in color matching (0. means match precisely the color and 1. means match every color).
* `(unlabeled)` (`source(audio='#a,video='#b+1,midi='#c)`)

Source / Visualization
----------------------
#### peak
```
(?id:string,?duration:'a,
 source(audio='#b,video='#c,midi='#d))->
((()->float)*source(audio='#b,video='#c,midi='#d))
where 'a is either float or ()->float```

Get current peak volume of the source. Returns a pair `(f,s)` where s is a new source and `f` is a function of type `() -> float` and returns the current peak volume of the source.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `duration` (```
anything that is either float or ()->float```
 -- defaults to `0.5`): Duration of the window (in seconds). A value <= 0, means that computation should not be performed.
* `(unlabeled)` (`source(audio='#b,video='#c,midi='#d)`)

#### peak.stereo
```
(?id:string,?duration:'a,
 source(audio='#b+2,video='#c,midi='#d))->
((()->(float*float))*
 source(audio='#b+2,video='#c,midi='#d))
where 'a is either float or ()->float```

Get current peak volume of the source. Returns a pair `(f,s)` where s is a new source and `f` is a function of type `() -> float` and returns the current peak volume of the source.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `duration` (```
anything that is either float or ()->float```
 -- defaults to `0.5`): Duration of the window (in seconds). A value <= 0, means that computation should not be performed.
* `(unlabeled)` (`source(audio='#b+2,video='#c,midi='#d)`)

#### rms
```
(?id:string,?duration:'a,
 source(audio='#b,video='#c,midi='#d))->
((()->float)*source(audio='#b,video='#c,midi='#d))
where 'a is either float or ()->float```

Get current RMS volume of the source. Returns a pair `(f,s)` where s is a new source and `f` is a function of type `() -> float` and returns the current RMS volume of the source.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `duration` (```
anything that is either float or ()->float```
 -- defaults to `0.5`): Duration of the window (in seconds). A value <= 0, means that computation should not be performed.
* `(unlabeled)` (`source(audio='#b,video='#c,midi='#d)`)

#### rms.stereo
```
(?id:string,?duration:'a,
 source(audio='#b+2,video='#c,midi='#d))->
((()->(float*float))*
 source(audio='#b+2,video='#c,midi='#d))
where 'a is either float or ()->float```

Get current RMS volume of the source. Returns a pair `(f,s)` where s is a new source and `f` is a function of type `() -> float` and returns the current RMS volume of the source.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `duration` (```
anything that is either float or ()->float```
 -- defaults to `0.5`): Duration of the window (in seconds). A value <= 0, means that computation should not be performed.
* `(unlabeled)` (`source(audio='#b+2,video='#c,midi='#d)`)

#### server.rms
```
(?id:string,source(audio='#a,video='#b,midi='#c))->
source(audio='#a,video='#b,midi='#c)```

Register a command that outputs the RMS of the returned source.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio='#a,video='#b,midi='#c)`)

#### video.volume
```
(?id:string,source(audio='#a+1,video=0,midi=0))->
source(audio='#a+1,video=1,midi=0)```

Graphical visualization of the sound.

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `(unlabeled)` (`source(audio='#a+1,video=0,midi=0)`)

#### vumeter
```
(?id:string,?scroll:bool,
 source(audio='#a+1,video='#b,midi='#c))->
source(audio='#a+1,video='#b,midi='#c)```

VU meter (display the audio volume).

* `id` (`string` -- defaults to `""`): Force the value of the source ID.
* `scroll` (`bool` -- defaults to `false`): Scroll.
* `(unlabeled)` (`source(audio='#a+1,video='#b,midi='#c)`)

Bool
----
#### !=
```
('a,'a)->bool where 'a is an orderable type```

Comparison of comparable values.

* `(unlabeled)` (`anything that is an orderable type`)
* `(unlabeled)` (`anything that is an orderable type`)

#### <
```
('a,'a)->bool where 'a is an orderable type```

Comparison of comparable values.

* `(unlabeled)` (`anything that is an orderable type`)
* `(unlabeled)` (`anything that is an orderable type`)

#### <=
```
('a,'a)->bool where 'a is an orderable type```

Comparison of comparable values.

* `(unlabeled)` (`anything that is an orderable type`)
* `(unlabeled)` (`anything that is an orderable type`)

#### ==
```
('a,'a)->bool where 'a is an orderable type```

Comparison of comparable values.

* `(unlabeled)` (`anything that is an orderable type`)
* `(unlabeled)` (`anything that is an orderable type`)

#### >
```
('a,'a)->bool where 'a is an orderable type```

Comparison of comparable values.

* `(unlabeled)` (`anything that is an orderable type`)
* `(unlabeled)` (`anything that is an orderable type`)

#### >=
```
('a,'a)->bool where 'a is an orderable type```

Comparison of comparable values.

* `(unlabeled)` (`anything that is an orderable type`)
* `(unlabeled)` (`anything that is an orderable type`)

#### and
```
(bool,bool)->bool```

Return the conjunction of its arguments

* `(unlabeled)` (`bool`)
* `(unlabeled)` (`bool`)

#### not
```
(bool)->bool```

Returns the negation of its argument.

* `(unlabeled)` (`bool`)

#### or
```
(bool,bool)->bool```

Return the disjunction of its arguments

* `(unlabeled)` (`bool`)
* `(unlabeled)` (`bool`)

#### random.bool
```
()->bool```

Generate a random value.

Control
-------
#### add_timeout
```
(?fast:bool,float,(()->float))->unit```

Call a function in N seconds. If the result of the function is positive or null, the task will be scheduled again after this amount of time (in seconds).

* `fast` (`bool` -- defaults to `true`): Set to `false` if the execution of the code can take long in order to lower its priority below that of request resolutions and fast timeouts. This is only effective if you set a dedicated queue for fast tasks, see the ``scheduler'' settings for more details.
* `(unlabeled)` (`float`)
* `(unlabeled)` (`()->float`)

#### ignore
```
('a)->unit```

Convert anything to unit, preventing warnings.

* `(unlabeled)` (`'a`)

Interaction
-----------
#### add_skip_command
```
(source('a))->unit```

Add a skip function to a source when it does not have one by default.

* `(unlabeled)` (`source('a)`): The source to attach the command to.

#### audioscrobbler.nowplaying
```
(user:string,password:string,?host:string,?port:int,
 ?length:bool,[(string*string)])->unit```

Submit a now playing song using the audioscrobbler protocol.

* `user` (`string`)
* `password` (`string`)
* `host` (`string` -- defaults to `"post.audioscrobbler.com"`): Host for audioscrobbling submissions.
* `port` (`int` -- defaults to `80`): Port for audioscrobbling submissions.
* `length` (`bool` -- defaults to `false`): Try to submit length information. This operation can be CPU intensive. Value forced to true when used with the ``user'' source type.
* `(unlabeled)` (`[(string*string)]`)

#### audioscrobbler.submit
```
(?source:string,user:string,password:string,?host:string,
 ?port:int,?length:bool,[(string*string)])->unit```

Submit a played song using the audioscrobbler protocol.

* `source` (`string` -- defaults to `"broadcast"`): Source for tracks. Should be one of: ``broadcast'', ``user'', ``recommendation'' or ``unknown''. Since liquidsoap is intented for radio broadcasting, this is the default. Sources other than user don't need duration to be set.
* `user` (`string`)
* `password` (`string`)
* `host` (`string` -- defaults to `"post.audioscrobbler.com"`): Host for audioscrobbling submissions.
* `port` (`int` -- defaults to `80`): Port for audioscrobbling submissions.
* `length` (`bool` -- defaults to `false`): Try to submit length information. This operation can be CPU intensive. Value forced to true when used with the ``user'' source type.
* `(unlabeled)` (`[(string*string)]`)

#### http.delete
```
(?headers:[(string*string)],?timeout:float,string)->
((((string*int)*string)*[(string*string)])*string)```

Perform a full Http DELETE request and return (status,headers),data.

* `headers` (`[(string*string)]` -- defaults to `[]`): Additional headers.
* `timeout` (`float` -- defaults to `10.0`): Timeout for network operations.
* `(unlabeled)` (`string`): Requested URL, e.g. ``http://www.google.com:80/index.html''.

#### http.get
```
(?headers:[(string*string)],?timeout:float,string)->
((((string*int)*string)*[(string*string)])*string)```

Perform a full Http GET request and return (status,headers),data.

* `headers` (`[(string*string)]` -- defaults to `[]`): Additional headers.
* `timeout` (`float` -- defaults to `10.0`): Timeout for network operations.
* `(unlabeled)` (`string`): Requested URL, e.g. ``http://www.google.com:80/index.html''.

#### http.head
```
(?headers:[(string*string)],?timeout:float,string)->
((((string*int)*string)*[(string*string)])*string)```

Perform a full Http HEAD request and return (status,headers),data.

* `headers` (`[(string*string)]` -- defaults to `[]`): Additional headers.
* `timeout` (`float` -- defaults to `10.0`): Timeout for network operations.
* `(unlabeled)` (`string`): Requested URL, e.g. ``http://www.google.com:80/index.html''.

#### http.post
```
(?data:string,?headers:[(string*string)],?timeout:float,
 string)->
((((string*int)*string)*[(string*string)])*string)```

Perform a full Http POST request and return (status,headers),data.

* `data` (`string` -- defaults to `""`): POST data.
* `headers` (`[(string*string)]` -- defaults to `[]`): Additional headers.
* `timeout` (`float` -- defaults to `10.0`): Timeout for network operations.
* `(unlabeled)` (`string`): Requested URL, e.g. ``http://www.google.com:80/index.html''.

#### http.put
```
(?data:string,?headers:[(string*string)],?timeout:float,
 string)->
((((string*int)*string)*[(string*string)])*string)```

Perform a full Http PUT request and return (status,headers),data.

* `data` (`string` -- defaults to `""`): POST data.
* `headers` (`[(string*string)]` -- defaults to `[]`): Additional headers.
* `timeout` (`float` -- defaults to `10.0`): Timeout for network operations.
* `(unlabeled)` (`string`): Requested URL, e.g. ``http://www.google.com:80/index.html''.

#### http_codes
```
[(string*string)]```

List of HTTP response codes and statuses.

#### http_response
```
(?protocol:string,?code:int,?headers:[(string*string)],
 ?data:string)->string```

Create a HTTP response string

* `protocol` (`string` -- defaults to `"HTTP/1.1"`): HTTP protocol used.
* `code` (`int` -- defaults to `200`): Response code.
* `headers` (`[(string*string)]` -- defaults to `[]`): Response headers.
* `data` (`string` -- defaults to `""`): Response data

#### https.delete
```
(?headers:[(string*string)],?timeout:float,string)->
((((string*int)*string)*[(string*string)])*string)```

Perform a full https DELETE request and return (status,headers),data.

* `headers` (`[(string*string)]` -- defaults to `[]`): Additional headers.
* `timeout` (`float` -- defaults to `10.0`): Timeout for network operations.
* `(unlabeled)` (`string`): Requested URL, e.g. ``http://www.google.com:80/index.html''.

#### https.get
```
(?headers:[(string*string)],?timeout:float,string)->
((((string*int)*string)*[(string*string)])*string)```

Perform a full https GET request and return (status,headers),data.

* `headers` (`[(string*string)]` -- defaults to `[]`): Additional headers.
* `timeout` (`float` -- defaults to `10.0`): Timeout for network operations.
* `(unlabeled)` (`string`): Requested URL, e.g. ``http://www.google.com:80/index.html''.

#### https.head
```
(?headers:[(string*string)],?timeout:float,string)->
((((string*int)*string)*[(string*string)])*string)```

Perform a full https HEAD request and return (status,headers),data.

* `headers` (`[(string*string)]` -- defaults to `[]`): Additional headers.
* `timeout` (`float` -- defaults to `10.0`): Timeout for network operations.
* `(unlabeled)` (`string`): Requested URL, e.g. ``http://www.google.com:80/index.html''.

#### https.post
```
(?data:string,?headers:[(string*string)],?timeout:float,
 string)->
((((string*int)*string)*[(string*string)])*string)```

Perform a full https POST request and return (status,headers),data.

* `data` (`string` -- defaults to `""`): POST data.
* `headers` (`[(string*string)]` -- defaults to `[]`): Additional headers.
* `timeout` (`float` -- defaults to `10.0`): Timeout for network operations.
* `(unlabeled)` (`string`): Requested URL, e.g. ``http://www.google.com:80/index.html''.

#### https.put
```
(?data:string,?headers:[(string*string)],?timeout:float,
 string)->
((((string*int)*string)*[(string*string)])*string)```

Perform a full https PUT request and return (status,headers),data.

* `data` (`string` -- defaults to `""`): POST data.
* `headers` (`[(string*string)]` -- defaults to `[]`): Additional headers.
* `timeout` (`float` -- defaults to `10.0`): Timeout for network operations.
* `(unlabeled)` (`string`): Requested URL, e.g. ``http://www.google.com:80/index.html''.

#### icy.update_metadata
```
(?host:string,?port:int,?user:string,?password:string,
 ?mount:string,?icy_id:int,?protocol:string,
 ?encoding:string,?headers:[(string*string)],
 [(string*string)])->unit```

Update metata on an icecast mountpoint using the ICY protocol.

* `host` (`string` -- defaults to `"localhost"`)
* `port` (`int` -- defaults to `8000`)
* `user` (`string` -- defaults to `"source"`)
* `password` (`string` -- defaults to `"hackme"`)
* `mount` (`string` -- defaults to `""`): Source mount point. Mandatory when streaming to icecast.
* `icy_id` (`int` -- defaults to `1`): Shoutcast source ID. Only supported by Shoutcast v2.
* `protocol` (`string` -- defaults to `"http"`): Protocol to use. One of: ``icy'', ``http'' or ``https''
* `encoding` (`string` -- defaults to `""`): Encoding used to send metadata, default (UTF-8) if empty.
* `headers` (`[(string*string)]` -- defaults to ```
[("User-Agent","Liquidsoap/1.4.0+scm (Unix; OCaml 4.07.1)")]```
): Additional headers.
* `(unlabeled)` (`[(string*string)]`)

#### interactive.bool
```
(string,bool)->()->bool```

Read a boolean from an interactive input.

* `(unlabeled)` (`string`)
* `(unlabeled)` (`bool`)

#### interactive.float
```
(string,float)->()->float```

Read a float from an interactive input.

* `(unlabeled)` (`string`)
* `(unlabeled)` (`float`)

#### interactive.string
```
(string,string)->()->string```

Read a string from an interactive input.

* `(unlabeled)` (`string`)
* `(unlabeled)` (`string`)

#### osc.bool
```
(string,bool)->()->bool```

Read from an OSC path.

* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`bool`): Initial value.

#### osc.float
```
(string,float)->()->float```

Read from an OSC path.

* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`float`): Initial value.

#### osc.float_pair
```
(string,(float*float))->()->(float*float)```

Read from an OSC path.

* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`(float*float)`): Initial value.

#### osc.int
```
(string,int)->()->int```

Read from an OSC path.

* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`int`): Initial value.

#### osc.int_pair
```
(string,(int*int))->()->(int*int)```

Read from an OSC path.

* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`(int*int)`): Initial value.

#### osc.on_bool
```
(string,((bool)->unit))->unit```

Register a callback on OSC messages.

* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`(bool)->unit`): Callback function.

#### osc.on_float
```
(string,((float)->unit))->unit```

Register a callback on OSC messages.

* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`(float)->unit`): Callback function.

#### osc.on_float_pair
```
(string,(((float*float))->unit))->unit```

Register a callback on OSC messages.

* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`((float*float))->unit`): Callback function.

#### osc.on_int
```
(string,((int)->unit))->unit```

Register a callback on OSC messages.

* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`(int)->unit`): Callback function.

#### osc.on_int_pair
```
(string,(((int*int))->unit))->unit```

Register a callback on OSC messages.

* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`((int*int))->unit`): Callback function.

#### osc.on_string
```
(string,((string)->unit))->unit```

Register a callback on OSC messages.

* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`(string)->unit`): Callback function.

#### osc.on_string_pair
```
(string,(((string*string))->unit))->unit```

Register a callback on OSC messages.

* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`((string*string))->unit`): Callback function.

#### osc.send_bool
```
(host:string,port:int,string,bool)->unit```

Send a value to an OSC client.

* `host` (`string`): OSC client address.
* `port` (`int`): OSC client port.
* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`bool`): Value to send.

#### osc.send_float
```
(host:string,port:int,string,float)->unit```

Send a value to an OSC client.

* `host` (`string`): OSC client address.
* `port` (`int`): OSC client port.
* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`float`): Value to send.

#### osc.send_float_pair
```
(host:string,port:int,string,(float*float))->unit```

Send a value to an OSC client.

* `host` (`string`): OSC client address.
* `port` (`int`): OSC client port.
* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`(float*float)`): Value to send.

#### osc.send_int
```
(host:string,port:int,string,int)->unit```

Send a value to an OSC client.

* `host` (`string`): OSC client address.
* `port` (`int`): OSC client port.
* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`int`): Value to send.

#### osc.send_int_pair
```
(host:string,port:int,string,(int*int))->unit```

Send a value to an OSC client.

* `host` (`string`): OSC client address.
* `port` (`int`): OSC client port.
* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`(int*int)`): Value to send.

#### osc.send_string
```
(host:string,port:int,string,string)->unit```

Send a value to an OSC client.

* `host` (`string`): OSC client address.
* `port` (`int`): OSC client port.
* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`string`): Value to send.

#### osc.send_string_pair
```
(host:string,port:int,string,(string*string))->unit```

Send a value to an OSC client.

* `host` (`string`): OSC client address.
* `port` (`int`): OSC client port.
* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`(string*string)`): Value to send.

#### osc.string
```
(string,string)->()->string```

Read from an OSC path.

* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`string`): Initial value.

#### osc.string_pair
```
(string,(string*string))->()->(string*string)```

Read from an OSC path.

* `(unlabeled)` (`string`): OSC path.
* `(unlabeled)` (`(string*string)`): Initial value.

#### print
```
(?newline:bool,'a)->unit```

Print on standard output.

* `newline` (`bool` -- defaults to `true`): If true, a newline is added after displaying the value.
* `(unlabeled)` (`'a`)

#### server.broadcast
```
(('a*('b*(()->'c))))->'c```

Restart all server clients waiting on the given condition

* `(unlabeled)` (`('a*('b*(()->'c)))`): condition

#### server.condition
```
()->((((()->string))->string)*((()->unit)*(()->unit)))```

Create a pair of functions `(wait,(signal,broadcast))` used to suspend and resume server command execution. Used to write interactive server commands through `server.wait`, `server.signal`, `server.broadcast` and `server.write`.

#### server.read
```
(((string)->string),string)->string```

Read a string from the client up-to a marker. Marker can be any string of regular expression. Should be used via the syntactic sugar: ```
server.read <marker> : <varname> then <after> end```


* `(unlabeled)` (`(string)->string`): function to run after write
* `(unlabeled)` (`string`): Read marker

#### server.readchars
```
(((string)->string),int)->string```

Read a string of fixed length from the client up-to a marker. Should be used via the syntactic sugar: ```
server.readchars <len> : <varname> then <after> end```


* `(unlabeled)` (`(string)->string`): function to run after write
* `(unlabeled)` (`int`): Number of characters to read

#### server.readline
```
(((string)->string))->string```

Read a line from the client. Should be used via the syntactic sugar: ```
server.readline <varname> then <after> end```


* `(unlabeled)` (`(string)->string`): function to run after write

#### server.register
```
(?namespace:string,?description:string,?usage:string,
 string,((string)->string))->unit```

Register a command. You can then execute this function through the server, either telnet or socket.

* `namespace` (`string` -- defaults to `""`)
* `description` (`string` -- defaults to `"No documentation available."`): A description of your command.
* `usage` (`string` -- defaults to `""`)
* `(unlabeled)` (`string`)
* `(unlabeled)` (`(string)->string`)

#### server.signal
```
(('a*((()->'b)*'c)))->'b```

Restart one server client waiting on the given condition

* `(unlabeled)` (`('a*((()->'b)*'c))`): condition

#### server.wait
```
(((((()->string))->string)*'a),(()->string))->string```

Wait on a server condition. Used to write interactive server command. Should be used via the syntactic sugar: ```
server.wait <condition> then <after> end```


* `(unlabeled)` (`((((()->string))->string)*'a)`): condition
* `(unlabeled)` (`()->string`): code to execute when resuming

#### server.write
```
((()->string),string)->string```

Execute a partial write while executing a server command. Should be used via the syntactic sugar: `server.write <string> then <after> end`

* `(unlabeled)` (`()->string`): function to run after write
* `(unlabeled)` (`string`): string to write

Liquidsoap
----------
#### add_decoder
```
(name:string,description:string,?mimes:[string],
 test:((string)->int),string)->unit```

Register an external decoder. The encoder should output in WAV format to his standard output (stdout) and read data from its standard input (stdin).

* `name` (`string`): Format/decoder's name.
* `description` (`string`): Description of the decoder.
* `mimes` (`[string]` -- defaults to `[]`): List of mime types supported by this decoder for decoding streams.
* `test` (`(string)->int`): Function used to determine if a file should be decoded by the decoder. Returned values are: 0: no decodable audio, -1: decodable audio but number of audio channels unknown, x: fixed number of decodable audio channels.
* `(unlabeled)` (`string`): Process to start.

#### add_metadata_resolver
```
(string,((string)->[(string*string)]))->unit```

Register an external file metadata decoder.

* `(unlabeled)` (`string`): Format/resolver's name.
* `(unlabeled)` (`(string)->[(string*string)]`): Process to start. The function takes the format and filename as argument and returns a list of (name,value) fields.

#### add_oblivious_decoder
```
(name:string,description:string,test:((string)->int),
 ?buffer:float,((string)->string))->unit```

Register an external file decoder. The encoder should output in WAV format to his standard output (stdout) and read data from the file it receives. The estimated remaining duration for this decoder will be unknown until the `buffer` last seconds of the file. If possible, it is recommended to decode from stdin and use `add_decoder`.

* `name` (`string`): Format/decoder's name.
* `description` (`string`): Description of the decoder.
* `test` (`(string)->int`): Function used to determine if a file should be decoded by the decoder. Returned values are: 0: no decodable audio, -1: decodable audio but number of audio channels unknown, x: fixed number of decodable audio channels.
* `buffer` (`float` -- defaults to `5.0`)
* `(unlabeled)` (`(string)->string`): Process to start. The function takes the filename as argument and returns the process to start.

#### add_playlist_parser
```
(format:string,strict:bool,
 ((?pwd:string,string)->[([(string*string)]*string)]))->
unit```

Register a new playlist parser. An empty playlist is considered as a failure to resolve.

* `format` (`string`): Playlist format. If possible, a mime-type.
* `strict` (`bool`): True if playlist format can be detected unambiguously.
* `(unlabeled)` (```
(?pwd:string,string)->[([(string*string)]*string)]```
): Playlist parser

#### add_protocol
```
(?temporary:bool,?static:bool,?syntax:string,?doc:string,
 string,
 ((rlog:((string)->unit),maxtime:float,string)->[string]))->
unit```

Register a new protocol.

* `temporary` (`bool` -- defaults to `false`): if true, file is removed when it is finished.
* `static` (`bool` -- defaults to `false`): if true, then requests can be resolved once and for all. Typically, static protocols can be used to create infallible sources.
* `syntax` (`string` -- defaults to `"Undocumented"`): URI syntax.
* `doc` (`string` -- defaults to `"Undocumented"`): Protocol documentation.
* `(unlabeled)` (`string`): Protocol name. Resolver will be called on uris of the form: `<protocol name>:...`.
* `(unlabeled)` (```
(rlog:((string)->unit),maxtime:float,string)->[string]```
): Protocol resolver. Receives a function to log protocol resolution, the `<arg>` in `<protocol name>:<arg>` and the max delay that resolution should take.

#### bool_getter
```
('a)->'b where 'a, 'b is either bool or ()->bool```

Create a bool getter

* `(unlabeled)` (```
anything that is either bool or ()->bool```
)

#### clock
```
(?sync:bool,?id:string,source('a))->source('a)```

Assign a new clock to the given source (and to other time-dependent sources) and return the source. It is a conveniency wrapper around clock.assign_new(), allowing more concise scripts in some cases.

* `sync` (`bool` -- defaults to `true`): Do not synchronize the clock on regular wallclock time, but try to run as fast as possible (CPU burning mode).
* `id` (`string` -- defaults to `""`)
* `(unlabeled)` (`source('a)`)

#### clock.assign_new
```
(?id:string,?sync:bool,[source('a)])->unit```

Create a new clock and assign it to a list of sources.

* `id` (`string` -- defaults to `""`): Identifier for the new clock. The default empty string means that the identifier of the first source will be used.
* `sync` (`bool` -- defaults to `true`): Do not synchronize the clock on regular wallclock time, but try to run as fast as possible (CPU burning mode).
* `(unlabeled)` (`[source('a)]`): List of sources to which the new clock will be assigned

#### clock.unify
```
([source('a)])->unit```

Enforce that a list of sources all belong to the same clock.

* `(unlabeled)` (`[source('a)]`)

#### configure.default_font
```
string```

Liquidsoap's default font file.

#### configure.libdir
```
string```

Liquidsoap's library directory.

#### configure.logdir
```
string```

Liquidsoap's logging directory.

#### configure.rundir
```
string```

Liquidsoap's PID file directory.

#### enable_external_faad_decoder
```
()->unit```

Enable or disable external FAAD (AAC/AAC+/M4A) decoders. Does not work on Win32.
Please note that built-in support for faad is available in liquidsoap if compiled and should be preferred over the external decoder.

#### enable_external_ffmpeg_decoder
```
()->unit```

Enable ffmpeg decoder.

#### enable_external_flac_decoder
```
()->unit```

Enable external FLAC decoders. Please note that built-in support for FLAC is available in liquidsoap if compiled and should be preferred over the external decoder.

#### enable_external_mpc_decoder
```
()->unit```

Enable external Musepack decoder.

#### enable_replaygain_metadata
```
(?delay:float,?extract_replaygain:string)->unit```

Enable replay gain metadata resolver. This resolver will process any file decoded by liquidsoap and add a replay_gain metadata when this value could be computed. For a finer-grained replay gain processing, use the replay_gain protocol.

* `delay` (`float` -- defaults to `-1.0`): Maximum delay for extracting metadata
* `extract_replaygain` (`string` -- defaults to ```
"/usr/local/lib/liquidsoap/scm/extract-replaygain"```
): The extraction program

#### file.mime
```
(string)->string```

Get the MIME type of a file, according to libmagic.

* `(unlabeled)` (`string`)

#### float_getter
```
('a)->'b where 'a, 'b is either float or ()->float```

Create a float getter

* `(unlabeled)` (```
anything that is either float or ()->float```
)

#### garbage_collect
```
()->unit```

Trigger full major garbage collection.

#### get
```
(default:'a,string)->'a
where 'a is unit, bool, int, float, string or [string]```

Get a setting's value.

* `default` (```
anything that is unit, bool, int, float, string or [string]```
)
* `(unlabeled)` (`string`)

#### get_clock_status
```
()->[(string*int)]```

Get the current time for all allocated clocks.

#### harbor.http.register
```
(port:int,method:string,string,
 ((protocol:string,data:string,headers:[(string*string)],
   string)->string))->unit```

Register a HTTP handler on the harbor. The given function receives as argument the full requested uri (e.g. ``foo?var=bar''), http protocol version, possible input data and the list of HTTP headers and returns the answer sent to the client, including HTTP headers. Registered uri can be regular expressions (e.g. ``.+\.php'') and can override default metadata handlers.

* `port` (`int`): Port to server.
* `method` (`string`): Accepted method
* `(unlabeled)` (`string`): URI to serve.
* `(unlabeled)` (```
(protocol:string,data:string,headers:[(string*string)],
 string)->string```
): Function to execute. method argument is ``PUT'' or ``GET'', protocol argument is ``HTTP/1.1'' or ``HTTP/1.0'' etc., data argument contains data passed in case of a PUT request, and ``'' otherwise. headers argument contains the HTTP headers. Unlabeled argument contains the requested URI.

#### harbor.http.remove
```
(method:string,port:int,string)->unit```

Remove a registered HTTP handler on the harbor.

* `method` (`string`): Method served.
* `port` (`int`): Port to server.
* `(unlabeled)` (`string`): URI served.

#### harbor.https.register
```
(port:int,method:string,string,
 ((protocol:string,data:string,headers:[(string*string)],
   string)->string))->unit```

Register a HTTPS handler on the harbor. The given function receives as argument the full requested uri (e.g. ``foo?var=bar''), http protocol version, possible input data and the list of HTTP headers and returns the answer sent to the client, including HTTP headers. Registered uri can be regular expressions (e.g. ``.+\.php'') and can override default metadata handlers.

* `port` (`int`): Port to server.
* `method` (`string`): Accepted method
* `(unlabeled)` (`string`): URI to serve.
* `(unlabeled)` (```
(protocol:string,data:string,headers:[(string*string)],
 string)->string```
): Function to execute. method argument is ``PUT'' or ``GET'', protocol argument is ``HTTP/1.1'' or ``HTTP/1.0'' etc., data argument contains data passed in case of a PUT request, and ``'' otherwise. headers argument contains the HTTP headers. Unlabeled argument contains the requested URI.

#### harbor.https.remove
```
(method:string,port:int,string)->unit```

Remove a registered HTTPS handler on the harbor.

* `method` (`string`): Method served.
* `port` (`int`): Port to server.
* `(unlabeled)` (`string`): URI served.

#### int_getter
```
('a)->'b where 'a, 'b is either int or ()->int```

Create a int getter

* `(unlabeled)` (`anything that is either int or ()->int`)

#### liquidsoap.version
```
string```

Liquidsoap version string.

#### log
```
(?label:string,?level:int,string)->unit```

Log a message.

* `label` (`string` -- defaults to `"lang"`)
* `level` (`int` -- defaults to `3`)
* `(unlabeled)` (`string`)

#### log_clocks
```
(?delay:float,?interval:float,string)->unit```

Create a log of clock times for all the clocks initially present. The log is in a simple format which you can directly use with gnuplot.

* `delay` (`float` -- defaults to `0.0`): Delay before setting up the clock logger. This should be used to ensure that the logger starts only after the clocks are created.
* `interval` (`float` -- defaults to `1.0`): Polling interval.
* `(unlabeled)` (`string`): Path of the log file.

#### metadata.export
```
([(string*string)])->[(string*string)]```

Filter-out internal metadata.

* `(unlabeled)` (`[(string*string)]`)

#### mutexify
```
('a)->'a```

Protect functions with a mutex to avoid concurrent calls, return original value otherwise.

* `(unlabeled)` (`'a`)

#### playlist.parse
```
(?path:string,?mime:string,string)->
[([(string*string)]*string)]```

Try to parse a local playlist. Return a list of (metadata,URI) items, where metadata is a list of (key,value) bindings.

* `path` (`string` -- defaults to `""`): Default path for files.
* `mime` (`string` -- defaults to `""`): Mime type for the playlist
* `(unlabeled)` (`string`)

#### process_uri
```
(extname:'a,?uri:string,string)->string```

Create a process: uri, replacing `:` with `$(colon)`

* `extname` (`'a`): Output file extension (with no leading '.')
* `uri` (`string` -- defaults to `""`): Input uri
* `(unlabeled)` (`string`): Command line to execute

#### register
```
(name:string,?descr:string,?on_change:(('a)->unit),
 string,'a)->unit
where 'a is unit, bool, int, float, string or [string]```

Register a new setting.

* `name` (`string`): Settings name
* `descr` (`string` -- defaults to `""`): Settings description
* `on_change` (```
('a)->unit
where 'a is unit, bool, int, float, string or [string]```
 -- defaults to `fun (_) -> ()`): Callback executed when the setting is changed.
* `(unlabeled)` (`string`): Setting key
* `(unlabeled)` (```
anything that is unit, bool, int, float, string or [string]```
): Setting initial value

#### register_flow
```
(?server:string,?user:string,?password:string,
 ?email:string,radio:string,website:string,
 description:string,genre:string,
 streams:[(string*string)],source('a))->source('a)```

Register a radio on Liquidsoap Flows.

* `server` (`string` -- defaults to `""`)
* `user` (`string` -- defaults to `"default"`)
* `password` (`string` -- defaults to `"default"`)
* `email` (`string` -- defaults to `""`)
* `radio` (`string`): Name of the radio.
* `website` (`string`): URL of the website of the radio.
* `description` (`string`): Description of the radio.
* `genre` (`string`): Genre of the radio (rock or rap or etc.).
* `streams` (`[(string*string)]`): List of streams for the radio described by a pair of strings consisting of the format of the stream and the url of the stream. The format should be of the form ``ogg/128k'' consisting of the codec and the bitrate, separated by ``/''.
* `(unlabeled)` (`source('a)`)

#### request.create
```
(?indicators:[string],?persistent:bool,string)->
request('a)```

Create a request. Creation may fail if there is no available RID, which cannot be detected currently: in that case one will obtain a request that will fail to be resolved.

* `indicators` (`[string]` -- defaults to `[]`)
* `persistent` (`bool` -- defaults to `false`)
* `(unlabeled)` (`string`)

#### request.create.raw
```
(?indicators:[string],?persistent:bool,string)->
request(audio=0,video=0,midi=0)```

Create a raw request, i.e. for files that should not be decoded for streaming. Creation may fail if there is no available RID, which cannot be detected currently: in that case one will obtain a request that will fail to be resolved.

* `indicators` (`[string]` -- defaults to `[]`)
* `persistent` (`bool` -- defaults to `false`)
* `(unlabeled)` (`string`)

#### request.destroy
```
(?force:bool,request('a))->unit```

Destroying a request causes any temporary associated file to be deleted, and releases its RID. Persistent requests resist to destroying, unless forced.

* `force` (`bool` -- defaults to `false`): Destroy the request even if it is persistent.
* `(unlabeled)` (`request('a)`)

#### request.duration
```
(string)->float```

Compute the duration in seconds of audio data contained in a request. The computation may be expensive. Returns -1. if computation failed, typically if the file was not recognized as valid audio.

* `(unlabeled)` (`string`)

#### request.filename
```
(request('a))->string```

Return a valid local filename if the request is ready, and the empty string otherwise.

* `(unlabeled)` (`request('a)`)

#### request.log
```
(request('a))->string```

Get log data associated to a request.

* `(unlabeled)` (`request('a)`)

#### request.metadata
```
(request('a))->[(string*string)]```

Get the metadata associated to a request.

* `(unlabeled)` (`request('a)`)

#### request.ready
```
(request('a))->bool```

Check if a request is ready, i.e. is associated to a valid local file. Unless the initial URI was such a file, a request has to be resolved before being ready.

* `(unlabeled)` (`request('a)`)

#### request.resolve
```
(?timeout:float,request('a))->bool```

Resolve a request, i.e. attempt to get a valid local file. The operation can take some time. Return true if the resolving was successful, false otherwise (timeout or invalid URI).

* `timeout` (`float` -- defaults to `30.0`): Limit in seconds to the duration of the resolving.
* `(unlabeled)` (`request('a)`)

#### server.execute
```
(string,?string)->[string]```

Execute a liquidsoap server command.

* `(unlabeled)` (`string`)
* `(unlabeled)` (`string` -- defaults to `""`)

#### set
```
(string,'a)->unit
where 'a is unit, bool, int, float, string or [string]```

Change some setting. Use `liquidsoap --conf-descr` and `liquidsoap --conf-descr-key KEY` on the command-line to get some information about available settings.

* `(unlabeled)` (`string`)
* `(unlabeled)` (```
anything that is unit, bool, int, float, string or [string]```
)

#### source.fallible
```
(source('a))->bool```

Indicate if a source may fail, i.e. may not be ready to stream.

* `(unlabeled)` (`source('a)`)

#### source.id
```
(source('a))->string```

Get one source's identifier.

* `(unlabeled)` (`source('a)`)

#### source.init
```
([source(audio=*,video=*,midi=*)])->
[source(audio=*,video=*,midi=*)]```

Simultaneously initialize sources, return the sublist of sources that failed to initialized.

* `(unlabeled)` (`[source(audio=*,video=*,midi=*)]`)

#### source.is_ready
```
(source('a))->bool```

Indicate if a source is ready to stream, or currently streaming.

* `(unlabeled)` (`source('a)`)

#### source.remaining
```
(source('a))->float```

Estimation of remaining time in the current track.

* `(unlabeled)` (`source('a)`)

#### source.seek
```
(source('a),float)->float```

Seek forward, in seconds. Returns the amount of time effectively seeked.

* `(unlabeled)` (`source('a)`)
* `(unlabeled)` (`float`)

#### source.shutdown
```
(source('a))->unit```

Desactivate a source.

* `(unlabeled)` (`source('a)`)

#### source.skip
```
(source('a))->unit```

Skip to the next track.

* `(unlabeled)` (`source('a)`)

#### source.time
```
(source('a))->float```

Get a source's time, based on its assigned clock

* `(unlabeled)` (`source('a)`)

#### string_getter
```
('a)->'b where 'a, 'b is either string or ()->string```

Create a string getter

* `(unlabeled)` (```
anything that is either string or ()->string```
)

#### to_bool_getter
```
('a)->()->bool where 'a is either bool or ()->bool```

Return a function from a bool getter

* `(unlabeled)` (```
anything that is either bool or ()->bool```
)

#### to_float_getter
```
('a)->()->float where 'a is either float or ()->float```

Return a function from a float getter

* `(unlabeled)` (```
anything that is either float or ()->float```
)

#### to_int_getter
```
('a)->()->int where 'a is either int or ()->int```

Return a function from a int getter

* `(unlabeled)` (`anything that is either int or ()->int`)

#### to_string_getter
```
('a)->()->string where 'a is either string or ()->string```

Return a function from a string getter

* `(unlabeled)` (```
anything that is either string or ()->string```
)

List
----
#### _[_]
```
(string,[(string*string)])->string```

l[k] returns the first v such that (k,v) is in the list l (or ``'' if no such v exists).

* `(unlabeled)` (`string`)
* `(unlabeled)` (`[(string*string)]`)

#### list.add
```
('a,['a])->['a]```

Add an element at the top of a list.

* `(unlabeled)` (`'a`)
* `(unlabeled)` (`['a]`)

#### list.append
```
(['a],['a])->['a]```

Catenate two lists.

* `(unlabeled)` (`['a]`)
* `(unlabeled)` (`['a]`)

#### list.assoc
```
(default:'a,'b,[('b*'a)])->'a```

Generalized l[k] with default value.

* `default` (`'a`): Default value if key does not exist
* `(unlabeled)` (`'b`)
* `(unlabeled)` (`[('b*'a)]`)

#### list.filter
```
((('a)->bool),['a])->['a]```

Filter a list according to a filtering function.

* `(unlabeled)` (`('a)->bool`)
* `(unlabeled)` (`['a]`)

#### list.filter_assoc
```
('a,[('a*'b)])->[('a*'b)] where 'a is an orderable type```

list.filter_assoc(key,l) returns all the elements of the form (key, value) from l.

* `(unlabeled)` (`anything that is an orderable type`): Key to look for
* `(unlabeled)` (`[('a*'b)] where 'a is an orderable type`): List of pairs (key,value)

#### list.fold
```
((('a,'b)->'a),'a,['b])->'a```

Fold a function on every element of a list: list.fold(f,x1,[e1,..,en]) is f(...f(f(x1,e1),e2)...,en).

* `(unlabeled)` (`('a,'b)->'a`): Function f for which f(x,e) which will be called on every element e with the current value of x, returning the new value of x.
* `(unlabeled)` (`'a`): Initial value x1, to be updated by successive calls of f(x,e).
* `(unlabeled)` (`['b]`)

#### list.hd
```
(default:'a,['a])->'a```

Return the head (first element) of a list, or 'default' if the list is empty.

* `default` (`'a`): Default value if key does not exist
* `(unlabeled)` (`['a]`)

#### list.iter
```
((('a)->unit),['a])->unit```

Call a function on every element of a list.

* `(unlabeled)` (`('a)->unit`)
* `(unlabeled)` (`['a]`)

#### list.length
```
(['a])->int```

Get the length of a list, i.e. its number of elements.

* `(unlabeled)` (`['a]`)

#### list.map
```
((('a)->'b),['a])->['b]```

Map a function on every element of a list.

* `(unlabeled)` (`('a)->'b`)
* `(unlabeled)` (`['a]`)

#### list.mapi
```
(((int,'a)->'b),['a])->['b]```

Map a function on every element of a list, along with its index.

* `(unlabeled)` (`(int,'a)->'b`)
* `(unlabeled)` (`['a]`)

#### list.mem
```
('a,['a])->bool where 'a is an orderable type```

Check if an element belongs to a list.

* `(unlabeled)` (`anything that is an orderable type`)
* `(unlabeled)` (`['a] where 'a is an orderable type`)

#### list.mem_assoc
```
('a,[('a*'b)])->bool where 'a is an orderable type```

list.mem_assoc(key,l) returns true if l contains a pair (key,value)

* `(unlabeled)` (`anything that is an orderable type`): Key to look for
* `(unlabeled)` (`[('a*'b)] where 'a is an orderable type`): List of pairs (key,value)

#### list.nth
```
(default:'a,['a],int)->'a```

Get the n-th element of a list (the first element is at position 0), or'default' if element does not exist.

* `default` (`'a`): Default value if key does not exist
* `(unlabeled)` (`['a]`)
* `(unlabeled)` (`int`)

#### list.randomize
```
(['a])->['a]```

Shuffle the content of a list.

* `(unlabeled)` (`['a]`)

#### list.remove
```
('a,['a])->['a]```

Remove a value from a list.

* `(unlabeled)` (`'a`)
* `(unlabeled)` (`['a]`)

#### list.remove_assoc
```
('a,[('a*'c)])->[('a*'c)]```

Remove the first pair from an associative list.

* `(unlabeled)` (`'a`): Key of pair to be removed
* `(unlabeled)` (`[('a*'c)]`): List of pairs (key,value)

#### list.rev
```
(['a])->['a]```

Revert list order.

* `(unlabeled)` (`['a]`)

#### list.sort
```
((('a,'a)->int),['a])->['a]```

Sort a list according to a comparison function.

* `(unlabeled)` (`('a,'a)->int`)
* `(unlabeled)` (`['a]`)

#### list.tl
```
(['a])->['a]```

Return the list without its first element.

* `(unlabeled)` (`['a]`)

Math
----
#### *
```
('a,'a)->'a where 'a is a number type```

Multiplication of numbers.

* `(unlabeled)` (`anything that is a number type`)
* `(unlabeled)` (`anything that is a number type`)

#### +
```
('a,'a)->'a where 'a is a number type```

Addition of numbers.

* `(unlabeled)` (`anything that is a number type`)
* `(unlabeled)` (`anything that is a number type`)

#### -
```
('a,'a)->'a where 'a is a number type```

Substraction of numbers.

* `(unlabeled)` (`anything that is a number type`)
* `(unlabeled)` (`anything that is a number type`)

#### /
```
('a,'a)->'a where 'a is a number type```

Division of numbers.

* `(unlabeled)` (`anything that is a number type`)
* `(unlabeled)` (`anything that is a number type`)

#### abs
```
('a)->'a where 'a is a number type```

Absolute value.

* `(unlabeled)` (`anything that is a number type`)

#### acos
```
(float)->float```

Arc cosine. The argument must fall within the range [-1.0, 1.0]. Result is in radians and is between 0.0 and pi.

* `(unlabeled)` (`float`)

#### asin
```
(float)->float```

Arc sine. The argument must fall within the range [-1.0, 1.0]. Result is in radians and is between -pi/2 and pi/2.

* `(unlabeled)` (`float`)

#### atan
```
(float)->float```

Arc tangent. Result is in radians and is between -pi/2 and pi/2.

* `(unlabeled)` (`float`)

#### bool_of_float
```
(float)->bool```

Convert a float to a bool.

* `(unlabeled)` (`float`)

#### bool_of_int
```
(int)->bool```

Convert an int to a bool.

* `(unlabeled)` (`int`)

#### cos
```
(float)->float```

Cosine. Argument is in radians.

* `(unlabeled)` (`float`)

#### cosh
```
(float)->float```

Hyperbolic cosine. Argument is in radians.

* `(unlabeled)` (`float`)

#### dB_of_lin
```
(float)->float```

Convert linear scale into decibels.

* `(unlabeled)` (`float`)

#### exp
```
(float)->float```

Exponential.

* `(unlabeled)` (`float`)

#### float_of_int
```
(int)->float```

Convert an int to a float.

* `(unlabeled)` (`int`)

#### int_of_float
```
(float)->int```

Convert a float to a int.

* `(unlabeled)` (`float`)

#### lin_of_dB
```
(float)->float```

Convert decibels into linear scale.

* `(unlabeled)` (`float`)

#### log_10
```
(float)->float```

Base 10 logarithm.

* `(unlabeled)` (`float`)

#### log_e
```
(float)->float```

Natural logarithm.

* `(unlabeled)` (`float`)

#### mkfade
```
(?type:string,?start:float,?stop:float,?duration:float,
 source('a))->()->float```

Make a fade function based on a source's clock.

* `type` (`string` -- defaults to `"lin"`): Fade shape. One of: ``sin'', ``exp'', ``log'', ``lin''
* `start` (`float` -- defaults to `0.0`): Start value.
* `stop` (`float` -- defaults to `1.0`): Stop value.
* `duration` (`float` -- defaults to `3.0`): Duration in seconds.
* `(unlabeled)` (`source('a)`)

#### mod
```
(int,int)->int```

Integer remainder. If y is not zero, x == (x / y) * y + x mod y, and abs(x mod y) <= abs(y)-1.

* `(unlabeled)` (`int`)
* `(unlabeled)` (`int`)

#### pow
```
('a,'a)->'a where 'a is a number type```

Exponentiation of numbers.

* `(unlabeled)` (`anything that is a number type`)
* `(unlabeled)` (`anything that is a number type`)

#### random.float
```
(?min:float,?max:float)->float```

Generate a random value.

* `min` (`float` -- defaults to `-1000000.0`)
* `max` (`float` -- defaults to `1000000.0`)

#### random.int
```
(?min:int,?max:int)->int```

Generate a random value.

* `min` (`int` -- defaults to `-536870911`)
* `max` (`int` -- defaults to `536870912`)

#### sin
```
(float)->float```

Sine. Argument is in radians.

* `(unlabeled)` (`float`)

#### sinh
```
(float)->float```

Hyperbolic sine. Argument is in radians.

* `(unlabeled)` (`float`)

#### sqrt
```
(float)->float```

Square root.

* `(unlabeled)` (`float`)

#### tan
```
(float)->float```

Tangent. Argument is in radians.

* `(unlabeled)` (`float`)

#### tanh
```
(float)->float```

Hyperbolic tangent. Argument is in radians.

* `(unlabeled)` (`float`)

#### ~-
```
('a)->'a where 'a is a number type```

Returns the opposite of its argument.

* `(unlabeled)` (`anything that is a number type`)

Pair
----
#### fst
```
(('a*'b))->'a```

Get the first component of a pair.

* `(unlabeled)` (`('a*'b)`)

#### snd
```
(('a*'b))->'b```

Get the second component of a pair.

* `(unlabeled)` (`('a*'b)`)

String
------
#### %
```
(string,[(string*string)])->string```

`pattern % [...,(k,v),...]` changes in the pattern occurences of:
- `$(k)` into `v`;
- `$(if $(k2),"a","b")` into ``a'' if k2 is found in the list, ``b'' otherwise.

* `(unlabeled)` (`string`)
* `(unlabeled)` (`[(string*string)]`)

#### ^
```
(string,string)->string```

Concatenate strings.

* `(unlabeled)` (`string`)
* `(unlabeled)` (`string`)

#### base64.decode
```
(string)->string```

Decode a Base64 encoded string.

* `(unlabeled)` (`string`)

#### base64.encode
```
(string)->string```

Encode a string in Base64.

* `(unlabeled)` (`string`)

#### bool_of_string
```
(?default:bool,string)->bool```

Convert a string to a bool.

* `default` (`bool` -- defaults to `false`)
* `(unlabeled)` (`string`)

#### float_of_string
```
(?default:float,string)->float```

Convert a string to a float.

* `default` (`float` -- defaults to `0.0`)
* `(unlabeled)` (`string`)

#### int_of_string
```
(?default:int,string)->int```

Convert a string to a int.

* `default` (`int` -- defaults to `0`)
* `(unlabeled)` (`string`)

#### json_of
```
(?compact:bool,'a)->string```

Convert a value to a json string.

* `compact` (`bool` -- defaults to `false`): Output compact text.
* `(unlabeled)` (`'a`)

#### of_json
```
(default:'a,string)->'a```

Parse a json string into a liquidsoap value.

* `default` (`'a`): Default value if string cannot be parsed.
* `(unlabeled)` (`string`)

#### quote
```
(string)->string```

Escape shell metacharacters.

* `(unlabeled)` (`string`)

#### string.capitalize
```
(?capitalize:bool,?space_sensitive:bool,string)->string```

Return a string with the first character set to upper case (capitalize), or to lower case (uncapitalize).

* `capitalize` (`bool` -- defaults to `true`): Capitalize if true, uncapitalize otherwise
* `space_sensitive` (`bool` -- defaults to `true`): Capitalize each space separated sub-string.
* `(unlabeled)` (`string`)

#### string.case
```
(?lower:bool,string)->string```

Convert a string to lower or upper case.

* `lower` (`bool` -- defaults to `true`): Convert to lower case if true and uppercase otherwise.
* `(unlabeled)` (`string`)

#### string.concat
```
(?separator:string,[string])->string```

Concatenate strings.

* `separator` (`string` -- defaults to `""`)
* `(unlabeled)` (`[string]`)

#### string.escape
```
(?special_chars:[string],?escape_char:((string)->string),
 string)->string```

Escape special charaters in a string. String is parsed char by char. See `string.utf8.escape` for an UTF8-aware parsing function.

* `special_chars` (`[string]` -- defaults to ```
["\127", "\\", "\"", "\000", "\001", "\002", "\003", "\004", "\005", "\006", "\007", "\b", "\t", "\n", "\011", "\012", "\r", "\014", "\015", "\016", "\017", "\018", "\019", "\020", "\021", "\022", "\023", "\024", "\025", "\026", "\027", "\028", "\029", "\030", "\031"]```
): List of characters that should be escaped. The first character of each element in the list is considered.
* `escape_char` (`(string)->string` -- defaults to <code><fun></code>): Function used to escape a character.
* `(unlabeled)` (`string`)

#### string.extract
```
(pattern:string,string)->[(string*string)]```

Extract substrings from a string. 
Perl compatible regular expressions are recognized. Hence, special characters should be escaped. 
Returns a list of (index,value).
If the list does not have a pair associated to some index, it means that the corresponding pattern was not found.

* `pattern` (`string`)
* `(unlabeled)` (`string`)

#### string.length
```
(string)->int```

Get the length of a string.

* `(unlabeled)` (`string`)

#### string.match
```
(pattern:string,string)->bool```

Match a string with an expression. 
Perl compatible regular expressions are recognized. Hence, special characters should be escaped.

* `pattern` (`string`)
* `(unlabeled)` (`string`)

#### string.recode
```
(?in_enc:string,?out_enc:string,string)->string```

Convert a string. Effective only if Camomile is enabled.

* `in_enc` (`string` -- defaults to `""`): Input encoding. Autodetected if empty.
* `out_enc` (`string` -- defaults to `"UTF-8"`): Output encoding.
* `(unlabeled)` (`string`)

#### string.replace
```
(pattern:string,((string)->string),string)->string```

Replace substrings in a string. 
Will replace all substrings matched in the pattern by the string returned by the replace function.

* `pattern` (`string`)
* `(unlabeled)` (`(string)->string`)
* `(unlabeled)` (`string`)

#### string.split
```
(separator:string,string)->[string]```

Split a string at 'separator'. 
Perl compatible regular expressions are recognized. Hence, special characters should be escaped.

* `separator` (`string`)
* `(unlabeled)` (`string`)

#### string.sub
```
(string,start:int,length:int)->string```

Get a substring of a string. Returns ``'' if no such substring exists.

* `(unlabeled)` (`string`)
* `start` (`int`): Return a sub string starting at this position. First position is 0.
* `length` (`int`): Return a sub string of `length` characters.

#### string.trim
```
(string)->string```

Return a string without leading and trailing whitespace.

* `(unlabeled)` (`string`)

#### string.utf8.escape
```
(?special_chars:[string],?escape_char:((string)->string),
 string)->string```

Escape special charaters in an UTF8 string.

* `special_chars` (`[string]` -- defaults to ```
["\127", "\\", "\"", "\000", "\001", "\002", "\003", "\004", "\005", "\006", "\007", "\b", "\t", "\n", "\011", "\012", "\r", "\014", "\015", "\016", "\017", "\018", "\019", "\020", "\021", "\022", "\023", "\024", "\025", "\026", "\027", "\028", "\029", "\030", "\031"]```
): List of characters that should be escaped. The first character of each element in the list is considered.
* `escape_char` (`(string)->string` -- defaults to <code><fun></code>): Function used to escape a character.
* `(unlabeled)` (`string`)

#### string_of
```
('a)->string```

Return the representation of a value.

* `(unlabeled)` (`'a`)

#### string_of_metadata
```
([(string*string)])->string```

Standard function for displaying metadata.
Shows artist and title, using ``Unknown'' when a field is empty.

* `(unlabeled)` (`[(string*string)]`): Metadata packet to be displayed.

#### url.decode
```
(?plus:bool,string)->string```

Decode an encoded url (e.g. ``%20'' becomes `` '').

* `plus` (`bool` -- defaults to `true`)
* `(unlabeled)` (`string`)

#### url.encode
```
(?plus:bool,string)->string```

Encode an url (e.g. `` '' becomes ``%20'').

* `plus` (`bool` -- defaults to `true`)
* `(unlabeled)` (`string`)

#### url.split
```
(string)->(string*[(string*string)])```

Split an url of the form foo?arg=bar&arg2=bar2 into (``foo'',[(``arg'',``bar''),(``arg2'',``bar2'')]).

* `(unlabeled)` (`string`): Url to split

#### url.split_args
```
(string)->[(string*string)]```

Split the arguments of an url of the form arg=bar&arg2=bar2 into [(``arg'',``bar''),(``arg2'',``bar2'')].

* `(unlabeled)` (`string`): Agument string to split

System
------
#### argv
```
(?default:string,int)->string```

Get command-line parameters.

* `default` (`string` -- defaults to `""`)
* `(unlabeled)` (`int`)

#### environment
```
()->[(string*string)]```

Return the process environment.

#### exe_ext
```
string```

Executable file extension.

#### exec_at
```
(?freq:float,pred:(()->bool),(()->unit))->unit```

Execute a given action when a predicate is true. This will be run in background.

* `freq` (`float` -- defaults to `1.0`): Frequency for checking the predicate, in seconds.
* `pred` (`()->bool`): Predicate indicating when to execute the function, typically a time interval such as `{10h-10h30}`.
* `(unlabeled)` (`()->unit`): Function to execute when the predicate is true.

#### exit
```
(int)->unit```

Immediately stop the application. This should only be used in extreme cases or to specify an exit value. The recommended way of stopping Liquidsoap is to use shutdown.

* `(unlabeled)` (`int`): Exit value.

#### file.contents
```
(string)->string```

Read the whole contents of a file.

* `(unlabeled)` (`string`)

#### file.exists
```
(string)->bool```

Returns true if the file or directory exists.

* `(unlabeled)` (`string`)

#### file.extension
```
(?dir_sep:string,string)->string```

Returns a file's extension.

* `dir_sep` (`string` -- defaults to `"/"`): Directory separator.
* `(unlabeled)` (`string`)

#### file.is_directory
```
(string)->bool```

Returns true if the file exists and is a directory.

* `(unlabeled)` (`string`)

#### file.mime_default
```
(string)->string```

Alias of file.mime (because it is available)

* `(unlabeled)` (`string`)

#### file.rmdir
```
(string)->unit```

Remove a directory and its content.

* `(unlabeled)` (`string`)

#### file.temp
```
(string,string)->string```

Return a fresh temporary filename. The temporary file is created empty, with permissions 0o600 (readable and writable only by the file owner).

* `(unlabeled)` (`string`): File prefix
* `(unlabeled)` (`string`): File suffix

#### file.temp_dir
```
(string,string)->string```

Return a fresh temporary directory name. The temporary directory is created empty, with permissions 0o700 (readable, writable and listable only by the file owner).

* `(unlabeled)` (`string`): Directory prefix
* `(unlabeled)` (`string`): Directory suffix

#### file.unlink
```
(string)->unit```

Remove a file.

* `(unlabeled)` (`string`)

#### file.watch
```
(string,(()->unit))->()->unit```

Call a function when a file is modified. Returns unwatch function.

* `(unlabeled)` (`string`): File to watch.
* `(unlabeled)` (`()->unit`): Handler function.

#### file.write
```
(data:string,?append:bool,?perms:int,string)->bool```

Write data to a file. Returns `true` if successful.

* `data` (`string`): Data to write
* `append` (`bool` -- defaults to `false`): Append data if file exists.
* `perms` (`int` -- defaults to `420`): Default file rights if created
* `(unlabeled)` (`string`): Path to write to

#### get_mime
```
(string)->string```

Generic mime test. First try to use file.mime if it exist. Otherwise try to get the value using the file binary. Returns ``'' (empty string) if no value can be found.

* `(unlabeled)` (`string`): The file to test

#### get_process_lines
```
(?timeout:float,?env:[(string*string)],?inherit_env:bool,
 string)->[string]```

Perform a shell call and return the list of its output lines.

* `timeout` (`float` -- defaults to `-1.0`): Cancel process after `timeout` has elapsed. Ignored if negative.
* `env` (`[(string*string)]` -- defaults to `[]`): Process environment
* `inherit_env` (`bool` -- defaults to `true`): Inherit calling process's environment when `env` parameter is empty.
* `(unlabeled)` (`string`): Command to run

#### get_process_output
```
(?timeout:float,?env:[(string*string)],?inherit_env:bool,
 string)->string```

Perform a shell call and return its output.

* `timeout` (`float` -- defaults to `-1.0`): Cancel process after `timeout` has elapsed. Ignored if negative.
* `env` (`[(string*string)]` -- defaults to `[]`): Process environment
* `inherit_env` (`bool` -- defaults to `true`): Inherit calling process's environment when `env` parameter is empty.
* `(unlabeled)` (`string`): Command to run

#### getenv
```
(string)->string```

Get the value associated to a variable in the process environment. Return ``'' if variable is not set.

* `(unlabeled)` (`string`)

#### getopt
```
(?default:string,string)->string```

Parse command line options:
`getopt("-o")` returns ``1'' if ``-o'' was passed without any parameter, ``0'' otherwise.
`getopt(default="X","-o")` returns ``Y'' if ``-o Y'' was passed, ``X'' otherwise.
The result is removed from the list of arguments, affecting subsequent
calls to `argv()` and `getopt()`.

* `default` (`string` -- defaults to `""`)
* `(unlabeled)` (`string`)

#### getpid
```
()->int```

Get the process' pid.

#### gettimeofday
```
()->float```

Return the current time since 00:00:00 GMT, Jan. 1, 1970, in seconds.

#### gmtime
```
(float,
 ((sec:int,min:int,hour:int,mday:int,mon:int,year:int,
   wday:int,yday:int,isdst:bool)->'a))->'a```

Convert a time in seconds into a date in the UTC time zone and execute passed callback with the result. Fields meaning same as POSIX's `tm struct`. Warning: ``year'' is: year - 1900, i.e. 117 for 2017!

* `(unlabeled)` (`float`)
* `(unlabeled)` (```
(sec:int,min:int,hour:int,mday:int,mon:int,year:int,
 wday:int,yday:int,isdst:bool)->'a```
)

#### localtime
```
(float,
 ((sec:int,min:int,hour:int,mday:int,mon:int,year:int,
   wday:int,yday:int,isdst:bool)->'a))->'a```

Convert a time in seconds into a date in the local time zone and execute passed callback with the result. Fields meaning same as POSIX's `tm struct`. Warning: ``year'' is: year - 1900, i.e. 117 for 2017!

* `(unlabeled)` (`float`)
* `(unlabeled)` (```
(sec:int,min:int,hour:int,mday:int,mon:int,year:int,
 wday:int,yday:int,isdst:bool)->'a```
)

#### on_shutdown
```
((()->unit))->unit```

Register a function to be called when Liquidsoap shuts down.

* `(unlabeled)` (`()->unit`)

#### on_start
```
((()->unit))->unit```

Register a function to be called when Liquidsoap starts.

* `(unlabeled)` (`()->unit`)

#### os.type
```
string```

Type of OS running liquidsoap.

#### path.basename
```
(string)->string```

Get the base name of a path.

* `(unlabeled)` (`string`)

#### path.concat
```
(string,string)->string```

Concatenate two paths, using the appropriate directory separator.

* `(unlabeled)` (`string`)
* `(unlabeled)` (`string`)

#### path.dirname
```
(string)->string```

Get the directory name of a path.

* `(unlabeled)` (`string`)

#### read
```
(?hide:bool)->string```

Read some value from standard input (console).

* `hide` (`bool` -- defaults to `false`): Hide typed characters (for passwords).

#### reopen.stderr
```
(string)->unit```

Reopen standard error on the given file

* `(unlabeled)` (`string`)

#### reopen.stdin
```
(string)->unit```

Reopen standard input on the given file

* `(unlabeled)` (`string`)

#### reopen.stdout
```
(string)->unit```

Reopen standard output on the given file

* `(unlabeled)` (`string`)

#### restart
```
()->unit```

Restart the application.

#### run_process
```
(?env:[(string*string)],?inherit_env:bool,?timeout:float,
 string)->((string*string)*(string*string))```

Run a process in a shell environment. Returns: `((stdout,stderr),status)` where status is one of: `("exit","<code>")`, `("killed","<signal number>")`, `("stopped","<signal number>")`, `("exception","<exception description>")`, `("timeout","<run time>")`.

* `env` (`[(string*string)]` -- defaults to `[]`): Process environment
* `inherit_env` (`bool` -- defaults to `true`): Inherit calling process's environment when `env` parameter is empty.
* `timeout` (`float` -- defaults to `-1.0`): Cancel process after `timeout` has elapsed. Ignored if negative.
* `(unlabeled)` (`string`): Command to run

#### setenv
```
(string,string)->unit```

Set the value associated to a variable in the process environment.

* `(unlabeled)` (`string`): Variable to be set.
* `(unlabeled)` (`string`): Value to set.

#### shutdown
```
()->unit```

Shutdown the application.

#### source.is_up
```
(source('a))->bool```

Check whether a source is up.

* `(unlabeled)` (`source('a)`)

#### source.on_shutdown
```
(source('a),(()->unit))->unit```

Register a function to be called when source shuts down.

* `(unlabeled)` (`source('a)`)
* `(unlabeled)` (`()->unit`)

#### system
```
(string)->unit```

Shell command call. Set verbose to true to log process' output and errors.

* `(unlabeled)` (`string`)

#### test_process
```
(?timeout:float,?env:[(string*string)],?inherit_env:bool,
 string)->bool```

Return true if process exited with 0 code.

* `timeout` (`float` -- defaults to `-1.0`): Cancel process after `timeout` has elapsed. Ignored if negative.
* `env` (`[(string*string)]` -- defaults to `[]`): Process environment
* `inherit_env` (`bool` -- defaults to `true`): Inherit calling process's environment when `env` parameter is empty.
* `(unlabeled)` (`string`): Command to test

#### time
```
()->float```

Return the current time since 00:00:00 GMT, Jan. 1, 1970, in seconds.

#### which
```
(string)->string```

which(``progname'') searches for an executable named ``progname'' using directories from the PATH environment variable and returns ``'' if it could not find one.

* `(unlabeled)` (`string`)


