# Get help

Liquidsoap is a self-documented application,
which means that it can provide help about several of its aspects.
You will learn here how to get help by yourself, by asking liquidsoap.
If you do not succeed in asking the tool, you can of course get help from
humans. We maintain the following communication channels:

- Slack: [slack.liquidsoap.info](http://slack.liquidsoap.info/)
- IRC: #savonet on [irc.libera.chat](https://libera.chat/) (through a slack bridge)
- Mailing list: [savonet-users@lists.sourceforge.net](mailto:savonet-users@lists.sourceforge.net)

## Scripting API

When scripting in liquidsoap, one uses functions that are either _builtin_
(_e.g._ `input.http` or `output.icecast`)
or defined in the [script library](script_loading.html) (_e.g_ `output`).
All these functions come with a documentation, that you can access by
executing `liquidsoap -h FUNCTION` on the command-line. For example:

```
$ liquidsoap -h sine

Generate a sine wave.

Type: (?id : string?, ?amplitude : {float}, ?duration : float,
 ?{float}) -> source(audio=internal('a),
video=internal('b), midi=internal('c))

Category: Source / Input

Parameters:

 * id : string? (default: null)
     Force the value of the source ID.

 * amplitude : {float} (default: 1.)
     Maximal value of the waveform.

 * duration : float (default: -1.)
     Duration in seconds (negative means infinite).

 * (unlabeled) : {float} (default: 440.)
     Frequency of the sine.

Methods:

 * fallible : bool
     Indicate if a source may fail, i.e. may not be ready to stream.

 * id : () -> string
     Identifier of the source.

 * is_active : () -> bool
     `true` if the source is active, i.e. it is continuously animated by its
     own clock whenever it is ready. Typically, `true` for outputs and
     sources such as `input.http`.

 * is_ready : () -> bool
     Indicate if a source is ready to stream. This does not mean that the
     source is currently streaming, just that its resources are all properly
     initialized.

 * is_up : () -> bool
     Indicate that the source can be asked to produce some data at any time.
     This is `true` when the source is currently being used or if it could be
     used at any time, typically inside a `switch` or `fallback`.

 * on_leave : ((() -> unit)) -> unit
     Register a function to be called when source is not used anymore by
     another source.

 * on_metadata : ((([string * string]) -> unit)) -> unit
     Call a given handler on metadata packets.

 * on_shutdown : ((() -> unit)) -> unit
     Register a function to be called when source shuts down.

 * on_track : ((([string * string]) -> unit)) -> unit
     Call a given handler on new tracks.

 * remaining : () -> float
     Estimation of remaining time in the current track.

 * seek : (float) -> float
     Seek forward, in seconds (returns the amount of time effectively
     seeked).

 * self_sync : () -> bool
     Is the source currently controlling its own real-time loop.

 * skip : () -> unit
     Skip to the next track.

 * time : () -> float
     Get a source's time, based on its assigned clock.
```

Of course if you do not know what function you need, you'd better go
through the [API reference](reference.html).

Please note that some functions
in that list are optional and may not be available with your local `liquidsoap`
install unless you install the optional dependency that enables it. The list of
optional dependencies is listed via `opam info liquidsoap` if you have installed
it this way or can in our [build page](build.html).

## Settings

Liquidsoap scripts contain expression like `settings.log.stdout := true`.
These are _settings_, global variables affecting the behaviour of the
application.

Some common settings have shortcut for convenience. These are all shortcuts to their respective `settings` values:

```liquidsoap
log.level :=4
log.file := true
log.stdout := true
init.daemon := true
audio.samplerate := 48000
audio.channels := 2
video.frame.width := 720
video.frame.height := 1280
```

You can have a list of available settings, with their documentation,
by running `liquidsoap --list-settings`.

The output of these commands is a valid liquidsoap script,
which you can edit to set the values that you want,
and load it ([implicitly](script_loading.html) or not) before you other scripts.

You can browse online the [list of available settings](settings.html).
