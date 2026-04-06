# Get help

Liquidsoap is a self-documented application, meaning it can answer many
questions about its own API and settings directly from the command line.
This page explains how to use those built-in help tools.

If you can't find what you need, you can also reach the community:

- **Chat:** Discord at [chat.liquidsoap.info](http://chat.liquidsoap.info/), or IRC `#savonet` on [irc.libera.chat](https://libera.chat/) (bridged to Discord)
- **Long-form support:** [GitHub Discussions](https://github.com/savonet/liquidsoap/discussions)
- **Bug reports and feature requests:** [GitHub Issues](https://github.com/savonet/liquidsoap/issues)

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

Category: Source / Input / Passive

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

If you don't know which function you need, browse the [API reference](reference.html).

Note that some functions are optional and may not be available in your local
`liquidsoap` install — they require an optional dependency to be enabled. You
can see the list of optional dependencies via `opam info liquidsoap` or on the
[build page](build.html).

## Settings

Liquidsoap scripts can contain expressions like `settings.log.stdout := true`.
These are _settings_: global variables that affect the behaviour of the
application.

Some common settings have shortcuts for convenience. These are all aliases for their respective `settings` values:

```{.liquidsoap include="settings.liq"}

```

You can have a list of available settings, with their documentation,
by running `liquidsoap --list-settings`.

The output is a valid liquidsoap script that you can edit to set the values
you want, then load it ([implicitly](script_loading.html) or explicitly) before
your other scripts.

You can browse online the [list of available settings](settings.html).
