# Get help

Liquidsoap is a self-documented application, meaning it can answer many
questions about its own API and settings directly from the command line.
This page explains how to use those built-in help tools.

If you can't find what you need, you can also reach the community:

- Chat: Discord at [chat.liquidsoap.info](http://chat.liquidsoap.info/), or IRC `#savonet` on [irc.libera.chat](https://libera.chat/) (bridged to Discord).
- Longer questions and support: [GitHub Discussions](https://github.com/savonet/liquidsoap/discussions).
- Bug reports and feature requests: [GitHub Issues](https://github.com/savonet/liquidsoap/issues).

## Scripting API

When scripting in liquidsoap, one uses functions that are either _builtin_
(_e.g._ `input.http` or `output.icecast`)
or defined in the [script library](./script_lifecycle.md#script-loading) (_e.g._ `output`).
All these functions come with documentation, which you can access by
executing `liquidsoap -h FUNCTION` on the command-line. For example:

```
$ liquidsoap -h sine
Generate a sine wave.

Type: (?id : string?, ?amplitude : {float}, ?duration : float?, ?{float}) ->
source(audio=pcm*)

Category: Source / Input / Passive

Composition:

  This source uses file composition by default.

Arguments:

 * amplitude : {float} (default: 1.0)
     Maximal value of the waveform.

 * duration : float? (default: null)
     Duration in seconds (`null` means infinite).

 * id : string? (default: null)
     Force the value of the source ID.

 * (unlabeled) : {float} (default: 440.0)
     Frequency of the sine.

Methods:

 * duration : () -> float
     Estimation of the duration of the current track.

 * elapsed : () -> float
     Elapsed time in the current track.

 * fallible : bool
     Indicate if a source may fail, i.e. may not be ready to stream.

...
```

If you don't know which function you need, browse the [API reference](./reference.md).

Note that some functions are optional and may not be available in your local
`liquidsoap` install: they require an optional dependency to be enabled. You
can see the list of optional dependencies via `opam info liquidsoap` or on the
[build page](./build.md).

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
you want, then load it ([implicitly](./script_lifecycle.md#script-loading) or explicitly) before
your other scripts.

You can browse online the [list of available settings](./settings.md).
