# Source callbacks

Sources tell you what they are doing through callbacks: `on_metadata` when they
send new metadata, `on_track` at track boundaries, `on_connect` when a client
shows up, and so on. You attach one by calling the method on the source:

```liquidsoap
music.on_metadata(synchronous=true, print_title)
```

A callback attached this way belongs to the source. It fires every time the
event happens, for as long as that source is alive — which, for a source defined
at the top level of a script, means until liquidsoap shuts down.

That is what most scripts want, and if all your callbacks are registered once at
startup there is nothing else to know here. The rest of this page is about the
case where they are not.

## Taking a callback back

Registering returns a value with a `release` method that detaches the callback:

```{.liquidsoap include="callback-release.liq"}

```

Once released, the callback stops firing and the function it wrapped is
forgotten. Releasing twice is harmless.

The returned value is otherwise `unit`, so you can keep ignoring it: a
registration whose result you drop is still valid liquidsoap.

## When you need to release

The question to ask is: **does the callback outlive whatever registered it?**

A function that registers on a source it is given, and that runs more than once,
answers yes. Each call leaves another callback on that source, and they all keep
firing. Common shapes:

- a telnet or HTTP command that builds something out of a long-lived source,
- a handler that decorates the current track and is itself called on every
  track,
- code that creates and destroys sources or outputs while the script runs, as in
  [dynamic source creation](./dynamic_sources.md).

In those cases the caller keeps the release and uses it when it tears down what
it built:

```{.liquidsoap include="callback-release-per-call.liq"}

```

Callbacks registered on a source the function created itself need nothing: they
are collected along with the source they are attached to.

Liquidsoap tells you when this goes wrong. Past five callbacks of the same kind
on the same source, it logs:

```
[music:3] 6 on_metadata callbacks registered on music. If you are registering
from a function that runs more than once, keep the value it returns and call
its release() method.
```

The count is per source _and_ per callback, so the message names what is piling
up: six `on_metadata` warns, three `on_metadata` alongside three `on_track` does
not. A script that registers and releases in step never reaches the threshold.

## Callbacks registered on your behalf

Plenty of operators register on the source you hand them: `fade.in` watches its
input for track marks and metadata, and it is not alone. Call one of them once
per stream, on a source that outlives the stream, and you have the same pile-up
with no registration of your own to hold onto.

`source.collect_callback_releases` runs a function and gathers everything it
registered on the sources you name into a single release:

```{.liquidsoap include="callback-collect.liq" from="BEGIN" to="END"}

```

It returns `result`, whatever the function returned, and `release`, which takes
back every callback the function registered on any of the listed sources.
Callbacks registered on other sources are left alone, and so is liquidsoap's own
internal wiring: only callbacks registered from a script are collected.

The sources are passed as a list of `source(_)`, so sources carrying different
content can be watched together.

## What the operators already do

`switch` (and `fallback`, `rotate`, `random`) hands sources to `on_select` and
`on_leave`, and `cross` hands them to its transition. Those functions run on
every selection and every crossing, so the operators collect what they register
and release it once the selection or the crossing is over. Writing [your own
transition](./composition.md) needs no bookkeeping on your part.

`source.dynamic` is the exception: its `next` function takes no source
arguments, so there is nothing for the operator to watch. If `next` registers on
a source from the enclosing script, release it yourself.
