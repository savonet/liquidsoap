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

That is what most scripts want. Two things are still worth deciding: which
thread the callback runs in, and what happens when a registration is not meant
to last as long as the source.

## Synchronous or not

`synchronous` has no default: every registration has to say which of the two
behaviours it wants.

With `synchronous=true` the callback runs where the event happened, which for
stream events — `on_track`, `on_metadata`, `on_frame` — is the streaming thread.
The stream waits for it. That is what you want when the callback has to take
effect before the stream moves on, and it means the callback must be quick:
no HTTP request, no database query, no `thread.pause`, nothing that waits on
something else. A callback that takes too long makes the streaming loop fall
behind and produces [catchup errors](./latency_control.md).

With `synchronous=false` the callback is handed to liquidsoap's scheduler and
runs on one of its generic queues instead. Blocking is fine there, so this is
the setting for posting to an API, writing to a database or calling out to an
external program. In exchange you give up three things:

- **Timing.** The callback runs shortly after the event, not at it. By then the
  source may have moved on, so use what the callback is handed rather than
  asking the source what it is doing now.
- **Ordering.** Firings are scheduled independently and run in parallel, one per
  core, so they can overlap and complete out of order. Several copies of a slow
  callback can be running at once, and a callback writing several references
  can be read half-done; see [sharing state between
  tasks](./scheduling.md#sharing-state-between-tasks).
- **Room for other work.** The scheduler also resolves requests — downloads,
  playlist reloads. A callback slower than the events feeding it builds a
  backlog and crowds them out. `settings.scheduler.blocking_tasks` caps how many
  such tasks run at once.

When in doubt, ask what the callback does: if it only reads its arguments and
sets a variable, `synchronous=true`; if it talks to anything outside
liquidsoap, `synchronous=false`.

## Taking a callback back

Registering returns a value with a `release` method that detaches the callback:

```{.liquidsoap include="callback-release.liq"}

```

Once released, the callback stops firing and the function it wrapped is
forgotten. Releasing twice is harmless.

Releasing takes the callback off the source; it does not cancel work already in
flight. An asynchronous firing that the scheduler has already queued still runs,
so code that releases a callback and then tears down what that callback touches
should be able to cope with one last call.

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
