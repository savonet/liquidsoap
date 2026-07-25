# Source composition

Operators such as `fallback`, `switch`, `rotate` and `random` all do the same
thing: at any moment they pick one of their sources and stream it. The
interesting part is not the picking itself but what happens **between two
sources** — when one stops being streamed and another takes over.

This page explains that moment. Once you think of it as a handoff between a
**leaving** source and an **entering** source, the whole API falls into place.

## The handoff

When a switch changes its mind, two sources are involved:

- the **leaving** source, which was being streamed until now,
- the **entering** source, which takes over.

Everything a switch does is decided by three questions, and each one is
answered by the sources themselves rather than by the switch:

1. **May the handoff happen right now?** — answered by `track_sensitive`
2. **What does the handoff sound like?** — answered by the entering source's `on_select`
3. **What does the leaving source do about it?** — answered by its own `on_leave`

This is the part worth remembering:

|             | Belongs to              | Called                                     |
| ----------- | ----------------------- | ------------------------------------------ |
| `on_select` | the **entering** source | when it is picked, to build the transition |
| `on_leave`  | the **leaving** source  | once it has been fully released            |

Note the asymmetry: **the leaving source appears in both**. The transition
_over_ it is written by the entering source's `on_select`, which receives it as
`ending`. Its own cleanup is written by its `on_leave`. A source therefore
describes two things: how it wants to come in, and how it wants to tidy up
after going out.

## Boundary or preemption?

There are only two kinds of handoff, and the difference drives everything else.

**At a track boundary.** The leaving source has finished its track — it has no
more data for it, or it is no longer available. There is nothing left to fade
out, so the entering source simply starts. `on_select` receives
`ending = null`.

**Mid-track (a preemption).** The leaving source was still playing when the
entering source took over. There is something to fade out, so `on_select`
receives `ending` set to the leaving source, and the default behaviour blends
the two.

So a simple rule: **`ending` is `null` exactly when nothing was interrupted.**

## When is a preemption allowed?

Cutting into a track that is still playing is not always desirable. Each source
carries a `track_sensitive` flag saying whether it cares about track
boundaries:

- `true` — "do not interrupt me mid-track, and do not start me mid-track"
- `false` — "I do not mind"

File-based sources (`playlist`, `single`) default to `true`. Live inputs
(`input.harbor`, `input.http`, sound cards) default to `false`.

The rule is:

> A switch may cut into a track that is still playing only if **at least one**
> of the two sources involved has `track_sensitive = false`. Otherwise it waits
> for the playing source to reach a track boundary.

Both parties get a say, which is what makes the common setups behave sensibly:

```liquidsoap
live  = input.harbor("live")     # track_sensitive = false
music = playlist("/music")       # track_sensitive = true
backup = single("/backup.mp3")   # track_sensitive = true

radio = fallback([live, music, backup])
```

- `music` → `live`: `live` does not need a boundary, so the live show cuts in
  immediately, fading out the music.
- `music` → `backup`: both insist on boundaries, so the switch waits for the
  end of the current track. Adding `live` to the list does **not** change this.

That last point matters: composition is decided per handoff, between the two
sources actually involved. A live source in the list never changes how two file
sources hand off to each other.

## The lifecycle

For a preemption, in order:

1. The switch picks the entering source.
2. The entering source's `on_select` is called with `ending` set to the leaving
   source. It returns the source that will actually be streamed — typically a
   blend of the two.
3. That returned source is streamed. The leaving source is still being consumed
   through it.
4. Once nothing pulls from the leaving source any more, it is released.
5. Its `on_leave` is called.

Step 4 is the one that catches people out. **`on_leave` only fires once the
leaving source is actually discarded.** If your custom `on_select` keeps
pulling from `ending` forever, it never fires. See
[migrating](migrating.html#per-source-methods-on-switch-fallback-rotate-random)
for the `max_duration` idiom that bounds it.

## Metadata on selection

When a source is selected, listeners need to know what is playing — but the
source may be resuming in the middle of a track it already announced. So by
default a switch replays that source's latest metadata as it comes in. This is
the `replay_metadata` flag, `true` by default.

Replayed metadata never overrides metadata the source provides itself: if the
entering source starts a fresh track with its own metadata, that metadata wins.
The replay only fills in what would otherwise be missing.

## Choosing a profile

Which defaults a source gets is decided by its `composition_type`, either
`"file"` or `"live"`. It is set automatically — passive inputs and file sources
become `"file"`, active inputs become `"live"` — and transform operators
inherit it from what they wrap.

You can override it when a source does not behave like its type suggests. A
relay carrying a playlist is the usual example:

```liquidsoap
relay = input.http("https://relay.example.com/stream")
relay.composition_type := "file"   # wait for track boundaries
```

The defaults themselves live in two global profiles,
`source.composition.file` and `source.composition.live`, which you can replace
wholesale. Each holds `on_select`, `on_leave`, `track_sensitive` and
`replay_metadata`.

## Per-source overrides

Any of these can be set on a single source without touching the profiles:

```liquidsoap
# Never play this source twice in a row.
radio = rotate([jingles.{single = true}, music])

# This source announces its own metadata; don't replay anything.
radio = fallback([s1.{replay_metadata = false}, s2])

# Custom transition, only for s1.
def my_on_select({ending, starting, replay_metadata = _}) =
  if null.defined(ending) then
    old = max_duration(3., null.get(ending))
    (add([fade.out(duration=3., old), fade.in(duration=3., starting)]) : source)
  else
    starting
  end
end

radio = fallback([s1.{on_select = my_on_select}, s2])
```

Use `liquidsoap -h <operator>` to see the composition methods and their current
defaults for any source.
