# Source composition

Operators such as `fallback`, `switch`, `rotate` and `random` all do the same
thing: at any moment they pick one of their sources and stream it. The
interesting part is not the picking itself but what happens **between two
sources** — when one stops being streamed and another takes over.

Think of it as a handoff between a **leaving** source and an **entering** one.
Each source says how it wants to be handed over to, so you rarely have to
configure the switch itself. This page walks through the situations you are
likely to build.

## A live show interrupting the music

The most common radio setup: music plays all day, and a live show takes over
whenever someone connects.

```liquidsoap
live  = input.harbor("live")
music = playlist("~/music")

radio = fallback([live, music])
```

What happens:

- **Nobody connected.** `live` is not available, so `music` plays.
- **The DJ connects, in the middle of a song.** A live input does not wait for
  track boundaries, so it cuts in right away. The song is faded out underneath
  it rather than being chopped off.
- **The DJ disconnects.** `live` is simply gone, so there is nothing to fade out
  and the music comes back immediately. Because the song had been interrupted,
  the playlist does not resume it half-way through — it starts a **fresh
  track**.

You did not have to say any of that. `input.harbor` is a live source and
`playlist` is a file source, and each behaves accordingly.

## Jingles between songs

Now a jingle every few songs. A jingle cutting into the middle of a song would
sound broken.

```liquidsoap
music   = playlist("~/music")
jingles = playlist("~/jingles")

radio = rotate([music.{weight = 3}, jingles.{weight = 1}])
```

Both are file sources, so neither is willing to interrupt the other: when the
jingle's turn comes up, the switch **waits for the current song to finish**.
The jingle then starts cleanly at the boundary, with no fade — nothing was
interrupted, so there is nothing to fade.

## A show on a schedule

```liquidsoap
show  = playlist("~/morning-show")
music = playlist("~/music")

radio = switch([({8h-10h}, show), ({true}, music)])
```

At 8h the show becomes eligible while a song is still playing. Both sides are
file sources, so the show does not barge in: it starts once the current song
ends. At 10h the same thing happens in reverse.

## Falling back to a backup file

```liquidsoap
radio = fallback([playlist("~/music"), single("~/backup.mp3")])
```

If the playlist cannot produce anything — no files, all requests failing — it is
simply unavailable. Nothing was interrupted, so the backup starts immediately
and without a fade.

## An announcement that cannot wait

Sometimes a file source _should_ interrupt. Say you push emergency
announcements into a queue and they must go out now, not after the current
song:

```liquidsoap
announcements = request.queue()
music         = playlist("~/music")

radio = fallback([announcements.{track_sensitive = false}, music])
```

`track_sensitive = false` means "I do not need to wait for a boundary". As soon
as something lands in the queue, the song is faded out and the announcement
plays.

This is the general rule for interruptions:

> A switch cuts into a song that is still playing only if **at least one** of
> the two sources involved has `track_sensitive = false`. Otherwise it waits for
> the end of the track.

Both sides get a say, which is why adding a live input to a `fallback` never
changes how two playlists hand over to each other.

## A relay that carries a playlist

`input.http` is treated as live by default: it cuts in immediately. But if the
stream you are relaying is itself a playlist of songs, you would rather wait for
a boundary:

```liquidsoap
relay = input.http("https://relay.example.com/stream")
relay.composition_type := "file"

radio = fallback([live_show, relay, music])
```

`composition_type` is either `"file"` or `"live"` and picks which set of
defaults a source gets. It is chosen automatically — inputs that run on their
own are live, files are not — and you only set it when a source does not behave
like its type suggests.

## Never the same jingle twice in a row

```liquidsoap
radio = rotate([music, jingles.{single = true}])
```

`single = true` forbids picking that source for two consecutive tracks.

## A source that announces its own metadata

When a switch selects a source, listeners need to know what is playing — but the
source may be resuming a track it already announced. So the switch replays that
source's latest metadata by default.

If a source manages its own announcements, turn it off:

```liquidsoap
radio = fallback([s1.{replay_metadata = false}, s2])
```

Replayed metadata never overwrites metadata the source provides itself. If the
entering source starts a fresh track with its own title, that title wins — the
replay only fills in what would otherwise be missing.

## Writing your own transition

The default handoff fades the leaving source out. To do something else, give
the **entering** source an `on_select`:

```liquidsoap
def my_transition({ending, starting, replay_metadata = _}) =
  if null.defined(ending) then
    old = max_duration(3., null.get(ending))
    (add([fade.out(duration=3., old), fade.in(duration=3., starting)]) : source)
  else
    starting
  end
end

radio = fallback([s1.{on_select = my_transition}, s2])
```

`ending` is the source being left, and it is `null` when nothing was
interrupted — so the `else` branch is the "started at a boundary" case, where
there is nothing to blend.

Note the `max_duration`. Whatever you return keeps pulling from `ending` until
you stop it, and the leaving source is only cleaned up once nothing pulls from
it any more. Without a bound, `add` would pull from it forever and the cleanup
would never run.

## Cleaning up after a source

The counterpart to `on_select` is `on_leave`, which belongs to the source being
**left** and runs once it has been released:

```liquidsoap
radio =
  fallback([
    live,
    music.{on_leave = fun ({track_sensitive}) ->
      log("music left, finished naturally: #{track_sensitive}")}
  ])
```

`track_sensitive` here tells you _how_ it ended: `true` if it played to a track
boundary, `false` if it was interrupted. This is what the default file behaviour
uses to decide to skip a half-played track so the source starts fresh next time.

## Where the defaults come from

Two global profiles hold the defaults, `source.composition.file` and
`source.composition.live`. Each carries `on_select`, `on_leave`,
`track_sensitive` and `replay_metadata`, and you can replace either wholesale to
change the behaviour of every source of that type. `max_fade` controls the
default fade length:

```liquidsoap
settings.source.composition.max_fade := 2.
```

To see what a given source is actually using, ask it:

```
liquidsoap -h playlist
```

The **Composition methods** section lists `composition_type`,
`track_sensitive`, `replay_metadata`, `single`, `on_select` and `on_leave`,
along with the defaults in force.
