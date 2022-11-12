# Integrating a music library: an example with Beets

Liquidsoap's native sources can read from files and folders,
but if your radio uses an important music library
(more than a thousand tracks)
sorting by folders may not be enough.
You will also need to adjust the playout gain per track (ReplayGain).
In that case you would better have a music library
queried by Liquidsoap.
In this section we'll do this with [Beets](http://beets.io/).
Beets holds your music catalog,
cleans tracks' tags before importing,
can compute each track's ReplayGain,
and most importantly has a command-line interface we can leverage from Liquidsoap.
The following examples may also inspire you to integrate another library or your own scripts.

After installing Beets,
enable the `random` plug-in
(see [Beets documentation on plug-ins](https://beets.readthedocs.io/en/stable/plugins/index.html#using-plugins)).
To enable gain normalization, install and configure the
[`replaygain`](https://beets.readthedocs.io/en/stable/plugins/replaygain.html) plug-in.
To easily add single tracks to you library,
you might also be interested in the
[drop2beets](https://github.com/martinkirch/drop2beets#drop2beets) plug-in.
The following examples suppose you defined a `BEET` constant,
which contains the complete path to your `beet` executable (on UNIX systems, find it with `which beet`). For example:

```
BEET = "/home/radio/.local/bin/beet"
```

Before creating a Liquidsoap source,
let's see why Beets queries are interesting for a radio.

## Beets queries

Queries are parameters that you usually provide to the `beet ls` command :
Beets will find matching tracks.
The `random` plug-in works the same, except that it returns only one track matching the query
(see [the plug-in's documentation](https://beets.readthedocs.io/en/stable/plugins/random.html)).
Once your library is imported,
you can try the following queries on the command line
by typing `beet ls [query]` or `beet random [query]`.
To test quickly, add the `-t 60` option to `beet random`
so it will select an hour worth of tracks matching your query.

Without selectors, queries search in a track’s title, artist, album name,
album artist, genre and comments. Typing an artist name or a complete title
usually match the exact track, and you could do a lovely playlist just by querying `love`.

But in a radio you'll usually query on other fields.
You can select tracks by genre with the `genre:` selector.
Be careful that `genre:Rock` also matches `Indie Rock`, `Punk Rock`, etc.
To select songs having english lyrics, use `language:eng`.
Or pick 80s songs with `year:1980..1990`.

Beets also holds internal meta-data, like `added`:
the date and time when you imported each song.
You can use it to query tracks inserted over the past month with `added:-1m..`.
Or you can query track imported more than a year ago with `added:..-1y`.
Beets also lets you
[set your own tags](https://beets.readthedocs.io/en/stable/guides/advanced.html#store-any-data-you-like).

You can use the `info` plug-in to see everything Beets knows about title(s) matching a query
by typing `beet info -l [query]`.
See also [the Beets' documentation](https://beets.readthedocs.io/en/stable/reference/query.html)
for more details on queries operators.
All these options should allow you to create both general and specialiazed Liquidsoap sources.

## A source querying each next track from Beets

As of Liquidsoap 2.x we can create a function that creates a dynamic source,
given its `id` and a Beet query.
We rely on `request.dynamic` to call `beet random`
(with `-f '$path'` option so beets only returns the matching track's path)
every time the source must prepare a new track:

```liquidsoap
def beets(id, query) =
  beets_src =
    request.dynamic(id=id, retry_delay=1., {
      request.create(
        string.trim(
          process.read("#{BEET} random -f '$path' #{query}")
        )
      )
    })
  (beets_src:source)
end

all_music = beets("all_music", "")
recent_music = beets("recent_music", "added:-1m..")
rock_music = beets("rock_music", "genre:Rock")
```

Note that

- `query` can be empty, it will match all tracks in the library.
- we set `retry_delay` to a second, to avoid looping on `beet` calls if something goes wrong.
- The final type hint (`:source`) will avoid false typing errors when the source is integrated in complex operators.

## Applying ReplayGain

When the [`replaygain` plug-in](https://beets.readthedocs.io/en/stable/plugins/replaygain.html)
is enabled, all tracks will have an additional metadata field called `replaygain_track_gain`.
Check that Beet is configured to
[write ID3 tags](https://beets.readthedocs.io/en/stable/reference/config.html#importer-options)
so Liquidsoap will be able to read this metadata -
your Beet configuration should include something like:

```
import:
    write: yes
```

Then we only need to add `amplify` to our source creation function. In the example below we also add `blank.eat`, to automatically cut silence at the beginning or end of tracks.

```liquidsoap
def beets(id, query) =
  beets_src =
    blank.eat(id="#{id}_", start_blank=true, max_blank=1.0, threshold=-45.0,
      amplify(override="replaygain_track_gain", 1.0,
        request.dynamic(id=id, retry_delay=1., {
          request.create(
            string.trim(
              process.read("#{BEET} random -f '$path' #{query}")
            )
          )
        })
      )
    )
  (beets_src:source)
end
```

This is the recommended Beets integration ;
such source will provide music continuously,
at a regular volume.

## Beets as a requests protocol

If you're queueing tracks with `request.queue`,
you may prefer to integrate Beets as a protocol.
In that case,
the list of paths returned by `beet random -f '$path'` fits directly
what's needed by protocol resolution:

```liquidsoap
def beets_protocol(~rlog,~maxtime,arg) =
  timeout = maxtime - time()
  command = "#{BEET} random -f '$path' #{arg}"
  p = process.run(timeout=timeout, command)
  if p.status == "exit" and p.status.code == 0 then
    [string.trim(p.stdout)]
  else
    rlog("Failed to execute #{command}: #{p.status} (#{p.status.code}) #{p.stderr}")
    []
  end
end
protocol.add("beets", beets_protocol,
  syntax = "same arguments as beet's random module, see https://beets.readthedocs.io/en/stable/reference/query.html"
)
```

Once this is done,
you can push a beets query from [the telnet server](server.html):
if you created `request.queue(id="userrequested")`,
the server command
`userrequested.push beets:All along the watchtower`
will push the Jimi Hendrix's song.

With this method, you can benefit from replay gain metadata too, by wrapping
the recipient queue in an `amplify` operator, like

```liquidsoap
userrequested = amplify(override="replaygain_track_gain", 1.0,
  request.queue(id="userrequested")
)
```
