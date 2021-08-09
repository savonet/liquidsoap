Integrating a music library: an example with Beets
==================================================

Liquidsoap's native sources can read from files and folders,
but if your radio uses an important music library
(more than a thousand tracks)
sorting this library by folders may not be enough.
In that case you would better maintain a music library
queried by Liquidsoap.
In this section we'll do this with [Beets](http://beets.io/).
Beets holds your music catalog,
cleans tracks' tags before importing,
and most importantly has a command-line interface we can leverage from Liquidsoap.

The following examples may also inspire you to integrate another library program or your own scripts.
If you're going with Beets,
you'll need an installation having the `random`
[plug-in enabled](https://beets.readthedocs.io/en/stable/plugins/index.html#using-plugins).
We'll examine how you can pick Beets tracks (local files) from Liquidsoap,
using 3 techniques fitting different Liquidsoap sources :

 * The most intuitive is an infite track source,
   that queries Beets for each track via `request.dynamic.list`.
 * We can also add Beets as a protocol so you can query beets from [requests](requests.html)
 * Finally we use Beets to generate a file suitable for `playlist`.

Before that, let's see Beets queries that are the most interesting for a radio.

Beet queries
------------

Queries are parameters that you usually append to the `beet ls` command :
Beets will use them to find matching tracks.
The `random` plug-in works the same, except that it returns only one track matching the query
(see [the plug-in's documentation](https://beets.readthedocs.io/en/stable/plugins/random.html)).
Once your library is imported,
you can try the following queries on the command line
by typing `beet ls [query]` or `beet random [query]`.
To test quickly, add the `-t 60` option to `beet random`
so it will select an hour worth of tracks matching your query.

Without selectors, queries search in a track’s title, artist, album name,
album artist, genre and comments. Typing an artist name or a complete title
usually match the exact track, by you could do a lovely playlist just by querying `love`.

But in a radio you'll usually query on other fields.
You can select tracks by genre with the `genre:` selector.
Be careful that `genre:Rock` also matches `Indie Rock`, `Punk Rock`, etc.
To select songs having english lyrics, use `language:eng`.
Or pick 80s songs with `year:1980..1990`.

Beets also holds internal meta-data, like `added` :
the date and time when you imported each song.
You can use it to query tracks inserted over the past month with `added:-1m..`.
Or you can query track imported more than a year ago with `added:..-1y`.
Beets also lets you 
[set your own tags](https://beets.readthedocs.io/en/stable/guides/advanced.html#store-any-data-you-like).

You can use the `info` plug-in to see everything Beets knows about title(s) matching a query
by typing `beet info -l [query]`.
See also [the Beets' documentation](https://beets.readthedocs.io/en/stable/reference/query.html)
for more details on queries operators.
All these options should allow you to create both general and specialiazed sources.

To use the following examples,
replace the `beet` path by the complete path on your own installation (on UNIX systems, find it with `which beet`).
We also always add the `-f '$path'` option,
so beets only returns the matching track's path.


A source querying each next track from Beets
--------------------------------------------

As of Liquidsoap 1.4.2 we can use the `request.dynamic.list`
to create a source calling Beets every time it needs to prepare its the next track:

```liquidsoap
def beets() =
  list.map(fun(item) -> request.create(item),
    process.read.lines(
      "/home/me/path/to/beet random -f '$path' my_own_query"
    )
  )
end

from_beets = request.dynamic.list(beets)
```

`process.read.lines` returns the command's output as a list of strings.
Each item in this list (so, each line returned by `beet random`)
is processed (`list.map`) by an anonymous function that turns it into a `request`.
Thus our function returns a list of requests,
as needed by `request.dynamic.list`. 

A more re-usable implementation
would store `beet`'s path in a constant and make a function returning a function,
so you can re-use it for all queries to Beets:

```liquidsoap
BEET = "/home/me/path/to/beet"

def beets(arg="") =
  fun() -> list.map(fun(item) -> request.create(item),
    process.read.lines(
      "#{BEET} random -f '$path' #{arg}"
    )
  )
end

music = request.dynamic.list(beets())
recent_music = request.dynamic.list(beets("added:-1m.."))
rock_music = request.dynamic.list(beets("genre:Rock"))
```


Beets as a requests protocol
----------------------------

If you're queueing tracks with `request.equeue`,
you may prefer to integrate Beets as a protocol.
In that case,
the list of paths returned by `beet random -f '$path'` fits directly
what's needed by protocol resolution:

```liquidsoap
def beets_protocol(~rlog,~maxtime,arg) =
  process.read.lines(
    "/home/me/path/to/beet random -f '$path' #{arg}"
  )
end
```

Then declare the `beets` protocol with

```liquidsoap
add_protocol("beets", beets_protocol,
  syntax = "Beets queries, see https://beets.readthedocs.io/en/stable/reference/query.html"
)
```

Once this is done,
you can push a beets query from [the telnet server](server.html) :
if you created `request.equeue(id="userrequested")`,
the server command 
`userrequested.push beets:All along the watchtower`
will push the Jimi Hendrix's song.


Generate playlists with Beets
-----------------------------

For Liquidsoap versions prior to 1.4.2,
or if you don't want to call Beets before every track,
you can ask `beet random` to return a track list and save this to a temporary file
suitable for `playlist` sources.
Note that this example uses a redirection that is only supported by UNIX systems.
Also it's hacky and error-prone, we just leave it here as an example.

To do this we will again integrate Beets as a protocol ;
the difference with the above protocol resolution is that our function will
only return the path to the playlist's temporary file.
To use both protocols,
be careful to give different protocol names (the first argument in `add_protocol`).

```liquidsoap
def beets_protocol(~rlog,~maxtime,arg) =
  tmp_playlist = file.temp("beetsplaylist", ".m3u8")
  ignore(process.read.lines(
    "/home/me/path/to/beet random -f '$path' #{arg} > #{tmp_playlist}"
  ))
  [tmp_playlist]
end

add_protocol("beets", beets_protocol)
```

Before cleaning files created by `file.temp`:
`beet random` returns only one track matching the query, remember ?
We can add `-t 60` to the query,
so it will return at most one hour of music matching the query.
Sources would look like:

```liquidsoap
music = playlist("beets:-t 60", mode="normal", reload=3600)
recent_music = playlist("beets:-t 60 added:-1m..", mode="normal", reload=3600)
rock_music = playlist("beets:-t 60 genre:Rock", mode="normal", reload=3600)
```

We use `mode="normal"` because we don't need Liquidsoap to re-randomize.
`reload=3600` will cause Liquidsoap to ask Beets for a new playlist every hour.

If you're playing from only one `playlist`,
be careful that `beet random -t 60` returns a list of 60 minutes _at most_
but it's usually less : it's likely that the first song will play again at the end of the hour.
Also Beets tends to fill the last minutes with short songs,
therefore short songs are picked more frequently than others.
You can mitigate these two problems by reloading before the playlist end,
by increasing `60` decreasing `3600`.

Another downside of this technique is that it creates a different playlist file
each time we reload the playlist.
After a while, your `/tmp` will be filled of files like `beetsplaylistXXXX123.m3u8`.
On UNIX we can add a cleanup function that regularly calls `find -delete`:

```liquidsoap
exec_at(freq=3600., pred={ true },
  fun () -> list.iter(fun(msg) -> log(msg, label="playlists_cleaner"),
    process.read.lines("find /tmp -iname beetsplaylist*m3u8 -mtime +0 -delete"),
  )
)
```
