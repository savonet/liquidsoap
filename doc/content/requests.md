# Requests

When you ask Liquidsoap to play a file or a stream, Liquidsoap first prepares it
through a process called _resolution_. This process lets Liquidsoap handle
remote files, files stored in a database, generated speech and many other kinds
of media.

This page explains how **requests** work, how Liquidsoap resolves them with
**protocols**, how to write your own protocols, and which sources play
requests.

## What is a request?

A **request** is an object describing something to play. Liquidsoap resolves
the request to find the corresponding media and a way to decode it.

You create a request with [`request.create`](./reference.md#requestcreate),
passing a **URI**.

A URI can be:

- A file path: `"/music/song.mp3"`
- A URL: `"https://example.com/song.ogg"`
- A URI using a custom protocol: `"media:12345"`

A request is played by a [request-based source](#request-based-sources) such
as `playlist`, `request.dynamic` or `single`. The source resolves and decodes
the request before playing it.

## The request lifecycle

A request goes through the following steps, from URI to playback:

1. **Request creation**: you pass a URI to Liquidsoap.
2. **Protocol resolution**: Liquidsoap finds the protocol that handles the URI.
3. **Chained resolution**: some protocols return another URI, which is resolved
   in turn.
4. **Local file reached**: the resolution process ends with a file on disk.
5. **Decoder selection**: Liquidsoap looks for a decoder that can read the file.
6. **Playback**: the source plays the decoded content.

**Note:** a request can resolve to a local file and still be unplayable, for
example when the file's content does not match what the source expects.

## External decoders

Once a local file is available, Liquidsoap checks whether an **external
decoder** is registered for its MIME type or file extension. You can register
external decoders in your script with `decoder.add` to support more file
formats.

An external decoder reads the local file and produces another file that
Liquidsoap can play.

## Protocols: how URIs are resolved

A **protocol** tells Liquidsoap how to resolve a particular kind of URI. The
request can point to a local file, a remote URL, a database entry, or something
generated on the fly.

The general format is:

```
protocol_name:arguments
```

For example:

```text
http://www.example.com/song.mp3
say:Hello world!
s3://my-bucket/path/to/file.mp3
```

In each case, the prefix before the `:` is the protocol, and the part after it
is the argument passed to the protocol's resolver.

When a request's URI starts with a known protocol name:

- The protocol returns either another URI or a local file.
- If the protocol returns another URI, resolution continues with that URI.
- Resolution repeats until Liquidsoap reaches a local file.

For example, the request

```
annotate:title="My Song":http://example.com/song.mp3
```

is resolved as follows:

1. The `annotate` protocol adds the `title` metadata.
2. The `http` protocol downloads the file.
3. Liquidsoap now has a local file and looks for a decoder.

### Built-in protocols

Liquidsoap ships with many protocols, most of them written in the Liquidsoap
scripting language. The [protocols reference](./protocols.md) lists all of
them. Some commonly used ones:

- **`annotate`**: attaches metadata to a request, see
  [below](#the-annotate-protocol).
- **`autocue`**: computes cue points and fade durations for transitions. See the
  [crossfade](./crossfade.md) documentation for details.
- **`http` / `https`**: downloads remote files before playback.
- **`say`**: generates speech from text using a text-to-speech program, for
  instance to announce the next track.
- **`s3`**: downloads files from Amazon S3 using the AWS command-line tool.

### The `annotate:` protocol

The built-in `annotate:` protocol attaches metadata to a request. You can use it
to set metadata on files that you cannot or do not want to modify.

The syntax is:

```
annotate:key1="value1",key2="value2":next_uri
```

For example:

```
annotate:artist="Liquid Artist",liq_cue_in="3.",liq_cue_out="23.":/music/song.mp3
```

The `liq_cue_in` and `liq_cue_out` metadata in this example are
[cue points](#cue-points).

## Writing your own protocol

You can define your own protocols with `protocol.add`. For example, a `media:`
protocol can look up record `123` in your database when it resolves
`media:123`, and return the path of the corresponding file. This way, your
playlists and queues can refer to tracks by their database identifier.

### The anatomy of a protocol

A protocol is defined by a resolver function. The resolver receives the
protocol argument and returns the resolved URI, or `null` when resolution fails.
The returned URI can itself use another protocol, which Liquidsoap resolves in
turn.

The resolver also receives two labeled arguments:

- `~rlog`: a logging function. Messages logged with `rlog` are attached to the
  request.
- `~maxtime`: a UNIX timestamp. The resolver should be done by that time.

### The `process.uri` helper

Many protocols run an external command. The `process.uri` function builds a URI
of the form:

```text
process:<extname>,<command>[:<uri>]
```

When Liquidsoap resolves such a URI, it executes the command and kills it if it
is still running at `~maxtime`.

If you pass a `uri` argument, Liquidsoap first resolves that URI to a local file.
The command can use two placeholders:

- `$(input)`: the local file resolved from the `uri` argument, when `uri` is
  given.
- `$(output)`: the path to a temporary file with the extension given by the
  `extname` argument.

Both placeholders are replaced with quoted file names. Liquidsoap creates the
output file, empty, before the command runs, so the command must be able to
overwrite it. Liquidsoap deletes the output file once the request is done
with it.

### Example 1: fetching from S3

Liquidsoap already provides the `s3:` protocol. Here is a simplified version of
its definition:

```liquidsoap
def s3_protocol(~rlog:_, ~maxtime:_, arg) =
  extname = file.extension(leading_dot=false, dir_sep="/", arg)
  process.uri(
    extname=extname,
    "aws s3 cp #{process.quote("s3:#{arg}")} $(output)"
  )
end

protocol.add(
  "s3",
  s3_protocol,
  doc="Fetch files from S3 using the AWS CLI",
  syntax="s3://bucket/path/to/file"
)
```

A request such as `s3://my-bucket/song.mp3` is then downloaded to a temporary
file, which Liquidsoap plays.

### Example 2: database lookup

Suppose that your database stores the path of each file, keyed by a track
number. The following protocol looks up the path:

```liquidsoap
def db_lookup_protocol(~rlog, ~maxtime, arg) =
  id = string.to_int(default=-1, arg)
  if id < 0 then
    rlog("Invalid track id: #{arg}")
    null
  else
    string.trim(
      process.read(
        timeout=maxtime - time(),
        "psql -t -c 'SELECT path FROM tracks WHERE id=#{id};'"
      )
    )
  end
end

protocol.add(
  "db_lookup",
  db_lookup_protocol,
  doc="Fetch file path from database by track ID"
)
```

The request `db_lookup:42` now resolves to the path stored for track `42`.
Converting the argument to an integer before putting it in the SQL query
protects the query against injection.

### Example 3: preprocessing

A protocol can also transform a file. The following protocol normalizes a file
with the `normalize-audio` program:

```liquidsoap
def normalize_protocol(~rlog:_, ~maxtime:_, arg) =
  process.uri(extname="wav", uri=arg, "normalize-audio $(input) $(output)")
end

protocol.add(
  "normalize",
  normalize_protocol,
  doc="Normalize audio levels before playback"
)
```

### Chaining protocols

Each protocol resolves to a URI, which can use another protocol. You can
therefore chain protocols:

```text
normalize:db_lookup:42
```

Here, `db_lookup` returns the path of track `42` and `normalize` produces a
normalized copy of it, which Liquidsoap plays.

### Tips for writing protocols

- Stop resolution by `~maxtime`, for instance by passing
  `timeout=maxtime - time()` to the functions that you call.
- Use `~rlog` to log what the resolver does:

  ```liquidsoap
  rlog("Downloading from S3: #{arg}")
  ```

- Quote or validate `arg` before putting it into a shell command, for instance
  with `process.quote`.
- Test each protocol on its own before chaining it with others.

## Cue points

Cue points are useful with tracks whose intros or outros have been identified
in advance. Two metadata keys control which part of a file is played:

- `liq_cue_in`: start playback at position _N_ seconds.
- `liq_cue_out`: stop playback at position _N_ seconds, counted from the start of
  the file.

In the [`annotate:` example](#the-annotate-protocol) above:

- `liq_cue_in="3."` skips the first 3 seconds.
- `liq_cue_out="23."` stops playback at second 23, so 20 seconds are played.

The decoder reads these metadata when it opens the file, so they must be set on
the request, for instance with `annotate:`. Metadata added later in the
chain, for instance with `metadata.map`, are ignored by the decoder.

The `prefix` parameter of `playlist` applies cue points to every track. The
following source cues in at 10 seconds and cues out at 45 seconds on all its
tracks:

```liquidsoap
s = playlist(prefix="annotate:liq_cue_in=\"10.\",liq_cue_out=\"45.\":",
             "/path/to/music")
```

Cue points also work well with `request.dynamic`: an external script can build
up the appropriate URI, including cue points, based on information from your
own scheduling back-end.

The names of these metadata can be changed with the `cue_in_metadata` and
`cue_out_metadata` arguments of `request.create`.

## Request-based sources

Playing files is the most common way to build an audio stream.
In liquidsoap, files are accessed through requests,
which combine the retrieval of a possibly remote file, and its
decoding.

Liquidsoap provides several operators for playing requests:
`single`, `playlist`, `request.dynamic` and `request.queue`.
In a few cases (such as `single` with a local file)
a request operator will know
that it can always get a ready request instantaneously.
It will then be [infallible](./sources.md).
Otherwise, it will have a queue of requests ready
to be played (local files with a valid content), and will
feed this queue in the background.
This process is described here.

### Common parameters

Queued request sources prepare requests in advance: they resolve requests in
the background and keep them in a queue, so that a new track is ready when the
current one ends.

The `prefetch` parameter sets how many requests are queued in advance. It
defaults to `settings.request.prefetch`, which is `1`. Increasing it prepares
things more in advance, which is good when resolution is long (_e.g._, heavily
loaded server, remote files), and helps to ensure that a song will be ready in
case of skip.

The `timeout` parameter sets the maximum time allowed to resolve a request. It
defaults to `settings.request.timeout`.

### Request.dynamic

This source takes a custom function for creating its new requests.
This function, of type `() -> request?`,
can for example call an external program.
When the function returns `null`, the source retries after `retry_delay`
seconds.

To create the request, the function will have
to use the `request.create` function, which takes
the initial URI of the request as argument.
This URI is resolved to get an audio file
(see [protocols](#protocols-how-uris-are-resolved) above).

An example that takes the output of an external script as an URI
to create a new request can be:

```liquidsoap
def my_request_function() =
  # Get the first line of my external process
  result =
    list.hd(default="", process.read.lines("my_script my_params"))
  # Create and return a request using this result
  if result == "" then null else request.create(result) end
end

# Create the source
s = request.dynamic(my_request_function)
```

### Queues

Liquidsoap features the `request.queue` source, which provides a request queue
that can be directly manipulated by the user, via the server interface
or from the script.

The operator actually deals with two queues: _primary_ and _secondary_ queues.
The secondary queue is user-controlled.
The primary queue is the one that all queued request sources have,
its behavior is the same as described above, and it cannot be changed
in any way by the user.
Requests added to the secondary queue sit there until
the feeding process gets them and attempts to prepare them
and put them in the primary queue.
You can set how many requests will be in that primary queue
with the `prefetch` parameter.

When the `interactive` parameter is `true` (the default), the queue is
controlled via the [command server](./server.md).
It features commands for pushing new requests (`push`), looking up the queue
(`queue`), removing requests (`remove`), skipping the current track (`skip`)
and flushing the queue (`flush_and_skip`).
In a script, use the `push` method of the source to add a request, and the
`queue` and `remove` methods to inspect and edit the queue.
