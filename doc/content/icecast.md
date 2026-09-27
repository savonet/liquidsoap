# Streaming to Icecast and Shoutcast

Icecast is the most common way to put a radio on the internet. Liquidsoap
generates and encodes the stream, sends it to an Icecast server, and the Icecast
server relays it to all your listeners. Each stream on the server lives at its
own _mount point_, for instance `/radio.mp3`, so a single Icecast server can
carry several streams. The [quick start](./quick_start.md) explains how
Liquidsoap, the streaming server and the listeners fit together.

Liquidsoap can also stream to Shoutcast servers, see [Shoutcast](#shoutcast)
below.

## Icecast output

Icecast outputs are done using the `output.icecast` operator. It takes an
encoding format, the source to stream, and the connection details of the
server:

```{.liquidsoap include="icecast-output-basic.liq"}

```

The main parameters are:

- `host` and `port`: the address of the Icecast server. The defaults are
  `localhost` and `8000`.
- `password`: the source password, set in the `<source-password>` entry of your
  `icecast.xml`. The default is `hackme`: change it on any public server.
- `user`: the source user, `source` by default. Set it when your server uses
  per-mount-point users.
- `mount`: the mount point of the stream. This parameter is mandatory.
- the encoding format, for instance `%mp3`, `%vorbis`, `%opus`, `%fdkaac` or
  `%ffmpeg(...)`. The [encoding formats](./encoding_formats.md) page lists all of
  them. The `format` parameter sets the stream's content type, for instance
  `"audio/mpeg"`. When `format` is empty, Liquidsoap guesses the content type
  from the encoder.
- `name`, `description`, `genre` and `url`: information about the stream shown
  by the Icecast server and by directory services. `public` tells the Icecast
  server whether the stream may be listed in public directories.

As usual, `liquidsoap -h output.icecast` gives you the full list of options for
this operator.

### Fallible sources

By default, `output.icecast` expects a source that is always available. This is
why the example above wraps the playlist in `mksafe`. If you want the output to
stop streaming when the source has nothing to play, set `fallible=true`: the
output stops when its source fails and starts again when the source becomes
available.

### Connection and reconnection

When the connection to the Icecast server fails, or when the server closes it,
Liquidsoap logs the error and tries to reconnect after 3 seconds. It keeps
trying for as long as the script runs. The `connection_timeout` parameter sets
how long Liquidsoap waits when establishing the connection (5 seconds by
default) and `timeout` sets how long it waits on reads and writes (30 seconds
by default).

The output has callback methods to react to the connection state:

- `on_connect` runs when the connection is established.
- `on_disconnect` runs when the connection stops.
- `on_error` runs when an error happens. It receives the error and a
  `restart_in` function. Call `restart_in(delay)` to reconnect after `delay`
  seconds, `restart_in(null)` to stop reconnecting, or pass a negative delay to
  raise the error. Only one `on_error` handler is active at a
  time: a new registration replaces the previous one.

For instance, the following script logs connection changes and waits 10
seconds before each reconnection:

```{.liquidsoap include="icecast-output-callbacks.liq"}

```

When Liquidsoap connects, it sends the last metadata of the source, so that
listeners see the current song right away. Set
`send_last_metadata_on_connect=false` to disable this.

## Protocols

`output.icecast` talks to the server using the HTTP protocol of Icecast 2. The
`method` parameter selects the HTTP request used to send the stream: `"source"`
(the default), `"put"` or `"post"`. Recent Icecast versions accept `"put"`. Set
`chunked=true` to use chunked transfer encoding.

Shoutcast servers use the older ICY protocol. `output.shoutcast` uses it, see
[Shoutcast](#shoutcast) below.

### HTTPS

To send the stream over an encrypted connection, pass an SSL or TLS transport
to the `transport` parameter. `http.transport.ssl` is available when Liquidsoap
is compiled with `libssl`, and `http.transport.tls` when it is compiled with
`ocaml-tls`:

```{.liquidsoap include="icecast-output-https.liq"}

```

The Icecast server must listen for TLS connections on that port.

## ICY metadata

_ICY metadata_ is the name for the mechanism used to update
metadata in icecast's source streams.
The technique is primarily intended for data formats that do not support in-stream
metadata, such as mp3 or AAC. However, it appears that icecast also supports
ICY metadata update for ogg/vorbis streams.

When using the ICY metadata update mechanism, new metadata are submitted separately from
the stream's data, via an HTTP GET request. The format of the request depends on the
protocol you are using (ICY for shoutcast or HTTP for icecast 2).

Formats using the ogg container, such as `%vorbis` or `%opus`, carry their
metadata inside the stream. Liquidsoap inserts the metadata in the encoded data
and listeners receive it along with the audio.

You can do several interesting things with ICY metadata updates
in liquidsoap. We list some of those here.

### Enable/disable ICY metadata updates

You can enable or disable icy metadata update in `output.icecast`
by setting the `send_icy_metadata` parameter to `null`, `true` or `false`. The default value is `null` and does the following:

- Set `true` for: mp3, aac, aac+, wav, flac
- Set `false` for any format using the ogg container

In some cases, `liquidsoap` might not be able to detect if
ICY metadata need to be enabled, in which case it will ask you
to set a `true` or `false` value for this parameter.

### Choosing the metadata sent

The `icy_metadata` parameter lists the metadata fields sent with each ICY
update. The default list is `song`, `title`, `artist`, `genre`, `date`,
`album`, `tracknum`, `comment`, `dj` and `next`. Only the fields in this list
are sent.

The `encoding` parameter sets the character encoding used to send metadata and
the stream information (`name`, `genre` and `description`). The default is
UTF-8. Some older servers and players expect `ISO-8859-1`.

### `song` metadata

Most Icecast listeners expect a `song` metadata to be generated. This metadata
should combine both artist and title metadata and will be displayed preferably.

We provide a default implementation that returns `artist` or `title` metadata
when only one of these two is available and `$(artist) - $(title)` otherwise.

You can use the `icy_song` parameter to use your own implementation. Returning
`null` from that function disables the metadata altogether.

The following example sends a shorter list of fields, builds `song` as
`artist: title`, and uses `ISO-8859-1`:

```{.liquidsoap include="icecast-output-icy.liq"}

```

### Update metadata manually

The function `icy.update_metadata` implements a manual metadata update
using the ICY mechanism. It can be used independently from the `send_icy_metadata`
parameter described above, provided icecast supports ICY metadata for the intended stream.

For instance the following script registers a telnet command named `metadata.update`
that can be used to manually update metadata:

```{.liquidsoap include="icy-update.liq"}

```

As usual, `liquidsoap -h icy.update_metadata` lists all the arguments
of the function.

## Shoutcast

Although Liquidsoap is primarily aimed at streaming to Icecast servers (which provide
many more features than Shoutcast), it is also able to stream to Shoutcast.

Shoutcast servers accept streams encoded with the MP3 or AAC/AAC+ codec. You need to compile Liquidsoap with
`lame` or FFmpeg support, so it can encode in MP3. Liquidsoap also has support for AAC+ encoding
using FDK-AAC or using an [external encoder](./external_programs.md#external-encoders). The recommended format is MP3.

Shoutcast outputs are done using the `output.shoutcast` operator with the appropriate parameters.
An example is:

```{.liquidsoap include="shoutcast.liq"}

```

`output.shoutcast` takes the same parameters as `output.icecast`, except
`mount`, `description` and `method`. Shoutcast v2 servers can carry several
streams: select the stream with the `icy_id` parameter (1 by default). The
`dj` parameter adds a `dj` metadata field to the stream, and `aim`, `icq` and
`irc` fill in the matching contact fields of the stream.

As usual, `liquidsoap -h output.shoutcast` gives you the full list of options for this operator.

### Shoutcast as relay

A side note for those of you who feel they "need" to use Shoutcast for
non-technical reasons (such as their stream directory service...): you can still
stream to an Icecast server with all its features, and then relay the Icecast
stream through a Shoutcast server. In the Shoutcast v2 server configuration, set
the relay URL of the stream to the full address of the Icecast mount point, for
instance `http://icecast.example.org:8000/radio.mp3`.

## Multiple outputs

A single source can feed as many outputs as you want. A common setup streams
the same radio in several formats, each on its own mount point, so that every
listener can pick one that suits their player and bandwidth:

```{.liquidsoap include="icecast-output-multiple.liq"}

```

Each output encodes the source separately. Liquidsoap computes the source once
and shares it between the outputs.

## Related pages

- To run an Icecast-compatible server inside Liquidsoap, and serve your streams
  to listeners without a separate Icecast server, see
  [Icecast server](./icecast_server.md).
- To stream with HLS, see [HLS output](./hls_output.md).
