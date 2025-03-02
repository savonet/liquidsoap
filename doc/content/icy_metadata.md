# ICY metadata

_ICY metadata_ is the name for the mechanism used to update
metadata in icecast's source streams.
The techniques is primarily intended for data formats that do not support in-stream
metadata, such as mp3 or AAC. However, it appears that icecast also supports
ICY metadata update for ogg/vorbis streams.

When using the ICY metadata update mechanism, new metadata are submitted separately from
the stream's data, via a http GET request. The format of the request depends on the
protocol you are using (ICY for shoutcast and icecast 1 or HTTP for icecast 2).

Starting with 1.0, you can do several interesting things with icy metadata updates
in liquidsoap. We list some of those here.

## Enable/disable ICY metadata updates

You can enable or disable icy metadata update in `output.icecast`
by setting the `send_icy_metadata` parameter to `null()`, `true` or `false`. The default value is `null()` and does the following:

- Set `true` for: mp3, aac, aac+, wav
- Set `false` for any format using the ogg container

In some cases, `liquidsoap` might not be able to detect if
ICY metadata need to be enabled, in which case it will ask you
to set a `true` or `false` value for this parameter.

## `song` metadata

Most Icecast listeners expect a `song` metadata to be generated. This metadata
should combine both artist and title metadata and will be played preferably.

We provide a default implementation that returns `artist` or `title` metadata
when only one of these two is available and `$(artist) - $(title)` otherwise.

You can use the `icy_song` parameter to use your own implementation. Returning
`null()` from that function disables the metadata altogether.

## Update metadata manually

The function `icy.update_metadata` implements a manual metadata update
using the ICY mechanism. It can be used independently from the `icy_metadata`
parameter described above, provided icecast supports ICY metadata for the intended stream.

For instance the following script registers a telnet command name `metadata.update`
that can be used to manually update metadata:

```{.liquidsoap include="icy-update.liq"}

```

As usual, `liquidsoap -h icy.update_metadata` lists all the arguments
of the function.
