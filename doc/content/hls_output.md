# HLS Output

HTTP Live Streaming (HLS) delivers a live stream as a series of small files
served by an ordinary web server. A player downloads a playlist, fetches the
files it lists one after the other, and reloads the playlist to learn about
new ones. Because every request is a plain HTTP download, HLS works through
caches, content delivery networks (CDNs) and firewalls, and it is the format
that browsers, phones and smart TVs play most readily.

HLS also lets a player choose between several versions of the same stream,
called _variants_: a phone on a weak connection picks a low bitrate, a desktop
on fibre picks the best quality, and both switch when conditions change.

Liquidsoap writes HLS with `output.file.hls`, which fills a directory that any
web server can serve, and `output.harbor.hls`, which serves that directory
itself.

## What the output writes

For each variant, the output writes:

- **Segments**: media files a few seconds long, named `<variant>_<n>.<ext>`
  by default.
- **Init segments**: for fragmented MP4 variants, a small `.mp4` file with the
  codec setup that every segment needs before it can be decoded.
- **A media playlist**, `<variant>.m3u8`, listing the most recent segments.

On top of these, the **main playlist** (`stream.m3u8` by default) lists every
variant with its bandwidth, codecs and resolution. This is the URL you give to
players.

Here is a minimal script with two audio variants:

```{.liquidsoap include="hls-basic.liq" from="BEGIN" to="END"}

```

Point a web server at `/var/www/hls` and give players the URL of
`stream.m3u8`. The output's `id` is the main playlist's file name, here
`stream.m3u8`, unless you set `id` yourself.

## Variants

### Describing variants

The second argument is a list of `(name, format)` pairs, one per variant. The
name is the base name of the variant's media playlist (`<name>.m3u8`) and of
its segments, so it must be non-empty and unique within the output.

Every variant encodes the same source. Within one output, all formats must
carry the same tracks: either every variant has video, or none does. To offer
an audio-only rendition of a video stream, use a second output.

Players choose a variant from what the main playlist announces about it. You
can set these values yourself by adding fields to the format:

```{.liquidsoap include="hls-variants.liq" from="BEGIN" to="END"}

```

| Field         | Type        | Effect                                                                                     |
| ------------- | ----------- | ------------------------------------------------------------------------------------------ |
| `bandwidth`   | `int`       | Announced bandwidth, in bits per second.                                                   |
| `codecs`      | `string`    | Announced codecs, such as `"mp4a.40.2"` or `"avc1.64001f,mp4a.40.2"`.                      |
| `extname`     | `string`    | Extension of the segment files.                                                            |
| `video_size`  | `int * int` | Announced resolution, width then height.                                                   |
| `id3`         | `bool`      | Carry track metadata as ID3, see [metadata](#metadata).                                    |
| `id3_version` | `int`       | ID3v2 version; default `3`.                                                                |
| `replay_id3`  | `bool`      | Repeat the current track's metadata in later segments; default `true`.                     |
| `extra_tags`  | `[string]`  | Playlist tags written at the top of this variant's media playlist, such as `#EXT-X-START`. |

### Accepted formats

HLS only defines a few segment formats, and a player that meets anything else
cannot play it. The output therefore accepts:

- `%mp3` and `%shine`, written as MP3 segments;
- `%fdkaac` with ADTS transport, its default;
- `%ffmpeg` with the `mpegts`, `mp4`, `adts`, `mp3`, `ac3`, `eac3` or `webvtt`
  muxer. `mp4`, `mov`, `ipod` and `ismv` are all written as fragmented MP4.

Any other format, such as `%wav`, `%ogg`, `%flac` or `%ffmpeg` with Matroska,
WebM or Ogg, is refused when the script starts, with an error naming the
variant.

MPEG-TS (`format="mpegts"`) plays almost everywhere and suits most streams.
Fragmented MP4 is the format Apple requires for HEVC video. Packed audio (`%mp3`, `%fdkaac`, `%ffmpeg` with `adts`) is the
lightest choice for radio.

With fragmented MP4, media segments use the `.m4s` extension and the init
segment keeps `.mp4`.

### What is announced by default

When a field is not given, the output works out the value itself, in this
order:

- **Bandwidth**: the encoder's bitrate, else the bitrate the format declares
  (`%mp3` constant or average bitrate, `%shine`, `%fdkaac`), else the highest
  bitrate measured over the segments in the playlist. Each of these gets 10%
  of headroom, because HLS announces a _peak_ bitrate and encoders only know an
  average one.
- **Codecs**: the codecs reported by the encoder for the segments in the
  playlist, else the codec string of the format (`mp4a.40.34` for MP3,
  `mp4a.40.2` for AAC-LC, `mp4a.40.5` for HE-AAC, and so on). When neither is
  known, no codecs are announced and a warning is logged.
- **Resolution**: the encoder's, else the format's video size.
- **Extension**: the format's file extension.

When the measured bitrate goes above the announced bandwidth, a warning names
the variant. A player trusting a bandwidth that is too low picks a variant it
cannot sustain, so raise `bandwidth` or lower the encoder's bitrate when you
see it.

The [`streams()` method](#inspecting-variants) shows the values each variant
announces.

## Segments

### Segment duration

`segment_duration` (default `10.`) sets how long segments are, in seconds.
Shorter segments bring players closer to live and let them switch variants
sooner; longer segments mean fewer requests and better compression. Values
between 2 and 6 seconds are common for live streams.

The playlists announce a _target duration_: `segment_duration` rounded to the
nearest second, and at least 1. No segment, once rounded to the nearest
second, is longer than it.

### One grid for every variant

A player switching variants expects segment _n_ of one variant to cover the
same time as segment _n_ of another. The output places segment boundaries on a
shared grid: multiples of `segment_duration` since the output started. Each
variant cuts as close to the grid as its content allows, so variants stay
aligned and never drift apart.

### Key frames

A video segment can only start on a key frame, so where cuts land depends on
the video:

- **Re-encoded video** (`%video(codec="libx264", ...)` and the like): the
  output asks the encoder for a key frame at every grid boundary. Segments are
  `segment_duration` long, to the frame.
- **Copied video** (`%video.copy`): the output cannot add key frames, so it
  cuts at the source's own key frame nearest to each boundary. Choose a source
  key-frame interval (GOP) that divides the segment duration: a 2 s GOP with
  6 s segments gives regular 6 s segments.

```{.liquidsoap include="hls-copy.liq" from="BEGIN" to="END"}

```

Audio-only variants can be cut anywhere, within one audio frame.

### Forced cuts

When no key frame comes in time and a segment would grow past the target
duration, the output asks the encoder to cut anyway and logs a warning naming
the variant and the time. A segment that starts this way may need earlier data
to decode, so its playlist stops announcing `#EXT-X-INDEPENDENT-SEGMENTS`
until the segment leaves it. If you see these warnings with copied video,
shorten the source's GOP or lengthen `segment_duration`.

`#EXT-X-INDEPENDENT-SEGMENTS` is announced only when every listed segment
starts with a key frame: it is also missing when the encoder cannot promise
it.

## Playlists

### The media playlist window

A live playlist lists only the latest segments. `segments` (default `10`) sets
how many. Players start a few segments from the end, so a longer window gives
them more room to absorb network trouble, at the cost of a playlist that
covers more past content. Apple recommends at least 6 segments; a lower value
is accepted with a warning.

The window always covers at least three target durations, as HLS requires: the
output keeps more than `segments` segments when some are short.

```{.liquidsoap include="hls-playlists.liq" from="BEGIN" to="END"}

```

### Keeping old segments on disk

A player may still be working through a playlist it downloaded a moment ago,
so a segment that leaves the window must stay on disk a little longer. The
output keeps it until the stream has moved on by the segment's length plus the
duration of the playlists that listed it.

`segments_overhead` (default `5`) keeps at least that many more segments
after they leave the window, which helps slow caches and players far behind
live. With `segments_overhead=null`, no segment is ever deleted while the
output runs, for instance to keep a recording.

### URIs and `prefix`

Playlists refer to files by their name relative to the output directory. To
serve segments from somewhere else, such as a CDN, set `prefix`: it is put in
front of every URI, followed by `/` if it does not end with one. The same
prefix applies to the media playlists listed in the main playlist.

### Extra tags

`extra_tags` adds playlist tags to the main playlist, and the variant field
`extra_tags` adds tags to the top of that variant's media playlist. Each entry
must be one tag line starting with `#`; anything else is refused when the
script starts.

### Discontinuities

When the output stops and starts again, for instance because a `fallible`
source went away for a while, the next segment is marked with
`#EXT-X-DISCONTINUITY`. Players then reset their decoders instead of mixing
timestamps from before and after the gap. Every variant gets the discontinuity
at the same place.

### The main playlist

The main playlist is written as soon as every variant has a segment, so every
media playlist it names exists. It is rewritten when a variant's bandwidth,
codecs or resolution changes.

By default it is written by `hls.playlist.main`, which you can also call
yourself. It writes one `#EXT-X-STREAM-INF` line per variant, for example:

```
#EXTM3U
#EXT-X-VERSION:6
#EXT-X-STREAM-INF:BANDWIDTH=140800,CODECS="mp4a.40.2"
aac.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=140800,CODECS="mp4a.40.34"
mp3.m3u8
```

To change it, pass your own `main_playlist_writer`. It receives the output's
`extra_tags`, `prefix` and playlist `version`, and the list of variant names,
each with `bandwidth`, `codecs` (empty when unknown) and, when known,
`video_size`. It returns the playlist's content, or `null` to leave the file
as it is. Here, variants are listed from highest to lowest bandwidth:

```{.liquidsoap include="hls-main-playlist.liq" from="BEGIN" to="END"}

```

With `main_playlist_writer=null`, no main playlist is written. This is useful
when another program builds it, or when players open a media playlist
directly.

## Metadata

### ID3 tags

Players show the current title from ID3 tags carried in the segments. How
they are carried depends on the format:

- **Packed audio** (`%mp3`, `%shine`, `%fdkaac`, `%ffmpeg` with `adts` and
  the like): every segment starts with an ID3 tag. It holds the segment's
  timestamp and, with `replay_id3=true`, the current track's metadata, so a
  player joining at any segment knows what is playing. With
  `replay_id3=false`, only the first segment after a change carries the
  metadata.
- **MPEG-TS**: metadata is sent as timed ID3 inside the stream, at the moment
  it changes. With `replay_id3=true`, it is repeated at the start of each
  segment for players that join later.

`id3=false` turns ID3 off for a variant, and `id3_version` picks the ID3v2
version. Only text metadata is sent: cover art and other binary entries are
left out.

```{.liquidsoap include="hls-metadata.liq" from="BEGIN" to="END"}

```

### Cutting on metadata

In packed audio, a track's metadata appears at the start of a segment, so a
new title can show up as much as one segment late. Cutting a new segment when
metadata arrives puts the title on time, at the cost of a shorter segment.

`cut_on_metadata` decides:

- `null` (default): cut when it keeps the variants aligned, that is when the
  output has one variant or every variant can be cut anywhere (audio-only
  variants). Metadata causes a cut only if some variant carries ID3; an
  inserted tag always does.
- `true`: always cut. With video variants, segment boundaries can then differ
  between variants.
- `false`: never cut. Metadata and tags wait for the next segment.

Metadata equal to what was last sent causes no cut.

### Inserting playlist tags

Some players and ad servers read tags from the media playlist, such as
`#EXT-X-CUE-OUT` for an advertisement break or `#EXT-X-DATERANGE` for a
program marker. `insert_tag` adds a tag in front of the next segment of every
variant. You can call it from any thread, for instance from a server command
or a scheduled function. Like metadata, an inserted tag may cut a new segment
so that it lands on time.

The tag must be a single playlist line starting with `#`. Anything else raises
an `error.invalid` error, which a script can catch with `try ... catch`, and
nothing is inserted.

### Inspecting variants

The `streams()` method returns one record per variant, read when it is
called: `name`, `encoder`, `bandwidth`, `codecs`, `extname`, `video_size`
(`{width, height}` or `null`), `id3_enabled`, `replay_id3`, `extra_tags`,
`discontinuity_count`, and `insert_tag` for that variant alone. A tag inserted
on one variant causes a cut only when the output has a single variant.

## Files

### Segment names

`segment_name` gives the file name of each segment. It receives a record with
`stream_name` (the variant), `position` (the segment's number), `extname`,
`duration` in seconds and `ticks`, and returns a name relative to the output
directory. The default is `"#{stream_name}_#{position}.#{extname}"`.

The name may contain `/`: missing sub-directories are created. Putting each
variant in its own directory keeps large outputs tidy:

```{.liquidsoap include="hls-files.liq" from="BEGIN" to="END"}

```

The function is called once per file, when the segment is complete, so
`duration` is its final length. It is also called for init segments, with
`duration` and `ticks` set to `0`. The name must stay inside the output
directory (relative, with no `..`), and must not be a playlist name or a file
the output already has. Otherwise the output fails: a naming function that
returns such a name is a script error that retrying would not fix.

### Permissions and temporary files

`perm` (default `0o666`) and `dir_perm` (default `0o777`) set the mode of
files and directories, as the process's umask allows.

Every file is first written as a temporary file, then renamed onto its final
name. A web server or a player reading the directory therefore only ever sees
complete files. Temporary files go to the output directory, or to `temp_dir`
when set. Keep `temp_dir` on the same file system as the output directory: on
another one, each file has to be copied into place, and a warning says so when
the script starts.

### Following file changes

To mirror the stream to a CDN or object storage, you need to know when files
appear and disappear. `on_file_change` calls a function with `{state, path}`,
where `path` is the output directory joined with the file's name:

- `created`: a new file is complete;
- `updated`: a file was replaced with complete new content (playlists, or a
  file left by an earlier run);
- `deleted`: the file is about to be removed.

For each file, `created` or `updated` comes first, then any `updated`, and
finally at most one `deleted`. Events cover segments, init segments, media
playlists and the main playlist, never temporary files. A segment's event
comes before the playlist that lists it, and a segment is deleted only after
the playlist that drops it is written.

A callback registered with `synchronous=true` runs in the streaming loop: an
error it raises ends the run like any other output error, and a slow one holds
up streaming. The example above runs each upload in its own thread, so
uploads may finish in a different order than their events.

### Stopping for good

When the script ends, or the output is shut down, without `persist_at`, the
output removes everything it wrote: the main playlist first, then the media playlists, then segments and
init segments, so that no player is sent to a missing file. Files you placed
in the directory yourself are left alone. With `persist_at`, every file is
kept for the next run.

## Restarts and errors

### Stop and start

Stopping the output with its `stop` method, or a `fallible` source that goes
away, keeps every file and the numbering. The next start continues the same
playlists after a discontinuity, so players keep following the stream.

### Keeping state across restarts

A new process normally starts numbering segments from 1 again. Players that
were following the stream then see it go back in time, and caches may serve
old segments with the same names. `persist_at` saves the playlists' state in a
file after every segment and at every stop. On the next start, the output
reads it back, reuses the segments still on disk, and continues the numbering
after a discontinuity.

A relative `persist_at` is relative to the output directory. A listed segment
whose file has gone missing is dropped with a warning, together with every
older one.

When the state file cannot be read back, the output logs a warning and starts
fresh. With `strict_persist=true` it fails instead, which suits setups where a
reset numbering would break something downstream.

```{.liquidsoap include="hls-persist.liq" from="BEGIN" to="END"}

```

### Errors while streaming

A disk that fills up or a network file system that goes away should not stop
the stream for good. When an error happens while streaming, the output drops
the segment it was writing, keeps everything already published, and calls
`reopen_on_error` with the error. The function returns how many seconds to
wait before starting again (default `2.`), or `null` (or a negative number) to
let the output fail. After the delay, the stream continues with a
discontinuity.

Some errors are not retried, because waiting would not fix them: an invalid
argument when the script starts, a `segment_name` result that is not a valid
name, and, with `strict_persist=true`, a state file that cannot be restored.
The output fails at once.

## Serving with `output.harbor.hls`

### Serving over HTTP

`output.harbor.hls` takes the same arguments as `output.file.hls`, without the
directory, and serves the stream from Liquidsoap's own HTTP server. No
separate web server is needed:

```{.liquidsoap include="hls-harbor.liq" from="BEGIN" to="END"}

```

Players open `http://<host>:8080/radio/stream.m3u8`. `port` defaults to
`8000` and `path` to `/`. The server answers `GET`, `HEAD` and `OPTIONS`.

Files go to `tmpdir`, or to a new temporary directory when it is `null`,
removed with its content when the process shuts down. Set
`tmpdir` to use `persist_at` with a relative path: otherwise the state lands
in a new directory at every run and can never be restored, and a warning says
so.

### Responses

Each file is sent with the content type players expect:

| File                    | Content type                                        |
| ----------------------- | --------------------------------------------------- |
| Playlists               | `application/vnd.apple.mpegurl`                     |
| MPEG-TS segments        | `video/mp2t`                                        |
| Fragmented MP4 segments | `video/iso.segment`                                 |
| Init segments           | `video/mp4`, or `audio/mp4` for audio-only variants |
| ADTS                    | `audio/aac`                                         |
| MP3                     | `audio/mpeg`                                        |
| AC-3 / E-AC-3           | `audio/ac3` / `audio/eac3`                          |
| WebVTT                  | `text/vtt`                                          |

Caching headers let proxies and browsers keep what they can:

- playlists are cached for half a target duration, so players always get a
  fresh one when they reload;
- segments and init segments are cached for as long as they stay available,
  with a matching `Expires` header, since their content never changes.

`headers` (default `[("Access-Control-Allow-Origin", "*")]`) are added to
every response, errors included, so browser players hosted on another site
can read the stream and see a missing file as a 404. A configured header
replaces a generated header of the same name: setting `Cache-Control` sets
your own cache policy.

### Other files in the directory

Besides the output's own files, the server sends any regular file you place
in the directory: encryption keys named by an `#EXT-X-KEY` tag, a player page,
images. Requests that try to leave the directory, the state file and
temporary files get a 404.

### HTTPS

To serve over HTTPS, pass an SSL transport, as with other harbor operators
(see [SSL / HTTPS](./harbor.md#ssl--https)):

```{.liquidsoap include="hls-harbor-https.liq" from="BEGIN" to="END"}

```

### Behind a CDN

A CDN in front of the server caches segments by name. If a new process starts
numbering again from 1, the CDN may serve segments from the previous run in
place of the new ones. Use `persist_at` (with `tmpdir`) so numbering
continues, or a `segment_name` that is unique across runs, for instance one
that includes the start time.

## See also

- [Encoding formats](./encoding_formats.md): the encoders and their options.
- [FFmpeg encoder](./ffmpeg_encoder.md): `%ffmpeg` formats, codecs and copy.
- [Video streams](./video.md): producing video with Liquidsoap.
- [Harbor input and output](./harbor.md): the HTTP server, ports and HTTPS.
