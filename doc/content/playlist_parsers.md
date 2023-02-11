# Playlist parsers

Liquidsoap supports various playlists formats. Those formats can be used
for `playlist` sources, `input.http` streams and manually using `request.create`.

## Supported formats

Most supported playlists format are _strict_, which means that the decoder can be sure
that is has found a correct playlist for that format. Some other format, such as `m3u`,
however, may cause _false positive_ detections.

All formats are identified by their _mime-type_ or _content-type_. Supported formats are the following:

- Text formats:

  - `audio/x-scpls`: [PLS format](http://en.wikipedia.org/wiki/PLS_%28file_format%29), **strict**
  - `application/x-cue`: [CUE format](http://en.wikipedia.org/wiki/.cue), **strict**. This format's usage is described below.
  - `audio/x-mpegurl`, `audio/mpegurl`: [M3U](http://en.wikipedia.org/wiki/M3u), **non strict**

- Xml formats:
  - `video/x-ms-asf`, `audio/x-ms-asx`: [ASX](http://en.wikipedia.org/wiki/Advanced_Stream_Redirector), **strict**
  - `application/smil`, `application/smil+xml`, [SMIL](http://en.wikipedia.org/wiki/Synchronized_Multimedia_Integration_Language), **strict**
  - `application/xspf+xml`, [XSPF](http://en.wikipedia.org/wiki/Xspf), **strict**
  - `application/rss+xml`, [Podcast](http://en.wikipedia.org/wiki/Podcast), **strict**

## Usage

Playlist files are parsed automatically when used in a `playlist` or `input.http` operator. Each of
these two operators has specific options to specify how to pick up a track from the playlist, _e.g._
pick a random track, the first one etc.

Additionally, you can also manually parse and process a playlist using `request.create` and `request.resolve`
and some programming magic. You can check the code source for `playlist.reloadable` in our standard library
for a detailed example.

## Special case: CUE format

The CUE format originates from CD burning programs. They describe the set of tracks of a whole CD and
are accompanied by a single file containing audio data for the whole CD.

This playlist format can be used in liquidsoap, using a `cue_cut` operator. By default, the CUE playlist
parser will add metadata from cue-in and cue-out points for each track described in the playlist, which
you can then pass to `cue_cut` to play each track of the playlist. Something like:

```liquidsoap
cue_cut(playlist("/path/to/file.cue"))
```

You can find an example of using `cue_cut` with cue sheets [here](split-cue.html) and a throughout
explanation of how seeking in liquidsoap works [there](seek.html).

The metadata added for cue-in and cue-out positions can be customized using the following
configuration keys:

```liquidsoap
settings.playlists.cue_in_metadata := "liq_cue_in"
settings.playlists.cue_out_metadata := "liq_cue_out"
```
