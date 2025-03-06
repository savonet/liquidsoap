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
  - `application/smil+xml`, `application/smil+xml`, [SMIL](http://en.wikipedia.org/wiki/Synchronized_Multimedia_Integration_Language), **strict**
  - `application/xspf+xml`, [XSPF](http://en.wikipedia.org/wiki/Xspf), **strict**
  - `application/rss+xml`, [Podcast](http://en.wikipedia.org/wiki/Podcast), **strict**
 
Playlist format is driven by the **Content-Type** and **Content-Disposition** HTTP headers *(see m3u example below)*. You should make sure that your HTTP endpoint returns appropriate values for those. 

As last resort, you should be able to use the `settings.http.mime.extnames` settings to add or adjust support 
for your endpoint's mime-type if liquidsoap supports its corresponding playlist format. See for instance issue [#3451](https://github.com/savonet/liquidsoap/issues/3451).

## Usage

Playlist files are parsed automatically when used in a `playlist` or `input.http` operator. Each of
these two operators has specific options to specify how to pick up a track from the playlist, _e.g._
pick a random track, the first one etc.

Additionally, you can also manually parse and process a playlist using `request.create` and `request.resolve`
and some programming magic. You can check the code source for `playlist.reloadable` in our standard library
for a detailed example.

### Remote M3U playlist example
Here is an example of a m3u playlist being read from nodejs/express .

liquidsoap script:
```liquidsoap
#!/usr/local/bin/liquidsoap

p = playlist(reload=10, "http://localhost:8080/radio/playlists/0/playlist.m3u")
```

nodejs/express app:
```js
import express from "express";
const app = express();
app.get('/radio/playlists/:id/playlist.m3u', async (req, res) => {
  const playlist = ["/media/foo.mp3", "/media/bar.mp3"]

  // Liquidsoap will use the file extension from the `Content-Disposition` header to guess
  // the playlist format
  res.set(`Content-Disposition: attachment; filename="playlist-${req.params.id}.m3u`);
  
  // Otherwise, it will try to guess the file extension from the playlist mime-type.
  res.set('Content-Type', 'audio/x-mpegurl')
  res.send(playlist.join("\r\n")+"\r\n").status(200).end()
})
const server = app.listen(8080);
```

## Special case: CUE format

The CUE format originates from CD burning programs. They describe the set of tracks of a whole CD and
are accompanied by a single file containing audio data for the whole CD.

By default, the CUE playlist parser will add metadata from cue-in and cue-out points for each track described in the playlist, which
are automatically handled with source-base operators such as `playlist`.

The metadata added for cue-in and cue-out positions can be customized using the following
configuration keys:

```liquidsoap
settings.playlists.cue_in_metadata := "liq_cue_in"
settings.playlists.cue_out_metadata := "liq_cue_out"
```
