# Serving over HTTP: `output.harbor.hls`

Part A. The standard library pairs an HLS output with a handler on
Liquidsoap's own HTTP server. This file owns that pairing: which files are
served, under which paths, with which response headers. The HTTP server
(transport, request parsing, the generic static handler) is the harbor's
([harbor spec](../../spec/harbor/README.md)).

## 1. Entry point

`output.harbor.hls(<every output.file.hls argument but the directory>,
~segment_name, ~headers=[("Access-Control-Allow-Origin", "*")],
~port=8000, ~path="/", ~tmpdir=null, ~transport=http.transport.unix,
formats, s)`. HTTPS is a `transport`.

## 2. Composition

1. The output directory is `tmpdir`, or a new temporary directory when
   `null`, removed with what it holds when the process shuts down. With `tmpdir` `null` and a relative `persist_at`, a warning says
   that the state lands in a new directory every run and can never be
   restored.
2. The HLS output is built on that directory, with every argument passed
   through.
3. The handler (§3) is registered for `GET`, `HEAD` and `OPTIONS` on
   `<path>.*`, with `path` normalised to start and end with `/`.

## 3. What is served

The handler serves, first, the files the output tracks
([files.md](files.md)): listed and retained media segments, their init
segments, the media playlists and the main playlist. It reads the output's
tracked set at each request, through an internal accessor that is not part
of the script API; it keeps no copy of its own. Then it serves other
regular files of the directory that a script placed there on purpose: keys
named by an `EXT-X-KEY` tag, player pages, images.

For a request path under the base path (other paths are the HTTP
server's, which the handler never sees):

1. The rest is percent-decoded. A tracked name equal to it: the response of
   §4, with the content type of its kind (§4.1).
2. Otherwise, a path with a NUL byte or a `..` component (split on `/`,
   and also on `\` where the system uses it) is a 404. So is the output's
   state file, and any temporary file of the output.
3. Otherwise, a regular file at that path under the directory: the
   response of §4, with the content type its extension gives, if any. A
   directory or a missing file: 404.

Reason: tracked files are found by lookup, so restored files are reachable
at once and nothing the output is still writing is. Other files are joined
to the directory only after a lexical check, so no request leaves it. A
symbolic link inside the directory is followed: placing one there serves
its target.

## 4. Responses

The configured `headers` go on every response, errors included. Reason: a
browser player needs the CORS header to see a 404 as a 404.

A successful response adds `Content-Type` (§4.1), `Content-Length` and
caching (§4.2). A configured header replaces a generated one of the same
name, compared case-insensitively: a script's `Cache-Control` is its cache
policy. `GET` gets the file's content; `HEAD` and `OPTIONS` get headers
only.

The content is read when served. A file deleted between lookup and read is
a 404.

### 4.1 Content types

| File               | Content type                                            |
| ------------------ | ------------------------------------------------------- |
| playlist           | `application/vnd.apple.mpegurl`                         |
| MPEG-TS segment    | `video/mp2t`                                            |
| fMP4 media segment | `video/iso.segment`                                     |
| fMP4 init segment  | `video/mp4`, or `audio/mp4` for a variant with no video |
| ADTS               | `audio/aac`                                             |
| MP3                | `audio/mpeg`                                            |
| AC-3 / E-AC-3      | `audio/ac3` / `audio/eac3`                              |
| WebVTT             | `text/vtt`                                              |
| other              | the variant format's MIME type                          |

The kind is known from the file and its variant, not guessed from an
extension, so `extname` overrides and init extensions need no table. The
types are those of Apple's authoring specification (§10) and RFC 8216.

Playlists SHOULD be sent with `Content-Encoding: gzip` when the request
accepts it (RFC 8216 §6.2.1; Apple §10.1 makes it a MUST).

### 4.2 Caching

- Playlists: `Cache-Control: max-age=<p>`, with `p` half the target
  duration rounded down (0 when `T` is 1). Reason: players reload about once per target
  duration, and a cache must not hold a playlist across a reload. Half a
  target duration is the second edition's value for non-blocking playlist
  responses.
- Media and init segments: `Cache-Control: max-age=<s>` and the matching
  `Expires`, with `s` recommended at the availability duration of
  [files.md §5](files.md#5-retention) (the segment's length plus the window
  duration). RFC 8216 §6.2.2 asks for an `Expires` that reflects the planned
  lifetime. A segment's content never changes under its name while it is
  served.

Names can be reused by a later run that starts from position 1 (a new
process without persistence, [playlists.md §2.1](playlists.md#21-media-sequence)):
a cache holding a segment from the previous run would serve it in its
place. A deployment behind a cache SHOULD use persistence, or segment names
that are unique across runs.
