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
   `null`. With `tmpdir` `null` and a relative `persist_at`, a warning says
   that the state lands in a new directory every run and can never be
   restored.
2. The HLS output is built on that directory, with every argument passed
   through.
3. The handler (§3) is registered for `GET`, `HEAD` and `OPTIONS` on
   `<path>.*`, with `path` normalised to start and end with `/`.

## 3. What is served

The handler serves exactly the files the output tracks
([files.md](files.md)): listed and retained media segments, their init
segments, the media playlists and the main playlist. It reads the output's
tracked set at each request; it does not keep a copy of its own.

For a request path:

1. Not under the base path: 404.
2. The rest, percent-decoded, is looked up by exact equality among the
   tracked names. No match: 404.
3. A match: the response of §4.

Reason: serving by lookup, not by joining a request path to a directory,
makes paths that leave the directory, temporary files, the state file and
anything else in the directory unreachable by construction, and restored
files reachable at once.

## 4. Responses

Headers, in order: the configured `headers`, then `Content-Type` (§4.1),
`Content-Length`, and `Cache-Control` (§4.2). `GET` gets the file's content;
`HEAD` and `OPTIONS` get headers only.

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
