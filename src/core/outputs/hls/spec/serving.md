# Serving over HTTP: `output.harbor.hls`

Part A. As built. The standard library pairs an HLS output with a static
file handler on Liquidsoap's own HTTP server. This file owns that pairing:
which directory is served, under which paths, with which response headers.
The HTTP server itself is the harbor's
([harbor spec](../../spec/harbor/README.md), which leaves this wrapper out).

## 1. Entry points

- `output.harbor.hls(<every output.file.hls argument but the directory>,
~segment_name, ~headers=[("Access-Control-Allow-Origin", "*")],
~port=8000, ~path="/", ~tmpdir=null, ~transport=http.transport.unix,
formats, s)`. HTTPS is a `transport` (an SSL transport where available).
- `output.harbor.hls.https(...)`: the same without `transport`, defined only
  if `harbor.https.static` exists. Nothing defines it, so this entry point
  never exists (finding F-SRV-2, checked: `-h` gives `Plugin not found!`).

## 2. Composition

1. The served directory is `tmpdir`, or a new temporary directory under the
   system temporary directory when `null`. It is the HLS output's output
   directory (not its `temp_dir`). A relative `persist_at` therefore lands
   in a new directory every run when `tmpdir` is `null`.
2. The static handler is registered first (§3), then `output.file.hls` is
   built with every argument passed through, the directory and the
   variants.

## 3. The static handler

Registered for `GET`, `HEAD` and `OPTIONS` on `<path>.*`, where `path` is
normalised to start and end with `/`.

For a request path:

1. Not under the base path: 404.
2. The rest is URL-decoded and joined to the directory. No normalisation:
   an encoded `../` (`..%2F`) leaves the directory (finding F-SRV-1,
   checked: a file beside the directory was served with 200).
3. Missing: 404. A directory: 403.
4. A file: the configured headers, then `Content-Type` from §3.1 when it
   gives one, then for `GET` the file's content. `HEAD` and `OPTIONS` get
   headers only. No `Content-Length`, no `Cache-Control`, no `ETag` or
   `Last-Modified` (checked).

Files are read when served. Thanks to atomic publication
([files.md §3](files.md#3-writing-a-file)) a final name never shows partial
content; temporary files in the directory are served by their own names.
The persisted state file is served if it is under the directory.

### 3.1 Content types

By the requested file's extension:

- `.m3u8`: `application/x-mpegURL`.
- Otherwise the first variant whose format's file extension, with a dot,
  equals it: that format's MIME type. Variant `extname` overrides are not
  consulted, and an `m4s` media segment matches no `mp4` format.
- No match: the empty string, so the header is sent empty or not at all.

Observed (checked): `.m3u8` → `application/x-mpegURL`; MPEG-TS `.ts` →
`application/octet-stream`; fMP4 init `.mp4` → `video/mp4`; fMP4 media
`.m4s` → no `Content-Type` (finding F-SRV-3).

## 4. What is not here

Retention, atomic writes and every file the directory holds are the HLS
output's ([files.md](files.md)). Caching behaviour is the HTTP server's: it
adds no caching headers of its own.
