# Prerolls

A preroll is a short audio clip a listener hears before joining the live
stream: a jingle, a sponsor message, an ad chosen for each listener. This
document owns what a preroll is to the server (bytes in the decision), the
cache that loads and keeps prerolls, and the precedence of the places they
come from. Sending the bytes is the harbor output's
([modes.md §2](../../../../core/outputs/spec/harbor/modes.md#2-initial-bytes)):
after the response head, before the stream, never copied per listener.

## 1. A preroll is bytes

The server knows prerolls only as the `preroll` field of the decision record
([listeners.md §2](listeners.md#2-the-listener-decision)): the bytes sent to
this listener, unchanged. The server has no preroll cache, no fetching and no
file reading of its own. The store of [§3](#3-the-cache) is process-wide, and
`icecast.server.preroll_cache` returns a view over it that decisions use to
turn a location into bytes.

- An empty preroll is no preroll.
- The bytes MUST be in the mount's container and codec (for MP3, MP3 frames;
  for AAC, ADTS frames). The server MUST NOT decode them. A Liquidsoap source
  cannot be a preroll: it would need encoding to the stream's format.
- **Improvement.** The server SHOULD read the container and codec of a
  preroll before sending it. A preroll of another container or codec than the
  mount's format ([mounts.md §3](mounts.md#3-the-format-match-rule)) is dropped
  with a log, and the listener gets the stream alone and valid. Icecast and
  RSAS send the bytes they are given, and a mismatch makes the listener's
  player fail (Icecast-KH realigns `withintro` bodies to frame boundaries). Sample rate and channels are not compared. This is not subject
  to `compatibility`.
- The server does not bound the bytes a decision hands it. The cache helper
  bounds what it loads ([§2](#2-locations)).

## 2. Locations

A location is a URL (`http://` or `https://`) or a local path. The cache
helper turns a location into bytes.

- A local path is relative to a base directory (`webroot` in the Icecast
  translation, [icecast-config.md](icecast-config.md)), whether or not it
  starts with `/`, as Icecast's intros are. It MUST NOT resolve outside the
  base directory: a path with `..` is a failed load ([§4](#4-failures)).
- A URL is fetched with `GET`, bounded by `preroll_timeout` (recommended 10
  seconds): admission waits for it. Once cached, it is revalidated
  ([§3](#3-the-cache)).
- A load larger than `preroll_max_size` (recommended 10 MiB) is a failed
  load. The limit protects the server's memory from a backend that points at
  a large file.
- A location chosen by a backend MUST be an `http` or `https` URL. Any other
  value, a local path included, is ignored with a log: a path from a backend
  would let it read any file the server can. Local paths come only from the
  operator's own parameters.

## 3. The cache

Loaded prerolls are kept in memory and shared by every listener, so the ad
server is fetched once for many listeners. There is **one store per process**,
bounded by the global setting `settings.icecast.server.preroll_cache_size`
([parameters.md §10](parameters.md#10-existing-global-settings)).
`icecast.server.preroll_cache(~directory, ~preroll_timeout, ~preroll_max_size,
~preroll_revalidate)` returns a view over the store with its own limits for
what it loads; `directory` is the base directory of its local locations
([§2](#2-locations)). The view loads an operator's location with
`view(location)`, which gives no bytes when the load fails, and one a backend
chose with `view.backend(location)`, which takes URLs only and gives an empty
preroll for any other value or a failed load ([§4](#4-failures)). A reload, or a new `icecast.server.config.load`, builds new views
and keeps the store warm: a preroll fetched before is not fetched again.
Observable rules:

- **Budget.** The cached prerolls of all views together hold at most
  `preroll_cache_size` bytes (recommended 256 MiB). Past it the least recently
  used are dropped first. A dropped preroll is fetched or read again on its next
  use. The size is read when a preroll is stored: a new value applies at the
  next storage.
- **Key.** A location is cached under its URL, query string included, or its
  resolved path. Per-listener URLs therefore each take cache space.
- **Revalidation.** A cached URL is used as it is for the view's
  `preroll_revalidate` (recommended 2 hours, in seconds) after it was fetched or
  last confirmed.
  After that, the next use requests the URL with `If-None-Match` carrying the
  ETag it stored (no validator when it had none). While that request is in
  flight, concurrent uses take the cached copy and do not wait. A `304` keeps
  the copy; a `2xx` replaces it with the new body and its ETag.
- **Failed revalidation.** Any other status, an error or a timeout keeps the
  copy, which stays fresh for another interval, and is logged. A copy is not
  dropped because the ad server is down.
- **Local files.** The modification time is checked at each use. A changed
  file is read again; an unchanged one is not.

An ad server sees: one `GET` per URL and per interval, `If-None-Match` on all
but the first; and no other request while the copy is fresh.

## 4. Failures

A location that cannot be loaded (not found, unreadable, fetch failed on its
first fetch, over `preroll_max_size`, outside the base directory, not a URL
where a backend chose it) is logged and yields no bytes. It MUST NOT refuse the
listener. A failed load is not remembered, so the next use tries again.

When a backend chose the location and it fails, the decision hands back an
empty preroll rather than none: `view.backend(location)` gives `""`. The listener then hears no preroll, and the
operator's own location of [§5](#5-precedence) is not used in its place: the
backend chose that one.

## 5. Precedence

A listener gets at most one preroll, the first that exists among:

1. the one its authentication chose
   ([url-auth.md](url-auth.md#54-a-preroll)): the `preroll` reply header, or
   with `compatibility` `"kh"` the reply body, empty when it failed to load;
2. the mount's `intro` (Icecast's);
3. the mount's `preroll` (RSAS's).

The order is the same for every `compatibility` value: `intro` and `preroll`
name the same thing in the implementations that have them, and one does not
override the other.

The operator's two locations reach the server through the decision: the
translation composes, last, a decision that loads the location with a view of
the cache and gives it as `preroll`
([icecast-config.md](icecast-config.md#53-intro-and-preroll)). Because
`icecast.server.decision.all` keeps the first `preroll` given, an empty one
included ([listeners.md §7](listeners.md#7-ready-made-decisions)), a location
chosen earlier wins. The translation composes `intro` first and `preroll`
second, after the authentication.

## 6. Conformance

**Binding**

- The bytes sent are the bytes the decision gave, unchanged; an empty preroll
  sends nothing. The container-and-codec check of [§1](#1-a-preroll-is-bytes)
  SHOULD be done; when it is, a preroll of another container or codec is not
  sent, a log says so, and the stream is.
- Precedence: authentication, then `intro`, then `preroll`; a failed
  authentication-chosen preroll is not replaced.
- A backend location that is not an `http` or `https` URL is ignored and logged.
- A local path under the base directory, none outside it.
- One fetch per URL while fresh; revalidation after `preroll_revalidate`
  carries `If-None-Match` when an ETag was stored; `304` keeps, `2xx`
  replaces, failure keeps the copy.
- A changed local file is read again at its next use.
- A load above `preroll_max_size`, or failing, admits without a preroll.
- Cache total never above `preroll_cache_size` after a storage; the least
  recently used are dropped first.
- A reload keeps the store: a preroll cached before it is not fetched again.

**Incidental**: the order of eviction among entries of the same age, log
wording, how concurrent revalidation is arranged, memory layout.

**Doubles.** An ad server counting requests and recording `If-None-Match`,
whose reply (`200`, `304`, `500`, a delay) the test sets; a directory of
preroll files whose modification times the test changes.

**Something ran.** The ad server double asserts the number of requests it
saw, so a test whose revalidation never fired fails.
