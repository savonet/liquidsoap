# Prerolls

A preroll is a short audio file a listener hears before joining the live
stream: a jingle, a sponsor message, an ad chosen for each listener. This
document owns where the server finds it, which one wins, and how the server
keeps and refreshes it. Sending its bytes is the harbor output's
([modes.md §2](../../../../core/outputs/spec/harbor/modes.md#2-initial-bytes)):
after the response head, before the stream, never copied per listener.

## 1. Sources and precedence

An admitted listener ([listeners.md](listeners.md#1-the-admission-pipeline))
gets at most one preroll, the first that exists among:

1. the one its `listener_decision` chose ([url-auth.md](url-auth.md#54-a-preroll):
   the `preroll` reply header, or with `compatibility` `"kh"` the reply body);
2. the mount's `intro` (Icecast's);
3. the mount's `preroll` (RSAS's).

`intro` and `preroll` are the mount's own value, else the default mount's.
Their order is the same for every `compatibility` value: they name the same
thing, and one does not override the other under any implementation.

A source is a URL (`http://` or `https://`) or a local path.

- A local path is relative to `webroot`, whether or not it starts with `/`,
  as Icecast's intros are. It MUST NOT resolve outside `webroot`: a path with
  `..` is a failed load ([§4](#4-failures)). A preroll chosen by a backend
  is not trusted more than a configured one.
- A URL is fetched with `GET` and, once cached, revalidated
  ([§3](#3-the-cache)). Fetching MUST be bounded by `preroll_timeout`
  (recommended 10 seconds): admission waits for it.
- A body preroll ([url-auth.md](url-auth.md#54-a-preroll)) is taken as it is,
  with the size limit of [§2](#2-format-and-size), and is not cached.

## 2. Format and size

The preroll's bytes are sent unchanged, so they MUST use the stream's codec
and parameters (for MP3, the same sample rate and channels; for AAC, ADTS
frames with the same parameters). The server MUST NOT decode or check them:
the operator and the ad server are responsible for the match, and a mismatch
makes the listener's player fail on its own.

`preroll_max_size` (recommended 10 MiB) bounds one preroll, protecting the
server's memory from a backend that points at a large file. A preroll above
it is a failed load.

## 3. The cache

Prerolls are kept in memory and shared by every listener, so the ad server
is fetched once for many listeners. Observable rules:

- **Budget.** The cached prerolls together hold at most
  `preroll_cache_size` bytes (recommended 256 MiB). Past it the least
  recently used are dropped first. A dropped preroll is fetched or read again
  on its next admission.
- **Key.** A source is cached under its URL, query string included, or its
  resolved path. Per-listener URLs therefore each take cache space.
- **Revalidation.** A cached URL is used as it is for `preroll_revalidate`
  (recommended 2 hours) after it was fetched or last confirmed. After that, the
  next admission marks it fresh at once, so that concurrent admissions use
  the cached copy rather than wait, then requests the URL with
  `If-None-Match` carrying the ETag it stored (none when it had none). A `304`
  keeps the copy. A `2xx` replaces it with the new body and its ETag.
- **Failed revalidation.** Any other status, an error or a timeout keeps the
  copy, which stays fresh for another interval, and is logged. A copy is not
  dropped because the ad server is down.
- **Local files.** The modification time is checked at each admission. A
  changed file is read again; an unchanged one is not.
- **Reload.** `preroll_cache_size` is read when a preroll is stored. A new
  value takes effect at the next storage.

An ad server sees: one `GET` per URL and per interval, `If-None-Match` on
all but the first; and no other request while the copy is fresh.

## 4. Failures

A preroll that cannot be loaded (not found, unreadable, fetch failed on its
first fetch, over `preroll_max_size`, outside `webroot`) MUST NOT refuse the
listener. The failure is logged and the listener is admitted without a
preroll, and no preroll is attempted from a lower-precedence source for this
listener: the decision chose that one. A failed load is not remembered, so
the next admission tries again.

An empty preroll is no preroll.

## 5. Conformance

**Binding**

- Precedence: decision, then `intro`, then `preroll`; the mount's value else
  the default mount's.
- Local path under `webroot`, none outside it.
- Bytes sent are the cached bytes, unchanged.
- One fetch per URL while fresh (an ad server counts requests); revalidation
  after `preroll_revalidate` carries `If-None-Match` when an ETag was stored; `304`
  keeps, `2xx` replaces, failure keeps the copy.
- A changed local file is read again at the next admission.
- A source above `preroll_max_size`, or failing to load, admits without a
  preroll.
- Cache total never above `preroll_cache_size` after a storage; the least
  recently used are dropped first.

**Incidental**: the order of eviction among entries of the same age, log
wording, how the revalidation request is made concurrent, memory layout.
