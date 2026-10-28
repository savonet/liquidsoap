# `icecast.server` as a user of `output.harbor`

`icecast.server` is a standard-library function that makes Liquidsoap act as
an Icecast server: source clients push streams to mounts, listeners pull them.
Every listener-facing byte it serves goes through `output.harbor`, one output
per live mount. This document states what `icecast.server` does with
`output.harbor` and what it relies on, so that a rebuilt operator keeps it
working. The rest of `icecast.server` (configuration file, source protocol,
logs, admin pages) is not specified here.

It is a primary use case of the operator: a rebuild is not complete until it
passes the `icecast.server` tests ([tests.md](tests.md#icecastserver)).

## 1. One port, many users

`icecast.server` puts on one port, with one transport:

- a dynamic harbor input on the pattern `/:mount` with ICY enabled, which
  receives source clients (`PUT`, `SOURCE`, and ICY sources on the next port);
- HTTP routes for its status page (`/`), `/status.json` and
  `/admin/listeners.json`, registered at startup;
- one `output.harbor` per live mount, registered while the server runs, each
  answering `GET` on its mount.

The server tells source requests from listener requests by method. Routes are
tried in registration order ([connection.md §1](connection.md#1-routing)), so
a mount whose path equals a status route is shadowed by it.

The transport may be TLS or SSL, and carries a `reload` method used to swap
certificates; `output.harbor` receives it as its `transport` parameter and
must accept a transport value that carries extra methods.

## 2. Creating a mount's output

When a source first connects to a mount, `icecast.server` builds the mount's
output with:

| `output.harbor` parameter    | Value                                                                                                                                  |
| ---------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `id`                         | `<mount without leading slash>.output`                                                                                                 |
| `fallible`                   | `true`                                                                                                                                 |
| `mount`, `port`, `transport` | the mount path; the server's port and transport                                                                                        |
| `headers`                    | the configuration's global `<http-headers>` followed by the mount's own                                                                |
| `timeout`                    | `<client-timeout>`, default 30 s                                                                                                       |
| `burst`                      | the mount's `<burst-size>`, else the global one, else **null**; null also when either is 0 or `<burst-on-connect>` is 0                |
| `dumpfile`                   | the mount's `<dump-file>`                                                                                                              |
| `dedicated_encoder`          | the `icecast.server` parameter of that name, default `false`                                                                           |
| `format`                     | the MIME type the source declared, or `""` to derive it                                                                                |
| encoder                      | a copy encoder for the source's container, with muxer options from `format_options` or `settings.icecast.server.default_muxer_options` |
| source                       | a fallback between the mount's live source and its fallback mount's source, each switched to at once                                   |

The output is created at run time, long after the server started, so handler
registration takes effect immediately
([contracts.md §1](contracts.md#1-harbor-server)). Its clock is unified with
the server's own clock.

With no burst configured, listeners get **no burst**, not the operator's
default ([findings.md I2](findings.md#i2-no-burst-unless-configured--checked)).

`icecast.server` also sets the default frame duration to 0.2 s, so the output
encodes and wakes its writers five times a second
([delivery.md §2](delivery.md#2-wake-ups)).

## 3. Content

The encoder copies the source's encoded stream without re-encoding: mp3, AAC,
Ogg (Opus, Vorbis, FLAC), or whatever container the source sent. A mount's
source can change (a reconnect, a switch to the fallback mount), so the copy
encoder sees a new source mid-stream; how it switches is owned by the FFmpeg
spec ([encoder-copy.md §12](../../../optionals/ffmpeg/spec/encoder-copy.md#12-source-switching-and-per-source-state)).

A late listener on a shared-encoder mount must still receive a decodable
stream ([shared-encoder.md §2](shared-encoder.md#2-initial-bytes)), with the
container's header first. For Ogg the header comes from the copy encoder's
captured container header, or from the current link's own header pages.

ICY metadata is not offered on Ogg mounts ([icy.md §1](icy.md#1-negotiation)),
as Icecast does: Icecast relays and GStreamer's Ogg demuxer request ICY and do
not strip it.

## 4. Lifecycle of a mount's output

- **Source reconnects or switches.** The output keeps running: listeners stay
  connected and the output follows the new source.
- **Last source gone, no fallback.** `icecast.server` shuts the output down
  (detaches it from its clock), which stops it: every listener is disconnected
  and the mount's handler is removed. Requests for the mount then get the
  server's 404.
- **A source returns.** A new `output.harbor` is created on the same mount and
  port. The old output's handler removal must not remove the new output's
  handler ([findings.md D5](findings.md#d5-two-outputs-on-one-mount--checked)).
  Observed: removal completes before the new source is accepted, so the new
  handler survives; this ordering is what keeps reconnects working.
- **Fallback.** A mount configured with a fallback mount keeps its output, and
  its listeners, while either source is live.
- **Reload.** Live outputs keep the parameters they were created with; new
  headers, burst and dump file apply to the next output of a mount.
- **Server shutdown** with a source still connected must not hang.

## 5. What it reads from the output

| Used                          | For                                                                                     |
| ----------------------------- | --------------------------------------------------------------------------------------- |
| `on_connect` (synchronous)    | listener counters and the public `on_listener_connect` handlers                         |
| `on_disconnect` (synchronous) | counters, the access log line, the public `on_listener_disconnect` handlers             |
| `listeners()`                 | stats, the status page, the admin page, the listener count passed with metadata updates |
| `on_metadata`                 | playlist log and the public `on_metadata` handlers                                      |
| `last_metadata`               | the mount's current metadata in stats                                                   |
| `clock`, `shutdown`           | clock unification, mount teardown                                                       |

Listener record fields it relies on:

- `id`, `connected_at`, `duration`, `bytes_sent`, `uri`, `protocol`: copied
  into its own listener view and the access log
  (`"GET <uri> HTTP/<protocol>" 200 <bytes_sent> … <duration>`);
- `ip`: the numeric peer address, replaced by the `X-Forwarded-For` value when
  configured, then hashed. It must never be a reverse-DNS name;
- `headers`, looked up by **lowercase** name: `x-forwarded-for`,
  `user-agent`, `referer`.

Guarantees it relies on: exactly one `on_disconnect` per `on_connect`, after
it, so its counters "cannot drift"
([concurrency.md §3](concurrency.md#3-guarantees)); `listeners()` agreeing with
those callbacks.

## 6. What it does not use

- `auth`: listener requests are always admitted. Icecast's listener
  authentication is not offered.
- `metaint`, `encoding`, `url`, `buffer`: left at the operator's defaults.
- Listener limits: Icecast's `<clients>` and `<max-listeners>` are parsed and
  not enforced. The operator has no way to refuse a listener except by
  requiring credentials ([findings.md I3](findings.md#i3-no-way-to-refuse-a-listener-without-credentials--read-only)).
