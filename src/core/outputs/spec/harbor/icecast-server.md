# `icecast.server` as a user of `output.harbor`

`icecast.server` is a standard-library function that makes Liquidsoap act as
an Icecast server: source clients push streams to mounts, listeners pull them.
Every listener-facing byte it serves goes through `output.harbor`, one output
per live mount. This document states what `icecast.server` does with
`output.harbor` and what it relies on. Every rule of the operator is checked
against it, and its tests are acceptance checks
([tests.md](tests.md#icecastserver)). The rest of `icecast.server`
(configuration file, source protocol, logs, admin pages) is not specified
here; where this document says what `icecast.server` SHOULD do, it states what
emulating Icecast asks of it, with the operator feature it uses.

## 1. One port, many users

`icecast.server` puts on one port, with one transport:

- a dynamic harbor input on the pattern `/:mount` with ICY enabled, which
  receives source clients (`PUT`, `SOURCE`, and ICY sources on the next port);
- HTTP routes for its status page (`/`), `/status.json` and
  `/admin/listeners.json`;
- one `output.harbor` per live mount, each answering `GET` on its mount.

Sources and listeners are told apart by method. A mount whose path equals a
status route would shadow it once its output registers
([connection.md §1](connection.md#1-routing)); `icecast.server` SHOULD refuse
a source on such a mount.

The transport may be TLS or SSL, and carries a `reload` method used to swap
certificates; the operator accepts a transport value that carries extra
methods ([operator.md §1](operator.md#1-parameters)).

## 2. Creating a mount's output

When a source first connects to a mount, `icecast.server` builds the mount's
output with:

| `output.harbor` parameter    | Value                                                                                                                                  |
| ---------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `id`                         | `<mount without leading slash>.output`                                                                                                 |
| `fallible`                   | `true`                                                                                                                                 |
| `mount`, `port`, `transport` | the mount path; the server's port and transport                                                                                        |
| `headers`                    | the configuration's global `<http-headers>`, then the mount's own, then the stream identity headers below                              |
| `timeout`                    | `<client-timeout>`, default 30 s                                                                                                       |
| `burst`                      | the mount's `<burst-size>`, else the global one; null when either is 0 or `<burst-on-connect>` is 0; the operator's default otherwise  |
| `accept`                     | the listener-limit predicate below                                                                                                     |
| `dedicated_encoder`          | the `icecast.server` parameter of that name, default `false`                                                                           |
| `format`                     | the MIME type the source declared, or `""` to derive it                                                                                |
| encoder                      | a copy encoder for the source's container, with muxer options from `format_options` or `settings.icecast.server.default_muxer_options` |
| source                       | a fallback between the mount's live source and its fallback mount's source, each switched to at once                                   |

**Burst.** With no burst configured, `icecast.server` leaves `burst` at the
operator's default, 65534 bytes, close to Icecast's own default of 64 KiB
(from memory, to verify), so a joiner gets audio at once as on Icecast.

**Stream identity.** Icecast answers listeners with the stream's identity
taken from the source's request: `icy-name`, `icy-description`, `icy-genre`,
`icy-url`, `icy-br`, `icy-pub` (Icecast's list is from memory, to verify).
`icecast.server` SHOULD pass the ones the source sent, through `headers`.
They are values from outside, so they go through the operator's header
validation ([operator.md §2](operator.md#2-validation-at-construction)); a
value with a line break SHOULD be dropped before building the output.

**Listener limits.** Icecast's `<max-listeners>` (per mount) and `<clients>`
(server-wide) are enforced through `accept`
([connection.md §4](connection.md#4-admission-predicate)): the predicate
compares the mount's and the server's listener counts, kept from the
callbacks, with the limits. A limit can be exceeded by the number of
admissions in progress at once.

**Dump file.** Icecast's `<dump-file>` is not offered by the operator. When
`icecast.server` supports it, it records the mount's source with an output of
its own.

The output is created at run time, long after the server started, so route
registration takes effect immediately
([contracts.md §1](contracts.md#1-harbor-server)). Its clock is unified with
the server's own clock.

`icecast.server` sets the default frame duration to 0.2 s, so the output
encodes and wakes its delivery groups five times a second
([delivery.md §2](delivery.md#2-wake-ups)): encoded data reaches listeners in
steps of one frame.

## 3. Content

The encoder copies the source's encoded stream without re-encoding: mp3, AAC,
Ogg (Opus, Vorbis, FLAC), or whatever container the source sent. A mount's
source can change (a reconnect, a switch to the fallback mount), so the copy
encoder sees a new source mid-stream; how it switches is owned by the FFmpeg
spec ([encoder-copy.md §12](../../../optionals/ffmpeg/spec/encoder-copy.md#12-source-switching-and-per-source-state)).

A late listener on a shared-encoder mount receives a decodable stream with
the container's header first ([shared-encoder.md §2](shared-encoder.md#2-initial-bytes)).
For Ogg the header comes from the copy encoder's container header, or from
the current link's own header pages; the encoder contract requires the header
to be complete ([contracts.md §4](contracts.md#4-encoder)).

ICY metadata is not offered on Ogg mounts ([icy.md §1](icy.md#1-negotiation)),
as on Icecast.

## 4. Lifecycle of a mount's output

- **Source reconnects or switches.** The output keeps running: listeners stay
  connected and the output follows the new source.
- **Last source gone, no fallback.** `icecast.server` shuts the output down,
  which stops it: every listener is disconnected and the mount's route is
  removed. Requests for the mount then get the server's 404.
- **A source returns.** A new `output.harbor` is created on the same mount and
  port. Whatever the order of the old output's removal and the new output's
  registration, the new output serves and keeps serving: removal takes only
  its own registration, and the most recent registration serves
  ([connection.md §1](connection.md#1-routing)).
- **Fallback.** A mount configured with a fallback mount keeps its output, and
  its listeners, while either source is live.
- **Reload.** Live outputs keep the parameters they were created with; new
  headers, burst and limits apply to the next output of a mount.
- **Server shutdown** with a source still connected MUST NOT hang
  ([operator.md §7](operator.md#7-lifecycle)).

## 5. What it reads from the output

| Used                          | For                                                                                     |
| ----------------------------- | --------------------------------------------------------------------------------------- |
| `on_connect` (synchronous)    | listener counters and the public `on_listener_connect` handlers                         |
| `on_disconnect` (synchronous) | counters, the access log line, the public `on_listener_disconnect` handlers             |
| `listeners()`                 | stats, the status page, the admin page, the listener count passed with metadata updates |
| `on_metadata`                 | playlist log and the public `on_metadata` handlers                                      |
| `last_metadata`               | the mount's current metadata in stats                                                   |
| `clock`, `shutdown`           | clock unification, mount teardown                                                       |

Listener record fields it relies on ([operator.md §5](operator.md#5-the-listener-record)):

- `id`, `connected_at`, `duration`, `bytes_sent`, `uri`, `protocol`: copied
  into its own listener view and the access log
  (`"GET <uri> HTTP/<protocol>" 200 <bytes_sent> … <duration>`);
- `ip`: the numeric peer address, replaced by the `X-Forwarded-For` value when
  configured, then hashed. It is never a reverse-DNS name;
- `headers`, looked up by **lowercase** name: `x-forwarded-for`,
  `user-agent`, `referer`.

Guarantees it relies on: exactly one `on_disconnect` per `on_connect`, after
it, with a final `bytes_sent`, so its counters cannot drift; `listeners()`
agreeing with those callbacks ([concurrency.md §3](concurrency.md#3-guarantees),
[operator.md §3](operator.md#3-methods)).

## 6. What it does not use

- `auth`: listener requests are not asked for credentials. Icecast's listener
  authentication is not offered.
- `metaint`, `encoding`, `url`, `buffer`: left at the operator's defaults.
