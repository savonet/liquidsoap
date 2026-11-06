# `icecast.server` as a user of `output.harbor`

`icecast.server` is a standard-library function that makes Liquidsoap act as
an Icecast server: source clients push streams to mounts, listeners pull them.
Every listener-facing byte it serves goes through `output.harbor`, one output
per live mount. This document states only what `icecast.server` asks of
`output.harbor` and what it relies on; its tests are acceptance checks
([tests.md](tests.md#icecastserver)). The policy (who may listen, limits,
time limits, prerolls, headers, fallbacks, logs, statistics) is the
[icecast-server specification](../../../../libs/extra/spec/icecast-server/README.md).

## 1. One port, many users

`icecast.server` puts on one port, with one transport:

- a dynamic harbor input on the pattern `/:mount` with ICY enabled, which
  receives source clients (`PUT`, `SOURCE`, and ICY sources on the next port);
- HTTP routes for its status and admin pages
  ([observability.md](../../../../libs/extra/spec/icecast-server/observability.md));
- one `output.harbor` per live mount, each answering `GET` on its mount.

Sources and listeners are told apart by method. A mount whose path equals a
reserved route would shadow it once its output registers
([connection.md §1](connection.md#1-routing)); the reserved paths and the
refusal are [sources.md](../../../../libs/extra/spec/icecast-server/sources.md)'s.

The transport may be TLS or SSL and carries a `reload` method used to swap
certificates; the operator accepts a transport value that carries extra
methods ([operator.md §1](operator.md#1-parameters)).

## 2. What the server asks of the output

When a feeder first takes a mount, or a listener asks for a configured mount
whose fallback has something to send
([mounts.md §1](../../../../libs/extra/spec/icecast-server/mounts.md#1-lifecycle)),
`icecast.server` builds the mount's output. The values come from the server's parameters
([parameters.md](../../../../libs/extra/spec/icecast-server/parameters.md)).

| `output.harbor` parameter    | Value                                                                                                             |
| ---------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| `id`                         | `<mount without leading slash>.output`                                                                            |
| `fallible`                   | `true`                                                                                                            |
| `keep_listeners`             | `true`: the server shuts the output down itself ([§4](#4-lifecycle-of-a-mounts-output))                           |
| `mount`, `port`, `transport` | the mount path; the server's port and transport                                                                   |
| `headers`                    | none: stream identity headers depend on the mount's current feeder and travel as admit headers                    |
| `timeout`                    | the server's `client_timeout` (server-wide only)                                                                  |
| `burst`                      | the mount's `burst_size`, else the server's; `0` when `burst_on_connect` is off                                   |
| `accept`                     | the admission of [listeners.md](../../../../libs/extra/spec/icecast-server/listeners.md#1-the-admission-pipeline) |
| `dedicated_encoder`          | the server's parameter of that name                                                                               |
| `format`, encoder, source    | as [mounts.md](../../../../libs/extra/spec/icecast-server/mounts.md) states                                       |

What it relies on in the output, each stated by the harbor spec:

- **Admission.** `accept` returns the decision record
  (`refuse`, `time_limit`, `preroll`, `headers`, `on_dropped`;
  [connection.md §4](connection.md#4-admission-predicate)). The output
  validates it: an invalid record, or an `accept` that raises, is
  `403 Forbidden`. The server validates the values it computes (header names
  and values) before handing the decision over and drops an invalid entry with
  a log; the output still validates what reaches it.
- **Admission that does not complete.** The server sets `on_dropped` to call
  the decision's `on_release` with a duration-0 release record. The output
  calls it when an admitted request ends before `on_connect`, so a limit
  counted at admission cannot leak
  ([listeners.md §5](../../../../libs/extra/spec/icecast-server/listeners.md#5-release)).
- **Preroll bytes.** Sent as they are after the response head
  ([modes.md §2](modes.md#2-initial-bytes)).
- **Admit headers.** A decision that admits carries headers that the output
  appends to the `200` reply, and a refusal carries the headers it is to send
  ([connection.md §4](connection.md#4-admission-predicate)). The stream
  identity headers (`icy-*`) are among them: the server builds them per reply
  from the mount's current feeder, so a listener joining after a feeder
  reconnect gets the new identity. The output writes
  them as given. The server builds the final list (status filter, computed
  values, de-duplication), because it alone knows the request and the policy
  ([listeners.md §3.2](../../../../libs/extra/spec/icecast-server/listeners.md#32-per-reply-headers)).
  The output takes no header callback.
- **Listeners kept while the source is unavailable.** With `keep_listeners`
  the output stays started and keeps its listeners while its source is
  unavailable, sends them nothing, and serves again when the source is
  available ([operator.md §7](operator.md#7-lifecycle)). The server needs it
  for `linger` and for an attached Liquidsoap source with no data
  ([mounts.md §1.1](../../../../libs/extra/spec/icecast-server/mounts.md#11-linger)).
  The server ends the output itself, with `shutdown()`, when nothing is left
  to send.
- **Time limits.** Enforced by the output
  ([delivery.md §6](delivery.md#6-disconnection)).
- **Burst.** With none configured the output keeps its default.

The output is created at run time, long after the server started, so route
registration takes effect immediately
([contracts.md §1](contracts.md#1-harbor-server)). Its clock is unified with
the server's own clock. `icecast.server` sets the default frame duration to
0.2 s, so the output encodes and wakes its delivery groups five times a
second ([delivery.md §2](delivery.md#2-wake-ups)).

## 3. Content

The encoder copies the source's encoded stream without re-encoding: mp3, AAC,
Ogg (Opus, Vorbis, FLAC), or whatever container the source sent. A mount's
source can change (a reconnect, a switch to the fallback), so the copy encoder
sees a new source mid-stream; how it switches is owned by the FFmpeg spec
([encoder-copy.md §12](../../../optionals/ffmpeg/spec/encoder-copy.md#12-source-switching-and-per-source-state)).

A late listener on a shared-encoder mount receives a decodable stream with the
container's header first ([shared-encoder.md §2](shared-encoder.md#2-initial-bytes)).
For Ogg the header comes from the copy encoder's container header, or from the
current link's own header pages; the encoder contract requires the header to
be complete ([contracts.md §4](contracts.md#4-encoder)).

ICY metadata is not offered on Ogg mounts ([icy.md §1](icy.md#1-negotiation)),
as on Icecast.

## 4. Lifecycle of a mount's output

- **Source reconnects or switches.** The output keeps running: listeners stay
  connected and the output follows the new source, unless the format changes
  ([mounts.md §3](../../../../libs/extra/spec/icecast-server/mounts.md#3-the-format-match-rule)).
- **Nothing left to send.** When no source of any kind feeds the output, and
  no `linger` runs
  ([mounts.md](../../../../libs/extra/spec/icecast-server/mounts.md#1-lifecycle)),
  `icecast.server` shuts the output down, which stops it: every listener is
  disconnected and the mount's route is removed. Requests for the mount then
  get the server's 404.
- **A source returns.** A new `output.harbor` is created on the same mount and
  port, with `override_mount`. Whatever the order of the old output's removal
  and the new output's registration, the new output serves and keeps serving:
  removal takes only its own registration, and the most recent registration
  serves ([connection.md §1](connection.md#1-routing)).
- **Reload.** Live outputs keep the parameters they were created with; what
  applies to a live output is [parameters.md](../../../../libs/extra/spec/icecast-server/parameters.md)'s.
- **Server shutdown** with a source still connected MUST NOT hang
  ([operator.md §7](operator.md#7-lifecycle)).

## 5. What it reads from the output

| Used                          | For                                                                                                                                             |
| ----------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| `on_connect` (synchronous)    | listener counts and the connect event                                                                                                           |
| `on_disconnect` (synchronous) | counts, the access log line, the disconnect event, the decision's `on_release` (`on_dropped` covers an admission that ends before `on_connect`) |
| `listeners()`                 | the stats snapshot                                                                                                                              |
| `on_metadata`                 | playlist log and the metadata event                                                                                                             |
| `last_metadata`               | the mount's current metadata in stats                                                                                                           |
| `clock`, `shutdown`           | clock unification, mount teardown                                                                                                               |

The listener record and the events built on it are owned by
[observability.md](../../../../libs/extra/spec/icecast-server/observability.md#2-the-listener-record).
From the output it uses the fields of
[operator.md §5](operator.md#5-the-listener-record): `id`, `connected_at`,
`duration`, `bytes_sent`, `uri`, `protocol`, `ip`, `headers` (looked up by
**lowercase** name) and `query_string`.

Guarantees it relies on: exactly one `on_disconnect` per `on_connect`, after
it, with a final `bytes_sent`, so its counters cannot drift; `listeners()`
agreeing with those callbacks ([concurrency.md §3](concurrency.md#3-guarantees),
[operator.md §3](operator.md#3-methods)).

## 6. What it does not use

- `auth`: listener requests are not asked for credentials by the output;
  authentication is part of the decision `accept` calls.
- `metaint`, `encoding`, `url`, `buffer`: left at the operator's defaults.
