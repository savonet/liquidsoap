# Icecast server: specification

**Scope.** A streaming server that speaks Icecast's protocols: source clients
connect to it and send encoded streams to mounts, listeners connect to it and
receive those streams. It replaces an Icecast, RSAS or Icecast-KH server for
the software around it — source clients, players, authentication and ad
backends, log analysers, monitoring — while its policy is decided by the
script that runs it, or by an Icecast configuration file translated into that
policy. A mount can also be fed by a Liquidsoap source, which no other
Icecast-compatible server can offer.

**Not in scope.** Delivery of bytes to listeners — sockets, bursts, buffers,
ICY framing, prerolls on the wire, time-limit enforcement, encoder modes — is
owned by the harbor output and specified in
[its own specification](../../../../core/outputs/spec/harbor/README.md). This
specification states what the server asks of it
([§ Requirements on the harbor output](#requirements-on-the-harbor-output)).

**Reader.** Someone implementing the server, writing a script against it, or
checking an implementation, without reading its code.

## Model

| Term              | Meaning                                                                                                                                                                   |
| ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| server            | One listening port and everything served on it.                                                                                                                           |
| source client     | A network client that sends an encoded stream to a mount (Icecast `PUT`/`SOURCE` or the Shoutcast protocol).                                                              |
| Liquidsoap source | A source of the script running the server. It can feed a mount, or be a mount's fallback, when it carries encoded content in the mount's format ([mounts.md](mounts.md)). |
| mount             | A path on the server, such as `/live.mp3`, served to listeners while something feeds it.                                                                                  |
| mount output      | What listeners of a mount read: one harbor output per mount, fed by the mount's source client or Liquidsoap source, or by its fallback while it has neither.              |
| listener          | A client reading a mount.                                                                                                                                                 |
| admission         | The decision taken for every listener request before any byte of the stream: admit (optionally with a time limit and a preroll), refuse with a status, or redirect.       |
| parameters        | The one record the server is started with: plain values and callbacks, each read at a stated point ([parameters.md](parameters.md)). A reload replaces the whole record.  |
| callback          | A function-valued parameter: a decision (who may send, who may listen), a per-mount lookup, a renderer, an address transform (`forwarded_address`, `address_hash`).       |
| Icecast config    | An Icecast, RSAS or Icecast-KH XML configuration file. It is one way to build the parameters ([icecast-config.md](icecast-config.md)), never a second source of truth.    |

## Layers

1. **Server behaviour** — what happens to source clients, mounts and
   listeners, given the parameters. Specified by every document below except
   [parameters.md](parameters.md) and [icecast-config.md](icecast-config.md).
2. **Parameters** — the record the server takes, its callbacks, the server's
   methods, and reload ([parameters.md](parameters.md)).
3. **Icecast config translation** — how an Icecast, RSAS or Icecast-KH file
   becomes parameters, element by element
   ([icecast-config.md](icecast-config.md)).

A behaviour that Icecast, RSAS or Icecast-KH implement through an external
service — URL authentication, prerolls fetched from an ad server, event
notifications — is a callback in this model: the server calls it, and the
Icecast-compatible protocol is one ready-made implementation of it
([url-auth.md](url-auth.md)).

## Compatibility

The server mimics one implementation, chosen by the `compatibility` parameter
([parameters.md](parameters.md)): Icecast 2.5, Icecast 2.4, RSAS or
Icecast-KH. Where implementations behave differently, the server MUST behave
as the chosen one does, including where RSAS or Icecast-KH depart from
Icecast. A behaviour only other implementations have, and that conflicts with
nothing the chosen one does, is honoured too, so that a backend written for
any of them keeps working. The surfaces other software depends on are:

- the source protocols and the replies source clients get;
- the HTTP replies listeners and players get, statuses included;
- the URL authentication protocol, request fields and reply headers;
- the status, statistics and admin endpoints read by players, widgets and
  monitoring;
- Icecast, RSAS and Icecast-KH configuration files;
- the access, playlist and error log formats.

A departure from all three on one of these surfaces is stated, with its
reason, next to the rule.

Compatibility is best effort. The server covers the common behaviours of each
implementation — what typical source clients, players, backends and
configuration files rely on — and does not emulate every corner of Icecast,
RSAS or Icecast-KH. Where an implementation's behaviour is intricate (scoped
header rules, per-request origin logic, rarely used options), the
configuration translation maps the simple forms, warns on the rest, and names
the native parameter, callback or method that does it. Advanced behaviour is
the job of the native parameters, not of emulation.

## Departures by design

Compatibility is the default, not the goal. Where this server can do better
than Icecast, RSAS and Icecast-KH by design, it does, and says so: each such
rule is marked **Improvement** with what it gives the user, and a test asserts
the improved behaviour, not Icecast's. Two kinds of departures are deliberate.

- **The server understands content.** Icecast relays bytes it does not parse;
  this server demuxes every stream into packets and remuxes it per mount
  output. A mount therefore switches between its source, its fallback and back
  without disconnecting a listener, at a packet boundary, where Icecast moves
  listeners between mounts or drops them. Every feature that touches what a
  listener receives — fallbacks, source reconnects and format changes,
  overflow, prerolls, metadata — SHOULD use this to keep listeners connected
  and the stream valid, rather than reproduce a limitation Icecast has because
  it cannot read what it relays.
- **A clean cut where Icecast is convoluted.** Where an Icecast mechanism is
  hard to configure correctly or hard to reason about — chained reverse-proxy
  handling of forwarded addresses, the many TLS and listen-socket modes and
  their version-specific spellings — this server specifies one simpler
  mechanism that covers the use cases, and does not reproduce the original
  exactly. The configuration translation maps the common forms of the old
  mechanism onto the new one; a form it does not reproduce is warned on, or
  rejected when ignoring it would weaken security (an unsupported TLS mode is
  rejected rather than served as plain HTTP), always with the reason
  ([icecast-config.md](icecast-config.md)).

Either kind of departure is stated next to its rule, with the Icecast, RSAS or
Icecast-KH behaviour it replaces, and is not subject to `compatibility`.

## Requirements on the harbor output

The harbor output owns delivery. This server relies on it for:

- an admission decision per request, carrying a refusal (status, reason,
  headers), a time limit and preroll bytes
  ([connection.md §4](../../../../core/outputs/spec/harbor/connection.md#4-admission-predicate));
- reply headers computed per request, on refusals and on stream replies
  alike, for the CORS rules of [mounts.md](mounts.md) and
  [listeners.md](listeners.md);
- the listener record and connect/disconnect callbacks it reports
  ([observability.md](observability.md)).

Features listed in [future-work.md](future-work.md) need more (closing one
chosen listener, sending bytes before closing at a time limit, answering
`OPTIONS` on a mount).

## Documents

| #   | Document                               | Owns                                                                                                           |
| --- | -------------------------------------- | -------------------------------------------------------------------------------------------------------------- |
| 1   | [sources.md](sources.md)               | Source clients: admission, protocols, stream identity, content handling, lifecycle; source control endpoints   |
| 2   | [mounts.md](mounts.md)                 | Mounts: lifecycle, mount outputs, Liquidsoap sources, fallbacks, metadata, recording                           |
| 3   | [listeners.md](listeners.md)           | Listener admission: order of checks, decisions, limits, time limits, refusals, ready-made decisions, addresses |
| 4   | [url-auth.md](url-auth.md)             | The Icecast, RSAS and Icecast-KH URL authentication protocol as a ready-made listener decision                 |
| 5   | [prerolls.md](prerolls.md)             | Preroll locations, precedence, and the preroll cache                                                           |
| 6   | [observability.md](observability.md)   | Events, the listener record, statistics, status and admin endpoints, metrics, log files                        |
| 7   | [parameters.md](parameters.md)         | The parameter record and its types, defaults and read points; server methods; reload; errors                   |
| 8   | [icecast-config.md](icecast-config.md) | Translation of Icecast, RSAS and Icecast-KH configuration files into parameters                                |
| 9   | [future-work.md](future-work.md)       | Features considered and not specified yet, with what they would need                                           |

Each document from 1 to 8 ends with its conformance checks: what a test suite
MUST establish, stated by principle, with what is binding and what is
incidental.
