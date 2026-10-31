# Icecast server: specification

**Scope.** A streaming server that speaks Icecast's protocols: source clients
connect to it and send encoded streams to mount points, listeners connect to
it and receive those streams. It replaces an Icecast or RSAS server for the
software around it — source clients, players, authentication and ad backends,
log analysers, monitoring — while its policy is decided by the script that
runs it, or by an Icecast configuration file translated into that policy.

**Not in scope.** Delivery of bytes to listeners — sockets, bursts, buffers,
ICY framing, prerolls on the wire, time-limit enforcement, encoder modes — is
owned by the harbor output and specified in
[its own specification](../../../../core/outputs/spec/harbor/README.md). This
specification states what the server asks of it.

**Reader.** Someone implementing the server, or checking an implementation,
without reading its code.

## Model

| Term            | Meaning                                                                                                                                                                                         |
| --------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| server          | One listening port and everything served on it.                                                                                                                                                |
| source          | A client that sends an encoded stream to a mount: a source client (Icecast `PUT`/`SOURCE` or the Shoutcast protocol).                                                                          |
| mount           | A path on the server, such as `/live.mp3`. A mount exists while a source feeds it, or while it serves as another mount's fallback.                                                            |
| mount output    | What listeners of a mount read: one harbor output per mount, fed by the mount's source, or by its fallback's source while it has none.                                                        |
| listener        | A client reading a mount.                                                                                                                                                                       |
| admission       | The decision taken for every listener request before any byte of the stream: admit (optionally with a time limit and a preroll), refuse with a status, or redirect.                            |
| policy          | Everything the server decides for its operator: who may send to which mount, who may listen, limits, prerolls, fallbacks, logs. Policy comes from settings ([settings.md](settings.md)).        |
| setting         | One named value or decision function of the policy, with a default and a point at which it is read.                                                                                            |
| Icecast config  | An Icecast or RSAS XML configuration file. It is one way to produce settings ([icecast-config.md](icecast-config.md)), never a second source of truth.                                          |

## Layers

1. **Server behaviour** — what happens to sources, mounts and listeners, given
   the settings. Specified by every document below except
   [icecast-config.md](icecast-config.md).
2. **Settings** — one set of values and decision functions, each read at a
   stated point; a reload replaces the whole set ([settings.md](settings.md)).
3. **Icecast config translation** — how an Icecast or RSAS file becomes
   settings, element by element ([icecast-config.md](icecast-config.md)).

A behaviour that Icecast or RSAS implement through an external service — URL
authentication, prerolls fetched from an ad server — is a policy in this model:
the server asks a decision function, and the Icecast-compatible protocol is one
implementation of that function ([url-auth.md](url-auth.md)).

## Compatibility

The server mimics one implementation, chosen by the `compatibility` setting
([settings.md](settings.md)): Icecast 2.5, Icecast 2.4, RSAS or Icecast-KH.
Where implementations behave differently, the server MUST behave as the chosen
one does, including where RSAS or Icecast-KH depart from Icecast. A behaviour
only other implementations have, and that conflicts with nothing the chosen
one does, is honoured too, so that a backend written for any of them keeps
working.
The surfaces other software depends on are:

- the source protocols and the replies source clients get;
- the HTTP replies listeners and players get, statuses included;
- the URL authentication protocol, request fields and reply headers;
- Icecast and RSAS configuration files;
- the access, playlist and error log formats.

A departure from Icecast on one of these surfaces is stated, with its reason,
next to the rule.

## Documents

| #   | Document                                 | Owns                                                                                        |
| --- | ---------------------------------------- | ------------------------------------------------------------------------------------------- |
| 1   | [sources.md](sources.md)                 | Source admission, source protocols, stream identity, content handling, source lifecycle    |
| 2   | [mounts.md](mounts.md)                   | Mount lifecycle, mount outputs, fallbacks, in-stream metadata                               |
| 3   | [listeners.md](listeners.md)             | Listener admission: order of checks, limits, time limits, refusals, addresses, privacy     |
| 4   | [url-auth.md](url-auth.md)               | The Icecast/RSAS URL authentication protocol as a listener decision function               |
| 5   | [prerolls.md](prerolls.md)               | Preroll sources, precedence and caching                                                    |
| 6   | [observability.md](observability.md)     | Events, statistics, status and admin pages, log files                                      |
| 7   | [settings.md](settings.md)               | The settings, their defaults, when each is read, reload                                    |
| 8   | [icecast-config.md](icecast-config.md)   | Translation of Icecast and RSAS configuration files into settings                          |

Each document ends with its conformance checks: what a test suite MUST
establish, stated by principle, with what is binding and what is incidental.
