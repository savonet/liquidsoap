# Listeners: admission

This document owns what happens to a listener request between its arrival and
the first byte of the stream: the order of the checks, the decision record, the
replies, the limits, time limits, the ready-made decisions, the listener's
address and its privacy. Delivering the reply and the stream is the harbor
output's
([connection.md §4](../../../../core/outputs/spec/harbor/connection.md#4-admission-predicate)).
The URL authentication protocol is [url-auth.md](url-auth.md); prerolls are
[prerolls.md](prerolls.md); the listener record observers see is owned by
[observability.md](observability.md#2-the-listener-record). The parameters
named here are defined in [parameters.md](parameters.md). Where
implementations differ, the behaviour is the one the `compatibility`
parameter names; every rule that depends on it says what each value does.

## 1. The admission pipeline

A listener request reaches a mount that has a mount output, or one that
[mounts.md §1](mounts.md#1-lifecycle) creates for it because its fallback has
something to send. The server takes these steps, in order, and stops at
the first that refuses or redirects. The order is the same for every
`compatibility` value: Icecast 2.5, Icecast 2.4 and Icecast-KH all count
the server's clients when the client is created, before any authentication,
and check the mount's limit after it.

| #   | Step                                                                                                                      | Refusal or redirect                                                                                               |
| --- | ------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| 0   | the mount's output, created through its fallback chain when it has none ([mounts.md §6.1](mounts.md#61-chains-and-loops)) | `503 Service Unavailable` under `rsas` when the chain is followed past its bound; the others stop the chain there |
| 1   | `max_clients`                                                                                                             | limit refusal ([§4](#4-listener-limits))                                                                          |
| 2   | `listener_decision` ([§2](#2-the-listener-decision))                                                                      | the decision's refusal or redirect; `403 Forbidden` when it fails ([§2](#2-the-listener-decision))                |
| 3   | `max_listeners`, `fallback_when_full` ([§4](#4-listener-limits))                                                          | limit refusal, or a redirect                                                                                      |
| 4   | time limit and preroll handed to the harbor output ([§6](#6-time-limits))                                                 | none                                                                                                              |

- Each step MUST see the parameters as they are when the request is admitted:
  a reload applies at once ([parameters.md](parameters.md)).
- A listener refused or redirected is never counted as a listener: no connect
  event, no listener count, no access-log line for a served stream.
- A listener refused at step 3 after the decision admitted it is
  released ([§5](#5-release)), and so is one whose admission ends for any
  other reason before it is served (an invalid record, an output stopped
  meanwhile): every admitting decision ends in a served listener's release or
  in this one.
- Admissions of one mount MAY run concurrently. A limit MAY be exceeded by the
  number of admissions in progress at once; the server MUST NOT admit beyond
  that.
- The decision may block (it can call a backend). Waiting for it MUST NOT
  delay the streaming of other listeners.

## 2. The listener decision

`listener_decision` is a callback of the mount (its mount parameters, see
[parameters.md](parameters.md)) from a listener request to a decision record.
URL authentication ([url-auth.md](url-auth.md)) and the ready-made decisions
of [§7](#7-ready-made-decisions) are implementations; an operator MAY supply
another. A mount without one admits every request.

**Input.** The listener request:

| Field              | Content                                                                                                                                                               |
| ------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `id`               | The listener's identifier, unique on the mount output                                                                                                                 |
| `mount`            | The mount path, query string excluded                                                                                                                                 |
| `query`            | The query string as received, empty when none                                                                                                                         |
| `user`, `password` | The HTTP Basic credentials of the request, empty when it sent none                                                                                                    |
| `address`          | The listener's address, resolved as in [§8](#8-the-listeners-address), never hashed                                                                                   |
| `headers`          | The request headers, looked up by lowercase name                                                                                                                      |
| `protocol`         | The HTTP version of the request                                                                                                                                       |
| `listeners`        | The number of listeners on the mount, the new one excluded                                                                                                            |
| `user_listeners`   | The number of admitted listeners of the request's `user` on the mount, the new one excluded; `0` when the request has no user ([§7.3](#73-per-user-connection-limit)) |
| `server`           | `{hostname, port, instance}` ([parameters.md §5](parameters.md#5-callbacks))                                                                                          |
| `compatibility`    | The server's `compatibility` ([parameters.md §3.2](parameters.md#32-compatibility))                                                                                   |

The server reads `server`, `compatibility` and `user_listeners` once and hands
them in, so a ready-made decision, built outside the server and again at each
reload, is given no argument for them and cannot disagree with the server.

**Output.** A record whose fields are all optional and nullable (an absent
field and a null one mean the same). The first three are the harbor output's
admission record
([connection.md §4](../../../../core/outputs/spec/harbor/connection.md#4-admission-predicate));
the last two are the server's.

| Field        | Content                                                                                                                        |
| ------------ | ------------------------------------------------------------------------------------------------------------------------------ |
| `refuse`     | `{status, reason, headers?}`: refuse, or redirect when `status` is a redirect status and `headers` carry `Location`            |
| `time_limit` | Seconds after admission at which the listener is disconnected ([§6](#6-time-limits))                                           |
| `preroll`    | The preroll, as bytes ([prerolls.md](prerolls.md))                                                                             |
| `exempt`     | `true`: the mount's `max_listeners` and `fallback_when_full` do not apply; `max_clients` still does ([§4](#4-listener-limits)) |
| `on_release` | A closure called once, with the release record, when the listener is gone ([§5](#5-release))                                   |

A record without `refuse` admits. Helpers build refusals and redirects from a
status, a reason and headers (a redirect from a status and a URL), so a
decision does not assemble headers by hand ([parameters.md](parameters.md)).

**Validation.** The harbor output validates the record: a `refuse.status`
outside 300-599, a `refuse.reason` with a line break, an invalid header, or a
`time_limit` that is negative or not finite is a decision failure
([connection.md §4](../../../../core/outputs/spec/harbor/connection.md#4-admission-predicate)).
The server does not validate it again.

**Failure.** A decision that raises, or returns a record the harbor output
rejects, MUST refuse the listener with `403 Forbidden` and no `Location`; the
failure is logged. A decision MUST NOT admit by default when it cannot decide:
an authentication that cannot reach its backend refuses
(URL authentication has the one exception of [url-auth.md](url-auth.md#7-failures)).

- A decision that raises gets the server's `403`, with the configured headers
  ([§3.2](#32-per-reply-headers)); it returned no record, so nothing is
  released. A record the harbor output rejects gets its bare `403`: no header.
  The server sets `on_dropped` on every record it hands over, so a rejected
  record that admitted is released ([§5](#5-release)). Decisions return valid
  records, as the ready-made ones do.

## 3. Replies

The server builds the refusal or redirect with its final header list
([§3.2](#32-per-reply-headers)). The harbor output writes it: status line
`HTTP/<protocol> <status> <reason>`, the headers in order, `Content-Length: 0`,
an empty body, then closes the connection. Neither listener callback fires.

### 3.1 Statuses

| Outcome               | Status              | Headers                                                                                                                        |
| --------------------- | ------------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| Admitted              | `200 OK`            | the stream's, the harbor output's                                                                                              |
| Refused with a status | the chosen one      | the chosen headers; for `401` also the default challenge when none is chosen                                                   |
| Redirected            | the chosen redirect | `Location: <url>`                                                                                                              |
| Past a limit          | by `compatibility`  | none                                                                                                                           |
| Decision failed       | `403 Forbidden`     | the configured ones when the decision raised ([§2](#2-the-listener-decision)); none when the harbor output rejected the record |

- The default challenge is `WWW-Authenticate: Basic realm="Icecast2 Server"`.
  A `WWW-Authenticate` header is sent only when the status is `401`, or when
  the decision chose one. Icecast sends the default challenge on `401` only.
- A refusal always names its status; decisions that authenticate choose `401`
  themselves (the ready-made ones do).
- Past a limit, the status follows `compatibility`:

| `compatibility` | Past `max_clients`        | Past `max_listeners`      |
| --------------- | ------------------------- | ------------------------- |
| `"icecast-2.5"` | `503 Service Unavailable` | `503 Service Unavailable` |
| `"icecast-2.4"` | `403 Forbidden`           | `403 Forbidden`           |
| `"kh"`          | `403 Forbidden`           | `403 Forbidden`           |
| `"rsas"`        | `503 Service Unavailable` | `503 Service Unavailable` |

The reason phrase is incidental; the status is binding. Players retry on
`503` and not on `403`, so the status is observable to them. RSAS does not
document the status of its limits, so the `rsas` row is this server's rule, the
one of `icecast-2.5`.

### 3.2 Per-reply headers

A header the operator configures (`http_headers`, global and per mount) MAY
have a value computed from the request and from the status of the reply it is
sent on, instead of a fixed string. The entry shape, the evaluation (status
filter, `compute`, de-duplication) and the order are the server's and are
[parameters.md §4.2](parameters.md#42-headers)'s.

- The headers apply to every reply the server's mount output sends to a
  listener: the stream reply, a refusal, a redirect and a decision failure.
  The order and the precedence between the decision's headers, the server's
  and the mount's are [parameters.md §4.2](parameters.md#42-headers)'s.
- The `icy-*` stream identity headers of a stream reply come from the mount's
  current feeder ([sources.md](sources.md), [mounts.md](mounts.md)). The
  server builds them for each reply and hands them in as admit headers, so a
  listener joining after a feeder reconnect gets the new identity.
- The server builds each refusal, redirect and decision-failure reply with its
  final list. For a stream reply it hands the harbor output the list as admit
  headers, which the output appends to the `200` reply
  ([icecast-server.md §2](../../../../core/outputs/spec/harbor/icecast-server.md#2-what-the-server-asks-of-the-output)).

**CORS.** The ready-made header set `icecast.server.headers.cors`
([parameters.md](parameters.md#57-decisions-and-helpers)) computes the CORS
headers from the request's `Origin`. This section owns what it computes. It is
the operator's to include; the server adds none by itself, under every value.
The set depends on `request.compatibility`:

| `compatibility`         | CORS headers                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| ----------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `"icecast-2.5"`         | Only on a request that carries `Origin`. `Access-Control-Allow-Origin`: the request's `Origin` on a 2xx reply, `null` on a 4xx or 5xx reply, absent on other statuses. `Access-Control-Allow-Methods`: the methods the mount allows, on a 2xx reply only. `Access-Control-Expose-Headers`: `content-range, icy-br, icy-description, icy-genre, icy-name, icy-pub, icy-url`. `Access-Control-Max-Age`: `300`. `Access-Control-Allow-Headers`: `range, if-range` |
| `"rsas"`                | `Access-Control-Allow-Origin`: the request's `Origin`. `Access-Control-Expose-Headers`: `Content-Type, icy-metaint, icy-br, ice-audio-info, icy-description, icy-genre, icy-name, icy-samplerate, icy-url`.                                                                                                                                                                                                                                                    |
| `"icecast-2.4"`, `"kh"` | These servers have no `cors` header type, and the translation builds no set from one; an RSAS `<cors>` block still becomes the CORS set ([icecast-config.md §8](icecast-config.md#8-http-headers)). A script that includes the helper gets the `"icecast-2.5"` set.                                                                                                                                                                                            |

The helper's two arguments apply to the set under every `compatibility` value,
since they conflict with nothing:

- `origins` lists the origins allowed. A request whose `Origin` is not in the
  list gets no `Access-Control-Allow-Origin`. An empty list allows any origin.
- `expose` names are appended to `Access-Control-Expose-Headers`.

RSAS sends CORS headers on every response without configuration; the server
does not emulate that default: an operator who wants them includes the helper.
Per-request origin logic beyond the helper is a `compute` entry of the header
list ([parameters.md §4.2](parameters.md#42-headers)).

Icecast 2.5 answers an `OPTIONS` request on a mount with `204` and the same
headers. The server SHOULD do the same under `"icecast-2.5"` once the harbor
output can answer `OPTIONS` ([future-work.md](future-work.md)).

## 4. Listener limits

`max_clients` counts every listener on the server. `max_listeners` counts the
listeners of one mount (the mount's own value). Both are read at each
admission. A limit is reached when the count is at or above it; an unset
`max_listeners` is unlimited.

- A listener counts from its connect event to its disconnect event, the same
  events observers see ([observability.md](observability.md)).
- Admissions in progress are not counted; that is the concurrency allowance
  of [§1](#1-the-admission-pipeline).
- `max_clients` is checked before the decision, so an `exempt` listener
  cannot skip it. Icecast-KH checks it the same way; its slave
  exemption skips the mount's limit only.
- A listener whose decision set `exempt` skips `max_listeners` and
  `fallback_when_full`, and is still subject to `max_clients`. The exemption is
  for relays the operator's backend trusts.

**`fallback_when_full`.** When a mount is at `max_listeners`, its `fallback`
names a mount and `fallback_when_full` is set, the listener is not refused: it
is redirected. Mainline and Icecast-KH both have the option, so every
`compatibility` value honours it.

- The target is the first mount of the fallback chain
  ([mounts.md §6.1](mounts.md#61-chains-and-loops): at most 10 mounts, stopping
  at a loop) that has a mount output and is not at its own `max_listeners`.
  A full mount whose own `fallback_when_full` is set passes on to its
  fallback. A `fallback` that is a Liquidsoap source is not a target.
- The reply is `307 Temporary Redirect`. `Location` is the target's path,
  followed by `?` and the request's query string when it has one.
- The first admission is released with a duration of 0 ([§5](#5-release)), and
  the listener's request is admitted at the target as any other: the target's
  `listener_decision` runs again, and its limits apply. The mount the listener
  was redirected from does not count it.
- With no target, the limit refusal applies.

_Departure._ Icecast serves the listener from the fallback mount on the same
connection and authenticates once. Serving in place needs a harbor operation
that moves an admitted listener into another mount's output, the capability
`moveclients` also needs ([future-work.md](future-work.md)).
Until the harbor output has it, the server redirects, so a URL authentication
backend sees two requests.

## 5. Release

Every decision that admitted a listener MUST have its `on_release` closure
called exactly once when the listener is gone:

- when the listener disconnects after being served, with the release record
  below;
- when a later step refuses or redirects the listener after the decision
  admitted it (step 3), with a release record whose `duration` and
  `bytes_sent` are `0`;
- when the admission ends before the listener is served for any other reason
  (the harbor output rejected the record, or its mount output stopped
  meanwhile), with the same duration-0 release record. The server obtains
  this through the `on_dropped` it sets on every record it hands the harbor
  output
  ([connection.md §4](../../../../core/outputs/spec/harbor/connection.md#4-admission-predicate)),
  which also covers the failure between the decision and the connect event.

The release record is `{duration, bytes_sent, listeners, metadata}`
([parameters.md §5.7](parameters.md#57-decisions-and-helpers)): the session's
length in seconds and the bytes sent to the listener, the mount's listener
count at release with this listener included, and the mount's current metadata.
For a duration-0 release, which follows no connect event, `listeners` is the
mount's count without this listener, since it was never counted.
The server fills it, so that a closure built outside the server, such as the
URL authentication notification, needs nothing else from it.

A decision that refused or redirected is not released: it did not admit. The
closure is called without delaying the disconnect. Its failure is logged and
changes nothing. When decisions are composed ([§7](#7-ready-made-decisions)),
each one that admitted is released.

_Rationale._ Backends bill or count concurrent sessions from the pair
add/remove; a missing remove is a leaked session. Icecast releases every
listener its authentication admitted. RSAS reports the listener count at
release with the leaving listener still in it.

## 6. Time limits

A time limit disconnects a listener a number of seconds after admission,
whatever the stream is doing. The limit that applies is, in order:

1. the decision's own `time_limit`, when it gave one;
2. the mount's `max_listener_duration` (seconds), when above 0.

- A decision's limit of `0` disconnects the listener at once, as Icecast
  does: the listener is admitted (its connect event fires), then disconnected.
  It is released like any other admitted listener.
- A mount's `max_listener_duration` of `0` or unset means no limit.
- A decision's limit overrides the mount's, longer or shorter.
- The server hands the harbor output the decision's `time_limit` as given.
  A negative or non-finite one is a decision failure, which the harbor output
  enforces ([§2](#2-the-listener-decision)). The server computes a limit of its
  own only from the mount's `max_listener_duration`, which validation keeps
  finite and non-negative ([parameters.md §7](parameters.md#7-validation-and-errors)).
- Enforcement is the harbor output's
  ([delivery.md](../../../../core/outputs/spec/harbor/delivery.md)): the
  listener is cut at the first delivery pass after the deadline.

## 7. Ready-made decisions

These are `listener_decision` callbacks the server's library provides and the
Icecast configuration translation uses ([icecast-config.md](icecast-config.md)).
They use the reply and release mechanisms above. They read `compatibility`,
`server` and `user_listeners` from the request they are called with
([§2](#2-the-listener-decision)), never from an argument.

**Composition.** Decisions compose in order; authentication comes before the
per-user limit ([§7.3](#73-per-user-connection-limit)). The first that refuses or
redirects wins, and decisions that had admitted before it are released with
a duration of 0. When all admit, the records merge: the first `time_limit`
and the first `preroll` given win (an empty preroll counts as given),
`exempt` is set if any set it, and every `on_release` is kept.
`icecast.server.decision.all` owns this rule; the precedence of prerolls
([prerolls.md §5](prerolls.md#5-precedence)) is one use of it.

### 7.1 Access lists

Refuses by the listener's address ([§8](#8-the-listeners-address), never
hashed) and by `User-Agent`.

- **Files.** A deny list and an allow list, each a file with one entry per
  line; blank lines and lines starting with `#` are ignored. The file is read
  again when its modification time changes, checked at most every 10 seconds.
- **Match.** Mainline: an entry is an address, compared exactly. Icecast-KH:
  an entry is an address or a wildcard pattern (`*`, `?`, `[...]`).
  The server accepts patterns under every `compatibility` value: a literal is
  a pattern without wildcard, so one reading serves all.
- **Rule.** An address in the deny list is refused. Otherwise, when an allow
  list is configured, an address not in it is refused. Otherwise the listener
  is admitted. Deny beats allow.
- **Agents.** A third list, matched against `User-Agent` without case, refuses
  the listener when an entry matches (Icecast-KH's `deny-agents`; no other
  server has it, and it conflicts with nothing, so every `compatibility`
  value honours it).
- **Reply.** `403 Forbidden`, no challenge.

_Departure._ Icecast 2.5 and Icecast-KH close the connection without a reply,
and apply the lists to source clients and admin requests too, before
counting the client against `max_clients`. The harbor output always writes
a status, and the decision sees listeners only, after step 1. Reason: a
refusal is the only outcome a decision has; a bare close is the harbor
output's.

### 7.2 htpasswd

Authenticates a listener's Basic credentials against a file of
`user:hash` lines (blank lines and `#` lines ignored; read again when the
modification time changes). It is provided under every `compatibility`
value: Icecast 2.5, Icecast 2.4 and Icecast-KH have it. RSAS documents only
webhook authentication for listeners (its static user and password are for
source clients), so under `rsas` it is this server's rule, with the hash set
of `icecast-2.5`. The value is the request's `compatibility`.

| `compatibility`           | Hash accepted                                                                          |
| ------------------------- | -------------------------------------------------------------------------------------- |
| `"icecast-2.5"`, `"rsas"` | the 32-hex-digit MD5 of the password, or a `crypt` string starting `$1$`, `$5$`, `$6$` |
| `"icecast-2.4"`, `"kh"`   | the 32-hex-digit MD5 of the password                                                   |

An unknown user or a wrong password refuses with `401` and the default
challenge. A request without credentials refuses the same way, which is what
makes a browser prompt. The admitted record has no other field. Because this
decision composes before the per-user limit, a wrong password is a `401`
whether or not the user is connected.

### 7.3 Per-user connection limit

An internal helper, `connections_per_user(n)`, which the translation composes
([parameters.md §5.7](parameters.md#57-decisions-and-helpers)). It refuses a
listener when the request's `user_listeners` is at least `n`. The server counts
admitted listeners per user and mount, from their connect event to their
disconnect event, and hands the count in the request, so the helper holds no
state and a reload cannot lose it. The counted user is the request's user as
admitted: when a URL authentication reply renames it (Icecast-KH's
`ice-username`), the new name is counted. Admissions in progress are not
counted, as for every limit ([§1](#1-the-admission-pipeline)); Icecast also
counts pending admissions, and this is an accepted difference under that
allowance. Icecast and Icecast-KH both count one mount's clients. A request without credentials has
`user_listeners` of `0` and is never limited. A limit of 0 is no limit.

| `compatibility`           | Status                  |
| ------------------------- | ----------------------- |
| `"icecast-2.5"`, `"rsas"` | `429 Too Many Requests` |
| `"icecast-2.4"`, `"kh"`   | `403 Forbidden`         |

Icecast 2.5 has the limit as `connections-per-user`; Icecast 2.4 and KH
have only the limit of one (`allow_duplicate_users`, which answers `403`). RSAS
documents none, so under `rsas` the 2.5 limit is this server's rule.

Icecast-KH's `drop_existing_listener` (drop the older connection instead of
refusing the new one) is not implemented. Icecast master maps a legacy
`allow_duplicate_users=0` to `connections-per-user="0"`, which is unlimited; the
translation maps it to a limit of 1, as the option says
([icecast-config.md](icecast-config.md#51-authentication)).

## 8. The listener's address

The address the decision, the access log and observers see is the peer's
address, unless the server runs behind reverse proxies. The `proxy_ips`
parameter ([parameters.md](parameters.md)) lists the trusted proxies, as
addresses or CIDR ranges. The same address is used for every client: listeners,
source clients ([sources.md](sources.md)) and admin requests.

- `forwarded_address` set: its answer is the address, or the peer address when
  it answers `null`. It replaces the rest of this list
  ([parameters.md §5.3](parameters.md#53-the-address-hash)).
- `proxy_ips` unset: the peer address.
- Otherwise, when `proxy_ips` is not empty and the peer address is not in it,
  it is the client's address and `X-Forwarded-For` is ignored: anyone can send
  that header. An empty `proxy_ips` is the explicit opt-in to trust the header
  from any peer.
- Otherwise, `X-Forwarded-For` is read as a comma-separated list and walked
  from the right. Entries in `proxy_ips` are skipped; the first other entry is
  the client's address. With an empty list that is the rightmost entry. A
  header that is absent, malformed or made only of proxies yields the peer
  address.

Only the rightmost entries are trusted: each proxy appends the address it
received the connection from, while anything to the left of them can be
forged. The address is never a reverse-DNS name. The resolution is the same
for the decision, the logs and the events, so one listener has one address.

**Improvement (clean cut).** Icecast 2.5 binds a `trusted-proxy` list to each
listen socket and names other listen sockets in it; each trusted hop switches
the connection to the listen socket whose `client-ip` matches, with that
socket's own settings, and the header is walked until an untrusted address.
Icecast-KH takes a list of addresses (`x-forwarded-for`). This server has one
list and one rule, which cover a reverse proxy and a chain of them, and does
not reproduce the per-socket switching. A front whose addresses cannot be
listed, such as a CDN, uses `forwarded_address`. The configuration translation
maps both forms to the list
([icecast-config.md](icecast-config.md#31-listen-socket)). This is not subject
to `compatibility`.

## 9. Privacy

A listener's address is personal data. `address_hash` is a callback from an
address to the string observers see.

- The default is a 16-character hexadecimal digest of the address: stable
  across runs, so unique listeners can be counted. It is unkeyed, hence
  reversible by guessing an address; an operator who needs better SHOULD set a
  keyed one.
- An operator MAY set `address_hash` to the identity to see plain addresses.
  Nothing else turns hashing off.

The listener decision's input carries the raw address, so URL authentication
sends it as Icecast does. Where every other address appears, raw or hashed, is
[observability.md §12](observability.md#12-where-addresses-appear)'s.

The server MUST NOT put a raw request header in any observer output: headers
can carry the address (`X-Forwarded-For`).

## 10. Conformance

**Binding**

- Order: a request past `max_clients` never reaches the decision; the
  decision's refusal beats `max_listeners`; a request past `max_listeners`
  always reaches the decision, and is released when refused.
- Statuses of [§3.1](#31-statuses) per outcome and per `compatibility`;
  `WWW-Authenticate` exactly when [§3.1](#31-statuses) says; `Location` on
  redirects; `Content-Length: 0`; no connect event for a refused listener.
- Validation, owned by the harbor output: a refusal outside 300-599, with a
  line break in its reason or an invalid header, and a negative `time_limit`,
  are `403 Forbidden`; a decision that raises is `403` with the configured
  headers.
- Per-reply headers computed from the request and the status, on stream,
  refusal and failure replies; the CORS values per `compatibility`, with
  `origins` and `expose` honoured under every value.
- Exactly one `on_release` per admitted listener, none for a refused one,
  including the listener a limit refused after admission and one whose
  admission ended before being served (duration 0, `listeners` without it),
  with a release record carrying `duration`, `bytes_sent`, `listeners` (this
  listener included for a served one) and `metadata`.
- Header precedence is [parameters.md §4.2](parameters.md#42-headers)'s: no
  restatement here.
- The `icy-*` identity of a reply is the current feeder's: a listener joining
  after a feeder reconnect gets the new `icy-name`.
- Requests carry `server`, `compatibility` and `user_listeners`: a per-user
  limit keeps counting across a reload.
- Time limit: a decision's `0` disconnects at once; a mount's `0` never; the
  decision's limit beats the mount's.
- `exempt` skips `max_listeners` and `fallback_when_full`, not `max_clients`.
- Authentication before the per-user limit: a wrong htpasswd password is `401`
  whether or not the user is connected.
- `fallback_when_full`: a `307` to the first non-full mount of the chain, with
  the query string kept; the first admission released with duration 0; the
  target's decision runs a second time; no target gives the limit refusal.
- A mount whose output exists only through its live fallback admits a
  listener request.
- Under `rsas`, a listener request that follows a fallback chain past its bound
  is `503`.
- Ready-made decisions: access-list rule (deny beats allow, allow list
  closes), htpasswd hash formats per `compatibility`, per-user limit and
  status; composition order and release of earlier admissions.
- Address: the rightmost entry not in `proxy_ips`, only when the peer is itself
  in the list (**Improvement**: one list, no per-socket switching); unset list
  gives the peer, empty list trusts the rightmost entry, CIDR ranges match,
  `forwarded_address` replaces the walk and `null` gives the peer; the same
  address for listeners, source clients and admin requests; hashed in every
  observer output; raw to the decision.
- No raw header in any observer output.

The default `address_hash` is a 16-character hexadecimal digest: its length is
binding ([§9](#9-privacy)).

**Incidental**: reason phrases, the wording of logs, the exact second a limit
disconnects (within a delivery pass), the interval at which files are checked
for changes.

**Doubles.** A scripted decision (admit, refuse with each status, redirect,
raise, return invalid records); an authentication backend; a source whose
count of connected listeners the test sets.

**Something ran.** Each limit test asserts the number of refused and of
admitted listeners it saw, so a test that never reached a limit fails.
