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

| #   | Step                                                             | Refusal or redirect                      |
| --- | ---------------------------------------------------------------- | ---------------------------------------- |
| 1   | `max_clients`                                                    | limit refusal ([§4](#4-listener-limits)) |
| 2   | `listener_decision` ([§2](#2-the-listener-decision))             | the decision's refusal or redirect       |
| 3   | validation of the decision ([§2](#2-the-listener-decision))      | `403 Forbidden`                          |
| 4   | `max_listeners`, `fallback_when_full` ([§4](#4-listener-limits)) | limit refusal, or a redirect             |
| 5   | time limit and preroll handed to the harbor output               | none                                     |

- Each step MUST see the parameters as they are when the request is admitted:
  a reload applies at once ([parameters.md](parameters.md)).
- A listener refused or redirected is never counted as a listener: no connect
  event, no listener count, no access-log line for a served stream.
- A listener refused at step 3 or 4 after the decision admitted it is
  released ([§5](#5-release)).
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

| Field              | Content                                                                             |
| ------------------ | ----------------------------------------------------------------------------------- |
| `id`               | The listener's identifier, unique on the mount output                               |
| `mount`            | The mount path, query string excluded                                               |
| `query`            | The query string as received, empty when none                                       |
| `user`, `password` | The HTTP Basic credentials of the request, empty when it sent none                  |
| `address`          | The listener's address, resolved as in [§8](#8-the-listeners-address), never hashed |
| `headers`          | The request headers, looked up by lowercase name                                    |
| `protocol`         | The HTTP version of the request                                                     |
| `listeners`        | The number of listeners on the mount, the new one excluded                          |

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
| `on_release` | A closure called once, with the session's duration in seconds, when the listener is gone ([§5](#5-release))                    |

A record without `refuse` admits. Helpers build refusals and redirects from a
status, a reason and headers (a redirect from a status and a URL), so a
decision does not assemble headers by hand ([parameters.md](parameters.md)).

**Validation.** The server MUST validate the record before the harbor output
sees it:

- `refuse.status` is in 300-599, `refuse.reason` has no line break and the
  headers are valid HTTP headers. A violation is a decision failure.
- `time_limit` is clamped as in [§6](#6-time-limits). It is never a failure.

**Failure.** A decision that raises, or returns a record violating the above,
MUST refuse the listener with `403 Forbidden` and no `Location`; the failure
is logged. A decision MUST NOT admit by default when it cannot decide: an
authentication that cannot reach its backend refuses
(URL authentication has the one exception of [url-auth.md](url-auth.md#7-failures)).

## 3. Replies

The harbor output writes the reply of a refusal or redirect: status line
`HTTP/<protocol> <status> <reason>`, the headers of the record in order, the
per-reply headers of [§3.2](#32-per-reply-headers), `Content-Length: 0`, an
empty body, then closes the connection. Neither listener callback fires.

### 3.1 Statuses

| Outcome               | Status              | Headers                                                                      |
| --------------------- | ------------------- | ---------------------------------------------------------------------------- |
| Admitted              | `200 OK`            | the stream's, the harbor output's                                            |
| Refused with a status | the chosen one      | the chosen headers; for `401` also the default challenge when none is chosen |
| Redirected            | the chosen redirect | `Location: <url>`                                                            |
| Past a limit          | by `compatibility`  | none                                                                         |
| Decision failed       | `403 Forbidden`     | none                                                                         |

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

A header the operator configures (`http_headers`, global and per mount; the
entry shape and the order are [parameters.md §4.2](parameters.md#42-headers))
MAY have a value computed from the request and from the status of the reply it
is sent on, instead of a fixed string; a callback returning null leaves the
header out.

- The headers apply to every reply the server's mount output sends to a
  listener: the stream reply, a refusal, a redirect and a decision failure.
  A header the decision chose is sent first; the rule that drops a configured
  entry of the same name is [parameters.md §4.2](parameters.md#42-headers)'s.
- This is a requirement on the harbor output: a refusal and a stream reply
  carry headers computed per request and per status
  ([icecast-server.md §2](../../../../core/outputs/spec/harbor/icecast-server.md#2-what-the-server-asks-of-the-output)).

**CORS.** The ready-made header set `icecast.server.headers.cors`
([parameters.md](parameters.md#57-decisions-and-helpers)) computes the CORS
headers from the request's `Origin`. This section owns what it computes. Under
`"icecast-2.5"` it is the operator's to include and the server adds none by
itself, under every value. The result depends on `compatibility`:

| `compatibility`         | CORS headers                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| ----------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `"icecast-2.5"`         | Only on a request that carries `Origin`. `Access-Control-Allow-Origin`: the request's `Origin` on a 2xx reply, `null` on a 4xx or 5xx reply, absent on other statuses. `Access-Control-Allow-Methods`: the methods the mount allows, on a 2xx reply only. `Access-Control-Expose-Headers`: `content-range, icy-br, icy-description, icy-genre, icy-name, icy-pub, icy-url`. `Access-Control-Max-Age`: `300`. `Access-Control-Allow-Headers`: `range, if-range`. A configured value replaces the computed one |
| `"icecast-2.4"`, `"kh"` | None. These servers send only the static headers.                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| `"rsas"`                | The allowed origins are the helper's `origins` list; `Access-Control-Expose-Headers` is `Content-Type, icy-metaint, icy-br, ice-audio-info, icy-description, icy-genre, icy-name, icy-samplerate, icy-url` followed by the helper's `expose` names;                                                                                                                                                                                                                                                          |

RSAS sends CORS headers on every response without configuration; the server
does not emulate that default: an operator who wants them includes the helper.
Per-request origin logic beyond the helper is a native header callback
([parameters.md §4.2](parameters.md#42-headers)).

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

- when the listener disconnects after being served, with the session's
  duration in seconds;
- when a later step refuses or redirects the listener after the decision
  admitted it (step 3 or 4), with a duration of 0.

A decision that refused or redirected is not released: it did not admit. The
closure is called without delaying the disconnect. Its failure is logged and
changes nothing. When decisions are composed ([§7](#7-ready-made-decisions)),
each one that admitted is released.

_Rationale._ Backends bill or count concurrent sessions from the pair
add/remove; a missing remove is a leaked session. Icecast releases every
listener its authentication admitted.

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
- The server clamps before the harbor output sees the record: a negative or
  non-finite `time_limit` is no limit. The server MUST NOT fail the request
  for it, and the harbor output, which treats a negative limit as an error,
  never receives one.
- Enforcement is the harbor output's
  ([delivery.md](../../../../core/outputs/spec/harbor/delivery.md)): the
  listener is cut at the first delivery pass after the deadline.

## 7. Ready-made decisions

These are `listener_decision` callbacks the server's library provides and the
Icecast configuration translation uses ([icecast-config.md](icecast-config.md)).
They use the reply and release mechanisms above; none needs the server's
internals.

**Composition.** Decisions compose in order; authentication comes before the
per-user limit ([§7.3](#73-per-user-connection-limit)). The first that refuses or
redirects wins, and decisions that had admitted before it are released with
a duration of 0. When all admit, the records merge: the first `time_limit`
and the first `preroll` given win, `exempt` is set if any set it, and every
`on_release` is kept. A preroll the translation of `intro` and `preroll`
adds is the last one composed ([prerolls.md](prerolls.md)).

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
of `icecast-2.5`.

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

Refuses a listener whose `user` already has `connections_per_user` listeners
on the mount, counted from the pair admit/release ([§5](#5-release)).
A request without credentials is never limited. A limit of 0 is no limit.

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
- Validation: a refusal outside 300-599, with a line break in its reason or an
  invalid header, is `403 Forbidden`; a negative `time_limit` is no limit and
  admits.
- Per-reply headers computed from the request and the status, on stream,
  refusal and failure replies; the CORS values per `compatibility`.
- Exactly one `on_release` per admitted listener, none for a refused one,
  including the listener a limit refused after admission (duration 0).
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
