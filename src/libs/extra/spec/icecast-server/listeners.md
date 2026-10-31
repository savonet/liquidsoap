# Listeners: admission

This document owns what happens to a listener request between its arrival and
the first byte of the stream: the order of the checks, the possible outcomes
and the reply each one gets, the limits, time limits, the listener's address
and its privacy. The reply format and the delivery of the stream are the
harbor output's
([connection.md §4](../../../../core/outputs/spec/harbor/connection.md#4-admission-predicate)).
The URL authentication protocol is [url-auth.md](url-auth.md); prerolls are
[prerolls.md](prerolls.md). Where implementations differ, the behaviour is the
one the `compatibility` setting names ([settings.md](settings.md)); rules that
depend on it say what each value does.

## 1. The admission pipeline

A listener request reaches a mount that has a mount output
([mounts.md](mounts.md)). The server takes these steps, in order, and stops
at the first that refuses or redirects.

| #   | Step                                  | Refusal or redirect                         |
| --- | ------------------------------------- | ------------------------------------------- |
| 1   | `max_clients` (see below)             | limit refusal ([§4](#4-listener-limits))    |
| 2   | `listener_decision` ([§2](#2-the-listener-decision)) | the decision's refusal or redirect |
| 3   | `max_listeners`                       | limit refusal ([§4](#4-listener-limits))    |
| 4   | time limit and preroll selection      | none: a failure only drops the preroll      |

Step 1 runs here only for `"icecast-2.5"`, `"icecast-2.4"` and `"kh"`, which
count the server's listeners before they ask any backend. With `"rsas"`
step 1 runs after step 2, together with step 3. Either way a listener refused
by a limit after the decision admitted it is released
([§5](#5-release-obligations)).

- Each step MUST see the settings as they are when the request is admitted:
  a reload applies at once ([settings.md](settings.md)).
- A listener refused or redirected is never counted as a listener: no
  connect event, no access-log line for a served stream, no listener count.
- Admissions of one mount MAY run concurrently. A limit MAY be exceeded by
  the number of admissions in progress at once; the server MUST NOT admit
  beyond that.
- The decision runs on work that may block (it can call a backend). Waiting
  for it MUST NOT delay the streaming of other listeners.

## 2. The listener decision

`listener_decision` is a setting ([settings.md](settings.md)): a function from
a listener request to an outcome. Per mount, else the default mount's.
URL authentication ([url-auth.md](url-auth.md)) is one implementation; an
operator MAY supply another. A mount without a decision admits every request.

**Input.** The decision sees:

| Field         | Content                                                                           |
| ------------- | --------------------------------------------------------------------------------- |
| `id`          | The listener's identifier, unique on the mount output                              |
| `mount`       | The mount path, query string excluded                                              |
| `query`       | The query string as received, empty when none                                      |
| `user`, `pass`| The HTTP Basic credentials of the request, empty when it sent none                 |
| `address`     | The listener's address, resolved as in [§7](#7-the-listeners-address), never hashed |
| `headers`     | The request headers, looked up by lowercase name                                   |
| `protocol`    | The HTTP version of the request                                                    |
| `listeners`   | The number of listeners on the mount, the new one excluded                         |

**Outcome.** Exactly one of:

- **admit**, with optionally a `time_limit` ([§6](#6-time-limits)), a
  `preroll` ([prerolls.md](prerolls.md)) and an `exempt` flag that skips the
  limits of steps 1 and 3;
- **refuse**, with a status in 300-599, a reason phrase and headers;
- **redirect**: a refusal whose status is a redirect status and whose headers
  carry `Location`.

A decision that fails (an error, or an outcome outside these shapes) MUST
refuse the listener. A decision MUST NOT admit by default when it cannot
decide: an authentication that cannot reach its backend is a refusal
(URL authentication has the one exception of [url-auth.md](url-auth.md#7-failures)).
The refusal a failing custom decision produces is `403 Forbidden`, as
the harbor output does.

**Release.** A decision that admitted is told when the listener leaves, or
when a later step refuses it ([§5](#5-release-obligations)).

## 3. Outcomes and replies

The harbor output writes the reply: status line
`HTTP/<protocol> <status> <reason>`, the headers of the outcome in order,
`Content-Length: 0`, an empty body, then closes the connection. Neither
listener callback fires.

| Outcome                              | Status             | Headers                                                   |
| ------------------------------------ | ------------------ | --------------------------------------------------------- |
| Admitted                             | `200 OK`           | the stream's, harbor's                                    |
| Refused, no status chosen            | `401`              | `WWW-Authenticate: Basic realm="Icecast2 Server"`         |
| Refused with a status                | the chosen one     | the chosen challenge; for `401` the default one if none   |
| Redirected                           | 303, 307, 308 or 302 | `Location: <url>`                                       |
| Past a limit                         | 503 or 403         | none                                                      |
| Decision failed                      | `403 Forbidden`    | none                                                      |

- A `WWW-Authenticate` header is sent with a refusal only when its status is
  `401`, or when the decision chose a challenge. Icecast sends the default
  challenge on `401` only.
- A refusal status outside 300-599 MUST be replaced by `401` for URL
  authentication and is a decision failure for any other decision.
- The limit status follows `compatibility`:

| `compatibility`          | Status past `max_listeners` or `max_clients`  |
| ------------------------ | --------------------------------------------- |
| `"icecast-2.5"`          | `503 Service Unavailable`                     |
| `"icecast-2.4"`          | `403 Forbidden`                               |
| `"kh"`                   | `403 Forbidden`                               |
| `"rsas"`                 | `503 Service Unavailable`                     |

The reason phrase is incidental; the status is binding. A listener that
players retry (for instance on `503`) makes the status observable to
monitoring tools.

## 4. Listener limits

`max_clients` counts every listener on the server. `max_listeners` counts the
listeners of one mount, its own value else the default mount's; both are read at each
admission. A limit is reached when the count is at or above it; a mount
without `max_listeners` is unlimited.

- A listener counts from its connect event to its disconnect event, the same
  events observers see ([observability.md](observability.md)).
- A listener marked `exempt` by the decision skips both limits. The limits
  protect the server's capacity; the exemption is for relays the operator's
  backend trusts.

## 5. Release obligations

Every decision that admitted a listener MUST be told exactly once when the
listener is gone:

- when the listener disconnects after being served, with the session's
  duration in whole seconds;
- when a limit refuses the listener after the decision admitted it
  (step 3, or step 1 under `"rsas"`), with a duration of 0.

A decision that refused or redirected is not told: it did not admit. The
notification is sent without delaying the disconnect. Its failure is logged
and changes nothing. The mount's decision at the time of the release is the
one told, so a reload applies to it.

*Rationale.* Backends bill or count concurrent sessions from the pair
add/remove; a missing remove is a leaked session. Icecast releases every
listener its authentication admitted.

## 6. Time limits

A time limit disconnects a listener a number of seconds after admission,
whatever the stream is doing. The limit that applies is, in order:

1. the decision's own `time_limit`, when it gave one;
2. the mount's `max_listener_duration` (seconds, its own value else the
   default mount's), when it is above 0.

- A decision's limit of `0` disconnects the listener at once, as Icecast
  does: the listener is admitted (its connect event fires), then disconnected.
  It is released like any other admitted listener.
- A mount's `max_listener_duration` of `0` or unset means no limit.
- A decision's limit overrides the mount's, longer or shorter.
- A negative or non-integer limit is no limit. The server MUST NOT fail the
  request for it.
- Enforcement is the harbor output's
  ([delivery.md](../../../../core/outputs/spec/harbor/delivery.md)): the
  listener is cut at the first delivery pass after the deadline.

## 7. The listener's address

The address the decision, the access log and observers see is the peer's
address, unless the server runs behind reverse proxies:

- `proxy_ips` unset and `forwarded_address` unset: the peer address.
- `proxy_ips` set: the `X-Forwarded-For` header is read as a comma-separated
  list and walked from the right. Entries in `proxy_ips` are skipped; the
  first other entry is the listener's address. A header that is absent or made
  only of proxies yields the peer address. An empty `proxy_ips` list trusts
  every proxy: the rightmost entry is the address.
- `forwarded_address` set: a function from the connection (peer address,
  headers, protocol, uri, `proxy_ips`) to an address, replacing the rule
  above. A function that gives no address yields the peer address.

Only the rightmost entries are trusted: each proxy appends the address it
received the connection from, while anything to the left of them can be
forged. The address is never a reverse-DNS name. This resolution is the same
for the decision, the logs and the events, so one listener has one address.

## 8. Privacy

A listener's address is personal data. `address_hash` is a function from an
address to the string observers see.

- The default `address_hash` is a 16-character hexadecimal MD5 digest of the
  address: stable across runs, so unique listeners can be counted. The
  function is unkeyed, hence reversible by guessing an address; an operator
  who needs better SHOULD set a keyed one.
- An operator MAY set `address_hash` to the identity to see plain addresses.
  Nothing else turns hashing off.

Raw addresses go to:

- the listener decision's input and its release notification
  ([§2](#2-the-listener-decision)), so URL authentication sends them as
  Icecast does;
- the input of `forwarded_address`;
- the server's own error log at information level and above (peer
  `address:port`).

Hashed addresses go to everything else: listener events, statistics, the
status and admin pages and the access log
([observability.md](observability.md)).

The server MUST NOT put a raw request header in a listener record: headers
can carry the address (`X-Forwarded-For`).

## 9. The listener record

Observers (events, statistics, admin page) see one record per listener
([observability.md](observability.md) owns the events and when they fire):

| Field          | Content                                                                          |
| -------------- | -------------------------------------------------------------------------------- |
| `id`           | Identifier, unique on the mount output                                           |
| `mount`        | The mount                                                                        |
| `ip`           | The resolved address of [§7](#7-the-listeners-address), through `address_hash`   |
| `user_agent`   | `User-Agent`, absent when not sent                                               |
| `referer`      | `Referer`, absent when not sent                                                  |
| `uri`          | The requested path                                                               |
| `protocol`     | The HTTP version                                                                 |
| `connected_at` | Connection time, seconds since the epoch                                         |
| `duration`     | Seconds connected: 0 at connect, the session's length at disconnect              |
| `bytes_sent`   | Bytes sent, the response head and any preroll included: 0 at connect, the total at disconnect |

## 10. Conformance

**Binding**

- Order: with `"rsas"`, a request past a limit still reaches the decision; with
  the other values a request past `max_clients` does not. A request past
  `max_listeners` always does. The decision's refusal beats a limit's.
- Statuses of [§3](#3-outcomes-and-replies) per outcome and per
  `compatibility`; `WWW-Authenticate` exactly when [§3](#3-outcomes-and-replies) says;
  `Location` on redirects; `Content-Length: 0`; no connect event for a refused
  listener.
- Exactly one release per admitted listener, none for a refused one, including
  the listener a limit refused after admission (duration 0).
- Time limit: a decision's `0` disconnects at once; a mount's `0` never; the
  decision's limit beats the mount's.
- `exempt` skips both limits.
- Address: rightmost non-proxy entry; hashed in every observer output; raw to
  the decision.
- No raw header in a listener record; `duration` and `bytes_sent` at connect
  are 0.

**Incidental**: reason phrases, the wording of logs, the exact second a
limit disconnects (within a delivery pass), the hash digest length.
