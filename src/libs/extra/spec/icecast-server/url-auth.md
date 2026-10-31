# URL authentication

URL authentication is a ready-made listener decision
([listeners.md](listeners.md#2-the-listener-decision)) built by a helper from
the options below: for each listener request it asks a web service of the
operator's, and the reply becomes the decision record (`refuse`, `time_limit`,
`preroll`, `exempt`, `on_release`). It is Icecast's protocol, extended the way RSAS and Icecast-KH extend
it, so a backend written for any of them keeps working. Where they differ, the
`compatibility` parameter ([parameters.md](parameters.md)) names the behaviour; a
behaviour only another implementation has, and that conflicts with nothing in
the chosen one, is honoured too. [§10](#10-what-depends-on-compatibility)
lists the rules that depend on it.

## 1. Configuration

The helper, `icecast.server.decision.url_auth(~cache, options)`
([parameters.md](parameters.md#57-decisions-and-helpers)), takes a preroll cache
([prerolls.md](prerolls.md#3-the-cache)), which loads the prerolls the backend
chooses and bounds them by its `preroll_max_size`, and these options. The
names are Icecast's; the Icecast
translation passes every one of them from an `<authentication type="url">`
element ([icecast-config.md](icecast-config.md)). `server` and `port` in the
request are the server's `hostname` and `port` ([parameters.md](parameters.md)),
which the helper is given.

| Option                                                                                              | Meaning                                                                                                                                                                                                               |
| --------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `listener_add` (also `client_add`)                                                                  | URL of the admission request. Without it every listener is admitted                                                                                                                                                   |
| `listener_remove` (also `client_remove`)                                                            | URL of the release notification ([§8](#8-the-release-notification))                                                                                                                                                   |
| `username`, `password`                                                                              | Credentials of the requests ([§3](#3-credentials))                                                                                                                                                                    |
| `auth_header`                                                                                       | The admitting reply header line, written `name: value`. Default `icecast-auth-user: 1`; with `"kh"` the name alone, `icecast-auth-user:`                                                                              |
| `timelimit_header`                                                                                  | The time limit header, written `name:`. Default `icecast-auth-timelimit:`                                                                                                                                             |
| `header_auth`, `header_timelimit`, `header_message`, `header_alter_action`, `header_alter_argument` | The names of the new-style reply headers ([§4](#4-the-reply)); defaults `x-icecast-auth-result`, `x-icecast-auth-timelimit`, `x-icecast-auth-message`, `x-icecast-auth-alter-action`, `x-icecast-auth-alter-argument` |
| `action_add`, `action_remove`                                                                       | The `action` values sent. Defaults `listener_add`, `listener_remove`                                                                                                                                                  |
| `headers`, `header_prefix`                                                                          | Request headers to forward ([§2](#2-the-request))                                                                                                                                                                     |
| `timeout`                                                                                           | Seconds before a request is a failure. Default 6                                                                                                                                                                      |
| `presume_innocent`, `on_error_wait`                                                                 | Failure handling ([§7](#7-failures))                                                                                                                                                                                  |

Setting `auth_header` or `header_auth` disables the default of the other one,
and likewise `timelimit_header` and `header_timelimit`: a backend that
configured one style does not get the other by accident. With neither set
both defaults apply.

## 2. The request

For each listener the server sends one `POST` to `listener_add`, with
`Content-Type: application/x-www-form-urlencoded`
([listeners.md](listeners.md#1-the-admission-pipeline) gives its place in the
order of checks: after `max_clients`, before `max_listeners`).
The body is the fields below in this order, each name and value
form-encoded. All fields are always sent: they conflict with nothing, and a
backend reads the spelling it knows.

| Field             | Value                                                                                                       | From       |
| ----------------- | ----------------------------------------------------------------------------------------------------------- | ---------- |
| `action`          | `action_add`, default `listener_add`                                                                        | Icecast    |
| `server`          | `hostname`                                                                                                  | Icecast    |
| `port`            | The server's port                                                                                           | Icecast    |
| `server-instance` | A random UUID, fixed for the life of the server                                                             | Icecast    |
| `client`          | The listener's `id`                                                                                         | Icecast    |
| `mount`           | The requested path, then `?` and the query string as received when not empty                                | Icecast    |
| `user`, `pass`    | The request's HTTP Basic credentials, empty when none                                                       | Icecast    |
| `ip`              | The listener's address as resolved in [listeners.md §8](listeners.md#8-the-listeners-address), never hashed | Icecast    |
| `agent`           | `User-Agent`, `-` when absent                                                                               | Icecast    |
| `protocol`        | `http`                                                                                                      | RSAS       |
| `referrer`        | `Referer`, empty when absent                                                                                | RSAS       |
| `listener-count`  | Listeners on the mount, the new one excluded                                                                | RSAS       |
| `referer`         | Same value as `referrer`                                                                                    | Icecast-KH |
| `listeners`       | Same value as `listener-count`                                                                              | Icecast-KH |

Then, for each name in the `headers` option (comma-separated) that the request
carries, a field named `header_prefix` followed by the header name, with the
header's value. `header_prefix` defaults to empty, and to `ClientHeader-` with
`"kh"`.

## 3. Credentials

The request itself carries HTTP Basic credentials:

1. when the URL embeds `user:password@`, only those;
2. else the `username` and `password` options, when both are set;
3. else the listener's own credentials, when it sent some;
4. else none.

The same rule applies to the release notification.

## 4. The reply

A backend observes the following. Header names and values are compared without
case; values are trimmed. Only the headers below are read; the body is ignored
except under `"kh"` ([§5.4](#54-a-preroll)).

**Admission.** The listener is admitted when:

- the reply carries the `auth_header` line, **whatever its status**
  (Icecast's old style);
- or the reply is `2xx` and carries `header_auth: ok` (Icecast 2.5's new style).

With `"kh"`, an old-style line admits when its name matches the configured
one and its value does not start with `0`, and the value `withintro` admits and
gives a preroll ([§5.4](#54-a-preroll)). With the other values the whole line
must equal the configured one, and `icecast-auth-user: withintro` does not admit.

A reply that does not admit leads to a refusal ([§5.3](#53-refusal)). The
refusal always names its status, so it passes the validation of
[listeners.md §2](listeners.md#2-the-listener-decision).

## 5. Effects of the reply

### 5.1 Time limit

An admitted listener's time limit
([listeners.md §6](listeners.md#6-time-limits)) comes from the old-style
`timelimit_header` line, whatever the status, else from `header_timelimit` on
a `2xx` reply. The value is a non-negative integer of seconds. A value that is
not one is ignored, except with `"kh"`, where it means 60 seconds. `0`
disconnects the listener at once.

### 5.2 Redirects

A redirect sends the listener to another URL with the status line and
`Location` of [listeners.md §3](listeners.md#3-replies).

| Reply                                                                                                                          | Redirect       | Admitted listener too | Honoured with |
| ------------------------------------------------------------------------------------------------------------------------------ | -------------- | --------------------- | ------------- |
| `header_alter_action: redirect` or `redirect_see_other`, and `header_alter_argument: <url>`, on a `2xx` reply                  | 303 to `<url>` | yes                   | every value   |
| `redirect_temporary`                                                                                                           | 307            | yes                   | every value   |
| `redirect_permanent`                                                                                                           | 308            | yes                   | every value   |
| `icecast-auth-redirect: <url>` on a reply that does not admit (303 is this server's choice: RSAS does not document the status) | 303 to `<url>` | not applicable        | every value   |
| `Location: <url>`, whatever the status                                                                                         | 302 to `<url>` | yes                   | `"kh"` only   |

- `header_alter_action: noop` does nothing. Another action is ignored and
  logged. An action without argument, or the reverse, is ignored.
- `Location` is a general HTTP header, a backend may send it for its own
  reasons; so only `"kh"`, which defines it as a redirect, honours it.

### 5.3 Refusal

A reply that does not admit and does not redirect refuses the listener:

- `icecast-resp-status` (300-599, else ignored) and `icecast-resp-reason`
  give the status and reason; they are RSAS's and honoured for every value
  (the 300-599 range is this server's). Without them the status is `401`.
- `icecast-www-authenticate` is the `WWW-Authenticate` challenge. A `401`
  without one gets `Basic realm="Icecast2 Server"`; another status gets no
  challenge.
- With `"kh"`, a backend reply of status 403, or an `icecast-auth-message`
  that starts with `403`, refuses with `403` and the rest of the text as
  reason.
- The message (`header_message`, else `x-icecast-auth-message`, else
  `icecast-auth-message`) is logged. It is not sent to the listener.

### 5.4 A preroll

Where the preroll comes from depends on `compatibility`:

| `compatibility`                            | Source of the decision's preroll                                                            |
| ------------------------------------------ | ------------------------------------------------------------------------------------------- |
| `"kh"`                                     | The reply body, when the admitting header's value is `withintro`; else the `preroll` header |
| `"icecast-2.5"`, `"icecast-2.4"`, `"rsas"` | The `preroll` header                                                                        |

A header value is loaded with the cache and becomes the record's `preroll`
bytes. Which locations a backend may choose is
[prerolls.md §2](prerolls.md#2-locations)'s. A load that fails gives an empty
preroll, so the operator's own prerolls do not replace it
([prerolls.md §4](prerolls.md#4-failures)).

With `"kh"` the body bytes are the preroll as received (up to
`preroll_max_size`; a larger body gives an empty preroll). They are per
listener and are not cached. The `preroll` header is RSAS's; with `"kh"` it is
honoured when the reply has no `withintro`.

### 5.5 Other effects

- `icecast-slave` (any value other than `0`): the record's `exempt` is set,
  which skips the mount's `max_listeners` and `fallback_when_full`
  ([listeners.md §4](listeners.md#4-listener-limits)); `max_clients` still
  applies. It is Icecast-KH's header for relays, honoured for every
  value.
- `ice-username: <name>` (Icecast-KH): the listener's user name for the rest
  of the session is `<name>`: it is the `user` of the release notification.
  Honoured for every value.

## 6. Precedence

For one reply, the first rule that applies decides:

1. With `"kh"`: `Location` redirects.
2. A `2xx` alter-action redirect.
3. Not admitted: `icecast-auth-redirect`, then the custom or default refusal.
4. Admitted: with its time limit, preroll, exemption and user name.

Redirects and refusals discard the time limit and preroll.

## 7. Failures

A request that fails (cannot connect, TLS error, no reply within `timeout`,
unparsable reply) refuses the listener with `401` and the default challenge.
The backend being unavailable never admits, except as below.

- `presume_innocent: yes` (Icecast-KH, honoured for every value): a failure,
  and any reply of status 400-599, admits the listener with no time limit
  and no preroll.
- `on_error_wait` (seconds, Icecast-KH): after a failure, no request is sent
  for that long. Listeners meanwhile get the failure outcome (refused, or
  admitted with `presume_innocent`). The wait defaults to 30 with `"kh"`;
  with the other values there is no wait unless the option is set.

_Rationale._ A backend that is down is a flood of timeouts without the wait;
admitting on failure is the operator's explicit choice.

## 8. The release notification

The record's `on_release` closure
([listeners.md §5](listeners.md#5-release)) sends, when the listener is
released, a `POST` to
`listener_remove` with the credentials of [§3](#3-credentials). Its body is the fields
of [§2](#2-the-request) up to `agent` (the `action` is `action_remove`,
default `listener_remove`; `user` is the `ice-username` when given), then
`listener-count` (RSAS: the listeners on the mount at release, this one
included), `song` (RSAS: the mount's current song at release), `sent`
(Icecast-KH: the bytes sent to the listener) and `duration`: the session's
length in whole seconds, truncated, as the wire format has it. The reply is
ignored.
A failed notification is logged and not retried. A mount with `listener_remove`
and no `listener_add` admits every listener and tracks them.

The notification MUST NOT delay the listener's disconnect or other
listeners.

## 9. Not supported

| Feature                                                                   | Reason                                                                                                    |
| ------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------- |
| `Mountpoint:` reply header (Icecast-KH)                                   | Moves the listener to another mount; a mount's admission cannot hand a listener to another mount's output |
| `icecast-auth-user: hijack` (Icecast-KH)                                  | Relay takeover, outside listener admission                                                                |
| `redirect_limit` and following the backend's own redirects                | A backend reply that redirects is a failure; the admission has a fixed budget                             |
| Realigning a `withintro` body to MP3 or AAC frame boundaries (Icecast-KH) | The preroll is sent as it is ([prerolls.md](prerolls.md#1-a-preroll-is-bytes))                            |

An unsupported option is ignored with a warning when the configuration is
read ([icecast-config.md](icecast-config.md)). `mount_add`, `mount_remove` and
`stream_auth` are supported: they concern source clients and are specified in
[sources.md](sources.md#32-url-authentication-of-source-clients).

## 10. What depends on `compatibility`

| Rule                                  | `"icecast-2.5"`, `"icecast-2.4"`, `"rsas"` | `"kh"`                                   |
| ------------------------------------- | ------------------------------------------ | ---------------------------------------- |
| Default `auth_header`                 | `icecast-auth-user: 1`                     | `icecast-auth-user:` (any value but `0`) |
| `withintro` value                     | does not admit                             | admits, body is the preroll              |
| Unparsable time limit                 | ignored                                    | 60 seconds                               |
| `Location` header                     | ignored                                    | 302 redirect                             |
| Refusal by 403 reply or `403` message | `401` or chosen status                     | `403` with the text                      |
| Default `header_prefix`               | empty                                      | `ClientHeader-`                          |
| Default `on_error_wait`               | none                                       | 30 s                                     |

Everything else, including the extra request fields, the RSAS headers and
Icecast's alter actions, is honoured for every value.

## 11. A worked example

Mount `/live.mp3`, `listener_add` = `http://auth.example/check`,
`username` = `radio`, `password` = `s3cret`, `hostname` = `radio.example`,
port 8000. Listener 42 sends
`GET /live.mp3?token=x HTTP/1.1` with `User-Agent: VLC/3.0`, no credentials,
from 203.0.113.7; the mount has 3 listeners.

```
POST /check HTTP/1.1
Host: auth.example
Content-Type: application/x-www-form-urlencoded
Authorization: Basic cmFkaW86czNjcmV0

action=listener_add&server=radio.example&port=8000&server-instance=0f7c2d9e-6a41-4b0e-9b52-1c3a8e5d7f10&client=42&mount=%2Flive.mp3%3Ftoken%3Dx&user=&pass=&ip=203.0.113.7&agent=VLC%2F3.0&protocol=http&referrer=&listener-count=3&referer=&listeners=3
```

Reply:

```
HTTP/1.1 200 OK
icecast-auth-user: 1
icecast-auth-timelimit: 1800
preroll: https://ads.example/spot.mp3
```

The listener is admitted, disconnected after 1800 seconds, and hears the
preroll first. When it leaves after 95 seconds the server sends
`action=listener_remove`, the same fields up to `agent`, then
`listener-count`, `song`, `sent` and `duration=95`.
A reply `HTTP/1.1 200 OK` with no `icecast-auth-user` would refuse it with
`401` and `WWW-Authenticate: Basic realm="Icecast2 Server"`.

## 12. Conformance

**Binding**

- The request: method, content type, the field names and values and their
  order of [§2](#2-the-request), the credentials of [§3](#3-credentials);
  `ip` never hashed; `mount` with the query string.
- Admission: the old-style line admits at any status; the new style only at
  `2xx`; an `auth_header` option disables the new-style default.
- Time limit: parsing of integers, `0` immediate, ignored when invalid (60 s
  with `"kh"`).
- Redirect and refusal statuses and `Location`; the 401 challenge only for
  `401`; precedence of [§6](#6-precedence), per `compatibility` as in
  [§10](#10-what-depends-on-compatibility).
- A failing request refuses (or admits with `presume_innocent`); a release
  notification per admitted listener, with `duration`.
- No request is sent for a listener refused by `max_clients`; a listener
  refused by `max_listeners` after admission is released with duration 0.
- A backend preroll is loaded as [prerolls.md §2](prerolls.md#2-locations)
  states; a failed load gives an empty preroll.

**Incidental**: `server-instance`'s value, `User-Agent` of the request,
reason phrases, log wording, the interval before a request times out beyond
the `timeout` option.

**Doubles.** A backend whose reply (headers, status, body, delay, failure) the
test sets and whose requests it records; an ad server for prerolls.

**Something ran.** The backend double asserts the number of requests it
received, so a test whose listener never reached it fails.
