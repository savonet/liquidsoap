# Accepting a listener

How an HTTP request reaches the output, is authenticated and admitted,
answered, and turned into a listener. Everything up to publication
([§6](#6-admission)) runs in the harbor request task of that connection
([concurrency.md §1](concurrency.md#1-work)).

## 1. Routing

On start the output registers one route on its port: the verb `GET` and the
mount path, with a leading `/` added when absent. The route matches a request
whose path, query string removed, equals the mount path byte for byte. The
mount is a literal path: no character of it has a pattern meaning, so
`stream.mp3` matches only `/stream.mp3`.

**Duplicate mounts.** A port MAY hold several outputs on the same mount. The
most recently registered one serves the mount; the others are shadowed, and
a registration that shadows another MUST log a warning naming the mount and
port. When the serving output removes its registration, the most recent
remaining one serves again. An output's removal MUST remove its own
registration only, never another output's, whatever their mounts. This keeps
a mount served without a gap when a new output takes over before the old one
is gone ([icecast-server.md §4](icecast-server.md#4-lifecycle-of-a-mounts-output)).

What the server provides for this is a contract
([contracts.md §1](contracts.md#1-harbor-server)): the port is opened by the
first registration with that registration's transport, and a later
registration with a transport of a different name fails with
`Port is already opened with a different transport`.

The server hands the route: the protocol version (`"1.0"` or `"1.1"`), the
request headers with lowercased names, the query-string arguments, the
socket, and the request path.

## 2. Request

Only `GET` is routed to the output. A `GET` carrying `Upgrade: websocket` with
`Sec-WebSocket-Protocol: webcast` is taken by the server's websocket path and
never reaches the output. The request body is ignored.

The output reads one request header, `icy-metadata`
([icy.md §1](icy.md#1-negotiation)).

## 3. Authentication

When `auth` is null, every request goes on to the admission predicate.

When `auth` is set, it is user code and may block, so it MUST run on work
allowed to block ([concurrency.md §1](concurrency.md#1-work)). Credentials are
extracted first:

1. If an `Authorization` header is present and its value, split on runs of
   spaces or tabs, starts with the token `Basic` followed by a token, that
   token is base64-decoded and split into user and password at the first `:`.
2. Otherwise, if the query has a `pass` argument, the password is that value
   and the user is the `user` argument, or `""` when there is none. Players
   that can only take a URL pass credentials this way.
3. Otherwise the request is refused as _no credentials_.

The user and the password are each converted to UTF-8 from an auto-detected
charset. The function is called with one record:

| Field      | Value                                                                                                                     |
| ---------- | ------------------------------------------------------------------------------------------------------------------------- |
| `address`  | Peer address without port; a reverse-DNS name instead when `settings.harbor.reverse_dns` is true and the lookup succeeds. |
| `login`    | The user.                                                                                                                 |
| `password` | The password.                                                                                                             |

`true` passes. `false` refuses as _wrong credentials_. A function that raises
the error kind `error.icecast.server.mount_taken` gets the 403 reply below;
any other exception is logged and the connection is closed with no reply.
Either refusal is logged as `Listener <peer:port> failed to authenticate`.

Replies; the status line is `HTTP/1.0` whatever the request version, and the
server closes the connection after the page:

```
HTTP/1.0 401 Unauthorized\r\n
WWW-Authenticate: Basic realm="Liquidsoap harbor"\r\n
Content-Type: text/html\r\n
\r\n
<?xml version="1.0" encoding="utf-8"?>\n
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">\n
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en"><head><title>Liquidsoap source harbor</title></head><body><p>MESSAGE</p></body></html>
```

`MESSAGE` is `No login / password supplied.` for no credentials and
`Wrong Authentication data` for wrong ones. The mount-taken reply is the same
page with status `403 Mountpoint already taken`, the same `WWW-Authenticate`
line, and `Mountpoint in use`. Binding: the status codes, the
`WWW-Authenticate` challenge, and that the connection closes after the page;
the page text is incidental.

## 4. Admission predicate

When `accept` is set, it is called with the listener record
([operator.md §5](operator.md#5-the-listener-record)) of a request that
passed authentication, before `on_connect`. It is user code and runs on work
allowed to block, like `auth`.

- `true` admits the request.
- `false` refuses it with `HTTP/<protocol> 403 Forbidden\r\n`, a
  `Content-Length: 0` header and an empty body, then closes the connection.
  Neither callback fires. Logged as `Listener <peer:port> refused`.
- An exception is logged and treated as `false`.

The predicate is what lets a caller enforce listener limits, per mount or
across mounts, address bans or header checks, without asking for
credentials. Admissions of one output MAY run concurrently, so a limit
counted by the predicate can be exceeded by the number of admissions in
progress at once.

## 5. The response head

Built for each admitted request:

```
HTTP/<protocol> 200 OK\r\n
Content-type: <mime>\r\n
Connection: close\r\n
[icy-metaint: <metaint>\r\n]
[<name>: <value>\r\n for each pair of `headers`, in order]
\r\n
```

- `<protocol>` echoes the request's version.
- `<mime>` is the resolved MIME type ([operator.md §2](operator.md#2-validation-at-construction)).
- The `icy-metaint` line is present exactly when ICY metadata was negotiated.
- Extra headers are written as given, in order, with no deduplication; they
  were validated at construction.
- The body is the stream, delimited by the connection closing: there is no
  `Content-Length` or `Transfer-Encoding`. `Connection: close` states this to
  HTTP/1.1 clients, which would otherwise expect a persistent connection.

Binding: the status line, the header names and values, the head ending with
an empty line. Incidental: the order of the built-in lines and the case of
their names.

**Worked example.** Request `GET /live?x=1 HTTP/1.1` with header
`Icy-MetaData: 1`; MIME `audio/mpeg`, `metaint=8192`,
`headers=[("Cache-Control", "no-cache")]`. Response head, 108 bytes:

```
HTTP/1.1 200 OK\r\nContent-type: audio/mpeg\r\nConnection: close\r\nicy-metaint: 8192\r\nCache-Control: no-cache\r\n\r\n
```

The response head is not stream data: the first ICY boundary falls `metaint`
bytes into what follows it.

## 6. Admission

In order:

1. Log `New listener connection from <peer:port>`.
2. Authenticate ([§3](#3-authentication)).
3. If the output is not started, or has no running encoder state, reply
   `HTTP/<protocol> 503 Service Unavailable\r\n`, `Content-Length: 0`, an
   empty line, and close.
4. Create the listener record and run the admission predicate ([§4](#4-admission-predicate)).
5. Create the mode's per-listener state ([modes.md §1](modes.md#1-listener-state));
   if the encoder state stopped meanwhile, reply as in step 3.
6. Run the `on_connect` handlers with the record.
7. Queue the listener's initial bytes: the response head, then what the mode
   sends first ([modes.md §2](modes.md#2-initial-bytes)).
8. Put the socket in non-blocking mode.
9. Publish the listener to a delivery group and wake that group
   ([delivery.md §2](delivery.md#2-wake-ups)). Log
   `Listener <peer:port> connected`.
10. Hand the socket over: the server writes nothing more and does not close
    it. From publication on, the socket belongs to the delivery group.

A listener published to a group that is stopping, or whose output stopped
between steps 6 and 9, MUST still be marked closed and its socket closed,
with `on_disconnect`: publication and stop are ordered so that either the
stop's last pass sees the listener, or the publication sees the stop
([delivery.md §7](delivery.md#7-stopping)).

Time to first byte is bounded by the admission steps and one delivery pass:
the initial bytes are queued before publication and the publication wakes
the group, so a joiner never waits for the next streaming cycle.

## 7. Failure paths

| Where                           | What happens                                                                     | `on_connect` / `on_disconnect` |
| ------------------------------- | -------------------------------------------------------------------------------- | ------------------------------ |
| No route matches                | The server answers (not-found or its own admin endpoints).                       | neither                        |
| Unsupported protocol version    | The server answers 505 ([contracts.md §1](contracts.md#1-harbor-server)).        | neither                        |
| Authentication refused          | 401 or 403 page, connection closed ([§3](#3-authentication)).                    | neither                        |
| Authentication raised           | Logged; connection closed with no reply.                                         | neither                        |
| Output not started (steps 3, 5) | 503 reply.                                                                       | neither                        |
| Predicate refused or raised     | 403 reply ([§4](#4-admission-predicate)).                                        | neither                        |
| Peer address unavailable        | The server answers its 500 reply.                                                | neither                        |
| Steps 7–9 fail                  | Logged `Listener … failed to connect: …`; listener marked closed, socket closed. | both                           |
| Output stopped after step 6     | Listener marked closed, socket closed.                                           | both                           |

Every reply the output sends on a refusal is complete: a status line, the
headers stated, an empty line, then the connection closes. Request-level
faults never affect other listeners or the output.
