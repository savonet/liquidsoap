# Accepting a listener

How an HTTP request reaches the output, is authenticated, answered, and turned
into a listener. Everything up to [§5](#5-admission) runs in the harbor
request task of that connection ([concurrency.md §1](concurrency.md#1-actors)).

## 1. Routing

On start the output registers one handler on its port for the verb `GET` and
the pattern `^<mount>$`, where `<mount>` is the `mount` parameter with a
leading `/` added when absent. The pattern is a PCRE regular expression built
by plain concatenation: characters of the mount are not escaped, so `.` in
`stream.mp3` matches any character, and a mount containing regex syntax is a
regex.

What the server does with it ([contracts.md §1](contracts.md#1-harbor-server)):

- the pattern is matched against the request path with the query string
  removed (everything up to the first `?` followed by at least one character);
- handlers on a port are tried in registration order and the first match
  wins; registering a pattern already present is accepted silently, the new
  handler going after the existing one;
- removal removes every handler on that port with the same verb and the same
  pattern text, whoever registered it;
- when a port holds no handler and no source any more, the server closes its
  listening sockets;
- a port is opened by the first registration on it, with that registration's
  transport; a later registration with a transport of a different name fails
  with `Port is already opened with a different transport`.

The server hands the handler: the protocol version (`"1.0"` or `"1.1"`), the
request headers with lowercased names, the query (named regex groups of the
pattern, then the query-string arguments), the socket, and the request path.

## 2. Request

Only `GET` is routed to the output. A `GET` carrying `Upgrade: websocket` with
`Sec-WebSocket-Protocol: webcast` is taken by the server's websocket path and
never reaches the output. The request body is ignored.

The output reads one request header, `icy-metadata`
([icy.md §1](icy.md#1-negotiation)).

## 3. Authentication

When `auth` is null, every request is admitted to the next step.

When `auth` is set, the request task first moves itself to the scheduler's
`Threaded` queue (the function may block), then extracts credentials:

1. If an `Authorization` header (name compared case-insensitively) is present
   and its value split on runs of spaces or tabs starts with the exact token
   `Basic` followed by a token, that token is base64-decoded and split into
   user and password at the first `:`.
2. Otherwise, if the query has a `pass` entry, the password is that value and
   the user is the query's `user` entry, or `""` when there is none.
3. Otherwise the request is refused as _no credentials_.

The user and the password are each converted to UTF-8 from an auto-detected
charset. The function is called with one record:

| Field      | Value                                                                                                                     |
| ---------- | ------------------------------------------------------------------------------------------------------------------------- |
| `address`  | Peer address without port; a reverse-DNS name instead when `settings.harbor.reverse_dns` is true and the lookup succeeds. |
| `login`    | The user.                                                                                                                 |
| `password` | The password.                                                                                                             |

`true` admits the request. `false` refuses it as _wrong credentials_. A
function that raises the error kind `error.icecast.server.mount_taken` gets
the 403 reply below; any other exception is caught by the output and the
connection is closed with no reply. Either refusal is logged as
`Listener <peer:port> failed to authenticate`.

Replies (bytes exact; the status line is `HTTP/1.0` whatever the request
version):

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
page with status `403 Mountpoint already taken`, the same
`WWW-Authenticate` line, and `Mountpoint in use`. The server closes the
connection after the page.

## 4. The response head

Built for each admitted request, before the listener exists:

```
HTTP/<protocol> 200 OK\r\n
Content-type: <mime>\r\n
[icy-metaint: <metaint>\r\n]
[<name>: <value>\r\n for each pair of `headers`, in order]
\r\n
```

- `<protocol>` echoes the request's version.
- `<mime>` is the resolved MIME type ([operator.md §2](operator.md#2-validation-at-construction)).
- The `icy-metaint` line is present exactly when ICY metadata was negotiated.
- Extra headers are written verbatim, with no validation or deduplication.
- There is no `Content-Length`, `Transfer-Encoding`, `Connection` or caching
  header: the body is the stream, delimited by the connection closing.

**Worked example.** Request `GET /live?x=1 HTTP/1.1` with header
`Icy-MetaData: 1`; MIME `audio/mpeg`, `metaint=8192`,
`headers=[("Cache-Control", "no-cache")]`. Response head, 89 bytes:

```
HTTP/1.1 200 OK\r\nContent-type: audio/mpeg\r\nicy-metaint: 8192\r\nCache-Control: no-cache\r\n\r\n
```

The response head is not part of the ICY byte count: the first metadata
boundary falls `metaint` bytes into the stream data that follows it.

## 5. Admission

In order:

1. Log `New listener connection from <peer:port>`.
2. Authenticate ([§3](#3-authentication)).
3. Take a snapshot of the output's shard array. If it is empty (the output
   was stopped meanwhile), reply `HTTP/<protocol> 503 Service Unavailable\r\n`
   ([findings.md](findings.md) on how this reply is sent).
4. Create the listener record ([operator.md §5](operator.md#5-the-listener-record))
   and the mode's per-listener state ([modes.md §1](modes.md#1-listener-state)).
   If the mode has no running encoder state, reply
   `HTTP/<protocol> 404 Not found\r\n` instead (same remark).
5. Run the `on_connect` handlers with the record.
6. Pick a shard round-robin: a per-output counter, incremented atomically,
   modulo the snapshot's length.
7. Queue the listener's initial bytes: the response head, then what the mode
   sends first ([modes.md §2](modes.md#2-initial-bytes)).
8. Put the socket in non-blocking mode.
9. Publish the listener: add it to the shard's member list.
10. If the shard's writer has been told to stop, mark the listener closed,
    close its socket, and finish.
11. Otherwise wake the shard ([delivery.md §2](delivery.md#2-wake-ups)), log
    `Listener <peer:port> connected`, move the task to the `Threaded` queue,
    and finish.

"Finish" means telling the server the handler has taken the socket over: the
server writes nothing and does not close it. From step 9 on, the socket
belongs to the shard's writer.

## 6. Failure paths

| Where                            | What happens                                                                                                        | `on_connect` / `on_disconnect` |
| -------------------------------- | ------------------------------------------------------------------------------------------------------------------- | ------------------------------ |
| No route matches                 | The server answers (not-found or its own admin endpoints).                                                          | neither                        |
| Authentication refused           | 401 or 403 page, connection closed ([§3](#3-authentication)).                                                       | neither                        |
| Authentication raised            | Logged (`Timeout for …`, `I/O error for …`, `Unix error for …`, or the exception); connection closed with no reply. | neither                        |
| Output stopping (steps 3–4)      | 503 or 404 reply.                                                                                                   | neither                        |
| Peer address unavailable         | The server answers its 500 reply.                                                                                   | neither                        |
| Steps 7–9 raise                  | Logged `Listener … failed to connect: …`; listener marked closed, socket closed.                                    | both                           |
| Writer already stopped (step 10) | Listener marked closed, socket closed.                                                                              | both                           |

Request-level faults never affect other listeners or the output.
