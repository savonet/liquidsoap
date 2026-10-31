# Sources: admission, content and lifecycle

A source is a client that sends an encoded stream to a mount. This document
states how the server admits it, what it answers, what it keeps of the stream
and what happens when the source goes away. The mount a source feeds is
specified in [mounts.md](mounts.md); delivery to listeners belongs to the
[harbor output](../../../../core/outputs/spec/harbor/icecast-server.md).

Setting names are defined in [settings.md](settings.md). Where implementations
differ, the `compatibility` setting (`"icecast-2.5"` default, `"icecast-2.4"`,
`"rsas"`, `"kh"`) chooses the one to mimic; a rule that depends on it says what
each value does.

## 1. Protocols

The server MUST accept a source over:

| Protocol  | Request                                                        | Port             |
| --------- | -------------------------------------------------------------- | ---------------- |
| HTTP PUT  | `PUT <mount>` with `Content-Type` and Basic credentials        | the server's     |
| SOURCE    | `SOURCE <mount> HTTP/1.0` (Icecast's legacy method), Basic     | the server's     |
| Shoutcast | the password on a line, then `icy-*` headers (the ICY source)  | the server's + 1 |

The server SHOULD also accept the `POST` method as `PUT`, and the legacy
`SOURCE <password> <mount> ICE/1.0` form. A client that sends `Transfer-Encoding: chunked`
is read as chunked.

**Shoutcast.** A Shoutcast source has no mount in its request. It feeds the
mount named by `shoutcast_mount` (recommended `/stream`, as Icecast's
`<shoutcast-mount>`), and the line may carry `user:password`. The server
answers `OK2`, `icy-caps:11` and an empty line, then reads the `icy-*`
headers. This rule applies under every `compatibility` value.

**Content type.** A `PUT` or `SOURCE` without `Content-Type` is refused
(see [§3](#3-replies)). A Shoutcast source without one is taken as `audio/mpeg`.

## 2. Admission

A source request is judged in this order. The first failing check decides the
reply.

1. **Reserved path.** The status pages (`/`, `/status.json`) and everything
   under `/admin`, `/admin.cgi` included, are never mounts. A source on one is
   refused. Rationale: a mount output on such a path would shadow the page.
2. **Credentials.** The user and password come from HTTP Basic credentials.
   The mount's `source_user` (recommended default `source`) and
   `source_password` apply, each falling back to the default mount's, and the
   password to the server's `source_password`. When the resulting password is
   empty, no credentials are required. Rationale: Icecast's per-mount
   `<username>`/`<password>` override the global source password.
3. **Mount taken.** A mount that already has a source refuses another. The
   check comes after authentication, so an unauthenticated client learns
   nothing about live mounts.
4. **`max_sources`.** When set, a source is refused while that many mounts have
   one. Unset means unlimited.

The decision of 2 may be taken by the `source_decision` function of the mount
instead (see [settings.md](settings.md)); steps 1, 3 and 4 always apply.

A mount that exists only as another mount's fallback has no source ([mounts.md
§1](mounts.md#1-lifecycle)): a source MAY take it.

## 3. Replies

| Refusal            | `icecast-2.5`       | `icecast-2.4`, `kh`, `rsas`   |
| ------------------ | ------------------- | ----------------------------- |
| bad credentials    | `401`               | `401`                         |
| mount taken        | `409`               | `403 Mountpoint in use`       |
| `max_sources`      | `503`               | `403 Too many sources`        |
| no `Content-Type`  | `400`               | `400`                         |
| unsupported type   | `415`               | `403`                         |
| reserved path      | `403`               | `403`                         |

A `401` MUST carry `WWW-Authenticate: Basic realm="Icecast2 Server"`, so source
clients that wait for the challenge before sending credentials (several do)
retry correctly. Rationale: Icecast's own realm; clients do not read it.
A refused source gets a short body and the connection is closed.

An admitted source is answered:

- `SOURCE` and Shoutcast: `200 OK` (Shoutcast: the `OK2` form above) at once;
- `PUT`: `100 Continue` when the client sent `Expect: 100-continue`, otherwise
  nothing until the first data is read, as Icecast does.

`rsas` and `kh` take the `icecast-2.4` column; they inherit its codes.

## 4. Stream identity

A source describes its stream with headers: `ice-name`, `ice-description`,
`ice-genre`, `ice-url`, `ice-bitrate`, `ice-public` over HTTP, or the `icy-*`
equivalents (`icy-name`, `icy-description`, `icy-genre`, `icy-url`, `icy-br`,
`icy-pub`) over Shoutcast and old clients.

The server MUST pass them to listeners as `icy-name`, `icy-description`,
`icy-genre`, `icy-url`, `icy-br` and `icy-pub` in the response of the mount.
When a source sends both forms the first one received wins. A value containing
a line break MUST be dropped: it would end the response head. The identity is
fixed when the mount output is created ([mounts.md §2](mounts.md#2-the-mount-output)).

## 5. Content

The server MUST NOT decode or re-encode a stream. It splits it into encoded
packets and writes them again with the container the source used (MP3, AAC,
Ogg with Opus, Vorbis or FLAC, or any container the source sent), so a source
is relayed at the cost of a copy. Rationale: listeners get what the source
produced, and a server holds many mounts.

- The container's `muxer_options` (a list per container name) apply to the
  mount's output. Their default is a server setting.
- The mount's MIME type is the type the source declared.
- A source that switches to another stream mid-connection (Ogg chaining, a
  codec change) MUST keep the mount alive. Whether a new link starts a new
  track is the output's ([harbor spec](../../../../core/outputs/spec/harbor/icecast-server.md)).
- A stream the server cannot parse is refused as unsupported ([§3](#3-replies))
  when detected at login, and otherwise ends the source like a disconnection.
- `dedicated_encoder` is a property of the mount output
  ([mounts.md §3](mounts.md#3-dedicated_encoder)).

## 6. Metadata

A source updates its mount's metadata in two ways.

**In stream.** Metadata the container carries (Ogg comments, ID3 or tags the
muxer exposes) updates the mount as it arrives, in order with the audio.

**Admin endpoint.** `GET /admin/metadata` (Icecast) and `GET /admin.cgi`
(Shoutcast), with `mode=updinfo`, `mount` (default `/` on `admin.cgi` is
replaced by `shoutcast_mount`), and `song`, or `title` and `artist`, `url`,
`charset`. Rules:

1. The request is authenticated as a source of that mount (Basic, or `user`
   and `pass` query parameters on `admin.cgi`); the admin user and password
   MUST also be accepted. Rationale: Icecast allows both.
2. Without `mode=updinfo` the reply is `400`. An unknown mount is `404`
   under `icecast-2.5` and `400` under the other values.
3. Only mounts whose container has no in-stream metadata take it: MP3, AAC,
   WAV and FLAC. Others get `405`.
4. `song` is read as the title. When `title` or `artist` is also given, `song`
   is dropped. `url` becomes the track's URL. Text is converted from
   `charset` (else the source's charset) to UTF-8.
5. The reply is `200`; Icecast's body is XML (`<iceresponse>`). Its wording is
   incidental.

The update reaches listeners and the playlist log as any metadata
([mounts.md §6](mounts.md#6-metadata), [observability.md](observability.md)).

## 7. Lifecycle

- **`source_timeout`** (recommended 30 s; Icecast's default is 10 s). A source
  that sends no data for this long is dropped as if it had disconnected. It is
  read when a source connects.
- **Disconnect.** When a source goes away, the mount keeps its listeners while
  its fallback has a source ([mounts.md §4](mounts.md#4-fallback)); otherwise
  the mount ends ([mounts.md §1](mounts.md#1-lifecycle)).
- **Reconnect.** A source that logs in again while the old connection is
  still open is refused as taken. The old connection's late disconnect MUST
  NOT take the mount from the new one.
- **Commands.** A mount's `on_connect_command` runs when its source logs in,
  `on_disconnect_command` when it leaves, each as a background process whose
  result is ignored. A command never blocks the stream.
- **Events.** The server raises a source-connect event (mount, format,
  streams, headers) and a source-disconnect event (mount). Their fields and
  ordering are in [observability.md](observability.md).
- **Log lines.** Login, exit and refusals are written to the error log with
  their reason ([observability.md](observability.md)).

## 8. Compatibility

Rules that depend on `compatibility`:

| Rule                      | Values                                                  |
| ------------------------- | ------------------------------------------------------- |
| refusal statuses (§3)     | `icecast-2.5` its own; the others `403`                 |
| unknown mount on metadata | `icecast-2.5` `404`; others `400`                       |

No other rule here differs between implementations.

## 9. Conformance

A test suite MUST establish:

- **Binding.** PUT, SOURCE and Shoutcast sources feed a mount; the order of
  checks (reserved, credentials, taken, `max_sources`); the status of each
  refusal per `compatibility`; `WWW-Authenticate` on `401`; no credentials
  needed when the password is empty; per-mount credentials override global;
  identity headers reach listeners under the `icy-*` names and a header with a
  line break does not; a stream is relayed byte-identical in its packets;
  admin metadata statuses and the `song`/`title` rule; a source silent for
  `source_timeout` is dropped; a late disconnect of a replaced source leaves
  the new one; the commands run once each way; the source events fire once.
- **Incidental.** Reply bodies and wording, the exact `source_timeout`
  tolerance, command start latency, log wording.
- Establish that tests ran: count the sources admitted and refused, and fail
  when zero.
