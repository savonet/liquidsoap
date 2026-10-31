# Icecast configuration translation

How an Icecast, RSAS or Icecast-KH XML configuration file becomes a set of
[settings](settings.md). The file is one way to build the set, never a second
source of truth: after translation the server knows only settings.

## 1. Scope and failure

1. Translation is **pure**: it reads the file and produces a complete set of
   settings. It opens no socket and writes no log file.
2. Settings the file does not mention take their defaults
   ([settings.md](settings.md)). Explicit overrides by the builder are applied
   by the builder after translation and are not merged by the server.
3. The root element MUST be `<icecast>`.
4. **Parse failures.** The file is rejected as a whole when:
   - it is not well-formed XML;
   - the root is not `<icecast>`;
   - a recognised element has a value of the wrong kind (a non-numeric
     `<port>`, a malformed `auth_header`);
   - the resulting set is invalid ([settings.md §3](settings.md#3-validation));
   - a feature that restricts access cannot be honoured (§5.4).
5. A rejected file at startup stops the server. A rejected file on reload is a
   reload failure ([settings.md §5.1](settings.md#51-validate-first)): the
   previous set stays in force, and the reason is reported to whoever triggered
   the reload and logged.
6. Departure from Icecast: mainline warns and keeps going on a malformed value.
   The server fails, because a typo in a limit or a password silently changing
   policy is worse than refusing to run.

## 2. Vocabulary

| Status              | Meaning                                                                                              |
| ------------------- | ---------------------------------------------------------------------------------------------------- |
| supported           | Translated to a setting as described.                                                                |
| not implemented     | Ignored, with a warning that names the element. May be supported later.                              |
| incompatible        | Ignored, with a warning, because the server's model has no such thing; the reason is given.          |

Every element or option the translation does not recognise MUST be ignored
with a warning naming it and its enclosing element. An ignored element MUST
NOT change the set.

Boolean values accept Icecast's forms: `1`, `true`, `yes`, `on` and `0`,
`false`, `no`, `off`. Numbers are decimal.

## 3. Root, hostname and listen-socket

| Element              | Status          | Translation                                                                                                   |
| -------------------- | --------------- | ------------------------------------------------------------------------------------------------------------- |
| `hostname`           | supported       | `hostname`.                                                                                                   |
| `listen-socket`      | supported       | See below.                                                                                                    |
| `port`, `bind-address` (root) | supported (Icecast 2.4 and KH form) | `port`, `bind_address`, when no `listen-socket` is present. |
| `source-password` (root, KH) | supported | `source_password`, when `<authentication>` has none.                                                    |
| `location`, `admin`, `server-id`, `server_id` | not implemented | Identity strings Icecast puts in replies.                                               |
| `fileserve`          | incompatible    | The server does not serve static files.                                                                       |
| `shoutcast-mount`    | not implemented | Shoutcast listeners' mount.                                                                                   |
| `relay`, `master`, `relays-on-demand`, `master-*` (§3.2), `directory`, `security`, `include` | incompatible or not implemented, §8 | |

### 3.1 listen-socket

| Child            | Status          | Translation                                                                                                      |
| ---------------- | --------------- | ---------------------------------------------------------------------------------------------------------------- |
| `port`           | supported       | `port`.                                                                                                          |
| `bind-address`   | supported       | `bind_address`.                                                                                                  |
| `tls`, `ssl`     | supported       | `tls`, see below.                                                                                                |
| `shoutcast-mount`, `shoutcast-compat` | not implemented | Shoutcast listener compatibility on the socket.                                              |
| `trusted-proxy` (2.5) | supported  | Appended to `proxy_ips`.                                                                                        |

- A server has **one** listening port. When several `listen-socket` elements
  are present, the first is used and a warning says that the others are
  ignored. Reason: one server is one port ([README](README.md#model)); a
  second port is a second server.
- `tls` values, in both generations, whatever `compatibility` says (they do
  not conflict):

  | Value                                                  | `tls`           |
  | ------------------------------------------------------ | --------------- |
  | `0`, `disabled`, `false`, `no`, `off`, absent          | off             |
  | `1`, `auto_no_plain`, `rfc2818`, `true`, `yes`, `on`   | on              |
  | `auto`, `rfc2817`                                       | off, with a warning: the server cannot serve plain and TLS on one port, and does not upgrade. |
  | anything else                                          | off, with a warning. |

  Reason for the two warned rows: refusing to start would break a file that
  works with Icecast; failing open to plain HTTP is safe because
  `tls_certificate` is then not required.
- When `tls` is on and no certificate is configured (§6), the set is invalid.

### 3.2 KH-only root elements

| Element                                           | Status          | Reason                                                                                                           |
| ------------------------------------------------- | --------------- | ---------------------------------------------------------------------------------------------------------------- |
| `master-redirect`, `max-redirect-slaves`, `redirect` (redirect-hosts) | incompatible | They redirect listeners to slave servers of a relay cluster. The server has no master/slave relationship. |
| `master-server`, `master-server-port`, `master-username`, `master-password`, `master-bind`, `master-update-interval`, `master-relay-auth`, `master-ssl-port`, `master` | incompatible | The server never pulls streams from another server. |
| `relay`, `relays-on-demand`                       | incompatible    | Relaying is not a server feature.                                                                                |
| `include`                                         | not implemented | The file is a single document.                                                                                   |

## 4. authentication and limits

### 4.1 authentication

| Element           | Status    | Translation        |
| ----------------- | --------- | ------------------ |
| `source-password` | supported | `source_password`. |
| `admin-user`      | supported | `admin_user`.      |
| `admin-password`  | supported | `admin_password`.  |
| `relay-user`, `relay-password` | incompatible | Relays do not exist. |

### 4.2 limits

| Element            | Status          | Translation                                                                                  |
| ------------------ | --------------- | -------------------------------------------------------------------------------------------- |
| `sources`          | supported       | `max_sources`.                                                                               |
| `clients`          | supported       | `max_clients`.                                                                               |
| `client-timeout`   | supported       | `client_timeout`, seconds (decimal allowed).                                                 |
| `source-timeout`   | supported       | `source_timeout`, seconds (decimal allowed).                                                 |
| `burst-size`       | supported       | `burst_size`, bytes. `0` disables bursts.                                                    |
| `burst-on-connect` | supported       | `0` sets `burst_on_connect` off. Obsolete in Icecast 2.5, kept by KH and 2.4.               |
| `queue-size`       | incompatible    | The server manages listener buffering itself ([harbor spec](../../../../core/outputs/spec/harbor/README.md)). |
| `header-timeout`, `body-timeout` | incompatible | Request headers are read by the HTTP layer with its own limits.                  |
| `workers`, `listen-backlog`, `connections-per-user` | not implemented | |

`burst-size` unset: `burst_size` keeps its default. A mount's own
`burst-size` (§5) overrides it.

## 5. mount

A `<mount>` is `type="normal"` (default when `type` is absent) or
`type="default"`. The default mount's values apply to every mount that has no
own value ([settings.md §1](settings.md#1-model)). With several default
mounts the first is used and a warning names the others. A `<mount>` of
another type is ignored with a warning. A normal mount without `mount-name`
is ignored with a warning. Mount names are matched exactly; the wildcard
patterns of Icecast's `mount-name` are not implemented.

| Element                 | Status          | Translation                                                                                                              |
| ----------------------- | --------------- | ------------------------------------------------------------------------------------------------------------------------ |
| `mount-name`            | supported       | The mount the entry applies to.                                                                                          |
| `username`              | supported       | `source_user`. Default `"source"`.                                                                                       |
| `password`              | supported       | `source_password` of the mount.                                                                                          |
| `max-listeners`         | supported       | `max_listeners`.                                                                                                         |
| `burst-size`            | supported       | `burst_size` of the mount.                                                                                               |
| `fallback-mount`, `fallback` (KH nested form with `<mount>` child) | supported | `fallback_mount`. The KH `rate` child is ignored with a warning.                                         |
| `hidden`                | supported       | `hidden`.                                                                                                                |
| `on-connect`, `on-disconnect` | supported | `on_connect_command`, `on_disconnect_command`.                                                                        |
| `http-headers`          | supported       | The mount's `http_headers` (§7).                                                                                         |
| `max-listener-duration` | supported       | `max_listener_duration`, seconds.                                                                                        |
| `intro`                 | supported       | `intro`.                                                                                                                 |
| `preroll`               | supported (RSAS) | `preroll`.                                                                                                              |
| `authentication`        | supported for `type="url"` | §5.1.                                                                                                         |
| `fallback-override`, `fallback-when-full` | incompatible when `0` | Value `1`, Icecast's default, is how the server behaves: listeners stay on their mount, which resumes when its source returns, and listeners over `max-listeners` are refused. `0` is ignored with an error-level log. |
| `dump-file`, `dump-file-*` | not implemented | Recording a stream to disk.                                                                                           |
| `public`, `stream-name`, `stream-description`, `stream-url`, `genre`, `bitrate`, `type` (mime), `subtype`, `listenurl`, `cluster-password` | not implemented | Directory (YP) registration. |
| `charset`, `mp3-metadata-interval`, `metadata-interval`, `ogg-passthrough`, `admin_comments_only`, `allow-url-ogg-metadata`, `allow-chunked`, `max-send-size`, `so-sndbuf`, `file-seekable`, `queue-size`, `min-queue-size`, `skip-accesslog`, `accesslog`, `preroll-log`, `ban-client`, `wait-time`, `linger-for`, `filter-theora`, `limit-rate`, `max-bandwidth`, `priority`, `source-timeout`, `intro-skip-replay`, `hijack` | not implemented | Per-mount tuning of the delivery layer. |
| `redirect`, `redirect-to` (KH), `no-mount`, `on-demand` | incompatible | Mount-level redirection to another server and relay behaviour. Listener redirection is done by a listener decision ([listeners.md](listeners.md)). |
| `max-stream-duration` (KH), `dump-file-size-limit`, `dump-file-time-limit` | not implemented | |

### 5.1 URL authentication

`<authentication type="url">` produces the mount's `listener_decision`, owned
by [url-auth.md](url-auth.md). Its `<option name= value=>` children:

| Option             | Translation                                                                                                              |
| ------------------ | ------------------------------------------------------------------------------------------------------------------------ |
| `listener_add`     | URL asked when a listener arrives.                                                                                       |
| `listener_remove`  | URL told when an admitted listener leaves.                                                                               |
| `auth_header`      | `name: value`, split at the first colon. A reply carrying that header, compared without regard to case, admits.          |
| `timelimit_header` | Name of the reply header carrying the time limit. A trailing colon, as Icecast writes `icecast-auth-timelimit:`, is dropped. |
| `username`, `password` | Credentials sent to `listener_add` and `listener_remove` (HTTP Basic). Used only when both are present.             |
| `mount_add`, `mount_remove`, `stream_start`, `stream_end`, `listener_auth_header` and other options | not implemented, ignored with a warning. |

- An `<authentication>` of a mount takes effect only when it has at least one
  of `listener_add` and `listener_remove`; otherwise the mount has no
  listener decision of its own (and inherits the default mount's).
- Header names and values from the file are compared lowercase.

### 5.2 Mount `hidden`

`hidden` is a boolean. Only the default mount and a mount's own `true` hide a
mount; a mount cannot un-hide what the default mount hides.

### 5.3 `intro` and `preroll`

Both are set when both are present in the file. Which wins at admission
depends on `compatibility` ([prerolls.md](prerolls.md)).

### 5.4 Unsupported authentication

A mount `<authentication>` whose `type` is not `url` (for example `htpasswd`)
protects listeners in Icecast. Ignoring it would open the mount to everyone,
so the file MUST be rejected (§1.4). Reason: a missing restriction is a
security failure that a warning in a log does not catch.

## 6. Paths and logging

### 6.1 paths

| Element                                               | Status          | Translation                                                                                         |
| ----------------------------------------------------- | --------------- | --------------------------------------------------------------------------------------------------- |
| `logdir`                                              | supported       | Directory of the log files named in `<logging>`.                                                    |
| `webroot`                                             | supported       | `webroot`, for prerolls only.                                                                       |
| `tls-certificate`, `ssl-certificate`, `ssl_certificate` | supported     | `tls_certificate`. Also read inside `<tls-context>` (Icecast 2.5). May contain the private key.     |
| `tls-key`, `ssl-private-key` (KH), inside `<tls-context>` | supported   | `tls_key`. When absent, the certificate file is expected to contain the key.                        |
| `x-forwarded-for` (KH)                                | supported       | Appended to `proxy_ips`.                                                                            |
| `basedir`, `pidfile`, `adminroot`, `mime-types`       | not implemented | |
| `allow-ip`, `deny-ip`, `ban-file`, `deny-agents`      | incompatible    | Access lists belong to a firewall or reverse proxy, or to a listener decision.                       |
| `ssl-allowed-ciphers`, `tls-allowed-ciphers`, `ssl-cafile` | not implemented | The TLS stack's defaults apply.                                                                  |
| `alias`                                               | incompatible    | The server has no static namespace to alias.                                                        |

### 6.2 logging

| Element         | Status          | Translation                                                                                           |
| --------------- | --------------- | ----------------------------------------------------------------------------------------------------- |
| `accesslog`     | supported       | `access_log`. Default name `access.log`.                                                              |
| `errorlog`      | supported       | `error_log`. Default name `error.log`.                                                                |
| `playlistlog`   | supported       | `playlist_log`. No default: absent means no playlist log.                                             |
| `loglevel`      | supported       | `log_level`: `1`/`error`, `2`/`warn`/`warning`, `3`/`info`/`information`, `4`/`debug`, case-insensitive. Any other value is ignored with a warning. |
| `logsize`       | supported       | `log_size`, in KiB.                                                                                   |
| `logarchive`    | supported       | `log_archive`.                                                                                        |
| `memorybacklog`, `accesslog_*`, `errorlog_*`, `playlistlog_lines` | not implemented | In-memory log views.                                                             |

- A log name is a file name under `logdir`. The name `-` is standard error
  and is not joined to `logdir`.
- When the file has no `logdir`, a named log file cannot be placed: only the
  `-` value takes effect, and a file name is ignored with a warning. Reason:
  the server has no install directory to default to, as Icecast has.
- Default names apply only when a file is translated. A set built without a
  file has no log files unless the builder names them.
- `logdir` and `webroot` MAY be relative; they are resolved against the
  process's working directory when the files are opened or read, not against
  the configuration file's directory. Reason: Icecast resolves them against
  its base directory, which the server does not have.

## 7. http-headers

`<http-headers>` (root) and `<http-headers>` in a mount are lists of
`<header name= value= type= status=>`. They produce the `http_headers` list
([settings.md §4.9](settings.md#49-header-lists)). The headers are sent on
listener replies; delivery is owned by [listeners.md](listeners.md).

- A header without `name` is ignored.
- `type` is `static` (default) or `cors`. `status`, when present, restricts
  the header to replies with that status.
- A `static` header is sent as given; without `value`, it is skipped.
- **CORS headers** (`compatibility` `"icecast-2.5"`, `"rsas"`): a `cors`
  header is sent only on a reply to a request that carries an `Origin`
  header. Its `value`, when given, is used as is. Otherwise:

  | Header                             | Default value                                                                  |
  | ---------------------------------- | ------------------------------------------------------------------------------ |
  | `Access-Control-Allow-Origin`      | the request's origin on a 2xx reply, `null` on a 4xx or 5xx reply, absent otherwise |
  | `Access-Control-Allow-Methods`     | the methods the resource allows, on a 2xx reply only                           |
  | `Access-Control-Allow-Headers`     | `range, if-range`                                                              |
  | `Access-Control-Expose-Headers`    | `content-range, icy-br, icy-description, icy-genre, icy-name, icy-pub, icy-url` |
  | `Access-Control-Max-Age`           | `300`                                                                          |

  A `cors` header with another name and no value is skipped with a warning.
- Under `"icecast-2.4"` and `"kh"`, which have no `cors` type, the `type`
  attribute is ignored with a warning and the header is `static`.
- An origin is echoed rather than `*` so that credentials and caches stay
  correct.

## 8. Elements not supported

| Element       | Status          | Reason                                                                                         |
| ------------- | --------------- | ---------------------------------------------------------------------------------------------- |
| `fileserve`   | incompatible    | No static file serving. Use an HTTP handler.                                                   |
| `relay`       | incompatible    | The server never pulls a stream.                                                               |
| `directory`   | not implemented | YP directory registration.                                                                     |
| `security`    | incompatible    | `chroot` and user change belong to the service manager.                                        |
| `master` and the KH master/slave elements (§3.2) | incompatible | No relay cluster.                                             |

## 9. Rules that depend on `compatibility`

No XML element sets `compatibility`. It is a server setting
([settings.md §4.1](settings.md#41-compatibility)), chosen by the operator
and supplied by whoever builds the set, because a file's dialect is
ambiguous: most elements are common to every implementation. A builder MAY
guess a value from a file (a `<preroll>` element suggests `"rsas"`, an
`<ssl-private-key>` or `<master-*>` suggests `"kh"`, a `<tls-context>`
suggests `"icecast-2.5"`, `<ssl-certificate>` under `<paths>` alone suggests
`"icecast-2.4"`) but the server does not.

| Rule                                                                                  | Where                           |
| ------------------------------------------------------------------------------------- | ------------------------------- |
| CORS header defaults and the `cors` type exist for `icecast-2.5` and `rsas`           | §7                              |
| Status of a listener refused for a listener limit                                     | [listeners.md](listeners.md)    |
| Whether `intro` or `preroll` wins when a mount has both                               | [prerolls.md](prerolls.md)      |

Every `tls` value form and every certificate location (§3.1, §6.1) is
accepted under every value: they do not conflict.

## 10. Conformance

**Binding**

- A file with a root other than `<icecast>`, with malformed XML, or with a
  non-numeric value for a numeric element, is rejected; on reload the previous
  set stays in force.
- A mount `<authentication>` of a type other than `url` rejects the file.
- Each element in §3 to §6 marked supported produces the setting named, with
  the default mount applying to mounts without their own value.
- Every unsupported element is ignored, produces a warning that names it, and
  leaves the set identical to the set without it.
- Several `listen-socket` elements: the first is used, the rest warn.
- `tls`: each value in the §3.1 table gives the `tls` shown; enabling TLS
  without a certificate gives an invalid set.
- `burst-on-connect` `0` and `burst-size` `0` disable bursts; both unset keep
  the default.
- Log names are joined to `logdir`; `-` is not; default names apply only to a
  translated file; a named file without `logdir` is ignored with a warning.
- CORS: no `Origin`, no CORS header; with `Origin`, defaults as in §7 under
  `"icecast-2.5"`, and `static` treatment under `"icecast-2.4"`.
- `auth_header` splits at the first colon and compares without regard to
  case; `timelimit_header` drops a trailing colon.
- Translation produces the same set for the same file, every time, and
  changes nothing outside the returned set.

**Incidental**

- Wording of warnings, the order in which they are emitted, the order in which
  independent elements are processed, how the file is read.
