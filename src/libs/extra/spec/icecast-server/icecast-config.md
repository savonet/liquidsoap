# Icecast configuration translation

How an Icecast, RSAS or Icecast-KH XML configuration file becomes the
[parameters](parameters.md). `icecast.server.config.load(~compatibility="icecast-2.5",
path)` takes the path of a file and the `compatibility` to mimic, and returns the
parameter record. The file is one way to build the record, never a second
source of truth: after translation the server knows only parameters.

## 1. Scope and failure

1. Translation is **pure**: it reads the file and returns a complete record. It
   opens no socket and writes no log file. The transport value it builds reads
   its certificate when the server starts, not before.
2. `compatibility` is an argument, not an element: the translation branches on
   it (mount selection, the CORS type) and writes it into the record, which is
   where the server reads it ([parameters.md §3.2](parameters.md#32-compatibility)).
   A `compatibility` outside its four values raises
   `error.icecast.server.invalid_parameters`.
3. Parameters the file does not mention take their defaults
   ([parameters.md](parameters.md)). An override is a record substitution made
   by the script after `load` ([parameters.md §9](parameters.md#9-overrides)).
4. The root element MUST be `<icecast>`.
5. **Parse failures.** `load` raises `error.icecast.server.invalid_parameters`
   when:
   - the file is not well-formed XML;
   - the root is not `<icecast>`;
   - a recognised element has a value of the wrong kind (a non-numeric
     `<port>`, a malformed `auth_header`);
   - the resulting record is invalid ([parameters.md §7](parameters.md#7-validation-and-errors));
   - a feature that restricts access cannot be honoured
     ([§5.4](#54-unsupported-authentication));
   - a `tls` value the server cannot serve ([§3.1](#31-listen-socket)).
6. A rejected file at startup stops the server. A rejected file on reload is a
   reload failure ([parameters.md §8.1](parameters.md#81-validate-first)): the
   previous record stays in force, the error reaches whoever called `reload`
   and it is logged.
7. Departure from Icecast. Mainline warns on an out-of-range value and reads
   non-numeric text silently as 0. The translation fails, so a typo in a limit
   or a password never changes policy silently. This is not subject to
   `compatibility`.

**Best effort.** The translation maps the common, simple forms. For anything
intricate it warns, naming the native parameter, callback or method that does
it ([parameters.md](parameters.md)); advanced behaviour lives there.

## 2. Vocabulary

| Status          | Meaning                                                                                     |
| --------------- | ------------------------------------------------------------------------------------------- |
| supported       | Translated to a parameter as described.                                                     |
| not implemented | Ignored, with a warning that names the element. May be supported later.                     |
| incompatible    | Ignored, with a warning, because the server's model has no such thing; the reason is given. |
| future work     | Ignored, with a warning; listed in [future-work.md](future-work.md).                        |

Every element or option the translation does not recognise MUST be ignored with
a warning naming it and its enclosing element. An ignored element MUST NOT
change the record.

Boolean values accept Icecast's forms: `1`, `true`, `yes`, `on` and `0`,
`false`, `no`, `off`. Numbers are decimal. Units are converted to the
parameters' bytes and seconds: where Icecast's element is in KiB, the
translation multiplies by 1024.

## 3. Root, hostname and listen-socket

| Element                       | Status                              | Translation                                                 |
| ----------------------------- | ----------------------------------- | ----------------------------------------------------------- |
| `hostname`                    | supported                           | `hostname`.                                                 |
| `location`                    | supported                           | `location`.                                                 |
| `admin`                       | supported                           | `admin_contact`.                                            |
| `server-id`, `server_id`      | supported                           | `server_id`.                                                |
| `listen-socket`               | supported                           | See below.                                                  |
| `port`, `bind-address` (root) | supported (Icecast 2.4 and KH form) | `port`, `bind_address`, when no `listen-socket` is present. |
| `source-password` (root, KH)  | supported                           | `source_password`, when `<authentication>` has none.        |
| `shoutcast-mount` (root, KH)  | supported                           | `shoutcast_mount`.                                          |
| `fileserve`                   | incompatible                        | The server does not serve static files.                     |

### 3.1 listen-socket

| Child                 | Status       | Translation                                                                                                                                                                                                                                             |
| --------------------- | ------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `port`                | supported    | `port`.                                                                                                                                                                                                                                                 |
| `bind-address`        | supported    | `bind_address`.                                                                                                                                                                                                                                         |
| `tls`, `ssl`          | supported    | `transport`, see below.                                                                                                                                                                                                                                 |
| `shoutcast-mount`     | supported    | `shoutcast_mount`.                                                                                                                                                                                                                                      |
| `shoutcast-compat`    | incompatible | The Shoutcast protocol is always served on `port + 1`.                                                                                                                                                                                                  |
| `trusted-proxy` (2.5) | supported    | The `client-ip` (address or CIDR range) of each listen socket it names is appended to `proxy_ips` ([listeners.md §8](listeners.md#8-the-listeners-address)). A chain across sockets with different settings is not reproduced: use `forwarded_address`. |

- A server has **one** listening port. When several `listen-socket` elements
  are present, the first is used and a warning says that the others are
  ignored. Reason: one server is one port ([README](README.md#model)); a
  second port is a second server.
- `tls` values, in both generations, whatever `compatibility` says (they do not
  conflict):

  | Value                                         | `transport`                                                                                                                                                         |
  | --------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
  | `0`, `disabled`, `false`, `no`, `off`, absent | plain HTTP                                                                                                                                                          |
  | `1`, `rfc2818`, `true`, `yes`, `on`           | a TLS transport                                                                                                                                                     |
  | `auto`, `auto_no_plain`, `rfc2817`            | rejected: the server cannot serve plain and TLS on one port or upgrade in place. The error names the alternatives, `rfc2818` for TLS and `disabled` for plain HTTP. |
  | anything else, including an empty element     | rejected, with the reason.                                                                                                                                          |

  Reason: silently serving plain HTTP where the file asked for TLS, or the
  reverse, changes who can read the stream.

- A certificate configured ([§6.1](#61-paths)) while no `tls` value is given is
  rejected the same way: Icecast 2.5 (default `auto`) and Icecast-KH (whenever a
  certificate is loaded) then serve TLS and plain HTTP on one port. Set
  `rfc2818`, or `disabled` to serve plain HTTP. Same-port detection is in
  [future-work.md](future-work.md).
- **Improvement (clean cut).** The server has one `transport` value
  ([parameters.md §3.3](parameters.md#33-transport-and-tls)), where Icecast has
  a mode per listen socket and several spellings of each, and the Shoutcast
  port setting. The common spellings map as above; the ones that need several
  sockets or an in-place upgrade on one port are rejected with the reason. This
  is not subject to `compatibility`.
- A TLS transport is built with the factory of
  `settings.icecast.server.tls_transport` ([parameters.md §10](parameters.md#10-existing-global-settings)),
  from the certificate and key of [§6.1](#61-paths). When `tls` is on and no
  certificate is configured, the record is invalid.

### 3.2 KH-only root elements

| Element                                                                                                                                                                | Status          | Reason                                                                                          |
| ---------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------- | ----------------------------------------------------------------------------------------------- |
| `master-redirect`, `max-redirect-slaves`, `redirect` (redirect-hosts)                                                                                                  | future work     | They redirect listeners to slave servers of a relay cluster ([future-work.md](future-work.md)). |
| `master-server`, `master-server-port`, `master-username`, `master-password`, `master-bind`, `master-update-interval`, `master-relay-auth`, `master-ssl-port`, `master` | future work     | The server never pulls streams from another server ([future-work.md](future-work.md)).          |
| `relay`, `relays-on-demand`                                                                                                                                            | future work     | Relays ([future-work.md](future-work.md)).                                                      |
| `include`                                                                                                                                                              | not implemented | The file is a single document.                                                                  |
| `inactivity-timeout`, `header-timeout`                                                                                                                                 | incompatible    | Request headers are read by the HTTP layer with its own limits.                                 |

## 4. authentication and limits

### 4.1 authentication

| Element                        | Status      | Translation                                                           |
| ------------------------------ | ----------- | --------------------------------------------------------------------- |
| `source-password`              | supported   | `source_password`.                                                    |
| `admin-user`                   | supported   | `admin_user`.                                                         |
| `admin-password`               | supported   | `admin_password`.                                                     |
| `health-password` (RSAS)       | supported   | `health_password`; also read in a `<mount>`, where it is the mount's. |
| `relay-user`, `relay-password` | future work | Relays ([future-work.md](future-work.md)).                            |

### 4.2 limits

| Element                         | Status          | Translation                                                                                                   |
| ------------------------------- | --------------- | ------------------------------------------------------------------------------------------------------------- |
| `sources`                       | supported       | `max_sources`.                                                                                                |
| `clients`                       | supported       | `max_clients`.                                                                                                |
| `max-listeners` (KH)            | supported       | The default of every mount's `max_listeners` ([§5](#5-mount)).                                                |
| `client-timeout`                | supported       | `client_timeout`, seconds (decimal allowed). Unset: the parameter's default, 30 s, which is Icecast's.        |
| `source-timeout`                | supported       | `source_timeout`, seconds (decimal allowed). Unset: the parameter's default, 10 s, which is Icecast's.        |
| `burst-size`                    | supported       | `burst_size`, bytes. `0` disables bursts.                                                                     |
| `burst-on-connect`              | supported       | `0` sets `burst_on_connect` to `false`, under every `compatibility` value.                                    |
| `queue-size`                    | incompatible    | The server manages listener buffering itself ([harbor spec](../../../../core/outputs/spec/harbor/README.md)). |
| `body-timeout`                  | incompatible    | Request bodies are read by the HTTP layer with its own limits.                                                |
| `max-bandwidth` (KH)            | future work     | [future-work.md](future-work.md).                                                                             |
| `workers`, `listen-backlog`     | not implemented |                                                                                                               |
| `connections-per-user` (limits) | not implemented | The per-user limit is a mount decision ([§5.1](#51-authentication)).                                          |

`burst-size` unset: `burst_size` keeps its default. A mount's own `burst-size`
(§5) sets the mount's `burst_size`; the mount's value wins in both directions
([parameters.md §4.1](parameters.md#41-fields)).

## 5. mount

`mount` is the callback of [parameters.md §4](parameters.md#4-mount-parameters).
The translation builds a table of the file's `<mount>` elements once, at
`load`, and the callback is a lookup in it.

A `<mount>` is `type="normal"` (default when `type` is absent) or
`type="default"`. A `<mount>` of another type is ignored with a warning. A
normal mount without `mount-name` is ignored with a warning.

**Selection.** The translation reads the `compatibility` argument. For a
requested path, under `"icecast-2.5"`, `"icecast-2.4"` and `"rsas"`:

1. the normal mount whose `mount-name` equals the path, if any;
2. else, the first `type="default"` mount in file order whose `mount-name`
   pattern matches the path (`*`, `?` and `[…]`; `*` does not match `/`), or
   that has no `mount-name`.

With `"kh"`, every mount's `mount-name` is a pattern (`*` also matches `/`).
The exact name wins; among matching patterns the lowest `priority` wins, the
first in file order on a tie. A `type="default"` mount is also honoured: it
conflicts with nothing in KH.

**Merge.** The selected normal mount's record is completed with the matching
default mount's: each parameter takes the mount's own value when it has one,
else the default mount's. `http_headers` are the mount's own, then the default
mount's, as Icecast sends them. A path matching no mount gets the matching
default mount alone, else `null`. Mount names have no other normalisation.

| Element                                                                                                                                                                                                                                                                                                                                                                                                                                            | Status                             | Translation                                                                                                                                                                                                                            |
| -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `mount-name`                                                                                                                                                                                                                                                                                                                                                                                                                                       | supported                          | The path or pattern the entry applies to.                                                                                                                                                                                              |
| `username`                                                                                                                                                                                                                                                                                                                                                                                                                                         | supported                          | `source_user`. Default `"source"`.                                                                                                                                                                                                     |
| `password`                                                                                                                                                                                                                                                                                                                                                                                                                                         | supported                          | `source_password` of the mount.                                                                                                                                                                                                        |
| `max-listeners`                                                                                                                                                                                                                                                                                                                                                                                                                                    | supported                          | `max_listeners`.                                                                                                                                                                                                                       |
| `burst-size`                                                                                                                                                                                                                                                                                                                                                                                                                                       | supported                          | `burst_size` of the mount.                                                                                                                                                                                                             |
| `fallback-mount`, `fallback` (KH nested form with `<mount>` child)                                                                                                                                                                                                                                                                                                                                                                                 | supported                          | `fallback = {mount = …}`. The KH `rate` child is ignored with a warning.                                                                                                                                                               |
| `fallback-when-full`                                                                                                                                                                                                                                                                                                                                                                                                                               | supported                          | `fallback_when_full`. Icecast's default is `0`, and so is KH's. RSAS shows the element in an example only, without a documented meaning or default: it is read the same way.                                                           |
| `fallback-override`                                                                                                                                                                                                                                                                                                                                                                                                                                | ignored, see below                 | Accepted values `1`, `true`, `all`, `own` and `none`, `0`, `false`.                                                                                                                                                                    |
| `linger-for` (KH)                                                                                                                                                                                                                                                                                                                                                                                                                                  | supported                          | `linger`, seconds ([mounts.md §1.1](mounts.md#11-linger)).                                                                                                                                                                             |
| `wait-time` (KH)                                                                                                                                                                                                                                                                                                                                                                                                                                   | not implemented                    | Keeps the mount name reserved for that many seconds after its listeners have gone; unrelated to `linger`.                                                                                                                              |
| `hidden`                                                                                                                                                                                                                                                                                                                                                                                                                                           | supported                          | `hidden`.                                                                                                                                                                                                                              |
| `on-connect`, `on-disconnect`                                                                                                                                                                                                                                                                                                                                                                                                                      | supported                          | `on_connect`, `on_disconnect`: callbacks that run the command as a background process whose result is ignored.                                                                                                                         |
| `http-headers`                                                                                                                                                                                                                                                                                                                                                                                                                                     | supported                          | The mount's `http_headers` ([§8](#8-http-headers)).                                                                                                                                                                                    |
| `max-listener-duration`                                                                                                                                                                                                                                                                                                                                                                                                                            | supported                          | `max_listener_duration`, seconds.                                                                                                                                                                                                      |
| `max-history`                                                                                                                                                                                                                                                                                                                                                                                                                                      | supported                          | `max_history`; values outside 1 to 256 become 256, as Icecast does.                                                                                                                                                                    |
| `intro`, `preroll`                                                                                                                                                                                                                                                                                                                                                                                                                                 | supported (`preroll`: RSAS)        | [§5.3](#53-intro-and-preroll).                                                                                                                                                                                                         |
| `authentication`                                                                                                                                                                                                                                                                                                                                                                                                                                   | supported for `url` and `htpasswd` | [§5.1](#51-authentication).                                                                                                                                                                                                            |
| `dump-file`, `dump-file-size-limit`, `dump-file-time-limit`                                                                                                                                                                                                                                                                                                                                                                                        | supported                          | `dump_file` (the element's text), `dump_file_size_limit` (bytes), `dump_file_time_limit` (seconds). The `interpolation` attribute: `strftime` or absent keeps the name; `none` doubles every `%`; `uuid` is not implemented and warns. |
| `public`, `stream-name`, `stream-description`, `stream-url`, `genre`, `bitrate`, `type` (mime), `subtype`, `listenurl`, `cluster-password`                                                                                                                                                                                                                                                                                                         | not implemented                    | Directory (YP) registration ([future-work.md](future-work.md)).                                                                                                                                                                        |
| `charset`, `mp3-metadata-interval`, `metadata-interval`, `ogg-passthrough`, `admin_comments_only`, `allow-url-ogg-metadata`, `allow-chunked`, `max-send-size`, `so-sndbuf`, `file-seekable`, `queue-size`, `min-queue-size`, `skip-accesslog`, `accesslog`, `preroll-log`, `ban-client`, `filter-theora`, `limit-rate`, `priority` (outside `"kh"`), `source-timeout`, `intro-skip-replay`, `hijack`, `allow-direct-access`, `max-stream-duration` | not implemented                    | Per-mount tuning of the delivery layer.                                                                                                                                                                                                |
| `max-bandwidth`                                                                                                                                                                                                                                                                                                                                                                                                                                    | future work                        | [future-work.md](future-work.md).                                                                                                                                                                                                      |
| `redirect`, `redirect-to` (KH), `no-mount`, `on-demand`                                                                                                                                                                                                                                                                                                                                                                                            | incompatible                       | Mount-level redirection to another server and relay behaviour. Listener redirection is a listener decision ([listeners.md](listeners.md)).                                                                                             |
| `postroll`, `postroll-*` (RSAS)                                                                                                                                                                                                                                                                                                                                                                                                                    | future work                        | [future-work.md](future-work.md).                                                                                                                                                                                                      |
| `event-bindings`, `kartoffelsalat`                                                                                                                                                                                                                                                                                                                                                                                                                 | supported                          | [§9](#9-event-bindings).                                                                                                                                                                                                               |

**`fallback-override`.** Ignored. Listeners always return to the mount's own
source when it comes back, and never leave their mount: an **Improvement**
over Icecast ([mounts.md §6](mounts.md#6-fallback)), not subject to
`compatibility`. The values `1`, `true`, `all` and `own` ask for what the server
does and are accepted silently. The values `0`, `false` and `none` are ignored
with a warning that names that improvement: on Icecast they leave the
listeners on the fallback mount after the source returns, here they come back.
Icecast's default is `none` (`0`); KH's default is `1`; RSAS documents the
element (optional, on the main mount) without a default.

### 5.1 authentication

A mount `<authentication type="…">` builds the mount's `listener_decision`.
The decisions of one mount, with the file's access lists ([§6.1](#61-paths))
and the intro and preroll of [§5.3](#53-intro-and-preroll), are composed with
`icecast.server.decision.all` ([listeners.md §7](listeners.md#7-ready-made-decisions)),
in this order: access lists, `htpasswd`, URL authentication, per-user limit,
prerolls (`intro`, then `preroll`). The first refusal wins.
Authentication comes before the per-user limit so that a wrong password is a
`401` whether or not the user is connected.

**`type="url"`.** The decision is URL authentication,
[url-auth.md](url-auth.md#1-configuration). The `<option name= value=>` children
become its options, one for one, under the names url-auth.md gives:

| Option                                                                                              | Translation                                                                                                                                                                                                                                                             |
| --------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `listener_add`, `client_add`; `listener_remove`, `client_remove`                                    | URL of the admission request; URL of the release notification.                                                                                                                                                                                                          |
| `username`, `password`                                                                              | Credentials of the requests.                                                                                                                                                                                                                                            |
| `auth_header`                                                                                       | `name: value`, split at the first colon, kept as written (url-auth.md compares without case).                                                                                                                                                                           |
| `timelimit_header`                                                                                  | Name of the reply header carrying the time limit, with or without Icecast's trailing colon.                                                                                                                                                                             |
| `header_auth`, `header_timelimit`, `header_message`, `header_alter_action`, `header_alter_argument` | The names of the new-style reply headers.                                                                                                                                                                                                                               |
| `action_add`, `action_remove`                                                                       | The `action` values sent.                                                                                                                                                                                                                                               |
| `headers`, `header_prefix`                                                                          | Request headers to forward and their prefix.                                                                                                                                                                                                                            |
| `timeout`                                                                                           | Seconds before a request is a failure (decimal allowed).                                                                                                                                                                                                                |
| `presume_innocent`, `on_error_wait`                                                                 | Failure handling (boolean; seconds).                                                                                                                                                                                                                                    |
| `mount_add`, `mount_remove`                                                                         | URLs told when a source client connects and leaves: the pair of callbacks `icecast.server.source_callbacks.url` builds ([sources.md §7](sources.md#7-notifications)). With `on-connect` and `on-disconnect` commands on the same mount, the mount's callback runs both. |
| `stream_auth`                                                                                       | URL authenticating source clients: the mount's `source_decision` ([sources.md](sources.md)).                                                                                                                                                                            |
| any other option                                                                                    | Ignored, with a warning that names it.                                                                                                                                                                                                                                  |

A mount's `<authentication>` takes effect as a listener decision only when it
has `listener_add` or `listener_remove`; otherwise the mount has no listener
decision of its own and inherits the default mount's. `mount_add`,
`mount_remove` and `stream_auth` take effect without them. Option values are
kept as written; the comparison rules are url-auth.md's.

**`type="htpasswd"`.** The decision is the ready-made `htpasswd` decision
([listeners.md](listeners.md#72-htpasswd)), reading the file named
by the `filename` option. The file is read again when it changes. A listener
whose credentials are not in the file is refused with `401` and the default
challenge. The option `allow_duplicate_users` set to `0` adds a per-user limit of 1
on the mount (Icecast master reads it as `connections-per-user="0"`, which is
unlimited, a defect the translation does not copy). The option
`drop_existing_listener` (KH) is not implemented and warns. The `htpasswd` type
exists in mainline and in KH; under `rsas` it is this server's rule
([listeners.md §7.2](listeners.md#72-htpasswd)).

**Per-user limit.** The attribute `connections-per-user` of `<authentication>`
or of its `<acl>` child (Icecast 2.5), a positive number, adds
the per-user limit ([listeners.md](listeners.md#73-per-user-connection-limit))
with that number on the mount; `0` and `unlimited` add nothing.

### 5.2 Mount `hidden`

`hidden` is a boolean. The mount's own value wins over the default mount's,
`false` included.

### 5.3 intro and preroll

`intro` (Icecast) and `preroll` (RSAS) are local file names, relative to
`webroot` (§6.1), or URLs. The translation makes one
`icecast.server.preroll_cache` view for the file, over the process-wide store
that a reload keeps warm ([prerolls.md §3](prerolls.md#3-the-cache)). It
composes, last in the mount's decision, `fun (_) -> {preroll = cache(location)}`
for the `intro`, then the same for the `preroll`. The rule that the first
`preroll` given wins is `icecast.server.decision.all`'s
([listeners.md §7](listeners.md#7-ready-made-decisions)), which gives the
precedence of [prerolls.md](prerolls.md#5-precedence). Both are composed when
both are present, the default mount's values included; the base directory is
`webroot`.

### 5.4 Unsupported authentication

A mount `<authentication>` whose `type` is neither `url` nor `htpasswd`, and
any `<role>`-based authentication, protects listeners in Icecast. Ignoring it
would open the mount to everyone, so the file MUST be rejected
([§1](#1-scope-and-failure)). Reason: a missing restriction is a security
failure that a warning in a log does not catch.

## 6. Paths and logging

### 6.1 paths

| Element                                                                    | Status          | Translation                                                                                                             |
| -------------------------------------------------------------------------- | --------------- | ----------------------------------------------------------------------------------------------------------------------- |
| `logdir`                                                                   | supported       | Directory of the log files named in `<logging>`.                                                                        |
| `webroot`                                                                  | supported       | Directory of local `intro` and `preroll` files ([§5.3](#53-intro-and-preroll)). Unset: the process's working directory. |
| `tls-certificate`, `ssl-certificate`, `ssl_certificate`                    | supported       | Certificate of the TLS transport. Also read inside `<tls-context>` (Icecast 2.5). May contain the private key.          |
| `tls-key`, `ssl-private-key` (KH), inside `<tls-context>`                  | supported       | Private key of the TLS transport. When absent, the certificate file is expected to contain the key.                     |
| `x-forwarded-for` (KH)                                                     | supported       | Appended to `proxy_ips`.                                                                                                |
| `allow-ip`, `deny-ip`, `ban-file` (KH), `banfile` (KH), `deny-agents` (KH) | supported       | Access lists, below.                                                                                                    |
| `basedir`, `pidfile`, `adminroot`, `mime-types`, `nulldevice`              | not implemented |                                                                                                                         |
| `ssl-allowed-ciphers`, `tls-allowed-ciphers`, `ssl-cafile`                 | not implemented | The TLS stack's defaults apply.                                                                                         |
| `alias`                                                                    | incompatible    | The server has no static namespace to alias.                                                                            |

**Access lists.** `deny-ip` (and its KH aliases `ban-file`, `banfile`) names a
file of addresses to refuse, `allow-ip` a file of the only addresses to admit,
`deny-agents` (KH) a file of `User-Agent` values to refuse. The translation
builds one `icecast.server.decision.access_lists`
([listeners.md §7.1](listeners.md#71-access-lists)) from the three files and
composes it first in the decision of every mount, including those with no other
decision. The files' format, the matching and the departure from Icecast are
listeners.md's.

### 6.2 logging

| Element                                                           | Status          | Translation                                                                                                                                                                                                                        |
| ----------------------------------------------------------------- | --------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `accesslog`                                                       | supported       | `access_log`. Default name `access.log`.                                                                                                                                                                                           |
| `errorlog`                                                        | supported       | `error_log`. Default name `error.log`.                                                                                                                                                                                             |
| `playlistlog`                                                     | supported       | `playlist_log`. No default: absent means no playlist log.                                                                                                                                                                          |
| `loglevel`                                                        | supported       | `log_level`: Icecast level _n_ is Liquidsoap level _n + 1_. `1`/`error` gives 2, `2`/`warn`/`warning` gives 3, `3`/`info`/`information` gives 4, `4`/`debug` gives 5, case-insensitive. Any other value is ignored with a warning. |
| `logsize`                                                         | supported       | `log_size`, in bytes: the element's KiB times 1024.                                                                                                                                                                                |
| `logarchive`                                                      | supported       | `log_archive`.                                                                                                                                                                                                                     |
| `memorybacklog`, `accesslog_*`, `errorlog_*`, `playlistlog_lines` | not implemented | In-memory log views.                                                                                                                                                                                                               |

- A log name is a file name under `logdir`. The name `-` is standard error and
  is not joined to `logdir`.
- When the file has no `logdir`, a named log file cannot be placed: only the
  `-` value takes effect, and a file name is ignored with a warning. Reason: the
  server has no install directory to default to, as Icecast has.
- Default names apply only when a file is translated. A record built without a
  file has no log files unless the script names them.
- `logdir` and `webroot` MAY be relative; they are resolved against the
  process's working directory when the files are opened or read, not against the
  configuration file's directory. Reason: Icecast resolves them against its
  base directory, which the server does not have.

## 7. Events

[§9](#9-event-bindings).

## 8. http-headers

`<http-headers>` (root) and `<http-headers>` in a mount are lists of
`<header name= value= type= status=>`. They produce the `http_headers` lists
([parameters.md §4.2](parameters.md#42-headers)). Delivery is
[listeners.md](listeners.md)'s.

- A header without `name` is ignored.
- `status`, when present, becomes the entry's `status`.
- `type` is `static` (default) or `cors`.
- A `static` header without `value` is skipped. With one, it is an entry with
  `value`.
- A `cors` header becomes the entry of that name from the CORS set
  `icecast.server.headers.cors`, under `"icecast-2.5"` and `"rsas"`: RSAS has
  no `type="cors"`, so the attribute conflicts with nothing there. The set is
  the one the RSAS `<cors>` block builds; when both are present, the first
  entry of a name wins ([parameters.md §4.2](parameters.md#42-headers)). What it computes, and
  the effect of a `value`, is [listeners.md §3.2](listeners.md#32-per-reply-headers)'s.
  A `cors` header with a name the set does not have and no value is skipped
  with a warning.
- Under `"icecast-2.4"` and `"kh"`, which have no `cors` type, the `type`
  attribute is ignored with a warning and the header is `static`. An RSAS
  `<cors>` block below still becomes the CORS set under every value.
- **RSAS.** A `<custom-headers>` element at the root or in a `<mount>` holds
  `<add-header name= value=/>` (a `static` entry) and one `<cors>` block with
  repeatable `<allow-origin>` and `<expose-headers>`: these become one
  `icecast.server.headers.cors(~origins, ~expose)` set, under every
  `compatibility` value (the helper honours both arguments under all of them,
  and the block conflicts with nothing). A `<custom-headers>` in a
  `<listen-socket>` or `<vhost>`, and any per-request origin logic beyond the
  helper, is not translated: it warns and points to a `compute` header
  ([parameters.md §4.2](parameters.md#42-headers)).

## 9. event-bindings

`<event-bindings>` (root or in a mount; `kartoffelsalat` is its old name)
holds `<event type= trigger=>` elements. Each trigger in the comma-separated
`trigger` list becomes one event binding
([parameters.md §5.5](parameters.md#55-event-bindings)); a binding inside a
mount carries that mount.

| `trigger`                            | Event                                        | Note                                            |
| ------------------------------------ | -------------------------------------------- | ----------------------------------------------- |
| `source-connect`                     | `source_connect`                             |                                                 |
| `source-disconnect`                  | `source_disconnect`                          |                                                 |
| `format-metadata-changed`            | `metadata`                                   |                                                 |
| `source-listener-attach`             | `listener_connect`                           |                                                 |
| `source-listeners-changed`           | `listener_connect` and `listener_disconnect` | One binding for each.                           |
| `source-listeners-is-zero`           | `listener_disconnect`                        | Fires only when the mount has no listener left. |
| `source-flags-changed`, `dumpfile-*` | none                                         | not implemented                                 |

| `type`      | Status          | Handler                                                                                                                            |
| ----------- | --------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| `exec`      | supported       | Runs the `executable` option as a background process with the arguments `argvtype` selects and `EVENT_TRIGGER` in its environment. |
| `url`       | supported       | POSTs the event as form fields to the `url` option, with `username`, `password` and `action` as given.                             |
| `log`       | supported       | Writes a line to the server's log, with the `prefix` and `level` options (Icecast's level, converted as in [§6.2](#62-logging)).   |
| `terminate` | not implemented | A configuration should not stop the server.                                                                                        |

An event with no `type`, no `trigger` or an unknown `type` is ignored with a
warning, as Icecast logs an error and drops it.

## 10. Elements not supported

| Element                                          | Status          | Reason                                                        |
| ------------------------------------------------ | --------------- | ------------------------------------------------------------- |
| `fileserve`                                      | incompatible    | No static file serving. Use an HTTP handler.                  |
| `relay`                                          | future work     | [future-work.md](future-work.md).                             |
| `directory`, `yp-directory`                      | not implemented | YP directory registration ([future-work.md](future-work.md)). |
| `security`                                       | incompatible    | `chroot` and user change belong to the service manager.       |
| `master` and the KH master/slave elements (§3.2) | future work     | [future-work.md](future-work.md).                             |
| `postroll` (RSAS)                                | future work     | [future-work.md](future-work.md).                             |

## 11. Rules that depend on `compatibility`

No XML element sets `compatibility`. It is the `load` argument and a parameter
read at startup ([parameters.md §3.2](parameters.md#32-compatibility)), chosen
by the script, because a file's dialect is ambiguous: most elements are common to every
implementation. A script MAY guess a value from a file (a `<preroll>` element
suggests `"rsas"`, an `<ssl-private-key>` or `<master-*>` suggests `"kh"`, a
`<tls-context>` suggests `"icecast-2.5"`, `<ssl-certificate>` under `<paths>`
alone suggests `"icecast-2.4"`), but the server does not.

| Rule                                                  | Where                                                       |
| ----------------------------------------------------- | ----------------------------------------------------------- |
| Mount selection: patterns, `priority`, default mounts | [§5](#5-mount)                                              |
| The `cors` type exists for `icecast-2.5` and `rsas`   | [§8](#8-http-headers)                                       |
| htpasswd hashes, per-user limit status                | [listeners.md](listeners.md#7-ready-made-decisions)         |
| Status of a listener refused for a listener limit     | [listeners.md](listeners.md)                                |
| The `fallbacks` request on a mount that is not live   | [mounts.md §6.3](mounts.md#63-fallbacks-admin-request)      |
| `503` past the fallback-chain bound (`rsas` only)     | [mounts.md §6.1](mounts.md#61-chains-and-loops)             |
| URL authentication defaults                           | [url-auth.md](url-auth.md#10-what-depends-on-compatibility) |

Fallback chains, `fallback-when-full` and `fallback-override` otherwise do not
depend on it. Every `tls` value form, every certificate location, `htpasswd` and
`mount-name` defaults are accepted under every value: they do not conflict.

## 12. Conformance

**Binding**

- A file with a root other than `<icecast>`, with malformed XML, or with a
  non-numeric value for a numeric element, makes `load` raise
  `error.icecast.server.invalid_parameters`; on reload the previous record stays
  in force.
- A mount `<authentication>` of a type other than `url` or `htpasswd` rejects
  the file.
- Each element marked supported produces the parameter named; KiB values
  (`logsize`) are multiplied by 1024; Icecast log level _n_ becomes _n + 1_.
- Every unsupported element is ignored, produces a warning that names it, and
  leaves the record identical to the one without it.
- Several `listen-socket` elements: the first is used, the rest warn.
- `tls`: each value in the §3.1 table gives the transport or the rejection
  shown; so does a certificate with no `tls` value; enabling TLS without a
  certificate gives an invalid record. `trusted-proxy` and
  `x-forwarded-for` fill `proxy_ips`.
- `burst-on-connect` `0` and `burst-size` `0` disable bursts; both unset keep
  the default; a mount's `burst-size` beats the root's both ways.
- `fallback-when-full` unset is `0`; `fallback-override` is ignored, with a
  warning for `0`, `false` and `none` only; `fallback-mount` gives
  `fallback = {mount = …}`.
- Mount selection per §5, including the merge and the header order.
- Every url-auth option of §5.1 reaches the decision; `mount_add`, `mount_remove`
  and `stream_auth` reach the source callbacks and decision with no
  `listener_add`.
- `htpasswd`, `connections-per-user` and the access lists give the decisions
  described, access lists first.
- Log names are joined to `logdir`; `-` is not; default names apply only to a
  translated file; a named file without `logdir` is ignored with a warning.
- CORS: a `cors` header becomes the CORS set's entry under `"icecast-2.5"` and
  a `static` header under `"icecast-2.4"` and `"kh"`; RSAS's `<cors>` block fills
  the helper's `origins` and `expose` under every value; scoped
  `<custom-headers>` warns. Values are listeners.md's.
- `load` writes its `compatibility` argument into the record, and an invalid
  one raises `error.icecast.server.invalid_parameters`; mount selection follows
  it.
- Event bindings per §9: one binding per trigger, a mount's bound to its mount.
- `load` returns the same record for the same file, every time, and changes
  nothing outside the returned record.

**Incidental**

- Wording of warnings, the order in which they are emitted, the order in which
  independent elements are processed, how the file is read.

**Doubles.** Files on disk for the access lists and `htpasswd`; a fake backend
for URL authentication and event URLs.

**Something ran.** Count the elements translated and the warnings emitted per
fixture file, and fail when a fixture produced neither.
