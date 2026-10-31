# Settings

The server's whole policy is one **set of settings**: values and decision
functions. Every other document refers to a setting by the name given here.
This document owns each setting's meaning, default and read point, and the
reload protocol.

## 1. Model

1. A setting has a **name**, a **default** and a **read point** (§2).
2. The set is **complete**: whoever builds it decides every value. Overrides
   are not merged. A builder that wants "file, except the port" computes that
   set itself. A setting left unspecified takes its default.
3. A **per-mount** setting has a value for the default mount and, optionally,
   one for each named mount. A mount's value is its own when it has one, else
   the default mount's, else the setting's default. Header lists are the one
   exception: they accumulate, default mount first (§4.9).
4. A **decision function** setting (`listener_decision`, `source_decision`,
   `status_access`, `status_json`, `status_html`, `forwarded_address`,
   `address_hash`) is an opaque value to this document. What it receives and
   may answer is owned by the document linked in §4.

## 2. Read points

| Read point        | Meaning                                                                                       |
| ----------------- | --------------------------------------------------------------------------------------------- |
| startup           | Read once when the server starts. A reload cannot change it.                                  |
| use               | Read each time the server needs it: per request, source connection, log write or event.       |
| mount output      | Read when a mount output is created. Existing mount outputs keep the values they were given.  |

A rule that reads a setting at "use" MUST NOT cache it across uses, except as
§5 allows. A reader sees the whole set as it stood at one instant (§5.2).

## 3. Validation

A set is **valid** when:

- `compatibility` is one of `"icecast-2.5"`, `"icecast-2.4"`, `"rsas"`, `"kh"`;
- every other setting has a value of its kind (numbers in range, paths strings,
  decision functions callable).

An invalid set MUST be rejected at startup (the server does not start) and on
reload (§5.1). An invalid value MUST NOT be discovered lazily at the first
request that reads it.

## 4. The settings

### 4.1 Compatibility

| Setting         | Default        | Read point | Meaning                                                                                       |
| --------------- | -------------- | ---------- | --------------------------------------------------------------------------------------------- |
| `compatibility` | `"icecast-2.5"` | use       | Which implementation the server mimics: `"icecast-2.5"`, `"icecast-2.4"`, `"rsas"` or `"kh"`. |

Wherever implementations behave differently, the chosen one wins, including
where RSAS or KH depart from mainline. A behaviour only other implementations
have, and that conflicts with nothing the chosen one does, is honoured too. A
rule that depends on the value says so, and states what each value does.
Owner of each dependent rule: the document that states it. The dependent
rules known are collected in [icecast-config.md §9](icecast-config.md#9-rules-that-depend-on-compatibility)
and in the documents it links.

### 4.2 Startup only

| Setting        | Default | Meaning                                                                                  | Behaviour owned by                                   |
| -------------- | ------- | ---------------------------------------------------------------------------------------- | ---------------------------------------------------- |
| `port`         | 8000    | TCP port the server listens on, for sources and listeners alike.                         | [sources.md](sources.md), [listeners.md](listeners.md) |
| `bind_address` | unset   | Local address to bind. Unset: the platform's default, all interfaces.                    | the same                                             |
| `tls`          | off     | Serve TLS on the port. When on, `tls_certificate` MUST be set, or the set is invalid.    | the same                                             |

### 4.3 Server-wide, read at use

| Setting                | Default                 | Meaning                                                                                                                       | Behaviour owned by                                         |
| ---------------------- | ----------------------- | ----------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------- |
| `tls_certificate`      | unset                   | Certificate file (it MAY hold the private key). Read again on reload and every `tls_reload_interval` (§5.5).                  | this document §5.5                                         |
| `tls_key`              | unset                   | Private key file, when not in the certificate file.                                                                           | this document §5.5                                         |
| `hostname`             | `"localhost"`           | Name the server calls itself in what it tells others (URL authentication `server` field, status pages).                      | [url-auth.md](url-auth.md), [observability.md](observability.md) |
| `webroot`              | unset                   | Directory against which local preroll and intro files are resolved. Not used to serve static files.                          | [prerolls.md](prerolls.md)                                 |
| `source_password`      | `"hackme"`              | Password sources present when their mount has none of its own. Icecast's sample value, so an unconfigured server is not open to guessing a vendor default; operators SHOULD set it. | [sources.md](sources.md)                                   |
| `admin_user`           | `"admin"`               | User name of the admin pages.                                                                                                 | [observability.md](observability.md)                       |
| `admin_password`       | unset                   | Password of the admin pages. Unset: the admin pages answer as if they did not exist.                                          | [observability.md](observability.md)                       |
| `max_sources`          | unlimited               | Most simultaneous sources. Read when a source connects.                                                                       | [sources.md](sources.md)                                   |
| `max_clients`          | unlimited               | Most simultaneous listeners over all mounts. Read at each admission.                                                          | [listeners.md](listeners.md)                               |
| `source_timeout`       | 30 s                    | Inactivity after which a source connection is dropped. Read when a source connects.                                           | [sources.md](sources.md)                                   |
| `proxy_ips`            | none                    | Addresses of trusted reverse proxies, skipped when walking `X-Forwarded-For`.                                                 | [listeners.md](listeners.md)                               |
| `forwarded_address`    | walk `X-Forwarded-For` right to left, skipping `proxy_ips` | Decision function: the listener's real address from the connection address and request headers. | [listeners.md](listeners.md)                               |
| `address_hash`         | keyed hash of the address | Decision function applied to every listener address before it appears in any output.                                       | [listeners.md](listeners.md), [observability.md](observability.md) |
| `status_page`          | on                      | When off, the public status endpoints answer as if absent. The admin pages are governed by `admin_password` only.             | [observability.md](observability.md)                       |
| `status_access`        | allow all               | Decision function: may this request see the status endpoints.                                                                 | [observability.md](observability.md)                       |
| `status_json`, `status_html` | built-in renderers | Renderers of the statistics for the status endpoints.                                                                    | [observability.md](observability.md)                       |
| `preroll_cache_size`   | 256 MiB                 | Bytes of preroll files kept in memory.                                                                                        | [prerolls.md](prerolls.md)                                 |
| `access_log`           | unset                   | Access log path. `"-"` is standard error.                                                                                     | [observability.md](observability.md)                       |
| `playlist_log`         | unset                   | Playlist log path. `"-"` is standard error.                                                                                   | [observability.md](observability.md)                       |
| `error_log`            | unset                   | Error log path. `"-"` is standard error.                                                                                      | [observability.md](observability.md)                       |
| `log_level`            | 3 (info)                | Most verbose level written to the error log, 1 (error) to 4 (debug).                                                          | [observability.md](observability.md)                       |
| `log_size`             | 1 GB                    | Size past which a log file is rotated.                                                                                        | [observability.md](observability.md)                       |
| `log_archive`          | off                     | Rotated logs keep a timestamped name instead of replacing the single previous copy.                                           | [observability.md](observability.md)                       |
| `tls_reload_interval`  | 24 h                    | See §5.5.                                                                                                                     | this document                                              |

`source_timeout` is a server-wide setting that bounds a source connection.
Changing it on reload affects sources that connect afterwards.

### 4.4 Server-wide, read at mount output creation

| Setting             | Default                     | Meaning                                                                                                              | Behaviour owned by                                   |
| ------------------- | --------------------------- | -------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------- |
| `client_timeout`    | 30 s                        | Time a listener's connection may stall before it is dropped.                                                         | [harbor spec](../../../../core/outputs/spec/harbor/README.md) |
| `burst_on_connect`  | on                          | When off, no listener gets a burst, whatever `burst_size`.                                                           | the same                                             |
| `burst_size`        | 65534 bytes (about 64 KiB)  | Default burst for new listeners. A mount's own `burst_size` takes precedence. `0` disables the burst.                | the same                                             |
| `http_headers`      | none                        | Headers added to every listener reply, before the mount's own (§4.9).                                                | [listeners.md](listeners.md)                         |
| `dedicated_encoder` | off                         | Give each listener its own encoder instead of sharing the mount's.                                                   | the harbor spec                                      |
| `muxer_options`     | live options for Matroska and WebM | Per container format, the muxer options used when a stream is remuxed.                                          | [mounts.md](mounts.md)                               |

### 4.5 Per mount

Each is the mount's own value, else the default mount's, else the default shown.

| Setting                  | Default | Read point    | Meaning                                                                                                          | Behaviour owned by           |
| ------------------------ | ------- | ------------- | ---------------------------------------------------------------------------------------------------------------- | ---------------------------- |
| `source_user`            | `"source"` | use (source connects) | User name sources present.                                                                              | [sources.md](sources.md)     |
| `source_password`        | server's | use (source connects) | Password sources present.                                                                                      | [sources.md](sources.md)     |
| `max_listeners`          | unlimited | use (admission) | Most simultaneous listeners on the mount.                                                                    | [listeners.md](listeners.md) |
| `burst_size`             | server's | mount output | Burst for the mount's new listeners.                                                                              | the harbor spec              |
| `fallback_mount`         | none    | use (source connects) and reload | Mount that feeds this one while it has no source. A reload applies it to live mounts (§5.3). | [mounts.md](mounts.md)       |
| `hidden`                 | off     | use (status)  | Hide the mount from public status listings.                                                                      | [observability.md](observability.md) |
| `on_connect_command`     | none    | use (source connects) | Command run when a source connects on the mount. The same value is used for the matching disconnect.     | [sources.md](sources.md)     |
| `on_disconnect_command`  | none    | use (source connects) | Command run when that source disconnects.                                                                | [sources.md](sources.md)     |
| `http_headers`           | none    | mount output  | Headers added to the mount's listener replies (§4.9).                                                            | [listeners.md](listeners.md) |
| `max_listener_duration`  | none    | use (admission) | Seconds after which an admitted listener is disconnected. `0` or none: no limit.                               | [listeners.md](listeners.md) |
| `intro`                  | none    | use (admission) | Audio sent to each listener before the stream. A file under `webroot` or a URL.                                | [prerolls.md](prerolls.md)   |
| `preroll`                | none    | use (admission) | The same, under RSAS's name. Precedence between the two depends on `compatibility`, see [prerolls.md](prerolls.md). | [prerolls.md](prerolls.md)   |
| `listener_decision`      | admit all | use (admission) | Decision function for listeners; URL authentication is one implementation.                                     | [listeners.md](listeners.md), [url-auth.md](url-auth.md) |
| `source_decision`        | by password | use (source connects) | Decision function for sources: replaces the user and password check when set.                           | [sources.md](sources.md)     |

### 4.6 Values

- Counts, sizes, durations and ports are non-negative numbers. A negative or
  non-numeric value makes the set invalid.
- Paths MAY be relative; they are resolved against the process's working
  directory, except log paths (resolved by the builder, see
  [icecast-config.md §6](icecast-config.md#6-paths-and-logging)) and local
  prerolls (resolved against `webroot`).
- "Unset" and "unlimited" are values distinct from `0`, except where a setting
  says `0` means unlimited or disabled.

### 4.7 Defaults

A defaulted setting is indistinguishable from one set explicitly to the
default. The defaults above are those of Icecast mainline where it has one;
`source_password`, `admin_user` and `webroot` follow Icecast's sample
configuration.

### 4.8 Settings not in the set

Process-level facts are not settings: the process's user, working directory,
signal handling and how a reload is triggered.

### 4.9 Header lists

`http_headers` is an ordered list of entries. Each has a name, an optional
value, a kind and an optional status. Kind is `static` (sent as given) or
`cors` (see [icecast-config.md §7](icecast-config.md#7-http-headers)). The
effective list for a mount is the server's list followed by the default
mount's, then the mount's own. A later entry with the same name does not
replace an earlier one; all are sent.

## 5. Reload

A **reload** replaces the whole set. Its trigger (a signal, a call) is the
operator's.

### 5.1 Validate first

The new set MUST be fully built and validated (§3) before anything changes.
If building or validating fails, the reload MUST fail, report why, and leave
the previous set, every mount and every connection unchanged. A server never
runs a half-applied set.

### 5.2 Atomic swap

On success the new set replaces the old one in a single step. A reader MUST
see either all of the old set or all of the new one, never a mixture. A
request in progress finishes with the values it already read.

### 5.3 After the swap

| Class of setting                 | Effect                                                                                     |
| -------------------------------- | ------------------------------------------------------------------------------------------ |
| startup                          | Not applied. The server logs, for each one that changed, that it was not applied.          |
| use                              | Applies at the next use: the next request, source connection, log write.                   |
| mount output                     | Applies when a mount output is next created. Existing outputs are not touched.            |
| acts on running mounts           | Applied during the reload, before it returns.                                              |

Settings that act on running mounts are `fallback_mount` and the
shutdown it can imply: a live mount follows its new fallback at once; a mount
that has no source and whose fallback was removed or never existed is shut
down ([mounts.md](mounts.md)). A reload MUST NOT disconnect a source or a
listener otherwise.

### 5.4 Overrides

A builder that applies explicit overrides on top of a file (a script argument
over an XML element) MUST apply the same overrides on every reload, so that a
reload does not silently undo them. The server does no merging of its own.

### 5.5 TLS material

`tls_certificate` and `tls_key` are read again on every reload and at most
`tls_reload_interval` after the last successful read (recommended 24 hours),
so a renewed certificate is used without a reload being asked for. The interval
is checked when a connection arrives. If reading fails, the server MUST keep
the certificate it has and log the failure; a failed read MUST be retried at
the next connection, not after another interval. Connections already
established are unaffected. A failed certificate read is not a reload failure
(§5.1): the rest of the set applies.

## 6. Conformance

**Binding**

- `compatibility` outside its four values fails startup and fails a reload,
  leaving the previous set in force.
- Any invalid value (§3) does the same. After a failed reload a request sees
  the previous set, sources and listeners stay connected, mounts unchanged.
- After a successful reload: a request made after it sees every use-class value
  new; one made before saw every one old. No request sees a mixture (test with
  two settings changed together and a request loop).
- A startup-class change is not applied (the port keeps serving) and is
  logged as not applied.
- A mount output created before a reload keeps its `client_timeout`, burst
  and headers; one created after has the new ones.
- A live mount follows a changed `fallback_mount` during the reload; an
  orphaned mount whose fallback was removed is gone when the reload returns.
- A mount's own value beats the default mount's; the default mount's beats the
  setting's default; headers accumulate default first.
- With an unreadable certificate after a reload or at the interval, TLS keeps
  serving the previous one, and a later connection retries.
- Overrides are not merged: a set built without a value gets the default, even
  if a previous set had one.

**Incidental**

- Log wording, the reload trigger, the moment within the interval at which the
  certificate is read, rendering of the defaults.
