# Plan: the icecast.server .liq refactor

Goal: replace src/libs/extra/icecast_server.liq (~3.4k lines) with short files implementing the spec (src/libs/extra/spec/icecast-server/\*.md; parameters.md is the API). `icecast.server` is unreleased: no backward compatibility with the old labeled-argument API; existing tests and docs are rewritten to the new API.

## Layout

`src/libs/extra/icecast_server.liq` becomes the entry point: a module recap (the API: `icecast.server(parameters)`, its methods, `icecast.server.config.load(path)`, the public helpers) and `%include`s of the parts, in dependency order. `%include` resolves relative to the including file. The parts live in `src/libs/extra/icecast_server/` (add an install stanza to src/libs/dune: `(glob_files extra/icecast_server/*.liq)`, same section as `extra/*.liq`).

Each file owns one concern, mirrors one spec section, and stays short (aim < 250 lines):

| File             | Owns (spec)                                                                                                                                                                       |
| ---------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `parameters.liq` | defaults, validation, `compatibility`, error kinds, global settings (parameters.md §2–§4, §7, §10)                                                                                |
| `refusals.liq`   | reason phrases, `icecast.server.refusal`, `.redirect`, limit refusal per `compatibility` (listeners.md §3.1)                                                                      |
| `headers.liq`    | header entries: order, first-name-wins, `compute`, `status`, validation; `icecast.server.headers.cors` (parameters.md §4.2, listeners.md §3.2)                                    |
| `addresses.liq`  | `proxy_ips` (CIDR) walk, `forwarded_address`, address hash (listeners.md §8–§9)                                                                                                   |
| `prerolls.liq`   | process-wide preroll store, `icecast.server.preroll_cache` views (prerolls.md)                                                                                                    |
| `decisions.liq`  | listener request record, `icecast.server.decision.all` composition (listeners.md §2, §7 composition)                                                                              |
| `url_auth.liq`   | URL auth wire protocol: request, reply, release (url-auth.md)                                                                                                                     |
| `access.liq`     | access lists, htpasswd, per-user limit (listeners.md §7)                                                                                                                          |
| `sources.liq`    | source protocols, reserved paths, source decision, stream_auth, admin commands (metadata, killsource), notifications (sources.md)                                                 |
| `mounts.liq`     | mount table, mount outputs (harbor), attach/detach, fallback chains, `fallback_when_full`, linger, format continuation, dump file (mounts.md)                                     |
| `listeners.liq`  | admission pipeline wired into harbor's decision, limits, per-user counts, release / `on_dropped` (listeners.md §1, §4–§6)                                                         |
| `events.liq`     | `on_*` registration, `event_bindings` (observability.md §1–§3)                                                                                                                    |
| `stats.liq`      | stats snapshot, `max_history` (observability.md §4–§5, §13)                                                                                                                       |
| `pages.liq`      | status page, `/status.json`, `/status-json.xsl`, admin pages, `/health`, `/metrics` (observability.md §6–§8)                                                                      |
| `logs.liq`       | access / playlist / error logs, rotation, `reopen_logs` (observability.md §9–§12)                                                                                                 |
| `server.liq`     | `icecast.server(parameters)`: assembles everything, methods, `reload` (parameters.md §6, §8, §9)                                                                                  |
| `config/*.liq`   | `icecast.server.config.load(~compatibility, path)`: one file per XML section, each a short table of element → parameter rows using the stdlib `xml.*` helpers (icecast-config.md) |

Split further if a file grows past the limit; merge two only if both stay tiny.

## Batches (sequential, each green before the next)

1. Foundations: parameters, refusals, headers, addresses, prerolls — plus language tests for each (pure functions).
2. Decisions: decisions, url_auth, access — plus tests.
3. Server core: sources, mounts, listeners, events, stats, server — wired to harbor's new capabilities (`on_dropped`, admit headers, `keep_listeners`); migrate the streaming tests (tests/harbor/icecast*server*\*.liq) to the new API.
4. Observability surfaces: pages, logs — plus tests.
5. Config translation: config/\*.liq — plus tests (tests/language/icecast_server_config.liq rewritten); docs (doc/content/icecast_server.md rewritten to the new API, examples as checked .liq files); remove the old file's remains.

Ports: batch 3 uses 18170–18189, batch 4 18190–18199 (unique across tests/, including `port + 1`).
