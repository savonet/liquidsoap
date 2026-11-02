# Code review of #5595 (2026-11-02)

Six Opus reviewers (config, core, decisions, pages and logs, tests and docs, ownership), read-only. The coordinator checked the high-confidence findings against the code (marked ✓). Fix in this order; each fix gets a test that fails without it.

## Security

1. ✓ source_admin.liq `allowed`: the `user`/`pass` query fallback applies to every admin mount command, not only `/admin.cgi` (sources.md §6). Source passwords end up in URLs and proxy logs.
2. ✓ addresses.liq `walk`: only the first `X-Forwarded-For` line is read. A client spoofs its address behind a proxy that adds its own line (listeners.md §8). Join all lines with "," first.
3. ✓ config/listen.liq: an empty `<trusted-proxy/>` gives `null`, which matches every listen socket without an `id` (`null == null`), so any address may be trusted as a proxy.
4. ✓ snapshot.liq: when `parameters.mount` raises, `hidden` becomes `false`, so a hidden mount shows on public pages (observability.md §6). Fail closed instead.
5. Not fixed (user, 2026-11-02: Shoutcast is not the main concern): recorded in future-work.md, "Shoutcast mount fixed at admission". sources.liq `connect` re-reads `shoutcast_mount` after `accept` authenticated against the old value: a reload in between lets a client feed a mount it was not checked for (parameters.md §8.2).

## Correctness

6. ✓ server.liq: `source_timeout` is read once at startup (use-class, parameters.md §3.4). Pass a getter if harbor takes one; otherwise check at connect.
7. ✓ `max_sources` is checked only at admission, not when the feed takes the mount: concurrent connects exceed it (sources.md §3).
8. ✓ mount_chain.liq: `fallback = {}` passes validation, then `null.get(next)` raises in every refresh. Validate or treat it as no fallback.
9. feeders.liq `take`: values that can raise (`event_bindings()`, `hostname()`, dump) are computed after the table changed, giving a disconnect with no connect (observability.md §3).
10. mount_chain.liq: a `fallback.source` counts as something to send even when it is not ready, so the mount never shuts down (mounts.md §1).
11. listeners.liq: a served listener's `on_release` runs synchronously on the harbor path (listeners.md §5 "without delaying the disconnect").
12. feeders.liq `end_linger` identifies the feeder by `source.id`: detach, re-attach the same source, detach ends the second linger early. Use a per-departure token.
13. source_admin.liq: admin commands build the source request with `ip=""` (listeners.md §10). harbor.http requests expose no peer address: may need a generic OCaml addition (ask).
14. feeders.liq: replacing an output for a new format does not drop its overlay entry (mounts.md §6.3). Low impact.
15. snapshot.liq/stats.liq: a mount's listener list and counters are read at different moments; peak can be below the count (observability.md §4–§5).
16. ✓ url_auth.liq: `http.post` turns connection failures into synthetic 520–526 replies with no headers, so `on_error_wait` never starts (url-auth.md §7).
17. ✓ string.liq `string.split.first` mixes the byte index of `string.index` with UTF-8 positions: non-ASCII credentials split wrongly, invalid UTF-8 raises outside the decision's try. Fix the stdlib function (root cause), not the three callers.
18. url_auth_reply.liq: a time limit too long for `int_of_string` raises (url-auth.md §5.1). Use `int_of_string(default=…)`.
19. url_auth_request.liq: a `withintro` body is buffered whole before `preroll_max_size` is checked. Low.
20. prerolls.liq: no single-flight on a miss; N concurrent admissions fetch the same URL (prerolls.md §3).
21. ✓ log_files.liq: after a failed rotation rename, every following line retries the rotation and logs severe. Reset or back off.
22. ✓ logs.liq/log_files.liq: a message filtered out by `log_level` reaches `write` with a null target, which closes the error log; the next one reopens it. Skip filtered messages.
23. pages.liq: admin renderers are not wrapped in `try` as the public ones are.

## Config translation

24. ✓ load.liq: TLS on a build without TLS calls `no_tls_transport`, which shuts the process down during `load` (and on reload). Reject when the factory is `"none"`.
25. ✓ mount_entry.liq: `<priority>` is parsed as an int outside `"kh"`, where it is ignored: `<priority>high</priority>` rejects the file.
26. ✓ config/headers.liq: a `type` other than `static`/`cors` drops the header; under 2.4/kh any type should be ignored with a warning and the header sent static. The comparison is case-sensitive.
27. ✓ paths.liq: `deny-ip`, `ban-file`, `banfile` are alternatives, so the second file is dropped silently. Pass all of them (access_lists takes one deny file: union or warn).
28. Under `"kh"`, event bindings of a pattern mount carry the raw pattern and never fire.
29. Unrecognised children of mount `<authentication>`, `<http-headers>`, `<custom-headers>`, `<cors>`, `<event-bindings>`, and an unknown log event `level`, are ignored without the warning of §2.

## Ownership

30. ✓ The header entry shape is built by hand in listeners.liq, mount_outputs.liq and config/headers.liq. `headers.of_pairs` is used only by a test. One normalising function in headers.liq.
31. ✓ `icecast.server.events` is a name list (parameter_checks.liq) then redefined as a namespace (events.liq). Validation works only through include order. Derive the names from the registry.
32. ✓ Direct `log.*(label="icecast.server")` calls where the caller has `state` (headers, source_admission, decisions, events, mount_parameters): these bypass `error_log` and the server's `name`. mount_parameters' "both mount and source" warning is logged at every `parameters.mount` call.
33. mount_requests.liq answers 404/503/307 with `http.response`, without `headers.reply`; the redirect copies listeners.liq's `?query` rule.
34. `compatibility` is guessed as `"icecast-2.5"` in source_url.liq and config/events.liq. The server should hand it in the source and binding records.
35. The URL-auth form fields are built in three places, which already differ (agent `-`, order).
36. "artist - title" is composed in stats.liq and again in url_auth_request.liq.
37. URL-auth option names and defaults are spread across files; the `auth_header` check lives only in the config translation.
38. Literals written several times: `"hackme"` (parameters, reload), the `compatibility` default and check (parameters, config/load), `Basic realm="Icecast2 Server"` (three files).
39. The reserved-path list is copied by hand from the routes; `POST /<mount>/manage` is missing.
40. Two listener counts (`stats.current()` vs `list.length(listeners.current)`) can disagree around a disconnect.
41. OCaml still matches `error.icecast.server.mount_taken` in harbor/operator.ml (predates the PR); the new generic refusal seam could replace it.
42. `icecast.server.mounts.buffer_size` is exported without `@flag hidden`; `access.pattern` logs "access list entry" for mount patterns.

## Tests (CI robustness)

43. ✓ icecast_server_listeners.liq: waits for count 0, then checks releases once; `on_release` runs after the count drops. Wait for the release count.
44. icecast_server_sources.liq `check_race`: 2.5 s of audio, then up to 23 s of waiting, while `source_timeout` is 10 s.
45. icecast_server_mounts.liq: "settled" after one 0.2 s window, then a fixed 1.5 s pause.
46. icecast_server_reload.liq: `thread.run(delay=0.5, reload)` as synchronization.
47. icecast_server_decisions.liq: file rewritten within one mtime tick. Use `touch -d` as the preroll test does.
48. icecast_server_ogg_late_listener.liq: fixed delay, bare `test.equal`, uncaught read timeout, no cleanup, no check count.
49. icecast_server_source_reconnect.liq: unlabelled `test.equal` in loops, uncaught socket errors.
50. icecast_server_sources.liq: temp mp3 not removed; `stats_of` helpers fall back to `list.hd(mounts)`.

Untested Binding items reported: reload keeps a mount output's `client_timeout` and burst; startup-class change logged; fallback chain bound and rsas `503`; `fallbacks` overlay lifetime per compatibility; both `mount` and `source` warning; `source_timeout` drop; RSAS `/manage kick`; `dump_file_time_limit`; `on_dropped` release; `-` stderr target; DBUG mapping; `/health` under rsas with status page off; page Content-Types; kh access log CR/LF; exact rotation point; config: rejected file on reload, `exec`/`url` event actions, url-auth options through the decision, `<acl>` per-user, `allow-ip`/`deny-agents`/`ban-file`, rsas `cors` type, root `<role>`, `uuid` warning, aliases.

## Docs (doc/content/icecast_server.md)

51. ✓ The `status_json` example maps every mount, hidden ones included, and the `status_html` example answers "Not found." with 200.
52. ✓ The reload timing list files `source_timeout` under "next use" (it is startup-only, see 6); `transport` changes are not logged; `name` is missing.
53. ✓ `status_access` does not gate `/health`; `status_page = false` keeps `/health` under rsas and turns off `/<mount>/health`.
54. Source event records carry the raw `ip`, contrary to "before anything sees it"; their `server`, `format`, `headers` fields are not listed.
55. Header precedence: the server's entries come before the mount's, so a mount cannot override a server header. Say so.
56. "A server takes a single line" next to a 15-line example; `POST` and `WEBSOCKET` source methods and the exact `/admin` path are missing.

## Decisions for the user

- A. Validate every mount record of a translated file at `load` (config reviewer's finding; parameters.md §8.1 "validate first" vs §4 "validated each time `mount` returns it")?
- B. `status_json`/`status_html` receive hidden mounts (spec: the snapshot includes them): keep and document it, or hand the callbacks a filtered snapshot?
- C. Admin requests have no peer address (13): add a generic peer-address field to harbor.http requests in OCaml?
- D. Apply `http_headers` (CORS) to the 404/503/307 answers of a path with no mount output (33)?
- E. KH pattern-mount event bindings (28): match the pattern, or warn and ignore?
- F. Which ownership refactors (30–42) to do in this PR.

Decided (user, 2026-11-02): A yes, validate translated mount records at load. B keep hidden mounts in the callbacks' snapshot; document and filter in the example. C yes, a generic `ip` on harbor.http requests (show the OCaml diff first). D yes, through `headers.reply`. E match the pattern under `"kh"`. F do 30–32, 33, 34–36, 38–40; skip 37 and 41. Spec questions: keep all seven rules; `trusted-proxy` also takes its own text as an address when no socket has that `id`. Old-vs-new comparison after the fixes. Order: security, correctness and config, ownership, tests and docs.

Progress (2026-11-02): fixed and committed, each with a test that fails without it: 1, 2, 3 (and `trusted-proxy` text as an address), 4, 6 (`input.harbor` `timeout` getter, OCaml approved), 7, 8, 13 (`harbor.http` `request.ip`, OCaml approved), 16, 17, 18, 21, 22. Item 5 recorded as future work. Then 9 and 11 (the listeners test's release count now waits, item 43). Item 10 is held: ignoring a fallback source that is not ready would also drop a new `fallback.source` given by a reload, which is not in the mount's clock yet; decide how readiness is judged first.
