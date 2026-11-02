Start a session with: "Read src/libs/extra/spec/icecast-server/remaining-work/README.md and continue from there." The other files of this folder are the implementer brief, the refactor plan and the memory-capped runner.

We are rewriting liquidsoap's `icecast.server` from a normative specification, on branch `icecast-server-refactor` in /home/toots/src/savonet/liquidsoap, rebased on `main` (which has #5598, the `try … finally` fix, and #5600, the shadowed-methods typechecker speedup). Pick up where the previous session stopped. Read this whole prompt, then the files it names, before doing anything.

## Where things stand

PRs:

- #5592 (`icecast-server-listener-auth`): URL listener auth, prerolls, time limits, redirects. Open, CI green.
- #5595 (`icecast-server-refactor`): this work, a **draft** stacked on #5592 with `gh stack` (stack #5596). Its description has a per-batch checklist. When #5592 merges, check its commits reached `main` (`git merge-base --is-ancestor`) before going further.

The branch history was squashed into one commit per batch; the remote still has the old history, so the next push is a force-push. Batch 4 (pages and logs) is committed but not pushed: when pushing, tick "status and admin pages, logs" in #5595's checklist (read the live PR body first and keep any lines the user added).

## Batch 5, config and docs: in progress (committed as "icecast.server: configuration translation, work in progress")

Done:

- `icecast.server.config.load(~compatibility, path)` in `src/libs/extra/icecast_server/config/` (read, listen, limits, paths, headers, events, mount_auth, mount_entry, mounts, load), installed by its own stanza in src/libs/dune. The config includes come **before** server.liq: each `icecast.server.*` definition after `def replaces icecast.server` costs a copy of the server's type (it doubled the stdlib's cache-miss typecheck, 564 MB → 1.14 GB).
- tests/language/icecast_server_config.liq: 256 checks over 97 fixtures, warnings counted per fixture from the "Translating <path>." info line `load` logs. 5/5 under CPU load; 9 mutations caught (log level, logsize KiB, unknown-element warnings, access lists first, `*` vs `/`, tls `auto`, is-zero filter, header order).
- Event bindings receive `{event, mount, listeners}`, the mount's count after the event (needed for `source-listeners-is-zero`); tests/harbor/icecast_server_events.liq checks it (mutation caught, 5/5 under load).
- `icecast.server.access.pattern` gained `~star_matches_slash` (non-KH mount patterns).
- Doc examples doc/content/liq/icecast-server-\*.liq rewritten to the record API (12) plus 6 new ones (mounts, decisions, url-auth, attach, headers, preroll). 17 pass `--check`; `icecast-server-reload.liq` hits the typechecker blowup below.

Not done:

- Rewrite doc/content/icecast_server.md itself (old labeled-argument API throughout) to use the new examples; run `dune build @gendune --auto-promote` for doc/dune.inc.
- Reviewer pass (one Opus reviewer), then fold the WIP commit into a proper batch-5 commit.
- Offer the spec-extraction skill's final step (compare new vs old implementation) before #5595 leaves draft.

Spec questions to raise with the user (implemented with these proposed rules):

- `trusted-proxy` (icecast-config.md §3.1): read as naming listen sockets by `id`, whose `<client-ip>` values go to `proxy_ips`.
- exec event `argvtype`: `no_defaults` → no args, `uri_and_trigger` → mount and trigger, otherwise the mount. URL event handler POSTs `action` (option, default the trigger), `mount`, `trigger`, `listeners`. Log handler level defaults to Icecast info.
- `on-connect`/`on-disconnect` commands get the mount as their argument.
- A `cors` header with a `value` is sent as that static value.
- `<role>` is rejected in the root `<authentication>` too, not only in mounts.
- Event bindings inside a `type="default"` mount are ignored with a warning (a binding belongs to one mount).
- `dump-file interpolation="uuid"`: warns, keeps the name as a strftime pattern.

## Current focus: typechecker memory blowup (follow-up optimisation, like #5600)

Two separate instances of a loaded record's `mount` callback type meeting in subtyping explode: 8 GB in 20 s uncapped. This is the canonical reload pattern:

```liquidsoap
server = icecast.server(icecast.server.config.load(path))
server.reload(icecast.server.config.load(path))
```

Repros in /var/tmp/claude-icecast-config/ (`m_nn.liq`: `[null.get(a.mount("/a")), null.get(b.mount("/a"))]` for two loads). Fast in comparison (~3.7 s): one instance used twice, native start + loaded reload, `load(…).{port = 8100}`, each mount field alone accessed with `?.`, and even a field-by-field `?.` copy of all 19 fields. Plain `.listener_decision` of two instances alone hangs; its printed type is small and concrete.

Trace so far (instrumentation patch: /var/tmp/claude-icecast-config/trace.patch, apply to src/lang/types/typing.ml, run with `LIQ_TRACE=1`; never commit it): the stack is shallow (`<:` → `sup`), and the covariant-link branch of `<:` runs `sup` on tiny types (2–3 nodes) hundreds of thousands of times: about 1 M `<:` calls, 380 k `sup`, 155 k covariant every few seconds. So the blowup is in the number of `<:` calls, not in type size. Next: find which `<:` recursion multiplies (count calls per case/caller, or log the path of the covariant variable), likely re-traversal of covariant links rebuilt by `sup` at each level. Also noted: `meth_sup`'s `try … with Incompatible -> sup a b` wraps the recursive `sup a b`, computing it twice on a deep `Incompatible` (exponential on its own); narrowing it alone did not fix the repro.

Every OCaml change needs the user's approval: minimal and generic.

## How we work

- Rules for implementers: `remaining-work/implementer-brief.md`. Layout and batches: `remaining-work/refactor-plan.md`. The spec (`src/libs/extra/spec/icecast-server/*.md`, `src/core/outputs/spec/harbor/*.md`, `src/core/optionals/ffmpeg/spec/io.md`) wins; spec questions go to the user with a proposed rule.
- The user prefers the work kept in the main session; agents only when really needed, and every agent uses `model: "opus"`.
- Memory:
  - every liquidsoap run is memory-capped with `remaining-work/run.sh` (`CAP=1500M src/libs/extra/spec/icecast-server/remaining-work/run.sh --check /abs/path.liq`; script paths must be absolute); a gdb session on a hung check must run capped too (start it with the Bash tool's `run_in_background`, attach within ~5 s, it hits the cap soon after); one uncapped run was OOM-killed;
  - measure stdlib memory with `--no-cache`: a cache hit hides the cost;
  - at most two liquidsoap processes at a time; no parallel dune builds;
  - large outputs in `/var/tmp/claude-*`, never /tmp (RAM);
  - kill only PIDs you started; never `pkill -f` a pattern matching your own command line;
  - stop after two consecutive OOM kills.
- Commit locally when a batch is reviewed and green, without asking. Every push and every `gh` write needs the user's explicit OK on the exact text: write the commands into /tmp/liq_push.sh with the Write tool (a Bash hook blocks text containing the push command), and the user runs `! sh /tmp/liq_push.sh`. PR bodies go in /tmp/pr-body-\*.md. No Claude attribution anywhere, no CHANGES entry for this work.
- Commit hooks: the formatter may change files on the first attempt (rerun); the changeset-review hook asks once (rerun).
- Never run `dune build @citest` / `@mediatest`. Single tests: `timeout 600 dune build --force @test_<name>`; dune prints `Ran test <name>: [ok]` only when it ran.
- Liquidsoap gotchas met so far:
  - `def foo.bar` drops methods attached to `foo.bar`: the entry point uses `def replaces icecast.server(parameters)`;
  - passing a huge inferred record into a closure stored in an unannotated record blows up the checker: pass the one field needed;
  - a field read as `x.f ?? d` or `null.defined(x.f)` can be inferred as an optional method (`f?`) and clash with a nullable field: use `""`/`max_int` sentinels or `null.get(default=…)`;
  - `c ? a : x.f` and `c ? a : f(x).g` parse as `(c ? a : x).f`: parenthesize the else branch;
  - `x ?? y` with a nullable `y` makes `x` doubly nullable: use `null.defined(x) ? x : y`;
  - lists spread together (`[...a, ...b]`) need one element type: normalize records (decisions to the full outcome, headers to `{name, value, compute, status}`); a literal list of differently shaped records is fine;
  - a record holding functions cannot be compared: compare `null.defined(…)` or projections;
  - `log.flush()` waits for the logging thread (~1 s): flush once, not per check;
  - `list.hd(default=null)` raises.
