Start a session with: "Read src/libs/extra/spec/icecast-server/remaining-work/README.md and continue from there." The other files of this folder are the implementer brief, the refactor plan and the memory-capped runner.

We are rewriting liquidsoap's `icecast.server` from a normative specification, on branch `icecast-server-refactor` in /home/toots/src/savonet/liquidsoap. Pick up where the previous session stopped. Read this whole prompt, then the files it names, before doing anything.

## Where things stand

PRs:

- #5592 (`icecast-server-listener-auth`): URL listener auth, prerolls, time limits, redirects. Open, CI green.
- #5595 (`icecast-server-refactor`): this work, a **draft** stacked on #5592 with `gh stack` (stack #5596). Its description lays out the design and has a per-batch checklist. When #5592 merges, check its commits reached `main` (`git merge-base --is-ancestor`) before going further.
- #5598 (`fix-try-finally`, worktree /var/tmp/claude-tryfinally): fixes `try … finally` without `catch` swallowing errors. Open against `main`. Remove the worktree once it is merged.

Commits on `icecast-server-refactor`:

- spec (c2511ad, af0ced6, 09ec971), `xml.*` helpers (1389a0b), harbor output capabilities (4d38a1d);
- e755507 batch 1 foundations, 8c0a965 batch 2 decisions, c82aecc batch 3 server core: all pushed;
- **fec2f55 batch 4, pages and logs, and the commit adding this file: committed, NOT pushed.** When pushing it, tick "status and admin pages, logs" in #5595's checklist (read the live PR body first and keep any lines the user added).

The tree is clean.

## Next: batch 5, config and docs

- `icecast.server.config.load(~compatibility, path)`, one short file per XML section on top of the `xml.*` helpers.
- Rewrite the config and tables tests.
- Rewrite `doc/content/icecast_server.md` and its 12 examples `doc/content/liq/icecast-server-*.liq` to the new API, as checked `.liq` files (`@doctest` is red until this lands; accepted).
- Remove what remains of the old implementation.
- Same flow: one Opus implementer, a separate Opus reviewer, commit when green.

Then offer the spec-extraction skill's final step (compare the new implementation against the old) before #5595 leaves draft, and update #5595's checklist when pushing.

## Open items to keep in mind

- No tests guard the batch 3 race fixes (atomic params swap, register `on_disconnect` before attach).
- Untested in batch 4: a log target becoming null, a failed rotation rename, routing precedence between routes.
- `icecast.server.locked` releases its lock only on catchable errors.

## How we work

- Rules for implementers: `remaining-work/implementer-brief.md`. Layout and batches: `remaining-work/refactor-plan.md`. The spec (`src/libs/extra/spec/icecast-server/*.md`, `src/core/outputs/spec/harbor/*.md`, `src/core/optionals/ffmpeg/spec/io.md`) wins; spec questions go to the user with a proposed rule.
- Every agent uses `model: "opus"`. Watch background agents every 15–20 minutes: `stat -L` on their transcript, `find src tests -type f -mmin -20`, the OOM count from journalctl. One agent once hung for 6 hours unnoticed.
- Memory:
  - every liquidsoap run is memory-capped with `remaining-work/run.sh` (`CAP=1500M src/libs/extra/spec/icecast-server/remaining-work/run.sh --check /abs/path.liq`; script paths must be absolute; the stdlib alone peaks around 650 MB);
  - at most two liquidsoap processes at a time;
  - no parallel dune builds;
  - large outputs in `/var/tmp/claude-*`, never /tmp (RAM);
  - kill only PIDs you started, held in a zsh array; never `pkill -f` a pattern matching your own command line;
  - stop after two consecutive OOM kills.
- Commit locally when a batch is reviewed and green, without asking. Every push and every `gh` write needs the user's explicit OK on the exact text: write the commands into /tmp/liq_push.sh with the Write tool (a Bash hook blocks text containing the push command), and the user runs `! sh /tmp/liq_push.sh`. PR bodies go in /tmp/pr-body-\*.md. No Claude attribution anywhere, no CHANGES entry for this work.
- Commit hooks: the formatter may change files on the first attempt (rerun); the changeset-review hook asks once (rerun).
- The user is very conservative about OCaml: new OCaml only with their approval, minimal and generic, no Icecast policy in it.
- Never run `dune build @citest` / `@mediatest`. Single tests: `timeout 600 dune build --force @test_<name>`.
- Liquidsoap gotchas met so far:
  - `def foo.bar` drops methods attached to `foo.bar`: the entry point uses `def replaces icecast.server(parameters)`;
  - passing a huge inferred record into a closure stored in an unannotated record blows up the checker: pass the one field needed;
  - optional-argument use (`clock.create(id=p.name)`) makes a field nullable: annotate;
  - records with nullable fields passed between functions hit a checker trap: use `""` defaults or annotate;
  - `{...r, f = x}` closes the record type: use `r.{f = x}`;
  - `c ? a : x.f()` parses as `(c ? a : x).f()`;
  - `list.hd(default=null)` raises;
  - `try … finally` without `catch` swallows errors until #5598 lands: catch and re-raise explicitly.
