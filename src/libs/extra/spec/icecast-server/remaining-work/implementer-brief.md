# Brief: implementing the icecast.server specification

Repo: /home/toots/src/savonet/liquidsoap, branch `icecast-server-refactor` (stacked on `icecast-server-spec`). The specs are the source of truth:

- /home/toots/src/savonet/liquidsoap/src/libs/extra/spec/icecast-server/\*.md (the server; parameters.md is the API)
- /home/toots/src/savonet/liquidsoap/src/core/outputs/spec/harbor/\*.md (the harbor output it relies on)

## Rules

- **The spec wins.** If the spec is wrong, ambiguous or silent, do not settle it in code: stop and report the question (with your proposed rule). Do not depart from the spec quietly.
- **Do not commit, push, or run `git` commands that change history.** Leave changes in the working tree; the coordinator reviews and commits.
- **Write only the files of your assignment.** Other agents may work in parallel on other files.
- **Simple and idiomatic.** The user found the previous code very hard to read. Short functions with purpose-based names; short files (aim under ~250 lines per .liq file); no clever indirection. Liquidsoap stdlib style: see /home/toots/.claude/CLAUDE.md sections "Liquidsoap", "Liquidsoap Gotchas", "Liquidsoap Language Reference", "OCaml", "Comments", "Code Style" — follow them (argument order: optional, named, then positional; records over >2-tuples; polymorphic variants; no partial application in .liq; `x?.foo` for optional fields; chained `??` needs parentheses; `c ? a : (f(x))` — parenthesize a call or a field access in the else branch of a ternary, the parser binds `c ? a : x.f()` as `(c ? a : x).f()`; `list.hd(default=null, …)` / `list.find(default=null, …)` raise instead of returning null — use `list.case` / `list.assoc.nullable`).
- **The OCaml core is conservative.** Only the harbor capabilities the spec requires; minimal, generic, no icecast policy in OCaml. New scripting primitives in the .liq stdlib are allowed when justified; report each one you add.
- **Comments** (enforced by a hook): only what is not obvious; never narrate; no history ("used to", "now"); at most two short paragraphs; never two comment blocks back to back; an important module opens with a recap of its API.
- **Tests** (CLAUDE.md "Tests Must Survive CI"): each behaviour the spec marks binding gets a check; new tests in tests/harbor (streaming) or tests/language (pure), OCaml unit tests in tests/core; register with `dune build @gendune --auto-promote` (run twice until it exits 0). Ports: unique across tests/, including `port + 1` for `icecast.server` and `input.harbor(icy=true)`; use only the range given in your assignment. No fixed sleeps as synchronization (poll with a deadline); release sockets in `finally`; failures say which check failed. Run each new/changed test with `dune build --force @test_<name>`; run it 5 times under full CPU load (see CLAUDE.md for the loop). For each important check, show it fails when the feature is broken (mutation: break, run, restore) and report it. Never run the full suites (`@citest`, `@mediatest`).
- **Building:** `dune build`; typecheck the stdlib with `./liquidsoap -h <op> > /tmp/... 2>&1; echo $?` (redirect to a file; a pipe hides the exit code). Formatting: the pre-commit hooks format .liq/.ml/.md at commit; you may run `pre-commit run --files <files>` to check.
- `/tmp` is RAM: keep large data in /var/tmp/claude-\*.

## Final message (under 60 lines)

Files changed; spec questions you hit (with proposed rules); new primitives added; tests added and their mutation results; load-test results; anything not done.
