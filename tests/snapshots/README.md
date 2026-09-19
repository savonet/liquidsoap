# Language pipeline snapshots

Each script in `cases/` is pushed through the whole language pipeline and every
intermediate representation is dumped to `expected/<case>.expected`:

| section  | what it shows                                                      |
| -------- | ------------------------------------------------------------------ |
| `parsed` | the parsed term, as the JSON the formatter and the LSP consume     |
| `hash`   | its hash, which is what the typechecking cache keys on             |
| `term`   | the runtime term, i.e. what `Term_reducer` desugared the script to |
| `type`   | the type inferred for the whole script                             |
| `value`  | the result of evaluating it                                        |

A stage that raises prints its error message and the following stages are
skipped, so error messages are snapshotted too.

These serve two purposes:

- **Regression safety.** Refactoring the parser, the reducer or the type checker
  must not change the output. If it does, the diff says exactly how.
- **Documentation.** `cases/control_flow.liq` shows that `if a then b else c end`
  is really `if(a, then={b}, else={c})`, `cases/argsof.liq` shows what
  `%argsof` expands to, and so on. This is the fastest way to learn what the
  language desugars into.

Everything runs against `liquidsoap_lang` alone — no standard library, no
streaming core — so the output only depends on the language implementation.
Scripts that use core builtins (sources, `time_in_mod`, …) still show their
desugaring; they simply fail at the `type` stage, which is fine and expected.

## The canonical suite

`cases_canonical/` → `expected_canonical/` is a second, much smaller suite that
dumps `Parsed_json.parse_string` instead: the flat, keyword-anchored JSON that
liquidsoap-prettier consumes, spec'd in `src/lang/tooling/parsed_json.mli`.

It exists because the pipeline snapshots above **strip every position**, while
block spans and comment offsets are precisely what prettier depends on — so a
parser change can leave `expected/` untouched and still break the formatter.
Keep these cases few and focused on block structure; the output is verbose.

## The analysis suite

`cases_analysis/` → `expected_analysis/` covers `Liquidsoap_tooling.Analysis`, which editor tooling uses to typecheck a script without running it. Unlike the other suites, it runs against the full standard library: `analysis_stdlib.types` is written by the native `liquidsoap --cache-js-stdlib`, so this suite needs the streaming core built.

Each snapshot shows the diagnostics, then answers the queries written in the case as comments:

```liquidsoap
#? type 3:2      # the type at line 3, byte column 2, as shown on hover
#? scope 3:2     # the names in scope there, minus the standard library's
#? methods 3:2   # the methods of the type there, as offered after a `.`
```

All suites share `snapshot.exe` (`--canonical` and `--analysis` select the other two) and the same two `--auto-promote` commands.

## File extensions in `cases/`

| extension      | what it is                                               |
| -------------- | -------------------------------------------------------- |
| `.liq`         | a case                                                   |
| `.invalid-liq` | a case that is deliberately not valid Liquidsoap         |
| `.liq-inc`     | not a case; a fixture some case pulls in with `%include` |

Only `.liq` is picked up by the repo-wide `**/*.liq` glob that the external
tree-sitter and lezer grammars are checked against, which is why the other two
are spelled the way they are.

## Adding a case

Drop a `.liq` file in `cases/`, then:

```
dune build @gendune --auto-promote        # generate the rules for the new case
dune build @lang_snapshot --auto-promote  # write expected/<case>.expected
```

Review the new `expected/<case>.expected` before committing it — that file _is_
the test.

## After an intentional change

Same command. Read the diff carefully: it is the full list of user-visible
consequences of the change.
