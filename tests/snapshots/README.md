# Language pipeline snapshots

Each script in `cases/` is pushed through the whole language pipeline and every
intermediate representation is dumped to `expected/<case>.expected`:

| section  | what it shows                                                      |
| -------- | ------------------------------------------------------------------ |
| `parsed` | the parsed term, as the JSON the formatter and the LSP consume     |
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
