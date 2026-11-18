# Conventions

How the specifications of Liquidsoap are written. Every tree follows these;
a tree states only where it departs, with its reason.

## 1. Normative language

- MUST, SHOULD and MAY are used as in RFC 2119.
- **Part A** is normative: behaviour any implementation could meet, written
  to be implemented from without the code in view. **Part B**
  (`ocaml-notes.md` or `language-notes/`) maps Part A onto the OCaml code.
  It is not normative, and Part A governs where they differ.
- A rule gives its reason where the reason is not obvious, in a "Reason" or
  "Rationale" paragraph next to it.

## 2. One owner per rule

- Every rule has one owner, the most general document where it holds. Other
  documents link to it, and do not restate it.
- The levels, from most general: the project ([README](README.md)), the
  streaming model and clocks, the generic seams of outputs and encoders, then
  components. A rule found in a component that holds beyond it moves up.
- **Exception: vendored modules.** Each directory under
  `src/modules/synced/` is mirrored, alone, into a repository of its own. Its
  specification is self-contained: it states the rules it relies on and links
  nowhere outside its directory, even where a rule holds more widely.
- A component that needs a different behaviour from a general rule states
  the departure next to the rule it departs from, with its reason, and links
  the general rule.
- A term is defined once, at the most general level that uses it. A lower
  level does not redefine it; a local meaning gets another word.

## 3. Documents of a tree

- **README.md**: scope, what is out of scope and who owns it, a reading
  order table that is also the order to build in, the tree's vocabulary,
  its compatibility surface, and which cross-cutting concerns get no
  document of their own and why.
- **Normative files**: each opens with what it owns and what it relies on.
  A seam has one contract file and one file per implementation, answering
  every section of the contract under the same number; a section an
  implementation does not need is answered "nothing".
- **conformance.md**: the checks an implementation is run against, grouped
  by principle, each saying what is binding. It lists the doubles a harness
  needs, marks checks drawn from known complexity (K) and from the binding
  scenarios (S), and keeps a table of the checks in the tree with whether
  each failed before. It follows [testing.md](testing.md).
- **known-complexity.md**: what the history shows to be hard, implementation
  free. Each entry: Situation, What went wrong, Why it is hard, How to tell,
  Rule (a link to the rule that answers it), and the count and traces.
- **ocaml-notes.md** or **language-notes/**: Part B.

Process documents (briefs, handoffs, plans, reviews, remaining work) do not
live in a specification tree. A ruling or a scenario one of them carries is
written into the specification before it is dropped.

## 4. Logs and errors

- What a log line or an error says is binding where a rule states it; the
  wording is free.
- Log levels are named: `critical`, `severe`, `important`, `info`, `debug`
  (1 to 5).

## 5. Compatibility surface

Each README states its tree's compatibility surface. By default:

- script-visible names (operators, parameters, methods, callbacks and their
  arguments), `settings.*` keys and the `liq_*` metadata keys are hard to
  change: each change is motivated, explained and confirmed;
- bytes on disk or on the wire, log wording and internal interfaces change
  freely, unless the tree says otherwise.
