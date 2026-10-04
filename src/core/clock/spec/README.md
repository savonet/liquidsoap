# Clock specification — as built

This directory describes what the clock code does today, read from the code.
It describes and does not judge: everything that looks wrong is in
[findings.md](findings.md), not in the text.

**Scope.** The clock library only: clocks, their registry, unification, the
tick, latency control, the animator, and the three text reports. Sources,
outputs, the scheduler and the time implementations are described only as the
contracts the clock relies on. How sources, audio servers and operators
with child clocks use the clock is described in
[clock.md §18](clock.md#18-how-the-clock-is-used), as the other half of those
contracts.

**Reader.** Someone rebuilding clocks without the code in view.

| File                                       | Content                                                                        |
| ------------------------------------------ | ------------------------------------------------------------------------------ |
| [clock.md](clock.md)                       | Part A. Entities, states, rules, algorithms, constants, failure behaviour.     |
| [reports.md](reports.md)                   | Part A. The three text reports, with exact formats.                            |
| [ocaml-notes.md](ocaml-notes.md)           | Part B. What is specific to OCaml and to the libraries used.                   |
| [tests.md](tests.md)                       | The test suite, by principle.                                                  |
| [findings.md](findings.md)                 | Defects, asymmetries, gaps and open checks. The agenda for the normative pass. |
| [known-complexity.md](known-complexity.md) | What the history shows to be hard, by situation. Partial: six areas mined.     |

No separate document is given to failure classification, crash safety or
security: a clock holds no durable state and crosses no trust boundary, and its
failure rules fit in [clock.md §13](clock.md#13-failure-behaviour).

## Vocabulary

- **Clock**: drives data production. One **tick** asks each animated source for
  one frame.
- **Handle**: what callers hold. Several handles can designate one clock after
  unification.
- **Source**: a producer attached to a clock. Its type is **output**, **active**
  or **passive**.
- **Animated source**: an output or an active source. It is called on every
  tick. A passive source produces only when another source reads from it.
- **Sync source**: something that sets the pace of a stream by its own means,
  such as a sound card.
- **Self-sync**: a clock that has a current sync source.
- **Sub-clock**: a clock ticked as part of another clock's tick.
- **Controller**: what animates a clock.
- **Animator**: the execution vehicle of a started, non-passive clock: a
  scheduler task or a thread.
- **Stream time**: `ticks × frame duration`.
- **Latency**: how far behind real time the stream time is.
