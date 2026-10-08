# Clock specification

What a clock MUST, SHOULD and MAY do. It is written to be implemented from,
without the code in view.

**Scope.** Clocks, their registry, unification, the tick, pacing, the
animator, failure, and what a clock reports about itself. Sources, outputs,
the scheduler and the time sources appear only as the contracts the clock
relies on, each stated where it is used.

**Reading order**, which is also the order to build in:

| File                                       | Content                                                                              |
| ------------------------------------------ | ------------------------------------------------------------------------------------ |
| [clock.md](clock.md)                       | Part A. Entities, states, registry, tick, sub-clocks, failure, shutdown, parameters. |
| [unification.md](unification.md)           | Part A. The protocol that makes two handles one clock.                               |
| [pacing.md](pacing.md)                     | Part A. Sync sources, rest and lateness, the time box, the animator.                 |
| [observability.md](observability.md)       | Part A. The status record, the log events, the two reports.                          |
| [conformance.md](conformance.md)           | The checks an implementation is run against, by principle.                           |
| [known-complexity.md](known-complexity.md) | What the history shows to be hard, by situation, each with its rule. Partial.        |
| [ocaml-notes.md](ocaml-notes.md)           | Part B. Notes on the OCaml implementation this was extracted from. Not normative.    |

No separate document is given to crash safety or security: a clock holds no
durable state and crosses no trust boundary. Failure is
[clock.md §11](clock.md#11-failure).

**Compatibility surface.** The four sync mode names
([clock.md §3](clock.md#3-sync-modes)) and the existing setting names and
defaults ([clock.md §13](clock.md#13-parameters)). Nothing else: for errors,
logs and reports the content is specified and the wording is free.

## Vocabulary

- **Clock**: drives data production. One **tick** asks each animated source for
  one frame.
- **Handle**: what callers hold. Several handles can designate one clock after
  unification.
- **Source**: a producer attached to a clock. Its type is **output**, **active**
  or **passive**.
- **Animated source**: an output or an active source. It is called on every
  tick. A passive source produces only when another source reads from it.
- **Stream time**: `ticks × frame duration`.
- **Sync source**: what sets the pace of a stream by its own means, such as a
  sound card. It is **self-paced** or **timed**, and it **blocks** or not.
- **Tracked** sync source: the one a clock's animated sources report.
  **Followed**: the one an automatic clock takes its pace from. **Self-sync**:
  a clock that follows one.
- **Time source**: where "now" comes from, and how to wait on it.
- **Lateness**: how far behind its time source a clock's stream time is.
- **Rest**: a paced clock that is ahead waits for its time source.
- **Time box**: the longest a clock keeps a scheduler worker. **Release**:
  giving the worker back at its end.
- **Animator**: what runs a started, non-passive clock: a scheduler task or a
  thread.
- **Passive clock**: a clock with no animator. Its **controller** ticks it.
- **Parent**: the clock inside whose ticks a passive clock is ticked.
  **Owner**: an external entity that alone may tick a passive clock. The
  controller is the owner if there is one, else the parent.
- **Sub-clock**: a passive clock registered on its parent.
- **Child clock**: the sub-clock an operator gives its child.
- **Stop signal**: what abandons a tick when the application stops. Not an
  error.
