# Unification (Part A)

A protocol document: goal, model, steps, guarantees, and what a failed
unification leaves behind.

## 1. Goal

Clocks are created freely, one per source that has none, while a script is
evaluated. Connecting two sources says that they belong to one clock.
`unify(a, b)` makes two handles designate one clock.

It goes on at run time, from several threads, for as long as the script
creates sources, while clocks are ticking.

## 2. Model

- A **handle** is what callers hold. Every handle designates exactly one clock
  at any moment. Several handles may designate the same clock.
- A unification **merges** one clock into another: the one merged away
  disappears, the **survivor** takes what it held, and every handle that
  designated the first now designates the survivor.
- **The clock merged away is always stopped.** A started clock is referred to
  by what is ticking it and MUST be the survivor.
- Clocks are **nested** through the parent relation
  ([clock.md §1](clock.md#1-entities)): the ancestors of a clock are its
  parent, its parent's parent, and so on. Since a parent is a handle, a merge
  changes the ancestors of every clock below the clock merged away.
- **Pending mode** of a clock: its sync mode, or `stopping` while it is
  stopping.

A unification is a **plan** followed by a **commit**. The plan checks
everything and changes nothing. The commit changes everything and cannot
fail.

## 3. Plan

The plan for `unify(a, b)` is a list of merges, built as follows. Any
violation ends the plan with its error.

1. If `a` and `b` designate the same clock, or the plan already holds a merge
   of these two clocks: nothing to add.
2. **Direction.**
   - both stopped with the same sync mode: merge `a` into `b`;
   - `a` stopped, and `a` is automatic or its mode equals `b`'s pending mode:
     merge `a` into `b`;
   - `b` stopped, and `b` is automatic or its mode equals `a`'s pending mode:
     merge `b` into `a`;
   - otherwise: **conflict**.
3. **Owners.** Only when both clocks are passive: they MUST have the same
   owner, or none both. Otherwise: **controller conflict**.

   Two passive clocks with different owners, or one with an owner and one
   without, therefore never unify. This is what gives an operator a child
   clock of which it is the only reader
   ([clock.md §15](clock.md#15-child-clocks)).

4. **Parents.** When both clocks have a parent: add the plan for `unify` of
   the two parents. So two child clocks unify exactly when the clocks that
   control them do.
5. **Nesting.** With every merge of the plan taken as done, neither clock may
   be an ancestor of the other: otherwise **loop**. The same check is made
   for every clock whose ancestors the plan changes.

A clock that is not passive has no owner and no parent
([clock.md §1](clock.md#1-entities)), so steps 3 and 4 never apply to it.

## 4. Commit

For each merge of `x` into `y` in the plan, in an order where no clock is
merged away before a merge that names it:

1. Move `x`'s pending sources to `y`, skipping duplicates.
2. Move `x`'s sub-clock registrations to `y`. Entries that now designate one
   clock become one entry, with the registrants of both. If `y` is started,
   a sub-clock that has just become registered on it is started, as for any
   registration ([clock.md §10](clock.md#10-sub-clocks)).
3. Move `x`'s error handlers to `y`.
4. Id: if `y` has none it takes `x`'s; if both have one, `y` keeps its own and
   the event is logged.
5. Stack: if `y` has none it takes `x`'s. Parent: the same.
6. Point every handle of `x` at `y`.
7. Take `x` out of the registry ([clock.md §5](clock.md#5-registry)).

`y` keeps its sync mode, state, stop reason, owner and streaming state. A merge into a started clock leaves it ticking; the sources that
joined it are activated at its next tick.

## 5. Guarantees

- **Atomic.** A unification either commits its whole plan or changes nothing.
  It never commits part of a plan.
- **Exclusive.** Unifications are mutually exclusive, and exclusive with the
  start and stop transitions of the clocks they name: the state of a clock
  MUST NOT change between the plan and the end of the commit.
- **Readers are free.** Reading through a handle never waits for a
  unification. A reader sees, for each handle, the clock it designated before
  or after; it MAY see some merges of a plan done and others not.
- **Total handles.** Every handle designates a clock at every moment, also
  with many threads unifying and reading at once.
- **No leftovers.** After a commit, no set of clocks (the registry, any
  sub-clock list) holds a clock merged away, and none holds one clock twice.
- **No cycle.** No sequence of creations, registrations and unifications
  yields a clock that is its own ancestor.
- **Symmetric outcome.** Whether a unification succeeds does not depend on
  the order of its two arguments. Which clock survives may.
- **Bounded cost.** The cost of resolving a handle, and the memory kept on
  behalf of clocks merged away, MUST NOT grow with the number of merges. A
  script that creates, unifies and discards clocks for as long as it runs
  uses flat memory.

## 6. Failure

A unification fails with one of **conflict**, **loop** or **controller
conflict** ([clock.md §11](clock.md#11-failure)), raised to the caller of
`unify`. It is an error of the script, not of a clock: no clock fails.

A failed unification leaves nothing behind. Every field of every clock, every
handle and every set is as it was, the controllers and parents included.

## 7. Not promised

- Which of two stopped clocks of the same mode survives.
- That a clock merged into a `stopping` clock runs soon: its sources wait as
  pending sources until that clock is started again.
- Undoing a unification. Two sources once put on one clock stay on it.
