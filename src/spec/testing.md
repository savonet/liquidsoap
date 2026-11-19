# Testing (Part A)

What every conformance suite of these specifications does, whatever it
tests. A component's conformance file adds its own checks, doubles and
harness, and does not restate these.

## 1. A pass must be able to say no

- **A check counts once it was seen to fail**: against the build before the
  fix, or with the guard taken out. A positive control is planted and seen
  reported before a clean run counts, for sanitisers and for any check that
  reports through logs or files.
- **A suite asserts that it ran.** It counts what actually ran and checks
  the count before reporting a pass. A test program that made no check
  fails. A cached replay is not a run.
- **A skip is never a pass.** Skips are named and counted, and reported as
  missing coverage.
- **A timeout is a failure**, and so is a crash.
- **An absence check also asserts the event happened**: "no second mark"
  holds only once the first mark was seen.
- **Every positive check has a negative twin** where one makes sense: what
  must be refused is refused.

## 2. Binding and incidental

- A check asserts what the specification binds and nothing else. Wording of
  messages and logs, timing, and the order of independent events are
  incidental unless a rule binds them.
- Audio is compared by sample, never per frame, and a check of a streaming
  property runs with two frame sizes.
- A reference output (a plot, a checksum, a snapshot) is regenerated only
  once the difference is understood, never accepted blindly.

## 3. Fixtures and misuse

- Each fixture records the property that makes it trigger its condition.
- Misuse is tested like use: the same misuse cases on every copy of an
  operation.
- Tests are independent of each other and of the order they run in.
