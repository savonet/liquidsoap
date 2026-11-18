# Conformance

What the conformance suite of every synced binding does beyond
[testing.md](../../../spec/testing.md), which it follows and does not
restate. A binding's own tests file lists its requirements, fixtures and
doubles, and the build alias the suite is attached to.

## 1. Versions

- The suite MUST run against the floor and the ceiling of the C library
  ([compatibility.md](compatibility.md) V3), with warnings as errors
  ([build.md](build.md) D15).
- It MUST run on the minimum and on the newest released OCaml version
  ([compatibility.md](compatibility.md) V10).

## 2. Harness

The harness rules of [testing.md §1](../../../spec/testing.md#1-a-pass-must-be-able-to-say-no)
and [§3](../../../spec/testing.md#3-fixtures-and-misuse) apply to every
binding suite. Their identifiers, used by binding trees:

| Rule | Owner                                                                                          |
| ---- | ---------------------------------------------------------------------------------------------- |
| H1   | The suite asserts that it ran; a cached replay is not a run (testing.md §1)                    |
| H2   | A test program that made no check fails (testing.md §1)                                        |
| H3   | A skip is named, counted, and never a pass (testing.md §1)                                     |
| H4   | A check counts once seen to fail; a sanitiser first reports a planted defect (§1)              |
| H5   | Requirements are independent of each other and of their order (testing.md §3), and below       |
| H6   | A timeout is a failure (testing.md §1)                                                         |
| H7   | A crash is a failure (testing.md §1); a death by signal is reported as such and names the step |

- **H5.** A failed requirement does not stop the others from running or
  being reported. A requirement does not read a file another requirement
  wrote.
- **H8. Declared inputs.** The suite's result depends on the C libraries it
  loaded: changing them re-runs it.

## 3. Run configurations

Three run configurations are part of every binding's suite:

- **Collection at every allocation.** Every operation that returns a compound
  value or takes a closure runs under a runtime that collects and compacts at
  each allocation, and its results are compared with a reference run
  (contract B1). A small minor heap is not this check.
- **Address and leak sanitisers.** The whole suite runs under both, with the
  runtime's own lifetime allocations suppressed and nothing else (H4).
- **Concurrent use.** For each handle with a guard, two threads, and two
  domains, call its operations at once: every call returns or raises the
  in-use error, and the sanitisers stay silent (contract M5, M6). The
  binding's hot loops run at the same speed, within measurement noise, with
  the guards compiled out (contract M12).

## 4. Build checks

| #   | Requirement                                                                                                                                                                      | Rule                                             |
| --- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------ |
| 4.1 | With the C library present and one library's stubs broken, the build fails. With it absent, the build succeeds, reports every library unavailable, and the install target fails. | [build.md](build.md) D9–D12                      |
| 4.2 | Installing the C library and rebuilding, with no clean, enables the libraries. Upgrading it in place regenerates generated code.                                                 | build D5                                         |
| 4.3 | The stubs include nothing beyond what X12 allows.                                                                                                                                | [cross-compilation.md](cross-compilation.md) X12 |
| 4.4 | Both invocations of cross-compilation §3 succeed unchanged, for a binding built for Windows.                                                                                     | cross-compilation §3                             |
