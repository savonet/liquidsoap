# Version compatibility

The rules every synced binding follows about the versions of its C library
and of OCaml it supports. Each binding's own compatibility file states its
floor, its ceiling, its OCaml minimum, and what it relies on from the floor.

## 1. The C library's range

- **V3.** The floor stated to users, the floor detection enforces
  ([build.md](build.md) D6) and the oldest release the conformance suite runs
  against ([tests.md §1](tests.md#1-versions)) MUST be the same release.

A floor that detection accepts and nobody builds is found by users, in the
middle of an install.

The ceiling is the newest release the conformance suite runs against; it is
the newest supported release.

## 2. Version-dependent behaviour

Rules for a conditional a release of the C library makes necessary:

- **V5.** A conditional MUST compare a library version, and MUST name the
  version at which the newer facility appeared. Both sides MUST be reachable
  inside the supported range; a conditional with one reachable side is
  removed.
- **V6.** A feature test on a preprocessor name is allowed only for a name
  checked to be a macro in the headers of every supported release. An
  enumeration member is not a macro.
- **V7.** The stubs MUST build without deprecation warnings against the
  ceiling. A library that removes deprecated API at a major release breaks
  the next one otherwise.

## 3. OCaml range

- **V8.** Each binding states its minimum OCaml version.
- **V9.** The bindings MUST be correct in a program that runs several domains
  ([binding-contract.md §6](binding-contract.md#6-blocking-and-concurrency)).
- **V10.** The conformance suite runs on the minimum and on the newest
  released OCaml ([tests.md §1](tests.md#1-versions)).
