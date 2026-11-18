# Build, detection and generated code

What decides whether a binding library is built, with which flags, how code
generated from a C library's headers is produced, and what an installed C
header promises. It relies on [cross-compilation.md](cross-compilation.md) for
the selection of pkg-config per context, on the environment variables of the
[README](README.md#compatibility-surface), and on
[compatibility.md](compatibility.md) for the floor.

A binding's own build file states its packages and libraries, the C
libraries each requires, its generators and tables, and its installed
headers. Advice specific to dune is in
[language-notes/build-system.md](language-notes/build-system.md).

## 1. Scope

Every binding library of `src/modules/synced/` and every build context it is
built in.

## 2. Availability detection

### 2.1 What detection decides

For each binding library and each build context, detection decides whether the
library is **available** and, when it is, the C flags its stubs compile with
and the link flags they link with.

- **D1.** Detection MUST be a build action that runs per library and per
  context. It MUST NOT fail the build when a library is unavailable.
- **D2.** A library is available when all of these hold:
  1. its name is not listed in `LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS`;
  2. pkg-config is found;
  3. pkg-config reports every C library the binding requires for it, at the
     stated minimum version (D6);
  4. every binding library it depends on is available.
- **D3.** The C flags of an available library are the flags pkg-config reports
  for its C libraries. Its link flags are the link flags pkg-config reports
  for them, after the filter of §2.4.
- **D4.** Flags MUST travel as argument vectors from pkg-config to the
  compiler. They MUST NOT be pasted into a shell command line.
- **D5.** Detection MUST be re-evaluated when the environment it reads
  changes, including a C library installed or upgraded under the same prefix
  since the previous build.

### 2.2 pkg-config selection

pkg-config is selected per context by
[cross-compilation.md §2.1](cross-compilation.md#21-per-context-pkg-config-selection)
(X4 to X7). The variables are listed, with their meaning, in the
[README](README.md#compatibility-surface).

### 2.3 Minimum versions

- **D6.** The minimum version of each C library in detection is the version
  it has in the floor release ([compatibility.md](compatibility.md) V3).

### 2.4 Windows link-flag filter

pkg-config for a static library returns linker options written for a C link
line. Some are rejected or misread when they pass through the OCaml
toolchain.

- **D7.** When the target's OS type is `Win32`, these link flags MUST be
  removed and every other flag kept in order:
  - every flag of three or more characters that starts with `-Wl`;
  - `-static-libgcc`;
  - `-lssp`;
  - `-lmingw32`.
- **D8.** For any other OS type the link flags are unchanged. The filter never
  applies to C flags.

The OS type is the target's, as passed to detection
([cross-compilation.md](cross-compilation.md) X10).

### 2.5 Gating

- **D9.** A binding library is part of the build exactly when it is
  available. When unavailable, its stubs are not compiled and nothing can
  depend on it.
- **D10.** Availability is the only reason a library may be absent. With a
  library available, any failure to generate its code, compile its stubs or
  resolve its dependencies MUST fail the build. No library may be declared
  optional to the build system on top of its availability gate.
- **D11.** The build MUST report, in its output, the availability of each of
  the binding's libraries.

### 2.6 Install-time check

Every package may legally be empty, so that an unavailable library does not
break a workspace build.

- **D12.** The installation target of each binding package MUST fail, with a
  message naming the library, when that library is unavailable.
- **D13.** The check is disabled when `LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL`
  equals `true`. A package whose library is unavailable then installs empty.

### 2.7 Compiler warnings

- **D14.** The stubs SHOULD be compiled with the compiler's general warning
  sets enabled.
- **D15.** A warning MUST NOT be an error in a packaged build. Warnings are
  promoted to errors in the development profile and in continuous integration
  only, where the floor and the ceiling are built
  ([tests.md §1](tests.md#1-versions)).

A warning promoted to an error is tested only on the versions someone builds;
a packaged build meets versions nobody built.

## 3. Generated code

A binding MAY generate code (enumeration tables, constants) from the C
library's headers at build time. Its own build file lists the generators,
their inputs and their outputs.

### 3.1 Source of truth

- **G3.** What a generator reads from a header MUST be obtained by running the
  target's C compiler as a preprocessor on a translation unit that includes
  the header by its public include path, with the library's detected C
  flags. The compiler finds the header exactly as it will when compiling the
  stubs.
- **G7.** A generator MUST exit with a failure, naming the header and what it
  looked for, when: the preprocessor cannot be run or fails; the header is
  not found; what it looked for is not found; a table has no member. It MUST
  NOT fall back to another way of reading the header.

### 3.2 Variant constants in C

An OCaml constant polymorphic variant is an immediate integer. For a
constructor name `s`, without the backquote:

1. `h = 0`; for each byte `c` of `s`, `h = 223 * h + c`, in unsigned
   arithmetic.
2. Keep the low 31 bits of `h`.
3. The constant is `2 * h + 1`, taken as a signed 32-bit integer and
   sign-extended to the platform word.

The number is directly comparable to an OCaml value received by a stub and can
be returned as an OCaml value without further encoding.

Known answers: `Jpeg` = 1652440465, `Unspecified` = -789514449, `Audio` =
1968951661, `Video` = -1806497609, `None` = 1741061553, `_0rgb` =
-1505465991.

- **G10.** A variant constant used by hand-written stub code MUST come from
  this algorithm, through generated definitions. A constant typed by hand is
  not allowed.
- **G11.** A variant constant MUST be held in C in a type wide enough for a
  negative OCaml immediate. It MUST NOT pass through a C enumeration type.

## 4. Installed C headers

A binding library MAY install C headers that the stubs of dependent binding
libraries include. Their file names are compatibility surface.

- **H1.** Every installed header MUST be includable by any number of
  translation units of one program: it has an include guard and defines no
  object or function with external linkage.
- **H3.** A function declared in an installed header MUST state, in the
  header, whether it requires the OCaml runtime lock and whether it can raise.
