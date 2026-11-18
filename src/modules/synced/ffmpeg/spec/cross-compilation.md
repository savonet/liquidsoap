# Cross-compilation

The bindings MUST build for Windows from a Linux or macOS machine through the
[opam-cross-windows](https://github.com/ocaml-cross/opam-cross-windows)
toolchain, driven by `dune -x windows`, both as standalone opam packages and
embedded in a larger dune workspace. This is frozen surface.

[build.md](build.md) owns detection (§2) and enumeration generation (§3). This
file owns the rules that keep both correct when the machine that builds is not
the machine that runs.

Nothing in this file has been exercised on a Windows toolchain: see §4.

## 1. Model

### 1.1 Two contexts

A cross build has two build contexts:

| Context | Compiler                                  | Role                                           |
| ------- | ----------------------------------------- | ---------------------------------------------- |
| host    | the build machine's OCaml and C compilers | builds the programs the build itself runs      |
| target  | the cross toolchain                       | builds the libraries and executables that ship |

With `dune -x windows` the host context is named `default` and the target
context `default.windows`. The target toolchain is an OCaml cross-compiler
whose C compiler is MinGW-w64 GCC. A library needed in the target context is
installed by an opam package named `<name>-windows`.

### 1.2 Build-time programs are host programs

A program that a build rule runs is built for, and runs on, the build machine,
whichever context the rule belongs to.

- **X1.** No rule outside the test and example aliases may run a program that
  links a binding library or any target object.
- **X2.** A build-time program MUST learn everything about the target from
  its arguments, its environment and the files it is pointed at. It MUST NOT
  inspect its own platform, its own OCaml runtime's configuration or its own
  compiler to decide something about the target.
- **X3.** A build-time program's own library dependencies are host
  dependencies; the binding libraries' dependencies are target dependencies.
  A dependency needed on both sides is declared on both.
- **X15.** A build-time program MUST build and run on a build machine that
  has no native-code OCaml compiler.

### 1.3 What the build system passes down

A rule of the target context MUST pass to the build-time programs it runs:

| Value                           | Used by                | Why                                                         |
| ------------------------------- | ---------------------- | ----------------------------------------------------------- |
| the context name                | detection              | selects the pkg-config environment (§2.1)                   |
| the target's OS type            | detection              | selects the link-flag filter (§2.4)                         |
| the target's C compiler command | enumeration generation | the target's preprocessor reads the target's headers (§2.3) |
| the detected C flags            | enumeration generation | the same flags the stubs are compiled with                  |

## 2. Rules

### 2.1 Per-context pkg-config selection

Both contexts run in one process environment, so one `PKG_CONFIG_PATH` cannot
serve both.

- **X4.** Detection MUST select pkg-config per context from the variables
  `PKG_CONFIG_PATH_<ctx>` and `PKG_CONFIG_<ctx>`, where `<ctx>` is the context
  name with every `.` replaced by `_`. For `default.windows` these are
  `PKG_CONFIG_PATH_default_windows` and `PKG_CONFIG_default_windows`.
  [build.md](build.md) §2.2 owns the algorithm.
- **X5.** A context with neither variable set MUST use the ambient
  `PKG_CONFIG_PATH` and `PKG_CONFIG` unchanged.
- **X6.** The selection MUST be implemented once. Every build-time program
  that needs pkg-config obtains its result from that one implementation.

### 2.2 Forcing one context's selection onto every context

- **X7.** When `LIQUIDSOAP_DUNE_TARGET` is set, its value MUST replace the
  context name in the selection of §2.1, in every context.

With `LIQUIDSOAP_DUNE_TARGET=default.windows`, the host context also detects
against the target's libraries. An embedding workspace relies on this so that
the host context finds no optional library by accident.

### 2.3 Headers and flags come from the target

- **X8.** Enumeration tables MUST be derived from the headers the target's C
  compiler finds with the target's detected C flags, as read by the target's
  preprocessor. The generator MUST NOT search for headers by any procedure of
  its own. [build.md](build.md) §3.2 owns the rule.

A cross build from a machine whose own FFmpeg is another version therefore
produces tables for the target's FFmpeg.

- **X9.** The constants that stand for OCaml polymorphic variant tags in C
  MUST be computed by the algorithm of [build.md](build.md) §3.6, which gives
  the same number on every host and for every target word size. They MUST NOT
  depend on the word size of the machine that runs the generator.

### 2.4 Link flags on Windows

- **X10.** The link-flag filter of [build.md](build.md) §2.4 MUST be applied
  according to the target's OS type as passed to detection, never according to
  the platform detection runs on.

### 2.5 What the stubs may use

- **X11.** The stubs MUST compile with MinGW-w64 GCC against a static FFmpeg.
- **X12.** The stubs MUST include only ISO C headers, the OCaml runtime's
  headers, FFmpeg's public headers and POSIX threads. MinGW-w64 supplies POSIX
  threads through winpthreads.
- **X13.** The stubs MUST NOT call a C library function outside ISO C. String
  duplication goes through FFmpeg's `av_strdup` and `av_strndup`: MinGW's C
  runtime has no `strndup`.

## 3. The two invocations

Both MUST keep working without change to the caller.

### 3.1 As opam packages

One `ffmpeg-<lib>-windows` package per binding library, each built with

    dune build -p ffmpeg-<lib> -x windows -j <jobs> @install

- `-p` restricts the build to one package. Each package's detection runs
  alone, and sibling binding libraries come from the installed `-windows`
  packages.
- The recipe sets no per-context variable: the ambient `PKG_CONFIG_PATH`
  points at the target's pkg-config directory for the whole build (X5).
- The recipe has no install step of its own: the build leaves an install
  description that opam consumes.
- **X14.** The build of a package MUST need no patch to the sources on any
  supported target.

### 3.2 Embedded in a workspace

The enclosing project builds the whole tree once with `dune build -x windows`
and sets, in the environment:

| Variable                              | Value                             | Rule                            |
| ------------------------------------- | --------------------------------- | ------------------------------- |
| `PKG_CONFIG_PATH_default_windows`     | the target's pkg-config directory | X4                              |
| `PKG_CONFIG_default_windows`          | the target's pkg-config wrapper   | X4                              |
| `LIQUIDSOAP_DUNE_TARGET`              | `default.windows`                 | X7                              |
| `LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL` | `true`                            | [build.md](build.md) §2.6       |
| `PKG_CONFIG_PATH`                     | unset                             | X5 then finds nothing by itself |

These five names and their meaning are compatibility surface.

The FFmpeg libraries are static. The enclosing project adds what its own final
link line needs; the bindings supply the link flags pkg-config reports, after
the filter of X10.

## 4. Not verified

No rule of this file has been run on a Windows toolchain or on macOS. In
particular these rest on documentation and reasoning only:

- the value the build system gives as the target's OS type and the exact form
  of the target's C compiler command;
- that the opam recipes install correctly with no install step of their own;
- that the static link flags pkg-config returns are complete after the
  filter;
- that the stubs build against winpthreads;
- X9 on a 32-bit target.

[tests.md](tests.md) §9 lists the checks a cross build must pass.
