# Cross-compilation

The bindings build for Windows from a Linux or macOS machine through the
[opam-cross-windows](https://github.com/ocaml-cross/opam-cross-windows)
toolchain, driven by `dune -x windows`. This is frozen surface: a rewrite
builds the same way, as a standalone set of opam packages and embedded in
liquidsoap.

[build.md](build.md) owns the detection algorithm (§2) and the enum generator
(§3). This file owns the rules that make both work when the machine that
builds is not the machine that runs.

## 1. What the build system does

### 1.1 Two contexts

`dune build -x windows` behaves as a workspace with one context stanza
`(context (default (targets windows)))`. It creates two build contexts:

| Context           | Build directory          | Compiler                              | Role                                           |
| ----------------- | ------------------------ | ------------------------------------- | ---------------------------------------------- |
| `default`         | `_build/default`         | the build machine's OCaml             | builds the programs the build itself runs      |
| `default.windows` | `_build/default.windows` | the findlib toolchain named `windows` | builds the libraries and executables that ship |

`@install` skips the `default` context: it exists only to supply build-time
programs to the other one.

The toolchain is the `ocaml-windows` package: an OCaml cross-compiler whose
C compiler is a MinGW-w64 GCC, registered with findlib so that
`ocamlfind -toolchain windows` resolves to it. Target libraries are installed
under a `windows-sysroot` directory of the opam prefix. A target library's
opam package is named `<name>-windows`.

### 1.2 Programs run by rules are always host programs

A rule in the `default.windows` context that runs `./foo.exe` runs
`_build/default/.../foo.exe`. No program built for the target is ever
executed during the build.

Consequences the bindings rely on:

1. The availability detector, the install-time checker, the all-available
   reducer, the include-path discoverer, the enum generator and the test and
   example rule generators are compiled for and run on the build machine.
2. Each of them is run once per context, with that context's variable
   expansions as arguments. The program learns which context it serves only
   from its arguments and environment.
3. A build-time program needs its own library dependencies installed for the
   build machine (`dune-configurator`, `unix`, `str`), and the binding
   libraries need theirs installed for the target. A dependency needed on
   both sides is listed twice: `foo` and `foo-windows`.

### 1.3 Variables in the target context

Rules pass these expansions to the build-time programs:

| Variable          | Passed to                         | Value in `default.windows`                                   |
| ----------------- | --------------------------------- | ------------------------------------------------------------ |
| `%{context_name}` | detector, include-path discoverer | `default.windows`                                            |
| `%{os_type}`      | detector                          | the target's OS type, `Win32`                                |
| `%{cc}`           | enum generator                    | the target C compiler command, with its flags, as one string |

A build-time program that reads the OS type from its own runtime gets the
build machine's. The detector takes the target's as an argument for that
reason and falls back to its own only when the argument is absent.

## 2. Rules the bindings follow

### 2.1 Per-context pkg-config selection

Both contexts run in one process environment, so one `PKG_CONFIG_PATH` cannot
serve both. The detector and the include-path discoverer therefore each select
pkg-config for the context they are run for, from environment variables named
after it: for `default.windows`, `PKG_CONFIG_PATH_default_windows` and
`PKG_CONFIG_default_windows`. [build.md](build.md) §2.1 and §2.2 own the
algorithm and the variables.

A context with neither variable set uses the ambient `PKG_CONFIG_PATH` and
`PKG_CONFIG` unchanged.

### 2.2 Forcing one context's selection onto every context

`LIQUIDSOAP_DUNE_TARGET` replaces the context name in that selection, in every
context ([build.md](build.md) §2.1). With
`LIQUIDSOAP_DUNE_TARGET=default.windows`, the rules of the `default` context
also detect against the Windows libraries.

### 2.3 Headers come from the target, hashes from the host

The enum generator preprocesses the FFmpeg headers found through the selected
pkg-config with the compiler named by `%{cc}`: the target's headers, read by
the target's preprocessor. Version-dependent enum members therefore follow the
FFmpeg the target links against.

The C constants standing for OCaml polymorphic variant tags are computed by
the generator itself, with the build machine's OCaml runtime, and written into
the generated header as literals. The target stubs compare against those
literals.

### 2.4 Link flags on Windows

The detector filters the link flags pkg-config returns when the OS type it was
given is `Win32` ([build.md](build.md) §2.4). It is given the target's OS type
as an argument, so the filter applies to the `default.windows` context and
never to `default`.

### 2.5 Nothing built for the target is run

Tests and examples are executables for the target. Their rules are generated
only when every library is available, and running them is attached to
aliases the cross build does not request.

## 3. The two ways the cross build is invoked

### 3.1 As opam packages

One `ffmpeg-<lib>-windows` package per binding library, each built with

    dune build -p ffmpeg-<lib> -x windows -j <jobs> @install

`-p` restricts the build to one package, so each package's detection runs
alone and sibling binding libraries come from the installed `-windows`
packages. The packages depend on `ocaml-windows`, on `dune`,
`dune-configurator` and `conf-pkg-config` for the build machine, and on the
`-windows` variant of each binding library they use. FFmpeg itself is an
external dependency taken from the MXE environment.

The package recipes set no per-context variable: the ambient
`PKG_CONFIG_PATH` points at the target's pkg-config directory for the whole
build.

The recipes have no install step of their own: opam installs from the
`.install` file the build leaves behind.

The newest packaged version at this snapshot is 1.2.1; the tree is at 1.4.0.
The 1.2.1 packages for `avutil`, `av` and `avfilter` each carry a patch that
replaces every `strndup` with `av_strndup` and includes `libavutil/mem.h`:
MinGW's C runtime has no `strndup`. The tree at this snapshot already uses
`av_strndup` and `av_strdup` everywhere and needs no patch.

### 3.2 Embedded in liquidsoap

liquidsoap builds the whole tree once:

    dune build -x windows --release <target>

with, in the environment:

| Variable                              | Value                             | Effect here                                                            |
| ------------------------------------- | --------------------------------- | ---------------------------------------------------------------------- |
| `PKG_CONFIG_PATH_default_windows`     | the target's pkg-config directory | §2.1                                                                   |
| `PKG_CONFIG_default_windows`          | the target's pkg-config wrapper   | §2.1                                                                   |
| `LIQUIDSOAP_DUNE_TARGET`              | `default.windows`                 | §2.2                                                                   |
| `LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL` | `true`                            | disables the install-time availability check ([build.md](build.md) §2) |
| `PKG_CONFIG_PATH`                     | unset on purpose                  | the `default` context finds no optional library by accident            |

The FFmpeg libraries are static (`x86_64-w64-mingw32.static`). liquidsoap adds
`-link <path>/libavutil.a` to its own final link line.

## 4. What this constrains in a rewrite

- Every build-time program stays a host program whose behaviour depends only
  on its arguments, its environment and the files it is pointed at. It never
  inspects its own platform to learn about the target.
- Availability, C flags and link flags are per context.
- No rule runs a binary that links the bindings, outside test and example
  aliases.
- The stubs compile with MinGW-w64 GCC against a static FFmpeg. String
  duplication goes through FFmpeg's `av_strdup` and `av_strndup`, never the
  C library's.
- The `avutil` stubs use POSIX threads (`pthread.h`), which the MinGW-w64
  toolchain supplies through winpthreads. It is the only system header the
  stubs include beyond ISO C, the OCaml runtime and FFmpeg.
