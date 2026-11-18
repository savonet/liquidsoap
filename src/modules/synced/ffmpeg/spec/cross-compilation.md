# Cross-compilation

The bindings MUST build for Windows from a Linux or macOS machine through the
[opam-cross-windows](https://github.com/ocaml-cross/opam-cross-windows)
toolchain, driven by `dune -x windows`, both as standalone opam packages and
embedded in a larger dune workspace. This is frozen surface
([binding-contract.md](binding-contract.md) §1.2).

The rules are the generic
[cross-compilation.md](../../spec/cross-compilation.md): build-time programs
(X1 to X3, X15), per-context pkg-config (X4 to X7), headers and variant
constants from the target (X8, X9), the link-flag filter (X10), what the
stubs may use (X11 to X13) and the two invocations (§3, X14). This file
states what FFmpeg adds.

Nothing in this file has been exercised on a Windows toolchain: see §4.

## 1. Model

The build-time programs are detection and the enumeration generator of
[build.md](build.md) §3. The generator receives the target's C compiler
command and the detected C flags, so that the tables follow the target's
FFmpeg.

## 2. Rules

- **X11.** The stubs compile against a static FFmpeg.
- **X13.** String duplication goes through FFmpeg's `av_strdup` and
  `av_strndup`.

## 3. The two invocations

### 3.1 As opam packages

One `ffmpeg-<lib>-windows` package per binding library, each built with

    dune build -p ffmpeg-<lib> -x windows -j <jobs> @install

### 3.2 Embedded in a workspace

The variables and values of the generic
[§3.2](../../spec/cross-compilation.md#32-embedded-in-a-workspace). The FFmpeg
libraries are static.

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
