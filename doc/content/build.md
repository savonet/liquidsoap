# Building Liquidsoap

## Forewords

Installing liquidsoap can be a difficult task. The software relies on an up-to date
OCaml compiler, as well as a bunch of OCaml modules and, for most of them, corresponding
C library dependencies.

Our recommended way of installing liquidsoap is via [opam](http://opam.ocaml.org/). `opam` can take
care of installing the correct OCaml compiler, optional and required dependencies as well as system-specific
package dependencies.

The `opam` method is described in details in the [documentation](doc/content/install.md).
We recommend that any interested user head over to this link to install the software via `opam`.

The remainder of this document describes how to compile liquidsoap locally for developers.

## Overview

Liquidsoap is compiled using [dune](https://dune.readthedocs.io/en/stable/), which is the most popular
OCaml build system at the moment. `dune` is tightly integrated with `opam` so, even if you are installing
from source using `dune`, `opam` remains an important tool.

Generally speaking, compiling from source may require the latest version of the liquidsoap code as well as its
dependencies. Some of its dependencies are optional and can be ignored at first and some are not.

Keep in mind that, although `opam` is generally aware of required minimal version for dependencies, `dune` is not.
If a dependency is outdated, `dune` compilation will simply fail, at which point your may have to figure out if
you need to update a dependency.

Each branch of liquidsoap is compiled using [github actions](https://github.com/savonet/liquidsoap/actions). When trying
to build a specific branch, if the CI passes with it then, most likely, you are missing a dependency, or it is not
the latest version.

## `opam` pinning

`opam` pinning is a mechanism to update `opam` with the latest version of a package, even before it is published to
the official opam repository. This is the easiest way to update a dependency to its latest version.

You can pin directly from a local git repository checkout:

```shell
git clone https://github.com/savonet/ocaml-metadata.git
cd ocaml-metadata
opam pin -ny .
```

You can also pin directly using a git url:

```shell
opam pin -ny git+https://github.com/savonet/ocaml-cry
```

See `opam pin --help` for more details about the available options.

## Dependencies

The best way to figure out what dependencies are required or optional and their versions is to use the latest `opam`
package. Since `liquidsoap` development is using `dune` and `opam`, the dependencies are kept in sync via the
local liquidsoap opam package(s) and this serves as the de-facto list of dependencies and their versions.

First, you should pin the latest liquidsoap code:

```shell
opam pin -ny git+https://github.com/savonet/liquidsoap
```

Then, ask `opam` to list all the dependencies for `liquidsoap`:

```shell
opam info liquidsoap
opam info liquidsoap-lang
```

This should give you a (long!) list of all dependencies. Then, you can query `opam` to see
what each dependency does. This is particularly useful for optional dependencies on `liquidsoap-core`
which provide opt-in features. For instance `opam info soundtouch` will let you know that this
package provides functions for changing pitch and timestretching audio data.

Lastly, there are two types of dependencies:

- Dependencies maintained by us
- Dependencies not maintained by us

For dependencies not maintained by us, most of the time, we rely on the latest published version. Very rarely should you
have to fetch/pin the latest version of these dependencies.

For dependencies maintained by us, we may break their API during our development cycle, and you maybe have to fetch/pin
the latest version when compiling the latest `liquidsoap` code. You may also have to check out a specific
branch when compiling `liquidsoap` from a specific development branch when the changes in the liquidsoap code are paired with
changes in one of our dependencies. Typically, this happens a lof with the `ffmpeg` binding.

## Environment variables

When compiling Liquidsoap from source, certain environment variables can be set to control the build process and customize the build
configuration. Here’s a brief overview of the relevant environment variables and their purposes:

- `IS_SNAPSHOT`: Set this variable to indicate whether you are building a snapshot version of Liquidsoap. It affects the version suffix and
  whether the Git commit is displayed.
- `LIQ_GIT_SHA`: Override Git commit hash (SHA) if the build system cannot automatically extract it from the repository.
- `LIQ_VERSION`: Override the displayed version of Liquidsoap.
- `LIQUIDSOAP_ENABLE_BUILD_CONFIG`: Determines whether the build configuration details are displayed during the build process.
- `LIQUIDSOAP_BUILD_TARGET`: Controls the runtime lookup paths for Liquidsoap components.
  - Set to `default`: Uses paths detected in the OPAM switch directory.
  - Set to `standalone`: Uses paths relative to the binary location, ideal for self-contained deployments.
  - Set to `posix`: Configures paths to standard system directories.

## Compiling

Once you have all dependencies installed, you should be able to compile via:

```shell
dune build
```

If an error occurs, you may need to see if you need to update a dependency. Hopefully, with a short iteration of this cycle,
you will end up with a successful build!

Once you have a successful build, you can also use the top-level `liquidsoap` script. This script builds the latest code and
executes it right away. It works as if you were calling the `liquidsoap` binary after installing it:

```shell
./liquidsoap -h output.ao
```

From here, you can start changing code, testing script etc. Happy hacking!
