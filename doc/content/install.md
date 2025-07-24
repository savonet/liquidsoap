# Installing Liquidsoap

You can install liquidsoap using binary builds, with OPAM or from source.

Binary builds are provided with our releases, either in the form of debian/ubuntu and alpine
packages or as docker images (also for debian or alpine). Your favorite distribution may also have
binary packages.

The binary package and docker images we provide are compiled in two flavors:

- The main `liquidsoap` packages are compiled with all available features and functions. This is a good starting point for general-purpose development
- Binary packages and docker images labelled `-minimal` are compiled without the extra libraries and with a limited set of essential optional features

Minimal builds are useful if you are concerned about size or memory usage. They also reduce the chances of running into issues that could be introduced
by optional dependencies that you do not use. If your script works with them, they are recommended over the fully featured builds for production.

Each binary build that we provide have a corresponding `*.config` file. This is a text file that lists all the features included in a specific
build. You can consult it to know what features are available. You can also get the same information by calling `liquidsoap --build-config`, for instance
when using a docker image.

Binary packages and docker images are useful in that they provide a readily available liquidsoap installation. If you
need more finer-grained build or if your distribution/OS does not have a binary build, you can
install via OPAM, which is a very convenient package manager that can compile liquidsoap from sources
and knows how to handle external dependencies for most OS/distributions.

Lastly, compiling from source should be reserved to developers.

- [Debian/Ubuntu](#debianubuntu)
- [Alpine](#alpine)
- [Docker](#docker)
- [Windows](#windows)
- [Using OPAM](#install-using-opam)
- [From source](#installing-from-source)

## Debian/Ubuntu

We generate debian and ubuntu packages automatically as part of our [release process](https://github.com/savonet/liquidsoap/releases). Otherwise, you
can check out the official [debian](https://packages.debian.org/liquidsoap) and [ubuntu](https://packages.ubuntu.com/liquidsoap) packages.

For debian packages, we recommend using debian `trixie` as the current debian stable carries rather old upstream libraries. When installing our own
packages on debian, you will also need to use the [deb-multimedia.org](https://www.deb-multimedia.org/) packages. These packages provide up-to date
libraries and also support for `fdk-aac` encoding in `ffmpeg`!

## Alpine

Alpine packages are also provided as part of our [release process](https://github.com/savonet/liquidsoap/releases).

## Docker

We provide production-ready docker images via [Docker hub](https://hub.docker.com/r/savonet/liquidsoap).
Docker images are tagged with a release tag (e.g. `v2.1.4`) and with the sha of their git commit (e.g. `a24bf49`).

For instance, to fetch release `2.3.1`, you would do:

```shell
docker pull savonet/liquidsoap:v2.3.1
```

Please note that images tagged with a release tag may change while images tagged with a commit sha will not.

## Windows

You can download a liquidsoap for windows from our [release page](https://github.com/savonet/liquidsoap/releases).

## Install using OPAM

The recommended method to install liquidsoap from source is by using the [OCaml Package
Manager](http://opam.ocaml.org/). OPAM is available in all major distributions
and on windows. We actively support the liquidsoap packages there and its
dependencies. You can read [here](https://opam.ocaml.org/doc/Usage.html) about
how to use OPAM. In order to use it:

- [you should have at least OPAM version 2.1](https://opam.ocaml.org/doc/Install.html),
- not all version of the OCaml compiler are supported. You can run `opam info liquidsoap-lang` to find out.

You can create a switch for a specific OCaml version as follows:

```
opam switch create <ocaml version>
```

A typical installation with most expected features is done by executing:

```
opam install ffmpeg liquidsoap
```

This will install `liquidsoap` along with the optional `ffmpeg` package, which provides most
of the expected functionalities (encoding, decoding, metadata support etc) out of the box.

The `opam` installer also handles external dependencies that is, dependencies from your operating system
that are required for your install. Typically, this would be the `ffmpeg` shared libraries here, as well
as `libcurl`, which is required for `liquidsoap` to install.

In most cases, `opam` will simply ask for your permission to install these dependencies on your behalf. In
some cases, however, you will have install them yourself.

Most of liquidsoap's dependencies are only optional. For
instance, if you want to enable opus encoding and decoding after you've already
installed liquidsoap, you should execute the following:

```
opam install opus
```

This will install `opus` and its dependencies and recompile `liquidsoap` to take advantage of it.

`opam info liquidsoap` should give you the list of all optional dependencies
that you may enable in liquidsoap.

**Note**

`opam` handles external dependencies via your system's packaging. In order to build
some of the associated OCaml modules, macos users using `homebrew` might need to add
the following to their environment/shell configuration:

```shell
export CPATH=/opt/homebrew/include
export LIBRARY_PATH=/opt/homebrew/lib
```

## Installing from source

See the [build instructions](build.html)
