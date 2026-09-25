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

- [Package repositories](#package-repositories)
- [Debian/Ubuntu](#debianubuntu)
- [Alpine](#alpine)
- [Docker](#docker)
- [Windows](#windows)
- [Using OPAM](#install-using-opam)
- [From source](#installing-from-source)

## Package repositories

If you would rather have `apt` or `apk` keep liquidsoap up to date than download a package by hand, we
publish a repository for each release. Setting one up is a single command, which asks which release you
want and configures the matching repository:

```shell
curl -fsSL https://repo.liquidsoap.info/setup.sh | sudo sh
```

You can also name the release up front, which is what you want in a `Dockerfile` or any other place with
no terminal to answer the question:

```shell
curl -fsSL https://repo.liquidsoap.info/setup.sh | sudo sh -s -- --channel v2.4.5
```

There is one channel per supported version: a stable channel following the latest release of that version,
and a rolling channel rebuilt on every commit. https://repo.liquidsoap.info lists the ones currently
published. A machine tracking a rolling channel picks up each new build with an ordinary `apt-get upgrade`
or `apk upgrade`, and the final release supersedes the rolling builds that led up to it.

Both the `liquidsoap` and `liquidsoap-minimal` packages are available from every channel. Re-running the
script switches channel.

### Manual configuration

If you would rather not pipe a script into a root shell, or manage your system's configuration with other
tools, here is what the script sets up. Replace `CHANNEL` with a channel name listed at
https://repo.liquidsoap.info/channels.txt, for instance `rolling-release-v2.5.x`.

On Debian and Ubuntu, install the signing key:

```shell
sudo install -d /etc/apt/keyrings
sudo curl -fsSL https://repo.liquidsoap.info/liquidsoap.asc -o /etc/apt/keyrings/liquidsoap.asc
```

and write `/etc/apt/sources.list.d/liquidsoap.sources`, with `CODENAME` being the `VERSION_CODENAME` from
`/etc/os-release` (`trixie`, `noble`, ...):

```
Types: deb
URIs: https://repo.liquidsoap.info/CHANNEL/deb/CODENAME
Suites: ./
Signed-By: /etc/apt/keyrings/liquidsoap.asc
```

On Alpine, install the signing key and add the repository:

```shell
curl -fsSL https://repo.liquidsoap.info/liquidsoap.rsa.pub -o /etc/apk/keys/liquidsoap.rsa.pub
echo https://repo.liquidsoap.info/CHANNEL/alpine >> /etc/apk/repositories
```

Then run `apt-get update` or `apk update` as usual.

Each channel covers the same distributions and architectures as our release assets: the current Debian stable
and testing, the current Ubuntu LTS and latest release, and Alpine edge. Debian and Ubuntu packages are built
for `amd64` and `arm64`, Alpine packages for `x86_64` and `aarch64`. https://repo.liquidsoap.info lists what
each published channel actually carries.

## Debian/Ubuntu

We generate debian and ubuntu packages automatically as part of our [release process](https://github.com/savonet/liquidsoap/releases). Otherwise, you
can check out the official [debian](https://packages.debian.org/liquidsoap) and [ubuntu](https://packages.ubuntu.com/liquidsoap) packages.

Starting from version `2.5.x`, our Debian/Ubuntu packages bundle a static FFmpeg build with full codec support, including `fdk-aac`. No third-party repositories are required.

For Debian releases prior to `2.5.x`, you also need the [deb-multimedia.org](https://www.deb-multimedia.org/) packages, which provide up-to-date FFmpeg libraries with `fdk-aac` support. Note that deb-multimedia.org is Debian-only and does not apply to Ubuntu.

## Alpine

Alpine packages are also provided as part of our [release process](https://github.com/savonet/liquidsoap/releases),
and through the [package repositories](#package-repositories) described above.

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

To install liquidsoap from a local source checkout using opam, pin the repository and let opam handle the build and install:

```shell
git clone https://github.com/savonet/liquidsoap.git
cd liquidsoap
opam pin -ny .
opam install liquidsoap
```

For a developer build using dune directly, see the [build instructions](./build.md).
