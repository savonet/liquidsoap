# Installing Liquidsoap

You can install liquidsoap using binary builds, with OPAM or from source.

Binary builds are provided with our releases, either in the form of debian/ubuntu, fedora and alpine
packages or as docker images (also for debian or alpine). Your favorite distribution may also have
binary packages.

The binary package and docker images we provide are compiled in two flavors:

- The main `liquidsoap` packages are compiled with all available features and functions. This is a good starting point for general-purpose development.
- Binary packages and docker images labelled `-minimal` are compiled without the extra libraries and with a limited set of essential optional features.

Minimal builds are useful if you are concerned about size or memory usage. They also reduce the chances of running into issues that could be introduced
by optional dependencies that you do not use. If your script works with them, they are recommended over the fully featured builds for production.

Each binary build that we provide has a corresponding `*.config` file. This is a text file that lists all the features included in a specific
build. You can consult it to know what features are available. You can also get the same information by calling `liquidsoap --build-config`, for instance
when using a docker image.

Binary packages and docker images are useful in that they provide a readily available liquidsoap installation. If you
need a finer-grained build or if your distribution/OS does not have a binary build, you can
install via OPAM, which is a very convenient package manager that can compile liquidsoap from sources
and knows how to handle external dependencies for most OS/distributions.

Lastly, compiling from source should be reserved for developers.

- [Package repositories](#package-repositories)
- [Debian/Ubuntu](#debianubuntu)
- [Fedora](#fedora)
- [Alpine](#alpine)
- [Docker](#docker)
- [Windows](#windows)
- [Using OPAM](#install-using-opam)
- [From source](#installing-from-source)

## Package repositories

If you would rather have `apt`, `dnf` or `apk` keep liquidsoap up to date than download a package by hand, we
publish a repository for each release. Setting one up is a single command, which asks which release you
want and configures the matching repository:

```shell
curl -fsSL https://repo.liquidsoap.info/setup.sh | sudo sh
```

You can also name the release up front, which is what you want in a `Dockerfile` or any other place with
no terminal to answer the question:

```shell
curl -fsSL https://repo.liquidsoap.info/setup.sh | sudo sh -s -- --channel rolling-release-v2.5.x
```

There is one channel per supported version: a stable channel following the latest release of that version,
and a rolling channel rebuilt on every commit. https://repo.liquidsoap.info lists the ones currently
published. Not every channel has packages for every distribution: the script only offers the ones that
have packages for your system, and refuses a `--channel` that has none. A machine tracking a rolling channel
picks up each new build with an ordinary `apt-get upgrade`, `dnf upgrade` or `apk upgrade`.

Both the `liquidsoap` and `liquidsoap-minimal` packages are available from every channel, except on Fedora,
which only carries `liquidsoap`. Re-running the script switches channel.

Channels that have one also carry `liquidsoap-asan`, a build with AddressSanitizer enabled, for Debian
testing on `amd64`. It replaces `liquidsoap` when installed and is meant for tracking down crashes, not for
production.

### Manual configuration

The script is [`scripts/setup-repository.sh`](https://github.com/savonet/liquidsoap/blob/main/scripts/setup-repository.sh)
in our repository, if you want to read it before running it. To set things up yourself instead, pick a channel
from https://repo.liquidsoap.info/channels.txt.

On Debian and Ubuntu:

```shell
CHANNEL=rolling-release-v2.5.x
. /etc/os-release
sudo install -d /etc/apt/keyrings
sudo curl -fsSL https://repo.liquidsoap.info/liquidsoap.asc -o /etc/apt/keyrings/liquidsoap.asc
sudo tee /etc/apt/sources.list.d/liquidsoap.sources << EOF
Types: deb
URIs: https://repo.liquidsoap.info/${CHANNEL}/deb/${VERSION_CODENAME}
Suites: ./
Signed-By: /etc/apt/keyrings/liquidsoap.asc
EOF
sudo apt-get update
```

On Fedora:

```shell
CHANNEL=rolling-release-v2.5.x
. /etc/os-release
sudo curl -fsSL https://repo.liquidsoap.info/${CHANNEL}/fedora/${VERSION_ID}/liquidsoap.repo -o /etc/yum.repos.d/liquidsoap.repo
sudo dnf makecache --repo liquidsoap
```

On Alpine:

```shell
CHANNEL=rolling-release-v2.5.x
curl -fsSL https://repo.liquidsoap.info/liquidsoap.rsa.pub -o /etc/apk/keys/liquidsoap.rsa.pub
echo https://repo.liquidsoap.info/${CHANNEL}/alpine >> /etc/apk/repositories
apk update
```

Each channel covers the same distributions and architectures as our release assets: the current Debian stable
and testing, the current Ubuntu LTS and latest release, the current Fedora release, and Alpine edge. Debian and
Ubuntu packages are built for `amd64` and `arm64`, Fedora and Alpine packages for `x86_64` and `aarch64`. https://repo.liquidsoap.info lists what
each published channel actually carries.

## Debian/Ubuntu

We generate debian and ubuntu packages automatically as part of our [release process](https://github.com/savonet/liquidsoap/releases). Otherwise, you
can check out the official [debian](https://packages.debian.org/liquidsoap) and [ubuntu](https://packages.ubuntu.com/liquidsoap) packages.

Starting from version `2.5.x`, our Debian/Ubuntu packages bundle a static FFmpeg build with full codec support, including `fdk-aac`. No third-party repositories are required.

For Debian releases prior to `2.5.x`, you also need the [deb-multimedia.org](https://www.deb-multimedia.org/) packages, which provide up-to-date FFmpeg libraries with `fdk-aac` support. Note that deb-multimedia.org is Debian-only and does not apply to Ubuntu.

## Fedora

Fedora packages are provided as part of our [release process](https://github.com/savonet/liquidsoap/releases),
and through the [package repositories](#package-repositories) described above.

They are built against Fedora's own FFmpeg, which leaves out patent-encumbered codecs such as the H.264
encoder. Switching to the full FFmpeg from [RPM Fusion](https://rpmfusion.org/Howto/Multimedia) replaces those
libraries, and liquidsoap picks up the extra codecs without being reinstalled.

## Alpine

Alpine packages are also provided as part of our [release process](https://github.com/savonet/liquidsoap/releases),
and through the [package repositories](#package-repositories) described above.

## Docker

We provide production-ready docker images via [Docker hub](https://hub.docker.com/r/savonet/liquidsoap).
Docker images are tagged with a release tag (e.g. `v2.4.5`) and with the sha of their git commit (e.g. `a24bf49`).

For instance, to fetch release `2.4.5`, you would do:

```shell
docker pull savonet/liquidsoap:v2.4.5
```

Please note that images tagged with a release tag may change while images tagged with a commit sha will not.

## Windows

You can download liquidsoap for Windows from our [release page](https://github.com/savonet/liquidsoap/releases).

The Windows binary is statically built. `%ffmpeg` and the encoders that share its libraries (for instance `libmp3lame` for `%mp3`) export conflicting C symbols, so they cannot be linked together. The Windows build enables `%ffmpeg` and disables the other encoders. Use `%ffmpeg` for all encoding. For instance, for variable bitrate MP3:

```liquidsoap
%ffmpeg(format="mp3", %audio(codec="libmp3lame", q=7))
```

## Install using OPAM

The recommended method to install liquidsoap from source is by using the [OCaml Package
Manager](https://opam.ocaml.org/). OPAM is available in all major distributions
and on Windows. We actively support the liquidsoap packages there and its
dependencies. You can read [here](https://opam.ocaml.org/doc/Usage.html) about
how to use OPAM. In order to use it:

- [you should have at least OPAM version 2.1](https://opam.ocaml.org/doc/Install.html),
- not all versions of the OCaml compiler are supported. You can run `opam info liquidsoap-lang` to find out.

You can create a switch for a specific OCaml version as follows:

```
opam switch create <ocaml version>
```

A typical installation with most expected features is done by executing:

```
opam install ffmpeg liquidsoap
```

This will install `liquidsoap` along with the optional `ffmpeg` package, which provides most
of the expected functionality (encoding, decoding, metadata support, etc.) out of the box.

The `opam` installer also handles external dependencies, that is, dependencies from your operating system
that are required for your install. Typically, this would be the `ffmpeg` shared libraries here, as well
as `libcurl`, which is required for `liquidsoap` to install.

In most cases, `opam` will simply ask for your permission to install these dependencies on your behalf. In
some cases, however, you will have to install them yourself.

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
some of the associated OCaml modules, macOS users using `homebrew` might need to add
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
