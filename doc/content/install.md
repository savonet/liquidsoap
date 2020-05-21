Installing Savonet/Liquidsoap
=============================

**Note** These instructions are from the documentation from liquidsoap `1.5.0`.
Make sure to consult the instructions from the version you wish to install,
most likely the latest stable release.

You can install liquidsoap with OPAM (recommended) or from source, or using a
package available for your distribution (not covered by this documentation).

* [Using OPAM](#install-using-opam)
* [Debian/Ubuntu](#debianubuntu)
* [Windows](#windows)
* [From source](#installing-from-source)
* [Latest development version](#latest-development-version)

Install using OPAM
------------------

The recommended method to install liquidsoap is by using the [OCaml Package
Manager](http://opam.ocaml.org/). OPAM is available in all major distributions
and on windows. We actively support the liquidsoap packages there and its
dependencies. You can read [here](https://opam.ocaml.org/doc/Usage.html) about
how to use OPAM. In order to use it:

- [you should have at least OPAM version 2.0](https://opam.ocaml.org/doc/Install.html),
- you should have at least OCaml version 4.08.0, which can be achieved by typing
  ```
  opam switch create 4.08.0
  ```

A typical installation with MP3 and Vorbis encoding/decoding and icecast support
is done by executing:

```
opam depext taglib mad lame vorbis cry samplerate liquidsoap
opam install taglib mad lame vorbis cry samplerate liquidsoap
```

* `opam depext ...` takes care of installing the required external
  dependencies. In some cases external dependencies might be missing for your
  system. If that is the case, please report it to us!
* Finally `opam install ...` installs the packages themselves.

Most of liquidsoap's dependencies are only optionally installed by OPAM. For
instance, if you want to enable opus encoding and decoding after you've already
installed liquidsoap, you should execute the following:

```
opam depext opus
opam install opus
```

`opam info liquidsoap` should give you the list of all optional dependencies
that you may enable in liquidsoap.

If you need to run liquidsoap as daemon, we provide a package named
`liquidsoap-daemon`.  See
[savonet/liquidsoap-daemon](https://github.com/savonet/liquidsoap-daemon) for
more information.

You can also install liquidsoap or any of its dependencies from source using
OPAM. For instance:

```
git clone https://github.com/savonet/liquidsoap.git
cd liquidsoap
opam pin add liquidsoap .
```

Most dependencies should be compatible with OPAM pinning. Let us know if you
find one that isn't.

Debian/Ubuntu
-------------

We generate debian and ubuntu packages automatically as part of our CI workflow.
These packages are available for quick testing of `liquidsoap` on certain Debian
and Ubuntu distributions. However, we do not recommend them yet for production 
purposes.

**Please note** We cannot guarantee that any of the distribution below will remain
available at all time and we reserve the right to purge old versions of the packages
at any time. If you plan on using some of these packages for any sort of production
use, make sure to copy them and use your own distribution channels.

Here's how to install:

* First install the repository signing key:
```
[sudo] apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 20D63CCDDD0F62C2
```

* Then one of the following source:

**debian/stable:**
```
[sudo] echo deb http://deb.liquidsoap.info/debian stable main >> /etc/apt/sources.list.d/liquidsoap.list
```

**debian/testing:**
```
[sudo] echo deb http://deb.liquidsoap.info/debian testing main >> /etc/apt/sources.list.d/liquidsoap.list
```

**ubuntu/bionic:**
```
[sudo] add-apt-repository ppa:sergey-dryabzhinsky/ffmpeg
[sudo] echo deb http://deb.liquidsoap.info/ubuntu bionic main >> /etc/apt/sources.list.d/liquidsoap.list
```

**ubuntu/eoan:**
```
[sudo] echo deb http://deb.liquidsoap.info/ubuntu eoan main >> /etc/apt/sources.list.d/liquidsoap.list
```

**ubuntu/focal:**
```
[sudo] echo deb http://deb.liquidsoap.info/ubuntu focal main >> /etc/apt/sources.list.d/liquidsoap.list
```

You can now see the list of available packages:
```
apt-cache show liquidsoap
```

Package names are of the form: `liquidsoap-<commit>` and `liquidsoap-<branch>`. For instance,
to install the latest `master` you can do:
```
[sudo] apt-get install liquidsoap-master
```


Windows
-------

You can download a liquidsoap for windows from our [release
page](https://github.com/savonet/liquidsoap/releases), starting with version
`1.3.4`.

Liquidsoap for windows is built using [opam-cross](https://github.com/ocaml-cross/opam-cross-windows). The build process is documented in  our [docker files](https://github.com/savonet/liquidsoap-full/tree/master/docker). `Dockerfile.win32-deps` installs all  the [mxe](https://mxe.cc/) dependencies and `Dockerfile.win32` produces the actual liquidsoap binary.

You might want to refer to each project, [mxe](https://mxe.cc/) and [opam-cross](https://github.com/ocaml-cross/opam-cross-windows) for more details about cross-compiling for windows.

Installing from source
----------------------

You can download source code published by Savonet from the [github releases
page](https://github.com/savonet/liquidsoap/releases).

The recommended way for newcomers is to use the liquidsoap-full-xxx.tar.gz
tarball. This tarball includes all required OCaml bindings and allows you to
compile and install liquidsoap in a single `configure`, `make` and ``` make
install``` procedure. You will still need the corresponding C libraries and
their development files, though.

You will then have to [build the source](build.html).

Latest development version
--------------------------

If you want a cutting-edge version, you can use the git repository.  To get a
copy of it, you have to run:

```
git clone https://github.com/savonet/liquidsoap-full.git liquidsoap
cd liquidsoap
make init
```

After, that you have to create a list of modules that you want to compile. A
good starting point is to do

```
cp PACKAGES.minimal PACKAGES
```

and edit the `PACKAGES` file to uncomment the libraries you are interested in.
You should then run the configuration scripts by

```
./bootstrap
./configure
```

and finally build Liquidsoap:

```
make
```

After that, you should synchronize the repository from time to time using

```
make update
```

Some more explanations can be found in the [build instructions](build.html).
