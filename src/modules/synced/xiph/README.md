# ocaml-xiph

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/xiph/` and will be mirrored here automatically.

![GitHub](https://img.shields.io/github/license/savonet/ocaml-xiph)
![CI](https://github.com/savonet/ocaml-xiph/workflows/CI/badge.svg)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/savonet/ocaml-xiph)

This repository provides various OCaml bindings to the [xiph](https://xiph.org/) libraries.

# Documentation:

The [API documentation is available here](http://www.liquidsoap.info/ocaml-xiph/).

# Prerequisites:

- ocaml
- dune
- findlib
- libogg
- libvorbis
- libspeex
- libflac
- libtheora
- libopus

See [dune-project](dune-project) file for versions.

# Installation:

The preferred installation method is via [opam](http://opam.ocaml.org/):

```
opam install ogg vorbis ...
```

If you wish to install the latest code from this repository, you can do:

```
opam install .
```

From within this repository.

# Compilation:

```
dune build
```
