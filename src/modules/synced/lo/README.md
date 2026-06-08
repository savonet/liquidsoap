# ocaml-lo

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/lo/` and will be mirrored here automatically.

OCaml bindings for [liblo](http://liblo.sourceforge.net/), a library for communicating using the [Open Sound Control](http://www.opensoundcontrol.org/) (OSC) protocol.

Please read the COPYING file before using this software.

## Prerequisites

- OCaml >= 4.14
- liblo (e.g. `apt install liblo-dev` or `brew install liblo`)
- dune >= 3.0

## Installation

Via `opam`:

```
$ opam install lo
```

## Building from source

```
$ dune build
$ dune install
```

## Contact

contact@liquidsoap.info
