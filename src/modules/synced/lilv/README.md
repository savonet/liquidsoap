# ocaml-lilv

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/lilv/` and will be mirrored here automatically.

OCaml bindings for [lilv](http://drobilla.net/software/lilv), a library to use [LV2 audio plugins](http://lv2plug.in/).

Please read the COPYING file before using this software.

## Prerequisites

- OCaml >= 4.14
- lilv (e.g. `apt install liblilv-dev` or `brew install lilv`)
- dune >= 3.23

## Installation

Via `opam`:

```
$ opam install lilv
```

## Building from source

```
$ dune build
$ dune install
```

## Contact

contact@liquidsoap.info
