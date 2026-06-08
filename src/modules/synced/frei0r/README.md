# ocaml-frei0r

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/frei0r/` and will be mirrored here automatically.

OCaml bindings for [frei0r](https://frei0r.dyne.org/), a minimalistic plugin API for video effects.

Please read the COPYING file before using this software.

## Prerequisites

- OCaml >= 4.14
- frei0r (e.g. `apt install frei0r-plugins-dev` or `brew install frei0r`)
- dune >= 3.0

## Installation

Via `opam`:

```
$ opam install frei0r
```

## Building from source

```
$ dune build
$ dune install
```

## Contact

contact@liquidsoap.info
