# ocaml-fdkaac

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/fdkaac/` and will be mirrored here automatically.

OCaml bindings for [fdk-aac](https://github.com/mstorsjo/fdk-aac), the Fraunhofer FDK AAC Codec Library.

Please read the COPYING file before using this software.

## Prerequisites

- OCaml >= 4.14
- fdk-aac >= 0.1.1 (e.g. `apt install libfdk-aac-dev` or `brew install fdk-aac`)
- dune >= 3.0

## Installation

Via `opam`:

```
$ opam install fdkaac
```

## Building from source

```
$ dune build
$ dune install
```

## Contact

contact@liquidsoap.info
