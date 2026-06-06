# ocaml-lame

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/lame/` and will be mirrored here automatically.

OCaml bindings for [LAME](https://lame.sourceforge.io/), the MP3 encoder.

Please read the COPYING file before using this software.

## Prerequisites

- OCaml >= 4.14
- liblame (e.g. `apt install libmp3lame-dev` or `brew install lame`)
- dune >= 3.0

## Installation

Via `opam`:

```
$ opam install lame
```

## Building from source

```
$ dune build
$ dune install
```

## Contact

savonet-users@lists.sourceforge.net
