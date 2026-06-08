# ocaml-faad

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/faad/` and will be mirrored here automatically.

OCaml bindings for [libfaad](https://www.audiocoding.com/faad2.html), an AAC audio decoder.

Please read the COPYING file before using this software.

## Prerequisites

- OCaml >= 4.14
- libfaad2 (e.g. `apt install libfaad-dev` or `brew install faad2`)
- dune >= 3.0

## Installation

Via `opam`:

```
$ opam install faad
```

## Building from source

```
$ dune build
$ dune install
```

## Contact

savonet-users@lists.sourceforge.net
