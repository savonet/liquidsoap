# ocaml-shine

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/shine/` and will be mirrored here automatically.

OCaml bindings for [libshine](https://github.com/toots/shine), a super-fast fixed-point MP3 encoder.

Please read the COPYING file before using this software.

## Prerequisites

- OCaml >= 4.03
- libshine (e.g. `apt install libshine-dev` or `brew install shine`)
- dune >= 3.0

## Installation

Via `opam`:

```
$ opam install shine
```

## Building from source

```
$ dune build
$ dune install
```

## Contact

contact@liquidsoap.info
