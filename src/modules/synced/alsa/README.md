# ocaml-alsa

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/alsa/` and will be mirrored here automatically.

This package contains an OCaml interface for the ALSA library, otherwise known
as libasound2.

## Prerequisites

- ocaml >= 3.0.6 (haven't tried earlier versions)
- libasound2 >= 1.0.14a (haven't tried earlier versions)
- findlib >= 0.8.1 (haven't tried earlier versions)
- dune >= 2.0

## Compilation

```sh
dune build
```

This should build both the native and the byte-code version of the
extension library.

## Installation

Via `opam`:

```sh
opam install alsa
```

Via `dune` (for developers):

```sh
dune install
```

This should install the library file (using ocamlfind) in the appropriate place.

## License

Please see the COPYING file.

## Author

This author of this software may be contacted by electronic mail at the
following address: contact@liquidsoap.info.
