# ocaml-duppy

ocaml-duppy is an advanced scheduler for Ocaml programmers.

Please read the COPYING file before using this software.

## Documentation

The API is documented here: https://www.liquidsoap.info/ocaml-duppy/

## Prerequisites:

- ocaml >= 5.5
- ocaml-re
- dune >= 3.23

## Compilation:

```sh
$ dune build
```

This should build both the native and the byte-code version of the
extension library.

## Installation:

Via `opam`:

```sh
$ opam install duppy
```

Via `dune` (for developers):

```sh
$ dune install
```

This should install the library file (using ocamlfind) in the
appropriate place.

## Author:

This author of this software may be contacted by electronic mail
at the following address: contact@liquidsoap.info.
