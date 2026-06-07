# ocaml-portaudio

This package contains an OCaml interface for the portable audio I/O library.

Please read the COPYING file before using this software.

# Prerequisites:

- ocaml
- portaudio
- findlib
- dune >= 2.0

# Compilation:

```
$ dune build
```

This should build both the native and the byte-code version of the
extension library.

# Installation:

Via `opam`:

```
$ opam install portaudio
```

Via `dune` (for developers):

```
$ dune install
```

This should install the library file (using ocamlfind) in the
appropriate place.
