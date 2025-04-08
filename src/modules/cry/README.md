ocaml-cry

This package contains an OCaml implementation of the
various icecast & shoutcast source client protocols.

Please read the COPYING file before using this software.

# Prerequisites:

- ocaml

- dune

- findlib

# Compilation:

```sh
dune build
```

This should build both the native and the byte-code version of the
extension library.

# Installation:

Using [opam](http://opam.ocaml.org/):

```sh
opam install cry
```

Using `dune` and a local copy (dev only):

```sh
dune install
```

This should install the library file (using ocamlfind) in the
appropriate place.

# Author:

This author of this software may be contacted by electronic mail
at the following address: savonet-users@lists.sourceforge.net.
