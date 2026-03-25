# ocaml-jack

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/jack/` and will be mirrored here automatically.

This package contains an OCaml interface for the JACK Audio Connection Kit.

Please read the COPYING file before using this software.

# Prerequisites:

- ocaml
- libjack (JACK Audio Connection Kit)
- findlib
- dune >= 3.23

# Compilation:

```
$ dune build
```

# Installation:

Via `opam`:

```
$ opam install jack
```

Via `dune` (for developers):

```
$ dune install
```

# Author:

This author of this software may be contacted by electronic mail
at the following address: savonet-users@lists.sourceforge.net.
