# ocaml-samplerate

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/samplerate/` and will be mirrored here automatically.

This package contains an OCaml interface for the [samplerate
library](http://libsndfile.github.io/libsamplerate/).

Please read the COPYING file before using this software.

## Prerequisites

- ocaml
- libsamplerate
- findlib
- dune >= 2.0

## Compilation

```
$ dune build
```

This should build both the native and the byte-code version of the extension
library.

## Installation

Via `opam`:

```
$ opam install samplerate
```

Via `dune` (for developers):

```
$ dune install
```

## Bugs

Please report any issue in [the dedicated
bugtracker](https://github.com/savonet/ocaml-samplerate/issues).

## Author

This author of this software may be contacted by electronic mail at the
following address: contact@liquidsoap.info.
