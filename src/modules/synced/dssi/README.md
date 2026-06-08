# ocaml-dssi

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/dssi/` and will be mirrored here automatically.

OCaml bindings for [DSSI](http://dssi.sourceforge.net/), the Disposable Soft Synth Interface API.

Please read the COPYING file before using this software.

## Prerequisites

- OCaml >= 4.08
- DSSI development headers (e.g. `apt install libdssi-dev` or via `conf-dssi`)
- LADSPA OCaml bindings (`ladspa` opam package)
- dune >= 3.0

## Installation

Via `opam`:

```
$ opam install dssi
```

## Building from source

```
$ dune build
$ dune install
```

## Contact

contact@liquidsoap.info
