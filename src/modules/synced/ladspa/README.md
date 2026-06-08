# ocaml-ladspa

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/ladspa/` and will be mirrored here automatically.

OCaml bindings for [LADSPA](https://www.ladspa.org/) (Linux Audio Developer's Simple Plugin API).

Please read the COPYING file before using this software.

## Prerequisites

- OCaml >= 4.08
- ladspa.h header (e.g. `apt install ladspa-sdk` or `brew install ladspa-sdk`)
- dune >= 3.0

## Installation

Via `opam`:

```
$ opam install ladspa
```

## Building from source

```
$ dune build
$ dune install
```

## Contact

savonet-users@lists.sourceforge.net
