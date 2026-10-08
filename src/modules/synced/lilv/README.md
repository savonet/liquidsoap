# ocaml-lilv

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/lilv/` and will be mirrored here automatically.

OCaml bindings for [lilv](https://drobilla.net/software/lilv), a library to use [LV2 audio plugins](https://lv2plug.in/).

Please read the COPYING file before using this software.

## Prerequisites

- OCaml >= 4.14
- lilv and pkg-config (e.g. `apt install liblilv-dev pkg-config` or `brew install lilv pkg-config`)
- ctypes and ctypes-foreign
- dune >= 3.23

## Installation

Via `opam`:

```
$ opam install lilv
```

## Building from source

```
$ dune build
$ dune install
```

## Examples

The `examples/bin` directory contains two programs: `inspect` lists the installed plugins with their ports, and `amp` instantiates and runs a plugin.

```
$ dune exec examples/bin/inspect.exe
```

## Contact

contact@liquidsoap.info
