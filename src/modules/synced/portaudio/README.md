# ocaml-portaudio

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/portaudio/` and will be mirrored here automatically.

OCaml bindings for [PortAudio](http://www.portaudio.com/), a portable audio I/O library.

Please read the COPYING file before using this software.

## Prerequisites

- OCaml >= 4.14
- portaudio (e.g. `apt install portaudio19-dev` or `brew install portaudio`)
- dune >= 3.0

## Installation

Via `opam`:

```
$ opam install portaudio
```

## Building from source

```
$ dune build
$ dune install
```

## Contact

contact@liquidsoap.info
