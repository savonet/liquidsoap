# ocaml-pulseaudio

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/pulseaudio/` and will be mirrored here automatically.

OCaml bindings for [PulseAudio](https://www.freedesktop.org/wiki/Software/PulseAudio/), a sound server for POSIX OSes.

Please read the COPYING file before using this software.

## Prerequisites

- OCaml >= 4.14
- libpulse and libpulse-simple (e.g. `apt install libpulse-dev`)
- dune >= 3.0

## Installation

Via `opam`:

```
$ opam install pulseaudio
```

## Building from source

```
$ dune build
$ dune install
```

## Contact

savonet-users@lists.sourceforge.net
