# ocaml-xiph

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/xiph/` and will be mirrored here automatically.

[![GitHub license](https://img.shields.io/github/license/savonet/ocaml-xiph)](https://github.com/savonet/ocaml-xiph/blob/main/LICENSE)
[![CI](https://github.com/savonet/ocaml-xiph/workflows/CI/badge.svg)](https://github.com/savonet/ocaml-xiph/actions)
[![GitHub release](https://img.shields.io/github/v/release/savonet/ocaml-xiph)](https://github.com/savonet/ocaml-xiph/releases)

OCaml bindings to the [Xiph.Org](https://xiph.org/) codec libraries. Provides
support for **Ogg**, **Vorbis**, **Opus**, **Speex**, **Theora**, and **FLAC**.

## Packages

| Package  | Wraps                            | opam                                                                                              |
| -------- | -------------------------------- | ------------------------------------------------------------------------------------------------- |
| `ogg`    | libogg — Ogg bitstream container | [![opam](https://img.shields.io/badge/opam-ogg-blue)](https://opam.ocaml.org/packages/ogg/)       |
| `vorbis` | libvorbis — Vorbis audio codec   | [![opam](https://img.shields.io/badge/opam-vorbis-blue)](https://opam.ocaml.org/packages/vorbis/) |
| `opus`   | libopus — Opus audio codec       | [![opam](https://img.shields.io/badge/opam-opus-blue)](https://opam.ocaml.org/packages/opus/)     |
| `speex`  | libspeex — Speex audio codec     | [![opam](https://img.shields.io/badge/opam-speex-blue)](https://opam.ocaml.org/packages/speex/)   |
| `theora` | libtheora — Theora video codec   | [![opam](https://img.shields.io/badge/opam-theora-blue)](https://opam.ocaml.org/packages/theora/) |
| `flac`   | libflac — FLAC lossless audio    | [![opam](https://img.shields.io/badge/opam-flac-blue)](https://opam.ocaml.org/packages/flac/)     |

## Installation

The recommended way is via [opam](https://opam.ocaml.org/):

```sh
opam install ogg vorbis opus speex theora flac
```

To install the latest development version directly from this repository:

```sh
opam install .
```

## Building from source

```sh
dune build
```

## Documentation

[API documentation](https://www.liquidsoap.info/ocaml-xiph/)

## License

LGPL-2.1-only — see [LICENSE](LICENSE).
