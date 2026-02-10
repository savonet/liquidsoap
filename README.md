# Liquidsoap

Liquidsoap is a swiss-army knife for multimedia streaming, notably
used for netradios and webtvs. It has tons of features, it's free and it's
open-source!

Liquidsoap is a powerful and flexible language for describing your streams. It
offers a rich collection of operators that you can combine to create and
transform streams. Liquidsoap is very light and easy to use, in the Unix
tradition of simple strong components working together.

Copyright 2003-2026 Savonet team

[![GPL license](https://img.shields.io/github/license/savonet/liquidsoap)](https://github.com/savonet/liquidsoap/blob/master/COPYING)
![CI](https://github.com/savonet/liquidsoap/workflows/CI/badge.svg)
[![GitHub release](https://img.shields.io/github/release/savonet/liquidsoap.svg)](https://GitHub.com/savonet/liquidsoap/releases/)
[![Install with Opam!](https://img.shields.io/badge/Install%20with-Opam-1abc9c.svg)](http://opam.ocaml.org/packages/liquidsoap/)
[![Chat on Discord!](https://img.shields.io/badge/Chat%20on-Discord-5865f2.svg)](http://chat.liquidsoap.info/)
[![](https://img.shields.io/badge/Gurubase-Ask%20Liquidsoap%20Guru-006BFF)](https://gurubase.io/g/liquidsoap)

[![Built with Depot](https://depot.dev/badges/built-with-depot.svg)](https://depot.dev/)

|                           |                                                                         |
| ------------------------- | ----------------------------------------------------------------------- |
| Homepage                  | http://liquidsoap.info                                                  |
| Discord Chat              | http://chat.liquidsoap.info                                             |
| Blog                      | https://www.liquidsoap.info/blog/                                       |
| Bug reports               | https://github.com/savonet/liquidsoap/issues                            |
| User questions            | https://github.com/savonet/liquidsoap/discussions                       |
| IRC (deprecated)          | #savonet on [irc.libera.chat](https://libera.chat/) (w/ discord bridge) |
| Mailing list (deprecated) | savonet-users@lists.sourceforge.net                                     |

## Installation

See the instructions [here](https://www.liquidsoap.info/doc.html?path=install.html).

## Release Details

Current release status by version:
| Branch | Latest release | Supported | Rolling Release |
| --------|----------------|-----------|-----------------|
| `2.5.x` | 🚧 | 🚧 | [main](https://github.com/savonet/liquidsoap) (docker: `savonet/liquidsoap`) |
| `2.4.x` | [2.4.2](https://github.com/savonet/liquidsoap/releases/tag/v2.4.2) (docker: `savonet/liquidsoap:v2.4.2`)| ✅ | [2.4.x](https://github.com/savonet/liquidsoap/releases/tag/rolling-release-v2.4.x) (docker: `savonet/liquidsoap:rolling-release-v2.4.x`) |
| `2.3.x` |[2.3.3](https://github.com/savonet/liquidsoap/releases/tag/v2.3.3) (docker: `savonet/liquidsoap:v2.3.3`) | ❌ | [2.3.x](https://github.com/savonet/liquidsoap/releases/tag/rolling-release-v2.3.x) (docker: `savonet/liquidsoap:rolling-release-v2.3.x`) |

### Versions

Liquidsoap releases follow a semantic versioning as follows:

```
<major_version>.<minor_version>.<bugfix_version>
```

Where:

- `major_version` is bumped when there are major changes, i.e. changes in the paradigm, major implementation change etc. Versions with different major versions **are** incompatible
- `minor_version` is bumped when there are minor changes, i.e. new operators, renaming, new modules etc. Version with different minor versions **may be** incompatible
- `bugfix_version` is bumped when a new bugfix version is published. Versions with only bugfix version changes **should be** compatible

Please note that liquidsoap is a complex framework with a lot of operators and advanced implementations. For this reason, it is possible that a bugfix actually fixes the behavior of an operator the way it was intended to be and may break scripts that previously relied on incorrect implementations.

Therefore, we **strongly** recommend maintaining a `staging` environment that makes it possible to test new versions before using them in production. In this context, the semantic versioning above should guide you in knowing how much scrutiny you should put into a new release before validating it in your staging environment.

### Assets

Release assets are provided at: https://github.com/savonet/liquidsoap/releases. Published, versioned releases are available using their published tag, i.e. `vx.y.z`.

We also provide **rolling releases**. A rolling release is a snapshot of a current, unpublished release. It can be a future stable release or a future bugfix release for a given major/minor version.

For both types of releases, we reserve the right to update, delete and add assets to the release at any time. If you are looking for permanent links to release assets, you should grab them from https://github.com/savonet/liquidsoap-release-assets/releases, which reflects all our releases but whose artifacts are never modified/deleted.

### Supported OSes for pre-built binary assets

We provide the pre-built binary assets in the form of native packages (or zip file for windows) and docker images.

We generally try to support the latest LTS release of each OS as well as their most recent release. Here's a table:

| OS      | Supported Releases                                         | Binary assets                  | Architectures       | Notes                                                                         |
| ------- | ---------------------------------------------------------- | ------------------------------ | ------------------- | ----------------------------------------------------------------------------- |
| Debian  | stable (currently: `trixie`), testing (currently: `forky`) | `.deb` packages, docker images | `amd64`, `arm64`    | `.deb` packages require [deb-multimedia.org](https://www.deb-multimedia.org/) |
| Ubuntu  | LTS (currently: `resolute`), latest (currently: `plucky`)  | `.deb` packages, docker images | `amd64`, `arm64`    |                                                                               |
| Alpine  | `edge`                                                     | `.apk` packages, docker images | `x86_64`, `aarch64` |                                                                               |
| Windows | N/A                                                        | `.zip` archive                 | Windows 64          |                                                                               |

### Supported FFmpeg versions

We support the last two major releases of FFmpeg. Currently, this means versions `7` and `8`.

## Tooling

|             |                                                                                                                                        |
| ----------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| Formatting  | [liquidsoap-prettier](https://github.com/savonet/liquidsoap-prettier)                                                                  |
| VSCode      | [vscode-liquidsoap](https://marketplace.visualstudio.com/items?itemName=savonet.vscode-liquidsoap)                                     |
| Neovim      | [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter), [formatter.nvim](https://github.com/mhartington/formatter.nvim) |
| Tree Sitter | [tree-sitter-liquidsoap](https://github.com/savonet/tree-sitter-liquidsoap)                                                            |
| CodeMirror  | [codemirror-lang-liquidsoap](https://github.com/savonet/codemirror-lang-liquidsoap)                                                    |
| Playground  | [https://www.liquidsoap.info/try/](https://www.liquidsoap.info/try/)                                                                   |

## Documentation

HTML documentation is available on our [website](http://liquidsoap.info)

We also have written _the Liquidsoap book_ which is [available
online](http://www.liquidsoap.info/book/book.pdf) and in [physical
version](https://www.amazon.com/dp/B095PVTYR3).

## Contributing

Contributions are more than welcome: you can submit
[issues](https://github.com/savonet/liquidsoap/issues) if you find some, or
contribute to the code through [pull
requests](https://github.com/savonet/liquidsoap/pulls). You can checkout the
code with

```sh
git checkout git@github.com:savonet/liquidsoap.git
```

Please see [our documentation page](https://www.liquidsoap.info/doc-dev/build.html) about how to build the code.

## License

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details, fully
stated in the COPYING file at the root of the liquidsoap distribution.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 59 Temple
Place, Suite 330, Boston, MA 02111-1307 USA

## Authors

- Developers:
  - [Romain Beauxis](https://github.com/toots)
  - [Samuel Mimram](http://www.mimram.fr)
- Former project leader and emeritus developer:
  - [David Baelde](http://www.lsv.fr/~baelde/)
- Contributors:
  - Florent Bouchez
  - Julien Cristau
  - Stéphane Gimenez
  - Clément Renard
  - Vincent Tabard
  - Sattisvar Tandabany
