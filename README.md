# Liquidsoap

Liquidsoap is a swiss-army knife for multimedia streaming, notably
used for netradios and webtvs. It has tons of features, it's free and it's
open-source!

Liquidsoap is a powerful and flexible language for describing your streams. It
offers a rich collection of operators that you can combine to create and
transform streams. Liquidsoap is very light and easy to use, in the Unix
tradition of simple strong components working together.

Copyright 2003-2023 Savonet team

[![GPL license](https://img.shields.io/badge/License-GPL-green.svg)](https://github.com/savonet/liquidsoap/blob/master/COPYING)
![CI](https://github.com/savonet/liquidsoap/workflows/CI/badge.svg)
[![GitHub release](https://img.shields.io/github/release/savonet/liquidsoap.svg)](https://GitHub.com/savonet/liquidsoap/releases/)
[![Install with Opam !](https://img.shields.io/badge/Install%20with-Opam-1abc9c.svg)](http://opam.ocaml.org/packages/liquidsoap/)
[![Chat on slack !](https://img.shields.io/badge/Chat%20on-Slack-1a1f9c.svg)](http://slack.liquidsoap.info/)

|                |                                                                    |
| -------------- | ------------------------------------------------------------------ |
| Slack Chat     | http://slack.liquidsoap.info                                       |
| IRC            | #savonet on [irc.libera.chat](https://libera.chat/) (slack bridge) |
| Mailing list.  | savonet-users@lists.sourceforge.net                                |
| Homepage       | http://liquidsoap.info                                             |
| Bug reports    | https://github.com/savonet/liquidsoap/issues                       |
| User questions | https://github.com/savonet/liquidsoap/discussions                  |

## Installation

See the instructions [here](https://www.liquidsoap.info/doc.html?path=install.html).

## Release Details

Current release status by version:
| Branch | Latest release | Supported | Rolling Release |
| --------|----------------|-----------|-----------------|
| `2.2.x` | In development | ⚠️ | ❌ |
| `2.1.x` | [2.1.3](https://github.com/savonet/liquidsoap/releases/tag/v2.1.3) | ✅ | [2.1.x](https://github.com/savonet/liquidsoap/releases/tag/rolling-release-v2.1.x) |
| `2.0.x` | [2.0.7](https://github.com/savonet/liquidsoap/releases/tag/v2.0.7) | ⚠️ | [2.0.x](https://github.com/savonet/liquidsoap/releases/tag/rolling-release-v2.0.x) |
| `1.4.x` | [1.4.4](https://github.com/savonet/liquidsoap/releases/tag/v1.4.4) | ❌ | ❌ |

### Notes

- Development branch `2.2.x` is currently under development and can break at any time. We do, however, encourage early testing and feedback!
- Development branch `2.0.x` is at the end of its development cycle and will only be updated on a best effort basis or in case of a major issue.

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

```
git checkout git@github.com:savonet/liquidsoap.git
```

and build with

```
dune build
```

In order to ensure the quality of your commits, you are recommended to install
the pre-commit hooks with

```
pre-commit install
```

Those will automatically check before each commit that those meet some of our
quality requirements.

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
