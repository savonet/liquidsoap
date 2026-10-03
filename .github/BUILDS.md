# Build kinds

This document defines what kind of build a CI run is and what each kind must guarantee. It is normative: workflows and scripts implement it, and where they disagree with it they are wrong.

The key words MUST, MUST NOT, SHOULD and MAY are to be interpreted as described in [RFC 2119](https://www.rfc-editor.org/rfc/rfc2119).

## Properties

Every build has three boolean properties.

| Property | Flag                 | Meaning                                                                                              |
| -------- | -------------------- | ---------------------------------------------------------------------------------------------------- |
| Release  | `is_release`         | The build produces what may be distributed to users as Liquidsoap: opam packages, binary packages.   |
| Rolling  | `is_rolling_release` | The build's assets are published to the rolling channel of its version line, replacing the last set. |
| Snapshot | `is_snapshot`        | The build's version identifies the commit it was built from.                                         |

The properties MUST be decided once per build, in one place, from the ref being built and the release matrix. Every other decision MUST be derived from the properties and MUST NOT inspect the ref again.

Snapshot is not independent: a build is a snapshot if and only if it is not a release or it is rolling.

## Kinds

The properties combine into four kinds of build.

| Kind                | Release | Rolling | Snapshot |
| ------------------- | ------- | ------- | -------- |
| Development         | no      | no      | yes      |
| Development rolling | no      | yes     | yes      |
| Rolling release     | yes     | yes     | yes      |
| Final release       | yes     | no      | no       |

A build's kind is determined as follows.

1. A build from a fork MUST be a development build, whatever its ref.
2. A build of a version tag MUST be a final release.
3. A build of a branch that the release matrix assigns to a version line MUST be rolling. It MUST be a release unless the matrix marks that version line as work in progress.
4. Any other build MUST be a development build.

A version line is work in progress from the start of its development cycle until it is considered ready for its first release candidate. Whether its branch publishes rolling builds during that time is optional and is decided by the release matrix alone.

## Requirements

### Release builds

A release build is one that could be handed to a user as is.

- It MUST build against its dependencies as published. It MUST NOT pin, vendor or otherwise substitute an in-tree copy of a dependency that is also distributed separately.
- Its packages MUST use the canonical package name, with nothing identifying a branch.

The first requirement is what proves the published opam package installs: a user has no in-tree copy to pin.

### Non-release builds

- They MUST build against the in-tree copies of the dependencies developed in this repository, so that a change to one of them and the code relying on it can land together.

### Rolling builds

Rolling builds are transient: each one replaces the previous one and none is kept as a long-term release.

- They MUST be published to the rolling channel of their version line, and only there.
- They MUST be published as pre-releases.
- Their packages MUST use the canonical package name, so that a rolling build upgrades to the next one and to the final release.
- Where the package format orders versions, their version MUST sort below the final release they lead up to, and builds of the same version line MUST sort by commit time.

### Snapshot builds

- Their version MUST carry the commit they were built from.
- A build that is not a snapshot MUST NOT carry a commit in its version.

### Development builds

- They MUST NOT be published to any release channel.
- Their packages MUST be named after the branch they were built from, so they cannot be mistaken for, or upgrade to, a release.

### Final releases

- They MUST be published as drafts. Making one available to users is a manual decision.

## Consequences

- A development rolling build is published like a rolling release but built like a development build. Users can test it; it makes no promise that the published dependencies are enough to build it.
- Once a version line is no longer work in progress, its branch builds as a release. A change that needs an unpublished dependency then fails there even though it passed as a development build, so the dependency has to be published first.
