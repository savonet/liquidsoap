#!/bin/sh

# Prints the pre-release part of release tag $1 for version $2: rc2 for
# v2.5.0-rc2, nothing for v2.5.0. Each packaging script spells it in its own
# format, so that the final release sorts above every pre-release.
#
# A tag that names another version, or a suffix that is not alpha, beta or rc
# followed by a number, fails: packaging it as the plain version would collide
# with the final release.

set -e

TAG="$1"
VERSION="$2"

case "${TAG}" in
  "v${VERSION}") exit 0 ;;
  "v${VERSION}-"*) ;;
  *)
    echo "prerelease: tag ${TAG} does not match version ${VERSION}" >&2
    exit 1
    ;;
esac

SUFFIX="${TAG#"v${VERSION}-"}"
if ! echo "${SUFFIX}" | grep -Eq '^(alpha|beta|rc)[0-9]+$'; then
  echo "prerelease: unsupported pre-release suffix ${SUFFIX} in ${TAG}" >&2
  exit 1
fi

echo "${SUFFIX}"
