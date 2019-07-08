#!/bin/sh

COMMIT=$1
BRANCH=$1

mkdir debian
id=$(docker create liquidsoap-build)
docker cp $id:/liquidsoap.deb debian/liquidsoap-${COMMIT}.deb
docker rm -v $id

if test -n "${BRANCH}"; then
  cp debian/liquidsoap-${COMMIT}.deb debian/liquidsoap-${BRANCH}.deb
fi
