#!/bin/sh

TRAVIS_COMMIT=$1
TRAVIS_BRANCH=$2
TRAVIS_PULL_REQUEST_BRANCH=$3
TRAVIS_PULL_REQUEST=$4

if test "${TRAVIS_PULL_REQUEST}" = "false"; then
  BRANCH="${TRAVIS_BRANCH}"
else
  BRANCH="${TRAVIS_PULL_REQUEST_BRANCH}"
fi

mkdir debian
id=$(docker create liquidsoap-build)
docker cp $id:/tmp/liquidsoap.deb debian/liquidsoap-${TRAVIS_COMMIT}.deb
docker rm -v $id

if test -n "${BRANCH}"; then
  cp debian/liquidsoap-${TRAVIS_COMMIT}.deb debian/liquidsoap-${BRANCH}.deb
fi
