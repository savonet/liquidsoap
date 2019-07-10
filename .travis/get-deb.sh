#!/bin/sh

set -e

TRAVIS_COMMIT=$1
TRAVIS_BRANCH=$2
TRAVIS_PULL_REQUEST_BRANCH=$3
TRAVIS_PULL_REQUEST=$4

if test "${TRAVIS_PULL_REQUEST}" = "false"; then
  BRANCH="${TRAVIS_BRANCH}"
else
  BRANCH="${TRAVIS_PULL_REQUEST_BRANCH}"
fi

rm -rf debian
id=$(docker create liquidsoap-build)
docker cp $id:/tmp/debian debian/
cp -rf debian/pkgs/liquidsoap_0+${TRAVIS_COMMIT}-1_amd64.deb debian/pkgs/liquidsoap_0+${BRANCH}-1_amd64.deb
docker rm -v $id
