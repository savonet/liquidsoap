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

rm -rf debian
id=$(docker create liquidsoap-build)
docker cp $id:/tmp/debian debian/
cp -rf debian/pkgs/liquidsoap_${TRAVIS_COMMIT}-1.deb debian/pkgs/liquidsoap-${BRANCH}-1.deb
docker rm -v $id
