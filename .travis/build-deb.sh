#!/bin/sh

set -e

TRAVIS_COMMIT_SHORT=$1
TRAVIS_BRANCH=$2
TRAVIS_PULL_REQUEST_BRANCH=$3
TRAVIS_PULL_REQUEST=$4
DOCKER_TAG=$5
RELEASE=`echo "${DOCKER_TAG}" | cut -d'_' -f 2`

DEBFULLNAME="The Savonet Team"
DEBEMAIL="savonet-users@lists.sourceforge.net"

if test "${TRAVIS_PULL_REQUEST}" = "false"; then
  BRANCH="${TRAVIS_BRANCH}"
else
  BRANCH="${TRAVIS_PULL_REQUEST_BRANCH}"
fi

DEB_VERSION=`date +"%Y%m%d%H%M"`

eval $(opam config env)

cd /tmp/liquidsoap-full/liquidsoap

dch --create --distribution unstable --package "liquidsoap" --newversion "1:0+${TRAVIS_COMMIT_SHORT}~${RELEASE}-${DEB_VERSION}" "Build ${TRAVIS_COMMIT_SHORT}"

fakeroot debian/rules binary

rm -rf debian/changelog

dch --create --distribution unstable --package "liquidsoap" --newversion "1:0+${BRANCH}~${RELEASE}-${DEB_VERSION}" "Build ${BRANCH}"

fakeroot debian/rules binary

mkdir -p "/tmp/debian/pkgs/${DOCKER_TAG}"

cp /tmp/liquidsoap-full/*.deb "/tmp/debian/pkgs/${DOCKER_TAG}"
