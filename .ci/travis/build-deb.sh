#!/bin/sh

set -e

TRAVIS_COMMIT_SHORT=$1
TRAVIS_BRANCH=$2
TRAVIS_PULL_REQUEST_BRANCH=$3
TRAVIS_PULL_REQUEST=$4
DOCKER_TAG=$5
TRAVIS_BUILD_NUMBER=$6
RELEASE=`echo "${DOCKER_TAG}" | cut -d'_' -f 2`

DEBFULLNAME="The Savonet Team"
DEBEMAIL="savonet-users@lists.sourceforge.net"

if test "${TRAVIS_PULL_REQUEST}" = "false"; then
  BRANCH="${TRAVIS_BRANCH}"
else
  BRANCH="${TRAVIS_PULL_REQUEST_BRANCH}"
fi

eval $(opam config env)

cd /tmp/liquidsoap-full/liquidsoap

TAG=`echo "${TRAVIS_COMMIT_SHORT}" | tr '[:upper:]' '[:lower:]' | sed -e 's#[^0-9^a-z^A-Z^.^-]#-#g'`

LIQ_PACKAGE="liquidsoap-${TAG}" 

echo "Building ${LIQ_PACKAGE}.."

rm -rf debian/changelog

cp -f debian/control.in debian/control

sed -e "s#@LIQ_PACKAGE@#${LIQ_PACKAGE}#g" -i debian/control

LIQ_VERSION=`opam show -f version .`

dch --create --distribution unstable --package "${LIQ_PACKAGE}" --newversion "1:${LIQ_VERSION}-${TRAVIS_BUILD_NUMBER}~${RELEASE}" "Build ${TRAVIS_COMMIT_SHORT}"

fakeroot debian/rules binary

TAG=`echo "${BRANCH}" | tr '[:upper:]' '[:lower:]' | sed -e 's#[^0-9^a-z^A-Z^.^-]#-#g'`

LIQ_PACKAGE="liquidsoap-${TAG}"

echo "Building ${LIQ_PACKAGE}.."

rm -rf debian/changelog

cp -f debian/control.in debian/control

sed -e "s#@LIQ_PACKAGE@#${LIQ_PACKAGE}#g" -i debian/control

dch --create --distribution unstable --package "${LIQ_PACKAGE}" --newversion "1:${LIQ_VERSION}-${TRAVIS_BUILD_NUMBER}~${RELEASE}" "Build ${TRAVIS_COMMIT_SHORT}"

fakeroot debian/rules binary

mkdir -p "/tmp/debian/pkgs/${DOCKER_TAG}"

cp /tmp/liquidsoap-full/*.deb "/tmp/debian/pkgs/${DOCKER_TAG}"
