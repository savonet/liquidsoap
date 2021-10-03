#!/bin/sh

set -e

GITHUB_SHA=$1
BRANCH=$2
DOCKER_TAG=$3
IS_RELEASE=$4

ARCH=`dpkg --print-architecture`

COMMIT_SHORT=`echo "${GITHUB_SHA}" | cut -c-7`

DEBFULLNAME="The Savonet Team"
DEBEMAIL="savonet-users@lists.sourceforge.net"

eval $(opam config env)

cd /tmp/liquidsoap-full/liquidsoap

LIQ_VERSION=`opam show -f version .`
LIQ_TAG=`echo ${DOCKER_TAG} | sed -e 's#_#-#g'`

if [ -z "${IS_RELEASE}" ]; then
  TAG=`echo "${BRANCH}" | tr '[:upper:]' '[:lower:]' | sed -e 's#[^0-9^a-z^A-Z^.^-]#-#g'`
  LIQ_PACKAGE="liquidsoap-${TAG}"
else
  LIQ_PACKAGE="liquidsoap"
fi

echo "Building ${LIQ_PACKAGE}.."

cp -rf .github/debian .

rm -rf debian/changelog

cp -f debian/control.in debian/control

sed -e "s#@LIQ_PACKAGE@#${LIQ_PACKAGE}#g" -i debian/control

dch --create --distribution unstable --package "${LIQ_PACKAGE}" --newversion "1:${LIQ_VERSION}-${LIQ_TAG}-1" "Build ${COMMIT_SHORT}"

fakeroot debian/rules binary

cp /tmp/liquidsoap-full/*.deb /tmp/debian

ls -lA /tmp/debian

echo "##[set-output name=basename;]${LIQ_PACKAGE}_${LIQ_VERSION}-${LIQ_TAG}-1_$ARCH"
