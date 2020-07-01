#!/bin/sh

set -e

GITHUB_SHA=$1
GITHUB_HEAD_REF=$2
DOCKER_TAG=$3
BUILD_NUMBER=$4
RELEASE=`echo "${DOCKER_TAG}" | cut -d'_' -f 2`

COMMIT_SHORT=`echo "${GITHUB_SHA}" | cut -c-7`

if test -z "${GITHUB_HEAD_REF}"; then
  BRANCH=master
else
  BRANCH=`basename "${GITHUB_HEAD_REF}"`
fi

DEBFULLNAME="The Savonet Team"
DEBEMAIL="savonet-users@lists.sourceforge.net"

eval $(opam config env)

cd /tmp/liquidsoap-full/liquidsoap

TAG=`echo "${COMMIT_SHORT}" | tr '[:upper:]' '[:lower:]' | sed -e 's#[^0-9^a-z^A-Z^.^-]#-#g'`

LIQ_PACKAGE="liquidsoap-${TAG}" 

echo "Building ${LIQ_PACKAGE}.."

rm -rf debian/changelog

cp -f debian/control.in debian/control

sed -e "s#@LIQ_PACKAGE@#${LIQ_PACKAGE}#g" -i debian/control

LIQ_VERSION=`opam show -f version .`

dch --create --distribution unstable --package "${LIQ_PACKAGE}" --newversion "1:${LIQ_VERSION}-${BUILD_NUMBER}~${RELEASE}" "Build ${COMMIT_SHORT}"

fakeroot debian/rules binary

TAG=`echo "${BRANCH}" | tr '[:upper:]' '[:lower:]' | sed -e 's#[^0-9^a-z^A-Z^.^-]#-#g'`

LIQ_PACKAGE="liquidsoap-${TAG}"

echo "Building ${LIQ_PACKAGE}.."

rm -rf debian/changelog

cp -f debian/control.in debian/control

sed -e "s#@LIQ_PACKAGE@#${LIQ_PACKAGE}#g" -i debian/control

dch --create --distribution unstable --package "${LIQ_PACKAGE}" --newversion "1:${LIQ_VERSION}-${BUILD_NUMBER}~${RELEASE}" "Build ${COMMIT_SHORT}"

fakeroot debian/rules binary

mkdir -p "/tmp/debian/pkgs/${DOCKER_TAG}"

cp /tmp/liquidsoap-full/*.deb "/tmp/debian/pkgs/${DOCKER_TAG}"
