#!/bin/sh

set -e

BRANCH=$1
DOCKER_TAG=$2
ARCH=$3
ALPINE_ARCH=$4
IS_RELEASE=$5
APK_RELEASE=0

cd /tmp/liquidsoap-full/liquidsoap

APK_VERSION=`opam show -f version . | cut -d'~' -f 1`

TAG=`echo "${BRANCH}" | tr '[:upper:]' '[:lower:]' | sed -e 's#[^0-9^a-z^A-Z^.^-]#-#g'`

if [ -z "${IS_RELEASE}" ]; then
  APK_PACKAGE="liquidsoap-${TAG}-${ALPINE_ARCH}"
else
  APK_PACKAGE="liquidsoap-${ALPINE_ARCH}"
fi

echo "Building ${APK_PACKAGE}.."

cd /tmp/liquidsoap-full

cat liquidsoap/.github/alpine/APKBUILD.in | \
  sed -e "s#@APK_PACKAGE@#${APK_PACKAGE}#" | \
  sed -e "s#@APK_VERSION@#${APK_VERSION}#" | \
  sed -e "s#@APK_RELEASE@#${APK_RELEASE}#" \
  > APKBUILD

cp liquidsoap/.github/alpine/liquidsoap.pre-install ${APK_PACKAGE}.pre-install

abuild-keygen -a -n
abuild

mv /home/opam/packages/tmp/${ALPINE_ARCH}/${APK_PACKAGE}-${APK_VERSION}-r${APK_RELEASE}.apk /tmp/${GITHUB_RUN_NUMBER}/${DOCKER_TAG}_${ARCH}/alpine
mv /home/opam/packages/tmp/${ALPINE_ARCH}/${APK_PACKAGE}-dbg-${APK_VERSION}-r${APK_RELEASE}.apk /tmp/${GITHUB_RUN_NUMBER}/${DOCKER_TAG}_${ARCH}/alpine

echo "##[set-output name=basename;]${APK_PACKAGE}-${APK_VERSION}-r${APK_RELEASE}.apk"
