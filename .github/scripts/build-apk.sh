#!/bin/sh

set -e

BRANCH=$1
ARCH=$2

cd /tmp/liquidsoap-full/liquidsoap

APK_VERSION=`opam show -f version .`

TAG=`echo "${BRANCH}" | tr '[:upper:]' '[:lower:]' | sed -e 's#[^0-9^a-z^A-Z^.^-]#-#g'`

if [ "${ARCH}" = "amd64" ]; then
  ALPINE_ARCH=x86_64
else
  ALPINE_ARCH=aarch64
fi

APK_PACKAGE="liquidsoap-${TAG}-${ALPINE_ARCH}"

echo "Building ${APK_PACKAGE}.."

cd /tmp/liquidsoap-full

cat liquidsoap/.github/alpine/APKBUILD.in | \
  sed -e "s#@APK_PACKAGE@#${APK_PACKAGE}#" | \
  sed -e "s#@APK_VERSION@#${APK_VERSION}#" \
  > APKBUILD

cp liquidsoap/.github/alpine/liquidsoap.pre-install ${APK_PACKAGE}.pre-install

abuild-keygen -a -n
abuild

mv /home/opam/packages/tmp/${ALPINE_ARCH}/${APK_PACKAGE}-${APK_VERSION}-r0.apk /tmp/alpine
mv /home/opam/packages/tmp/${ALPINE_ARCH}/${APK_PACKAGE}-dbg-${APK_VERSION}-r0.apk /tmp/alpine

echo "##[set-output name=basename;]${APK_PACKAGE}-${APK_VERSION}-r0.apk"
