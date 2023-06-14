#!/bin/sh

set -e

BRANCH="$1"
DOCKER_TAG="$2"
ARCH="$3"
ALPINE_ARCH="$4"
IS_ROLLING_RELEASE="$5"
IS_RELEASE="$6"
MINIMAL_EXCLUDE_DEPS="$7"
APK_RELEASE=0

cd /tmp/liquidsoap-full/liquidsoap

APK_VERSION=$(opam show -f version ./liquidsoap.opam | cut -d'-' -f 1)
COMMIT_SHORT=$(echo "${GITHUB_SHA}" | cut -c-7)

if [ -n "${IS_ROLLING_RELEASE}" ]; then
  APK_PACKAGE="liquidsoap-${COMMIT_SHORT}-${ALPINE_ARCH}"
elif [ -n "${IS_RELEASE}" ]; then
  APK_PACKAGE="liquidsoap-${ALPINE_ARCH}"
else
  TAG=$(echo "${BRANCH}" | tr '[:upper:]' '[:lower:]' | sed -e 's#[^0-9^a-z^A-Z^.^-]#-#g')
  APK_PACKAGE="liquidsoap-${TAG}-${ALPINE_ARCH}"
fi

echo "Building ${APK_PACKAGE}.."

cd /tmp/liquidsoap-full

sed -e "s#@APK_PACKAGE@#${APK_PACKAGE}#" liquidsoap/.github/alpine/APKBUILD.in |
  sed -e "s#@APK_VERSION@#${APK_VERSION}#" |
  sed -e "s#@APK_RELEASE@#${APK_RELEASE}#" \
    > APKBUILD

cp "liquidsoap/.github/alpine/liquidsoap.pre-install" "${APK_PACKAGE}.pre-install"

abuild-keygen -a -n
abuild

mv /home/opam/packages/tmp/"${ALPINE_ARCH}"/*.apk "/tmp/${GITHUB_RUN_NUMBER}/${DOCKER_TAG}_${ARCH}/alpine"

rm -rf APKBUILD /home/opam/packages/tmp/"${ALPINE_ARCH}"

echo "Building ${APK_PACKAGE}-minimal.."

eval "opam remove --force -y $MINIMAL_EXCLUDE_DEPS"

sed -e "s#@APK_PACKAGE@#${APK_PACKAGE}-minimal#" liquidsoap/.github/alpine/APKBUILD-minimal.in |
  sed -e "s#@APK_VERSION@#${APK_VERSION}#" |
  sed -e "s#@APK_RELEASE@#${APK_RELEASE}#" \
    > APKBUILD

cp "liquidsoap/.github/alpine/liquidsoap.pre-install" "${APK_PACKAGE}-minimal.pre-install"

abuild-keygen -a -n
abuild

echo "##[set-output name=basename;]${APK_PACKAGE}-${APK_VERSION}-r${APK_RELEASE}.apk"
echo "##[set-output name=basename-minimal;]${APK_PACKAGE}-minimal-${APK_VERSION}-r${APK_RELEASE}.apk"
