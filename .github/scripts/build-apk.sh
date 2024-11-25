#!/bin/sh

set -e

cd /tmp/liquidsoap-full/liquidsoap

APK_VERSION=$(opam show -f version ./liquidsoap.opam | cut -d'-' -f 1)

export LIQUIDSOAP_BUILD_TARGET=posix
export ABUILD_APK_INDEX_OPTS="--allow-untrusted"

APK_PACKAGE="${LIQ_PACKAGE}-${ALPINE_ARCH}"

echo "::group:: build ${APK_PACKAGE}.."

cd /tmp/liquidsoap-full

sed -e "s#@APK_PACKAGE@#${APK_PACKAGE}#" liquidsoap/.github/alpine/APKBUILD.in |
  sed -e "s#@APK_VERSION@#${APK_VERSION}#" |
  sed -e "s#@APK_RELEASE@#${APK_RELEASE}#" \
    > APKBUILD

cp "liquidsoap/.github/alpine/liquidsoap.post-install" "${APK_PACKAGE}.post-install"

abuild-keygen -a -n
abuild

mv /home/opam/packages/tmp/"${ALPINE_ARCH}"/*.apk "/tmp/${GITHUB_RUN_NUMBER}/${DOCKER_TAG}_${ARCH}/alpine"

echo "::endgroup::"

if [ "${ARCH}" = "amd64" ]; then
  echo "::group:: save build config for ${APK_PACKAGE}.."

  eval "$(opam config env)"
  OCAMLPATH=$(cat .ocamlpath)
  export OCAMLPATH
  cd liquidsoap && ./liquidsoap --build-config > "/tmp/${GITHUB_RUN_NUMBER}/${DOCKER_TAG}_${ARCH}/alpine/${APK_PACKAGE}-${APK_VERSION}-r${APK_RELEASE}.config"

  echo "::endgroup::"
fi

rm -rf APKBUILD /home/opam/packages/tmp/"${ALPINE_ARCH}"

echo "::group:: building ${APK_PACKAGE}-minimal.."

# shellcheck disable=SC2086
opam remove -y $MINIMAL_EXCLUDE_DEPS

eval "$(opam config env)"

cd /tmp/liquidsoap-full
make clean
cp PACKAGES.minimal-build PACKAGES

cd liquidsoap
./.github/scripts/build-posix.sh 1

cd /tmp/liquidsoap-full

OCAMLPATH=$(cat .ocamlpath)
export OCAMLPATH

sed -e "s#@APK_PACKAGE@#${APK_PACKAGE}-minimal#" liquidsoap/.github/alpine/APKBUILD-minimal.in |
  sed -e "s#@APK_VERSION@#${APK_VERSION}#" |
  sed -e "s#@APK_RELEASE@#${APK_RELEASE}#" \
    > APKBUILD

cp "liquidsoap/.github/alpine/liquidsoap.post-install" "${APK_PACKAGE}-minimal.post-install"

abuild-keygen -a -n
abuild

mv /home/opam/packages/tmp/"${ALPINE_ARCH}"/*.apk "/tmp/${GITHUB_RUN_NUMBER}/${DOCKER_TAG}_${ARCH}/alpine"

echo "::endgroup::"

if [ "${ARCH}" = "amd64" ]; then
  echo "::group:: save build config for ${APK_PACKAGE}-minimal.."

  cd liquidsoap && ./liquidsoap --build-config > "/tmp/${GITHUB_RUN_NUMBER}/${DOCKER_TAG}_${ARCH}/alpine/${APK_PACKAGE}-minimal-${APK_VERSION}-r${APK_RELEASE}.config"
fi

echo "::endgroup::"

{
  echo "basename=${APK_PACKAGE}-${APK_VERSION}-r${APK_RELEASE}.apk"
  echo "basename-minimal=${APK_PACKAGE}-minimal-${APK_VERSION}-r${APK_RELEASE}.apk"
} >> "${GITHUB_OUTPUT}"
