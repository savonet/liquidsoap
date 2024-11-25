#!/bin/sh

set -e

ARCH=$(dpkg --print-architecture)

export DEBFULLNAME="The Savonet Team"
export DEBEMAIL="savonet-users@lists.sourceforge.net"
export LIQUIDSOAP_BUILD_TARGET=posix

cd /tmp/liquidsoap-full/liquidsoap

eval "$(opam config env)"
OCAMLPATH="$(cat ../.ocamlpath)"
export OCAMLPATH

LIQ_VERSION=$(opam show -f version ./liquidsoap.opam | cut -d'-' -f 1)
LIQ_TAG=$(echo "${DOCKER_TAG}" | sed -e 's#_#-#g')

echo "::group:: build ${LIQ_PACKAGE}.."

cp -rf .github/debian .

rm -rf debian/changelog

cp -f debian/control.in debian/control

sed -e "s#@LIQ_PACKAGE@#${LIQ_PACKAGE}#g" -i debian/control

dch --create --distribution unstable --package "${LIQ_PACKAGE}" --newversion "1:${LIQ_VERSION}-${LIQ_TAG}-${DEB_RELEASE}" "Build ${COMMIT_SHORT}"

fakeroot debian/rules binary

echo "::endgroup::"

if [ "${PLATFORM}" = "amd64" ]; then
  echo "::group:: save build config for ${LIQ_PACKAGE}.."

  ./liquidsoap --build-config > "/tmp/${GITHUB_RUN_NUMBER}/${DOCKER_TAG}_${PLATFORM}/debian/${LIQ_PACKAGE}_${LIQ_VERSION}-${LIQ_TAG}-${DEB_RELEASE}.config"

  mv /tmp/liquidsoap-full/*.deb "/tmp/${GITHUB_RUN_NUMBER}/${DOCKER_TAG}_${PLATFORM}/debian"
fi

echo "::endgroup::"

echo "::group:: build ${LIQ_PACKAGE}-minimal.."

# shellcheck disable=SC2086
opam remove -y --verbose $MINIMAL_EXCLUDE_DEPS

cd /tmp/liquidsoap-full
make clean
cp PACKAGES.minimal-build PACKAGES
rm .ocamlpath
cd liquidsoap
./.github/scripts/build-posix.sh 1
OCAMLPATH="$(cat ../.ocamlpath)"
export OCAMLPATH

rm -rf debian

cp -rf .github/debian .

rm -rf debian/changelog

cp -f debian/control.in debian/control

sed -e "s#@LIQ_PACKAGE@#${LIQ_PACKAGE}-minimal#g" -i debian/control

cp -rf debian/rules-minimal debian/rules

dch --create --distribution unstable --package "${LIQ_PACKAGE}-minimal" --newversion "1:${LIQ_VERSION}-${LIQ_TAG}-${DEB_RELEASE}" "Build ${COMMIT_SHORT}"

fakeroot debian/rules binary

echo "::endgroup::"

if [ "${PLATFORM}" = "amd64" ]; then
  echo "::group:: save build config for ${LIQ_PACKAGE}.."

  ./liquidsoap --build-config > "/tmp/${GITHUB_RUN_NUMBER}/${DOCKER_TAG}_${PLATFORM}/debian/${LIQ_PACKAGE}-minimal_${LIQ_VERSION}-${LIQ_TAG}-${DEB_RELEASE}.config"

  echo "::endgroup::"
fi

mv /tmp/liquidsoap-full/*.deb "/tmp/${GITHUB_RUN_NUMBER}/${DOCKER_TAG}_${PLATFORM}/debian"

{
  echo "basename=${LIQ_PACKAGE}_${LIQ_VERSION}-${LIQ_TAG}-${DEB_RELEASE}_$ARCH"
  echo "basename-minimal=${LIQ_PACKAGE}-minimal_${LIQ_VERSION}-${LIQ_TAG}-${DEB_RELEASE}_$ARCH"
} >> "${GITHUB_OUTPUT}"
