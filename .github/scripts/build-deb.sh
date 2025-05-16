#!/bin/sh

set -e

ARCH=$(dpkg --print-architecture)

COMMIT_SHORT=$(echo "${GITHUB_SHA}" | cut -c-7)

export DEBFULLNAME="The Savonet Team"
export DEBEMAIL="savonet-users@lists.sourceforge.net"
export LIQUIDSOAP_BUILD_TARGET=posix

cd /tmp/liquidsoap-full/liquidsoap

eval "$(opam config env)"
OCAMLPATH="$(cat ../.ocamlpath)"
export OCAMLPATH

LIQ_VERSION=$(opam show -f version ./opam/liquidsoap.opam | cut -d'-' -f 1)
LIQ_TAG=$(echo "${DOCKER_TAG}" | sed -e 's#_#-#g')

if [ -n "${IS_ROLLING_RELEASE}" ]; then
  LIQ_PACKAGE="liquidsoap-${COMMIT_SHORT}"
elif [ -n "${IS_RELEASE}" ]; then
  LIQ_PACKAGE="liquidsoap"
else
  TAG=$(echo "${BRANCH}" | tr '[:upper:]' '[:lower:]' | sed -e 's#[^0-9^a-z^A-Z^.^-]#-#g')
  LIQ_PACKAGE="liquidsoap-${TAG}"
fi

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

  ./liquidsoap --build-config > "${LIQ_TMP_DIR}/${LIQ_PACKAGE}_${LIQ_VERSION}-${LIQ_TAG}-${DEB_RELEASE}.config"

  mv /tmp/liquidsoap-full/*.deb "${LIQ_TMP_DIR}"
fi

echo "::endgroup::"

echo "::group:: build ${LIQ_PACKAGE}-minimal.."

# shellcheck disable=SC2086
opam remove -y --verbose --assume-depexts $MINIMAL_EXCLUDE_DEPS

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

  ./liquidsoap --build-config > "${LIQ_TMP_DIR}/${LIQ_PACKAGE}-minimal_${LIQ_VERSION}-${LIQ_TAG}-${DEB_RELEASE}.config"

  echo "::endgroup::"
fi

mv /tmp/liquidsoap-full/*.deb "${LIQ_TMP_DIR}"

{
  echo "basename=${LIQ_PACKAGE}_${LIQ_VERSION}-${LIQ_TAG}-${DEB_RELEASE}_$ARCH"
  echo "basename-minimal=${LIQ_PACKAGE}-minimal_${LIQ_VERSION}-${LIQ_TAG}-${DEB_RELEASE}_$ARCH"
} >> "${GITHUB_OUTPUT}"
