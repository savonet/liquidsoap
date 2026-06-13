#!/bin/sh

set -e

ARCH=$(dpkg --print-architecture)

COMMIT_SHORT=$(echo "${GITHUB_SHA}" | cut -c-7)

export DEBFULLNAME="The Savonet Team"
export DEBEMAIL="savonet-users@lists.sourceforge.net"
export LIQUIDSOAP_BUILD_TARGET=posix

cd /tmp/liquidsoap

eval "$(opam config env)"

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

cp -R .github/debian .

rm -rf debian/changelog

cp -f debian/control.in debian/control

sed -e "s#@LIQ_PACKAGE@#${LIQ_PACKAGE}#g" -i debian/control

dch --create --distribution unstable --package "${LIQ_PACKAGE}" --newversion "1:${LIQ_VERSION}-${LIQ_TAG}-${DEB_RELEASE}" "Build ${COMMIT_SHORT}"

fakeroot debian/rules binary

echo "::endgroup::"

if [ "${PLATFORM}" = "amd64" ]; then
  echo "::group:: save build config for ${LIQ_PACKAGE}.."

  ./liquidsoap --build-config > "${LIQ_TMP_DIR}/${LIQ_PACKAGE}_${LIQ_VERSION}-${LIQ_TAG}-${DEB_RELEASE}.config"

  mv /tmp/*.deb "${LIQ_TMP_DIR}"
fi

echo "::endgroup::"

echo "basename=${LIQ_PACKAGE}_${LIQ_VERSION}-${LIQ_TAG}-${DEB_RELEASE}_$ARCH" >> "${GITHUB_OUTPUT}"
