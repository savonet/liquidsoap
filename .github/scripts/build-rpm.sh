#!/bin/sh

set -e

cd /tmp/liquidsoap

eval "$(opam config env)"

COMMIT_SHORT=$(echo "${GITHUB_SHA}" | cut -c-7)
RPM_VERSION=$(opam show -f version ./opam/liquidsoap.opam | cut -d'-' -f 1)

# Rolling builds keep the release's version and sort below it through a 0.
# release, Fedora's pre-release convention: `~` would do the same, but GitHub
# rewrites it in asset names.
if [ -n "${IS_ROLLING_RELEASE}" ]; then
  RPM_PACKAGE="liquidsoap"
  RPM_RELEASE="0.${BUILD_STAMP}.${COMMIT_SHORT}"
elif [ -n "${IS_RELEASE}" ]; then
  RPM_PACKAGE="liquidsoap"
  RPM_RELEASE=1
else
  TAG=$(echo "${BRANCH}" | tr '[:upper:]' '[:lower:]' | sed -e 's#[^0-9a-z.-]#-#g')
  RPM_PACKAGE="liquidsoap-${TAG}"
  RPM_RELEASE="0.${BUILD_STAMP}.${COMMIT_SHORT}"
fi

TOPDIR=$(mktemp -d)

echo "::group:: build ${RPM_PACKAGE}.."

sed -e "s#@RPM_PACKAGE@#${RPM_PACKAGE}#" \
  -e "s#@RPM_VERSION@#${RPM_VERSION}#" \
  -e "s#@RPM_RELEASE@#${RPM_RELEASE}#" \
  .github/fedora/liquidsoap.spec.in > "${TOPDIR}/liquidsoap.spec"

rpmbuild --define "_topdir ${TOPDIR}" -bb "${TOPDIR}/liquidsoap.spec"

echo "::endgroup::"

# Several OCaml versions build the same package, so the asset name carries the
# one it was built with. build-repo.sh selects on it.
RPM=$(find "${TOPDIR}/RPMS" -name '*.rpm')
RPM_ARCH=$(rpm -qp --qf '%{ARCH}' "${RPM}")
BASENAME="$(basename "${RPM}" ".${RPM_ARCH}.rpm")-${RPM_TAG}.${RPM_ARCH}"
mv "${RPM}" "${LIQ_TMP_DIR}/${BASENAME}.rpm"
rm -rf "${TOPDIR}"

if [ "${PLATFORM}" = "amd64" ]; then
  ./liquidsoap --build-config > "${LIQ_TMP_DIR}/${BASENAME}.config"
fi

echo "basename=${BASENAME}" >> "${GITHUB_OUTPUT}"
