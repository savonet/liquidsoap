#!/bin/sh

set -e

BASE_DIR="$(pwd)"
VERSION="$(opam show -f version ./opam/liquidsoap.opam | cut -d'-' -f 1)"
COMMIT_SHORT="$(echo "${GITHUB_SHA}" | cut -c-7)"

if [ -n "${IS_ROLLING_RELEASE}" ]; then
  TAG="${COMMIT_SHORT}-"
elif [ -n "${IS_RELEASE}" ]; then
  TAG=""
else
  TAG="${BRANCH}-"
fi

BUILD="${TAG}${VERSION}-win64"

echo "::group::Build liquidsoap-windows"

eval "$(opam env)"

# The image ships the overlay it was built against. Point it at this checkout
# so a branch changing .github/opam is built with its own packages.
opam repository set-url liquidsoap-devel "${BASE_DIR}/.github/opam"
opam update liquidsoap-devel

opam install -y --deps-only .github/opam/liquidsoap-windows.opam

# The image sets PKG_CONFIG_PATH so that the opam packages configure against
# mxe. Building liquidsoap goes through the dune context instead, and leaving
# it set enables optional modules whose transitive libraries are not linked.
unset PKG_CONFIG_PATH

export LIQUIDSOAP_BUILD_VERSION="${TAG}${VERSION}"
export LIQUIDSOAP_BUILD_TARGET=standalone
export LIQUIDSOAP_SYS_CONFIG=mingw
export LIQUIDSOAP_ENABLE_BUILD_CONFIG=false
export LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL=true
export LIQUIDSOAP_DUNE_TARGET=default.windows
export LIQUIDSOAP_LDFLAGS="-lcurl -lssh2 -lsecur32 -lpsl -liphlpapi -lnghttp2 -lwldap32 -link /usr/src/mxe/usr/x86_64-w64-mingw32.static/lib/libavutil.a"
dune build -x windows --release _build/default.windows/src/bin/liquidsoap.exe --verbose

echo "::endgroup::"

echo "::group::Save build config"

wine "${BASE_DIR}/_build/default.windows/src/bin/liquidsoap.exe" --build-config >> "/tmp/${GITHUB_RUN_NUMBER}/win32/dist/liquidsoap-$BUILD.config"

echo "Build config:"

cat "/tmp/${GITHUB_RUN_NUMBER}/win32/dist/liquidsoap-$BUILD.config"

echo "::endgroup::"

echo "::group::Bundling executable"

cd ~
cp -R "${BASE_DIR}/.github/win32" "liquidsoap-$BUILD"
cp -R "${BASE_DIR}/src/libs" "liquidsoap-$BUILD"
cd "liquidsoap-$BUILD"
cp "${BASE_DIR}/_build/default.windows/src/bin/liquidsoap.exe" ./liquidsoap.exe
cd ..
zip -r "liquidsoap-$BUILD.zip" "liquidsoap-$BUILD"

mv "liquidsoap-$BUILD.zip" "/tmp/${GITHUB_RUN_NUMBER}/win32/dist"

echo "basename=liquidsoap-${BUILD}" >> "${GITHUB_OUTPUT}"

echo "::endgroup::"
