#!/bin/sh

set -e

export PKG_CONFIG_PATH=/usr/src/mxe/usr/x86_64-w64-mingw32.static/lib/pkgconfig

SYSTEM="$1"
BRANCH="$2"
CPU_CORES="$3"
IS_ROLLING_RELEASE="$4"
IS_RELEASE="$5"
GITHUB_SHA="$6"

OPAM_PREFIX="$(opam var prefix)"
VERSION="$(opam show -f version ./liquidsoap.opam | cut -d'-' -f 1)"
PWD="$(dirname "$0")"
BASE_DIR="$(cd "${PWD}/../.." && pwd)"
COMMIT_SHORT="$(echo "${GITHUB_SHA}" | cut -c-7)"

if [ -n "${IS_ROLLING_RELEASE}" ]; then
  TAG="${COMMIT_SHORT}-"
elif [ -n "${IS_RELEASE}" ]; then
  TAG=""
else
  TAG="${BRANCH}-"
fi

if [ "${SYSTEM}" = "x64" ]; then
  HOST="x86_64-w64-mingw32.static"
  BUILD="${TAG}${VERSION}-win64"
  PKG_CONFIG_PATH="/usr/src/mxe/usr/x86_64-w64-mingw32.static/lib/pkgconfig/"
else
  # shellcheck disable=SC2034
  HOST="i686-w64-mingw32.static"
  BUILD="${TAG}${VERSION}-win32"
  # shellcheck disable=SC2034
  PKG_CONFIG_PATH="/usr/src/mxe/usr/i686-w64-mingw32.static/lib/pkgconfig/"
fi

export OPAMSOLVERTIMEOUT=480
export OPAMJOBS="$CPU_CORES"
export CC=""

echo "::group::Installing deps"

eval "$(opam config env)"
opam repository set-url windows https://github.com/ocaml-cross/opam-cross-windows.git
opam update

opam install -y posix-socket.3.0.0 srt-windows.0.3.4 prometheus-app-windows cohttp-lwt-unix-windows ffmpeg-avutil-windows.1.2.3

echo "::endgroup::"

echo "::group::Install liquidsoap-windows"
opam install -y liquidsoap-windows
echo "::endgroup::"

echo "::group::Save build config"

wine "${OPAM_PREFIX}/windows-sysroot/bin/liquidsoap" --build-config >> "/tmp/${GITHUB_RUN_NUMBER}/win32/dist/liquidsoap-$BUILD.config"

echo "Build config:"

cat "/tmp/${GITHUB_RUN_NUMBER}/win32/dist/liquidsoap-$BUILD.config"

echo "::endgroup::"

echo "::group::Bundling executable"

cd ~
cp -rf "${BASE_DIR}/.github/win32" "liquidsoap-$BUILD"
cp -rf "${BASE_DIR}/src/libs" "liquidsoap-$BUILD"
cd "liquidsoap-$BUILD"
cp "${OPAM_PREFIX}"/windows-sysroot/bin/liquidsoap ./liquidsoap.exe
cp -rf "$(ocamlfind -toolchain windows ocamlc -where)/../../share/camomile" .
cd ..
zip -r "liquidsoap-$BUILD.zip" "liquidsoap-$BUILD"

mv "liquidsoap-$BUILD.zip" "/tmp/${GITHUB_RUN_NUMBER}/win32/dist"

echo "basename=liquidsoap-${BUILD}" >> "${GITHUB_OUTPUT}"

echo "::endgroup::"
