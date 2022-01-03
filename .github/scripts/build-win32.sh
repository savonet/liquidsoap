#!/bin/sh

set -e

SYSTEM=$1
BRANCH=$2
CPU_CORES=$3
OPAM_PREFIX=`opam var prefix`
PWD=`dirname $0`
BASE_DIR=`cd "${PWD}/../.." && pwd`

if [ "${SYSTEM}" = "x64" ]; then
  HOST="x86_64-w64-mingw32.static"
  BUILD="${BRANCH}-win64"
  PKG_CONFIG_PATH="/usr/src/mxe/usr/x86_64-w64-mingw32.static/lib/pkgconfig/"
else
  HOST="i686-w64-mingw32.static"
  BUILD="${BRANCH}-win32"
  PKG_CONFIG_PATH="/usr/src/mxe/usr/i686-w64-mingw32.static/lib/pkgconfig/"
fi

export OPAMSOLVERTIMEOUT=480
export OPAMJOBS=$CPU_CORES
export CC=""

eval `opam config env`
opam repository set-url default https://github.com/ocaml/opam-repository.git
opam update
opam upgrade -y --verbose `echo $OPAM_DEPS | sed -e 's#,# #g'` liquidsoap-windows

cd ~
cp -rf ${BASE_DIR}/.github/win32 liquidsoap-$BUILD
cd liquidsoap-$BUILD
cp ${OPAM_PREFIX}/bin/liquidsoap.exe .
cp ${OPAM_PREFIX}/lib/liquidsoap/libs/*.liq libs
cp -rf `ocamlfind -toolchain windows ocamlc -where`/../../share/camomile .
cd ..
zip -r liquidsoap-$BUILD.zip liquidsoap-$BUILD

mv liquidsoap-$BUILD.zip /tmp/${GITHUB_RUN_NUMBER}/win32/dist

echo "##[set-output name=basename;]liquidsoap-${BUILD}"
