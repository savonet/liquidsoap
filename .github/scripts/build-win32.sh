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

echo "::group::Install liquidsoap-windows"

eval "$(opam env)"

opam install -y --deps-only .github/opam/liquidsoap-windows.opam

export LIQUIDSOAP_BUILD_VERSION="${TAG}${VERSION}"
export LIQUIDSOAP_BUILD_TARGET=standalone
export LIQUIDSOAP_SYS_CONFIG=mingw
export LIQUIDSOAP_ENABLE_BUILD_CONFIG=false
export LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL=true
export LIQUIDSOAP_DUNE_TARGET=default.windows
export LIQUIDSOAP_LDFLAGS="-lcurl -lssh2 -lssl -lcrypto -lsecur32 -lgdi32 -lws2_32 -lwldap32 -ldl -lnghttp2 -lpsl -lidn2 -lzstd -lunistring -lbrotlicommon -lbrotlidec -lcrypt32 -liconv -lpthread -lz -lbcrypt -lwinmm -lksuser -link /usr/src/mxe/usr/x86_64-w64-mingw32.static/lib/libavutil.a"
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
cp -R "$(ocamlfind -toolchain windows ocamlc -where)/../../share/camomile" .
cd ..
zip -r "liquidsoap-$BUILD.zip" "liquidsoap-$BUILD"

mv "liquidsoap-$BUILD.zip" "/tmp/${GITHUB_RUN_NUMBER}/win32/dist"

echo "basename=liquidsoap-${BUILD}" >> "${GITHUB_OUTPUT}"

echo "::endgroup::"
