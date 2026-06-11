#!/bin/sh

set -e

export PKG_CONFIG_PATH="/usr/src/mxe/usr/x86_64-w64-mingw32.static/lib/pkgconfig/"
export PKG_CONFIG=x86_64-w64-mingw32.static-pkg-config

echo "::group::Install liquidsoap-windows"
opam install -y --deps-only .github/opam/liquidsoap-windows.opam

export LIQUIDSOAP_BUILD_TARGET=standalone
export LIQUIDSOAP_SYS_CONFIG=mingw
export LIQUIDSOAP_ENABLE_BUILD_CONFIG=false
export LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL=true
export LIQUIDSOAP_LDFLAGS="-lcurl -lwldap32 -ldl -lnghttp2 -lpsl -lssh2 -lidn2 -lzstd -lunistring -lbrotlicommon -lbrotlidec -lcrypt32 -liconv -lpthread -lz -lbcrypt -lwinmm -lksuser -link /usr/src/mxe/usr/x86_64-w64-mingw32.static/lib/libavutil.a"
export PKG_CONFIG_PATH_default_windows=/usr/src/mxe/usr/x86_64-w64-mingw32.static/lib/pkgconfig
dune build -x windows @install --release

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
