#!/bin/sh

set -e

export CPU_CORES=$1

PLATFORM=$2

eval $(opam config env)

cd /tmp/liquidsoap-full

echo "\n### Preparing bindings\n"

git remote set-url origin https://github.com/savonet/liquidsoap-full.git
git fetch --recurse-submodules=no && git checkout origin/master -- Makefile.git
make public
git reset --hard

git pull
make clean
make public
make update

echo "\n### Checking out CI commit\n"

cd liquidsoap
git fetch origin $GITHUB_SHA
git checkout $GITHUB_SHA
mv .github /tmp
rm -rf *
mv /tmp/.github .
git reset --hard

echo "\n### Setting up specific dependencies\n"

./.github/scripts/checkout-deps.sh

opam update
# See: https://github.com/whitequark/ocaml-inotify/pull/20
opam install -y ocurl uri inotify.2.3 mem_usage.0.0.4 ctypes-foreign

cd /tmp/liquidsoap-full

sed -e 's@ocaml-gstreamer@#ocaml-gstreamer@' -i PACKAGES

export PKG_CONFIG_PATH=/usr/share/pkgconfig/pkgconfig

echo "\n### Compiling\n"

./bootstrap
./configure --prefix=/usr --includedir=\${prefix}/include --mandir=\${prefix}/share/man \
            --infodir=\${prefix}/share/info --sysconfdir=/etc --localstatedir=/var \
            --with-camomile-data-dir=/usr/share/liquidsoap/camomile \
            CFLAGS=-g

cd liquidsoap
make -j $CPU_CORES

if [ "${PLATFORM}" = "armhf" ]; then
  exit 0;
fi

echo "\n### Basic tests\n"

./src/liquidsoap --no-stdlib --version
./src/liquidsoap --no-stdlib ./libs/stdlib.liq --check 'print("hello world")'
