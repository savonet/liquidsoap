#!/bin/sh

set -e

CPU_CORES=$1

eval $(opam config env)

cd /tmp/liquidsoap-full

echo "\n### Preparing bindings\n"

git pull
make clean
make update

echo "\n### Checking out CI commit\n"

cd liquidsoap
git fetch origin $GITHUB_SHA
git checkout $GITHUB_SHA

echo "\n### Setting up specific dependencies\n"

./.github/scripts/checkout-deps.sh

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
