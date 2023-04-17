#!/bin/sh

set -e

export CPU_CORES=$1

PLATFORM=$2

eval $(opam config env)

cd /tmp/liquidsoap-full

echo "\n### Preparing bindings\n"

git remote set-url origin https://github.com/savonet/liquidsoap-full.git
git fetch --recurse-submodules=no && git checkout origin/master -- Makefile.git
git reset --hard
git pull

# Remove later
git submodule init ocaml-metadata
git submodule update ocaml-metadata

git pull
make public
make clean || true
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
git clone https://github.com/whitequark/ocaml-inotify.git
cd ocaml-inotify && opam pin -n .
opam install -y uri inotify camomile.1.0.2

cd /tmp/liquidsoap-full

sed -e 's@ocaml-gstreamer@#ocaml-gstreamer@' -i PACKAGES

export PKG_CONFIG_PATH=/usr/share/pkgconfig/pkgconfig

echo "\n### Compiling\n"

cd liquidsoqp && ./boostrap

export CC="`ocamlc -config | grep native_c_compiler | cut -d':' -f 2 | xargs`"

./configure --prefix=/usr --includedir=\${prefix}/include --mandir=\${prefix}/share/man \
            --infodir=\${prefix}/share/info --sysconfdir=/etc --localstatedir=/var \
            --with-camomile-data-dir=/usr/share/liquidsoap/camomile \
            CFLAGS=-g

cd liquidsoap
make -j $CPU_CORES

echo "\n### Basic tests\n"

./src/liquidsoap --no-stdlib --version
./src/liquidsoap --no-stdlib ./libs/stdlib.liq --check 'print("hello world")'
