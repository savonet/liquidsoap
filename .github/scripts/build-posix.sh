#!/bin/sh

set -e

export CPU_CORES=$1

eval $(opam config env)

cd /tmp/liquidsoap-full

opam update
opam remove -y ocamlsdl sdl-liquidsoap
opam depext -yi tsdl-image.0.3.2

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

echo "\n### Setting up specific dependencies\n"

./.github/scripts/checkout-deps.sh

cd /tmp/liquidsoap-full

sed -e 's@ocaml-gstreamer@#ocaml-gstreamer@' -i PACKAGES

# Remove after next release
sed -e "s@ocaml-mm@ocaml-mem_usage\nocaml-mm@" -i PACKAGES

export PKG_CONFIG_PATH=/usr/share/pkgconfig/pkgconfig

echo "\n### Compiling\n"

./bootstrap
./configure --prefix=/usr --includedir=\${prefix}/include --mandir=\${prefix}/share/man \
            --infodir=\${prefix}/share/info --sysconfdir=/etc --localstatedir=/var \
            --with-camomile-data-dir=/usr/share/liquidsoap/camomile \
            CFLAGS=-g

cd liquidsoap
make -j $CPU_CORES

echo "\n### Basic tests\n"

./src/liquidsoap --no-stdlib --version
./src/liquidsoap --no-stdlib ./libs/stdlib.liq --check 'print("hello world")'
