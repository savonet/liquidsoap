#!/bin/sh

set -e

eval $(opam config env)

# Remove after liquidsoap 2.0 release
opam install -y posix-time2

cd /tmp/liquidsoap-full

echo "### Preparing bindings\n"

git pull
make clean
make update

echo "### Setting up specific dependencies\n"

./liquidsoap/.github/scripts/checkout-deps.sh

echo "### Checking out CI commit\n"

cd liquidsoap
git fetch origin $GITHUB_SHA
git checkout $GITHUB_SHA

cd /tmp/liquidsoap-full

export PKG_CONFIG_PATH=/usr/share/pkgconfig/pkgconfig

echo "### Compiling\n"

./bootstrap
./configure --prefix=/usr --includedir=\${prefix}/include --mandir=\${prefix}/share/man \
            --infodir=\${prefix}/share/info --sysconfdir=/etc --localstatedir=/var \
            --with-camomile-data-dir=/usr/share/liquidsoap/camomile

cd liquidsoap
make
