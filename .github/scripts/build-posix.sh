#!/bin/sh

set -e

eval $(opam config env)

# Remove after next base image rebuild
opam install -y ocurl posix-time2

cd /tmp/liquidsoap-full

git pull
make clean
make update
./liquidsoap/.github/scripts/checkout-deps.sh

export PKG_CONFIG_PATH=/usr/share/pkgconfig/pkgconfig

./bootstrap
./configure
cd liquidsoap
make
make doc
