#!/bin/sh

set -e

# Remove after next base image rebuild
apt-get update && apt-get install -y libcurl4-gnutls-dev

sudo -i -u opam /bin/sh << EOF
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
EOF
