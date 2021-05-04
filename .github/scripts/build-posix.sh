#!/bin/sh

set -e

# Remove after next base image rebuild
opam install -y ocurl posix-time2

cd /tmp/liquidsoap-full

eval $(opam config env) && git pull && make clean && make update && \
     cd liquidsoap && git reset --hard && git checkout main && git pull && \
     git fetch origin $GITHUB_SHA && git checkout $GITHUB_SHA && \
     ./.github/scripts/checkout-deps.sh && \
     cd .. && ./bootstrap && \
     export PKG_CONFIG_PATH=/usr/share/pkgconfig/pkgconfig && \
     ./configure --prefix=/usr --includedir=\${prefix}/include --mandir=\${prefix}/share/man \
                 --infodir=\${prefix}/share/info --sysconfdir=/etc --localstatedir=/var \
                 --with-camomile-data-dir=/usr/share/liquidsoap/camomile && \
     cd liquidsoap && make && make doc
