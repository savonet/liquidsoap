#!/bin/sh

cd /tmp/liquidsoap-full/liquidsoap

VERSION=`./src/liquidsoap --version | head -n 1 | cut -d' ' -f 2 | cut -d'+' -f 1`
DATE=`date +%Y%m%d`
COMMIT=`git rev-parse --short HEAD`

DEBFULLNAME="The Savonet Team"
DEBEMAIL="savonet-users@lists.sourceforge.net"

dch --create --distribution unstable --package "liquidsoap" --newversion "${VERSION}~${DATE}+${COMMIT}-1" "Build ${COMMIT}"

eval $(opam config env)

fakeroot debian/rules binary

mkdir -p /tmp/debian/pkgs

cp /tmp/liquidsoap-full/*.deb /tmp/debian/pkgs
