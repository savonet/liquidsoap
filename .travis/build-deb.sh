#!/bin/sh

set -e

cd /tmp/liquidsoap-full/liquidsoap

COMMIT=$1

DEBFULLNAME="The Savonet Team"
DEBEMAIL="savonet-users@lists.sourceforge.net"

dch --create --distribution unstable --package "liquidsoap" --newversion "0+${COMMIT}-1" "Build ${COMMIT}"

eval $(opam config env)

fakeroot debian/rules binary

mkdir -p /tmp/debian/pkgs

cp /tmp/liquidsoap-full/*.deb /tmp/debian/pkgs
