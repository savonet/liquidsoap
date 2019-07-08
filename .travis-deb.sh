#!/bin/sh

COMMIT=$1

mkdir debian
id=$(docker create ${IMAGE})
docker cp $id:/tmp/liquidsoap-full/liquidsoap/debian/liquidsoap.deb debian/liquidsoap-${COMMIT}.deb
docker rm -v $id
