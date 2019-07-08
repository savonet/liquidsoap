#!/bin/sh

COMMIT=$1

mkdir debian
id=$(docker create ${IMAGE})
docker cp $id:/liquidsoap.deb debian/liquidsoap-${COMMIT}.deb
docker rm -v $id
