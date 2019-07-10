#!/bin/sh

set -e

rm -rf debian
id=$(docker create liquidsoap-build)
docker cp $id:/tmp/debian debian/
docker rm -v $id
