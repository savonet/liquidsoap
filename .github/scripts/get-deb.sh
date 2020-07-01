#!/bin/sh

set -e

DOCKER_TAG=$1
DOCKER_IMAGE=savonet/liquidsoap-github-actions-deb:${DOCKER_TAG}

rm -rf debian
id=$(docker create ${DOCKER_IMAGE})
docker cp $id:/tmp/debian debian/
docker rm -v $id
