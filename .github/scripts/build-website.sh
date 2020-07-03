#!/bin/sh

set -e

DOCKER_IMAGE=savonet/liquidsoap-github-actions-website

docker build --no-cache -t "${DOCKER_IMAGE}" -f docker/Dockerfile.website .

id=$(docker create ${DOCKER_IMAGE})
docker cp $id:/tmp/liquidsoap-fullwebsite/html html/
docker rm -v $id

