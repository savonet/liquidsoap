#!/bin/sh

set -e

FILENAME=$1
BASENAME=`basename "${FILENAME}"`
DOCKER_IMAGE=savonet/liquidsoap-github-actions-build:tarball

rm -rf tarball
mkdir tarball
id=$(docker create ${DOCKER_IMAGE})
docker cp "${id}:${FILENAME}" "tarball/${BASENAME}"
docker rm -v $id
