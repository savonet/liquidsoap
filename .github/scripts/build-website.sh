#!/bin/sh

set -e

PWD=$(dirname "$0")
BASE_DIR=$(cd "${PWD}/../.." && pwd)

DOCKER_IMAGE=savonet/liquidsoap-github-actions-website

docker build --no-cache --tag "${DOCKER_IMAGE}" --file "${BASE_DIR}/.github/docker/website.dockerfile" .

id="$(docker create "${DOCKER_IMAGE}")"
docker cp "$id:/tmp/liquidsoap-full/website/html" html/
docker cp "$id:/tmp/liquidsoap-full/website/content/doc-dev/reference.md" html/reference.md
docker rm -v "$id"
