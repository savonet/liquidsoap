#!/bin/sh

set -eux

DEB_FILE="$1"
DEB_DEBUG_FILE="$2"
TAG="$3"
ARCHITECTURE="$4"

docker build \
  --pull \
  --no-cache \
  --provenance false \
  --build-arg "DEB_FILE=$DEB_FILE" \
  --build-arg "DEB_DEBUG_FILE=$DEB_DEBUG_FILE" \
  --file .github/docker/debian.dockerfile \
  --tag "savonet/liquidsoap-ci-build:${TAG}_${ARCHITECTURE}" \
  .

if [ "${PUBLISH_DOCKER_IMAGE}" != "true" ]; then
  exit 0
fi

docker tag \
  "savonet/liquidsoap-ci-build:${TAG}_${ARCHITECTURE}" \
  "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_${ARCHITECTURE}"

docker push "savonet/liquidsoap-ci-build:${TAG}_${ARCHITECTURE}"
docker push "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_${ARCHITECTURE}"
