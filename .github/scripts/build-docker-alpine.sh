#!/bin/sh

set -eux

APK_FILE="$1"
TAG="$2"
ARCHITECTURE="$3"

docker build \
  --pull \
  --no-cache \
  --provenance false \
  --build-arg "APK_FILE=$APK_FILE" \
  --file .github/docker/alpine.dockerfile \
  --tag "savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}" \
  .

if [ "${PUBLISH_DOCKER_IMAGE}" != "true" ]; then
  exit 0
fi

docker tag \
  "savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}" \
  "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}"

docker push "savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}"
docker push "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}"
