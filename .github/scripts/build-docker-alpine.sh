#!/bin/sh

set -e

APK_FILE="$1"
TAG="$2"
USER="$3"
PASSWORD="$4"
ARCHITECTURE="$5"

cp "$APK_FILE" .

if [ "${PUBLISH_DOCKER_IMAGE}" = "true" ]; then
  PUSH_OPTION=--push
fi

# shellcheck disable=SC2086
docker build \
  --pull \
  --no-cache \
  --provenance false \
  --build-arg "APK_FILE=$APK_FILE" \
  --file .github/docker/alpine.dockerfile \
  --tag "savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}" \
  ${PUSH_OPTION} \
  .

if [ "${PUBLISH_DOCKER_IMAGE}" != "true" ]; then
  exit 0
fi

docker login -u "$USER" -p "$PASSWORD"

docker pull "savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}"

docker tag \
  "savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}" \
  "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}"

docker push "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}"
