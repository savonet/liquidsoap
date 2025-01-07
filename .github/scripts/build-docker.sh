#!/bin/sh

set -e

DEB_FILE="$1"
DEB_DEBUG_FILE="$2"
TAG="$3"
USER="$4"
PASSWORD="$5"
ARCHITECTURE="$6"

cp "$DEB_FILE" "$DEB_DEBUG_FILE" .

DOCKERFILE=.github/docker/debian.dockerfile

if [ "${PUBLISH_DOCKER_IMAGE}" = "true" ]; then
  PUSH_OPTION=--push
fi

# shellcheck disable=SC2086
docker build \
  --pull \
  --no-cache \
  --provenance false \
  --build-arg "DEB_FILE=$DEB_FILE" \
  --build-arg "DEB_DEBUG_FILE=$DEB_DEBUG_FILE" \
  --file "${DOCKERFILE}" \
  --tag "savonet/liquidsoap-ci-build:${TAG}_${ARCHITECTURE}" \
  ${PUSH_OPTION} \
  .

if [ "${PUBLISH_DOCKER_IMAGE}" != "true" ]; then
  exit 0
fi

docker login -u "$USER" -p "$PASSWORD"

docker pull "savonet/liquidsoap-ci-build:${TAG}_${ARCHITECTURE}"

docker tag \
  "savonet/liquidsoap-ci-build:${TAG}_${ARCHITECTURE}" \
  "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_${ARCHITECTURE}"

docker push "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_${ARCHITECTURE}"
