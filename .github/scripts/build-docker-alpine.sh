#!/bin/sh

set -e

APK_FILE="$1"
APK_DBG_FILE="$2"
TAG="$3"
USER="$4"
PASSWORD="$5"
ARCHITECTURE="$6"
DOCKER_PLATFORM="$7"

cp "$APK_FILE" "$APK_DBG_FILE" .

docker login -u "$USER" -p "$PASSWORD"

docker buildx build \
  --pull \
  --platform "${DOCKER_PLATFORM}" \
  --no-cache \
  --build-arg "APK_FILE=$APK_FILE" \
  --build-arg "APK_DBG_FILE=$APK_DBG_FILE" \
  --file .github/docker/Dockerfile.production-alpine \
  --tag "savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}" \
  --push \
  .

docker pull "savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}"

docker tag \
  "savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}" \
  "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}"

docker push "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}"
