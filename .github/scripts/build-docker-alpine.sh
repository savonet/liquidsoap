#!/bin/sh

set -e

APK_FILE="$1"
TAG="$2"
USER="$3"
PASSWORD="$4"
ARCHITECTURE="$5"

cp "$APK_FILE" .

docker login -u "$USER" -p "$PASSWORD"

docker build \
  --pull \
  --no-cache \
  --provenance false \
  --build-arg "APK_FILE=$APK_FILE" \
  --file .github/docker/alpine.dockerfile \
  --tag "savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}" \
  --push \
  .

docker pull "savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}"

docker tag \
  "savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}" \
  "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}"

docker push "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_${ARCHITECTURE}"
