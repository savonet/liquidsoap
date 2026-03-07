#!/bin/sh

set -e

COMMIT_SHORT=$(echo "${GITHUB_SHA}" | cut -c-7)$(echo "${GITHUB_SHA}" | cut -d'-' -f 2 -s | while read -r i; do echo "-$i"; done)

echo "TAG: ${TAG}"
echo "OCAML_DOCKER_RELEASE_VERSION: ${OCAML_DOCKER_RELEASE_VERSION}"
echo "amd64 image: ghcr.io/savonet/liquidsoap:ci-build-${TAG}_amd64-${OCAML_DOCKER_RELEASE_VERSION}"
echo "arm64 image: ghcr.io/savonet/liquidsoap:ci-build-${TAG}_arm64-${OCAML_DOCKER_RELEASE_VERSION}"

docker login -u "$USER" -p "$PASSWORD"
docker login ghcr.io -u "$GHCR_USER" -p "$GHCR_PASSWORD"

AMD64="ghcr.io/savonet/liquidsoap:ci-build-${TAG}_amd64-${OCAML_DOCKER_RELEASE_VERSION}"
ARM64="ghcr.io/savonet/liquidsoap:ci-build-${TAG}_arm64-${OCAML_DOCKER_RELEASE_VERSION}"
ALPINE_AMD64="ghcr.io/savonet/liquidsoap:ci-build-${TAG}_alpine_amd64-${OCAML_DOCKER_RELEASE_VERSION}"
ALPINE_ARM64="ghcr.io/savonet/liquidsoap:ci-build-${TAG}_alpine_arm64-${OCAML_DOCKER_RELEASE_VERSION}"

# Push to Docker Hub
docker buildx imagetools create \
  -t "savonet/liquidsoap:${TAG}" \
  -t "savonet/liquidsoap:${COMMIT_SHORT}" \
  "${AMD64}" "${ARM64}"

docker buildx imagetools create \
  -t "savonet/liquidsoap-alpine:${TAG}" \
  -t "savonet/liquidsoap-alpine:${COMMIT_SHORT}" \
  "${ALPINE_AMD64}" "${ALPINE_ARM64}"

# Push to GHCR
docker buildx imagetools create \
  -t "ghcr.io/savonet/liquidsoap:${TAG}" \
  -t "ghcr.io/savonet/liquidsoap:${COMMIT_SHORT}" \
  "${AMD64}" "${ARM64}"

docker buildx imagetools create \
  -t "ghcr.io/savonet/liquidsoap-alpine:${TAG}" \
  -t "ghcr.io/savonet/liquidsoap-alpine:${COMMIT_SHORT}" \
  "${ALPINE_AMD64}" "${ALPINE_ARM64}"
