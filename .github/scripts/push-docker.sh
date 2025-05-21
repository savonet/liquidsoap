#!/bin/sh

set -e

rm -rf ~/.docker/config.json
mkdir -p ~/.docker
echo "{ \"experimental\": \"enabled\" }" > ~/.docker/config.json

COMMIT_SHORT=$(echo "${GITHUB_SHA}" | cut -c-7)$(echo "${GITHUB_SHA}" | cut -d'-' -f 2 -s | while read -r i; do echo "-$i"; done)

echo "TAG: ${TAG}"
echo "OCAML_DOCKER_RELEASE_VERSION: ${OCAML_DOCKER_RELEASE_VERSION}"
echo "amd64 image: savonet/liquidsoap-ci-build:${TAG}_amd64-${OCAML_DOCKER_RELEASE_VERSION}"
echo "arm64 image: savonet/liquidsoap-ci-build:${TAG}_arm64-${OCAML_DOCKER_RELEASE_VERSION}"

docker login -u "$USER" -p "$PASSWORD"

docker manifest create "savonet/liquidsoap:${TAG}" --amend "savonet/liquidsoap-ci-build:${TAG}_amd64-${OCAML_DOCKER_RELEASE_VERSION}" --amend "savonet/liquidsoap-ci-build:${TAG}_arm64-${OCAML_DOCKER_RELEASE_VERSION}"
docker manifest push "savonet/liquidsoap:${TAG}"

docker manifest create "savonet/liquidsoap:${COMMIT_SHORT}" --amend "savonet/liquidsoap-ci-build:${TAG}_amd64-${OCAML_DOCKER_RELEASE_VERSION}" --amend "savonet/liquidsoap-ci-build:${TAG}_arm64-${OCAML_DOCKER_RELEASE_VERSION}"
docker manifest push "savonet/liquidsoap:${COMMIT_SHORT}"

docker manifest create "savonet/liquidsoap-alpine:${TAG}" --amend "savonet/liquidsoap-ci-build:${TAG}_alpine_amd64-${OCAML_DOCKER_RELEASE_VERSION}" --amend "savonet/liquidsoap-ci-build:${TAG}_alpine_arm64-${OCAML_DOCKER_RELEASE_VERSION}"
docker manifest push "savonet/liquidsoap-alpine:${TAG}"

docker manifest create "savonet/liquidsoap-alpine:${COMMIT_SHORT}" --amend "savonet/liquidsoap-ci-build:${TAG}_alpine_amd64-${OCAML_DOCKER_RELEASE_VERSION}" --amend "savonet/liquidsoap-ci-build:${TAG}_alpine_arm64-${OCAML_DOCKER_RELEASE_VERSION}"
docker manifest push "savonet/liquidsoap-alpine:${COMMIT_SHORT}"

docker login ghcr.io -u "$GHCR_USER" -p "$GHCR_PASSWORD"

docker manifest create "ghcr.io/savonet/liquidsoap:${TAG}" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_amd64-${OCAML_DOCKER_RELEASE_VERSION}" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_arm64-${OCAML_DOCKER_RELEASE_VERSION}"
docker manifest push "ghcr.io/savonet/liquidsoap:${TAG}"

docker manifest create "ghcr.io/savonet/liquidsoap:${COMMIT_SHORT}" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_amd64-${OCAML_DOCKER_RELEASE_VERSION}" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_arm64-${OCAML_DOCKER_RELEASE_VERSION}"
docker manifest push "ghcr.io/savonet/liquidsoap:${COMMIT_SHORT}"

docker manifest create "ghcr.io/savonet/liquidsoap-alpine:${TAG}" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_amd64-${OCAML_DOCKER_RELEASE_VERSION}" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_arm64-${OCAML_DOCKER_RELEASE_VERSION}"
docker manifest push "ghcr.io/savonet/liquidsoap-alpine:${TAG}"

docker manifest create "ghcr.io/savonet/liquidsoap-alpine:${COMMIT_SHORT}" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_amd64-${OCAML_DOCKER_RELEASE_VERSION}" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_arm64-${OCAML_DOCKER_RELEASE_VERSION}"
docker manifest push "ghcr.io/savonet/liquidsoap-alpine:${COMMIT_SHORT}"
