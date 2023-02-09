#!/bin/sh

set -e

TAG=$1
USER=$2
PASSWORD=$3
GHCR_USER=$4
GHCR_PASSWORD=$5
GITHUB_SHA=$6

rm -rf ~/.docker/config.json
mkdir -p ~/.docker
echo "{ \"experimental\": \"enabled\" }" > ~/.docker/config.json

COMMIT_SHORT=$(echo "${GITHUB_SHA}" | cut -c-7)

docker login -u "$USER" -p "$PASSWORD"

# Something is odd with the docker repo
## REMOVE WHEN FIXED ##
docker login ghcr.io -u "$GHCR_USER" -p "$GHCR_PASSWORD"

docker pull "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_amd64"
docker tag "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_amd64" "savonet/liquidsoap-ci-build:${TAG}_amd64"
docker push "savonet/liquidsoap-ci-build:${TAG}_amd64"

docker pull "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_arm64"
docker tag "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_arm64" "savonet/liquidsoap-ci-build:${TAG}_arm64"
docker push "savonet/liquidsoap-ci-build:${TAG}_arm64"

docker pull "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_armhf"
docker tag "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_armhf" "savonet/liquidsoap-ci-build:${TAG}_armhf"
docker push "savonet/liquidsoap-ci-build:${TAG}_armhf"

docker pull "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_amd64"
docker tag "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_amd64" "savonet/liquidsoap-ci-build:${TAG}_alpine_amd64"
docker push "savonet/liquidsoap-ci-build:${TAG}_alpine_amd64"

docker pull "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_arm64"
docker tag "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_arm64" "savonet/liquidsoap-ci-build:${TAG}_alpine_arm64"
docker push "savonet/liquidsoap-ci-build:${TAG}_alpine_arm64"

docker pull "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_armhf"
docker tag "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_armhf" "savonet/liquidsoap-ci-build:${TAG}_alpine_armhf"
docker push "savonet/liquidsoap-ci-build:${TAG}_alpine_armhf"
## REMOVE WHEN FIXED ##

docker manifest create "savonet/liquidsoap:${TAG}" --amend "savonet/liquidsoap-ci-build:${TAG}_amd64" --amend "savonet/liquidsoap-ci-build:${TAG}_arm64" "savonet/liquidsoap-ci-build:${TAG}_armhf"
docker manifest push "savonet/liquidsoap:${TAG}"

docker manifest create "savonet/liquidsoap:${COMMIT_SHORT}" --amend "savonet/liquidsoap-ci-build:${TAG}_amd64" --amend "savonet/liquidsoap-ci-build:${TAG}_arm64" "savonet/liquidsoap-ci-build:${TAG}_armhf"
docker manifest push "savonet/liquidsoap:${COMMIT_SHORT}"

docker manifest create "savonet/liquidsoap-alpine:${TAG}" --amend "savonet/liquidsoap-ci-build:${TAG}_alpine_amd64" --amend "savonet/liquidsoap-ci-build:${TAG}_alpine_arm64" "savonet/liquidsoap-ci-build:${TAG}_alpine_armhf"
docker manifest push "savonet/liquidsoap-alpine:${TAG}"

docker manifest create "savonet/liquidsoap-alpine:${COMMIT_SHORT}" --amend "savonet/liquidsoap-ci-build:${TAG}_alpine_amd64" --amend "savonet/liquidsoap-ci-build:${TAG}_alpine_arm64" "savonet/liquidsoap-ci-build:${TAG}_alpine_armhf"
docker manifest push "savonet/liquidsoap-alpine:${COMMIT_SHORT}"

docker login ghcr.io -u "$GHCR_USER" -p "$GHCR_PASSWORD"

docker manifest create "ghcr.io/savonet/liquidsoap:${TAG}" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_amd64" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_arm64" "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_armhf"
docker manifest push "ghcr.io/savonet/liquidsoap:${TAG}"

docker manifest create "ghcr.io/savonet/liquidsoap:${COMMIT_SHORT}" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_amd64" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_arm64" "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_armhf"
docker manifest push "ghcr.io/savonet/liquidsoap:${COMMIT_SHORT}"

docker manifest create "ghcr.io/savonet/liquidsoap-alpine:${TAG}" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_amd64" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_arm64" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_armhf"
docker manifest push "ghcr.io/savonet/liquidsoap-alpine:${TAG}"

docker manifest create "ghcr.io/savonet/liquidsoap-alpine:${COMMIT_SHORT}" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_amd64" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_arm64" --amend "ghcr.io/savonet/liquidsoap-ci-build:${TAG}_alpine_armhf"
docker manifest push "ghcr.io/savonet/liquidsoap-alpine:${COMMIT_SHORT}"
