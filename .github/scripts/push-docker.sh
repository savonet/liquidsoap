#!/bin/sh

set -e

TAG=$1
USER=$2
PASSWORD=$3

rm -rf ~/.docker/config.json
mkdir -p ~/.docker
echo "{ \"experimental\": \"enabled\" }" > ~/.docker/config.json

docker login -u "$USER" -p "$PASSWORD"

docker manifest create savonet/liquidsoap:$TAG --amend savonet/liquidsoap-ci-build:${TAG}_amd64 --amend savonet/liquidsoap-ci-build:${TAG}_arm64
docker manifest push savonet/liquidsoap:$TAG

