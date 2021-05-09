#!/bin/sh

set -e

DEB_FILE=$1
DEB_DEBUG_FILE=$2
TAG=$3
USER=$4
PASSWORD=$5
ARCHITECTURE=$6

cp $DEB_FILE $DEB_DEBUG_FILE .

docker build --no-cache  --build-arg "DEB_FILE=$DEB_FILE"  --build-arg "DEB_DEBUG_FILE=$DEB_DEBUG_FILE" -f .github/docker/Dockerfile.production -t savonet/liquidsoap-ci-build:${TAG}_${ARCHITECTURE} .

docker login -u "$USER" -p "$PASSWORD" 

docker push savonet/liquidsoap-ci-build:${TAG}_${ARCHITECTURE}
