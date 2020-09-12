#!/bin/sh

set -e

DEB_FILE=$1
TAG=$2
USER=$3
PASSWORD=$4
ARCHITECTURE=$5

cp $DEB_FILE .
docker build --no-cache -f .github/docker/Dockerfile.production -t savonet/liquidsoap:${TAG}_${ARCHITECTURE} .

docker login -u "$USER" -p "$PASSWORD" 

docker push savonet/liquidsoap:${TAG}_${ARCHITECTURE}
