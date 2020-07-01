#!/bin/sh

set -e

SYSTEM=$1
RELEASE=$GITHUB_SHA
OCAML_VERSION=4.08.0

if test -z "${GITHUB_HEAD_REF}"; then
  BRANCH=master
else
  BRANCH=`basename "${GITHUB_HEAD_REF}"`
fi

DEPS_IMAGE="savonet/liquidsoap-win32-deps-${SYSTEM}"
BASE_IMAGE="savonet/liquidsoap-win32-base-${SYSTEM}"
IMAGE="savonet/liquidsoap-win32-${SYSTEM}"
COMMIT_SHORT=`echo "${GITHUB_SHA}" | cut -c-7`

if [ "${SYSTEM}" = "x64" ]; then
  HOST="x86_64-w64-mingw32.static"
  BUILD="${COMMIT_SHORT}-win64~${GITHUB_RUN_NUMBER}"
else
  HOST="i686-w64-mingw32.static"
  BUILD="${COMMIT_SHORT}-win32~${GITHUB_RUN_NUMBER}"
fi

docker build -f .github/docker/Dockerfile.win32-deps -t ${BASE_IMAGE} \
  --build-arg IMAGE=${DEPS_IMAGE} .
docker build -f .github/docker/Dockerfile.win32 -t ${IMAGE} --no-cache --build-arg RELEASE=${RELEASE} \
  --build-arg IMAGE=${BASE_IMAGE} --build-arg HOST=${HOST} --build-arg BUILD=${BUILD} .
id=$(docker create ${IMAGE})
docker cp ${id}:/tmp/win32 .
docker rm -v ${id}
echo "##[set-output name=path;]liquidsoap-${BUILD}"

