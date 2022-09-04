#!/bin/bash

set -e

if [ -n "${GITHUB_HEAD_REF}" ]; then
  BRANCH=${GITHUB_HEAD_REF}
else
  BRANCH=${GITHUB_REF#refs/heads/}
fi

echo "Detected branch: ${BRANCH}"

echo "Branch is release branch"
IS_RELEASE=true

echo "Building on all architectures"
BUILD_OS='["debian_testing"]'
BUILD_PLATFORM='["amd64"]'
BUILD_INCLUDE='[{"platform": "amd64", "runs-on": "ubuntu-latest"}]'

echo "Disabling opam build"
echo "##[set-output name=build_opam;]false"

DOCKER_RELEASE=

SHA=`git rev-parse --short HEAD`
IS_ROLLING_RELEASE=

echo "##[set-output name=branch;]${BRANCH}"
echo "##[set-output name=is_release;]${IS_RELEASE}"
echo "##[set-output name=build_os;]${BUILD_OS}"
echo "##[set-output name=build_platform;]${BUILD_PLATFORM}"
echo "##[set-output name=build_include;]${BUILD_INCLUDE}"
echo "##[set-output name=docker_release;]${DOCKER_RELEASE}"
echo "##[set-output name=is_rolling_release;]${IS_ROLLING_RELEASE}"
echo "##[set-output name=sha;]${SHA}"
echo "##[set-output name=s3-artifact-basepath;]s3://liquidsoap-artifacts/${GITHUB_WORKFLOW}/${GITHUB_RUN_NUMBER}"
