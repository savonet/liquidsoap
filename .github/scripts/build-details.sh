#!/bin/bash

set -e

if [ -n "${GITHUB_HEAD_REF}" ]; then
  BRANCH=${GITHUB_HEAD_REF}
else
  BRANCH=${GITHUB_REF#refs/heads/}
fi

echo "Detected branch: ${BRANCH}"

if [[ "${BRANCH}" =~ ^v[0-9] ]] && ! [[ "${BRANCH}" =~ preview$ ]]; then
  echo "Branch is release branch"
  IS_RELEASE=true
else
  echo "Branch is not release branch"
  IS_RELEASE=
fi

if [[ "${BRANCH}" =~ ^v[0-9] ]]; then
  echo "Branch is versioned: building on all architectures"
  BUILD_OS='["debian_bookworm", "debian_buster", "debian_bullseye", "ubuntu_impish", "ubuntu_focal", "alpine"]'
  BUILD_PLATFORM='["amd64", "arm64", "armhf"]'
  BUILD_INCLUDE='[{"platform": "amd64", "runs-on": "ubuntu-latest", "alpine-arch": "x86_64", "docker-platform": "linux/amd64"}, {"platform": "arm64", "runs-on": "self-hosted", "alpine-arch": "aarch64", "docker-platform": "linux/arm64"}, {"platform": "armhf", "runs-on": "self-hosted", "alpine-arch": "armv7", "docker-platform": "linux/arm/v7"}]'

  echo "Enabling opam build"
  echo "##[set-output name=build_opam;]true"
else
  echo "Branch is not versioned: building on amd64 only"
  BUILD_OS='["debian_bookworm", "debian_buster", "debian_bullseye", "ubuntu_impish", "ubuntu_focal", "alpine"]'
  BUILD_PLATFORM='["amd64"]'
  BUILD_INCLUDE='[{"platform": "amd64", "runs-on": "ubuntu-latest", "alpine-arch": "x86_64", "docker-platform": "linux/amd64"}]'

  echo "Not enabling opam build"
  echo "##[set-output name=build_opam;]false"
fi

if [[ "${BRANCH}" = "main" ]] || [[ -n "${IS_RELEASE}" ]]; then
  echo "Branch has a docker release"
  DOCKER_RELEASE=true
else
  echo "Branch does not have a docker release"
  DOCKER_RELEASE=
fi

SHA=`git rev-parse --short HEAD`

echo "##[set-output name=branch;]${BRANCH}"
echo "##[set-output name=is_release;]${IS_RELEASE}"
echo "##[set-output name=build_os;]${BUILD_OS}"
echo "##[set-output name=build_platform;]${BUILD_PLATFORM}"
echo "##[set-output name=build_include;]${BUILD_INCLUDE}"
echo "##[set-output name=docker_release;]${DOCKER_RELEASE}"
echo "##[set-output name=sha;]${SHA}"

git fetch origin main > /dev/null 2>&1
CHANGED_SRC_FILES=`git diff --name-only origin/main..HEAD | grep '^src/' | xargs`
CHANGED_LIBS_FILES=`git diff --name-only origin/main..HEAD | grep '^libs/' | xargs`

if [[ "${IS_RELEASE}" = "true" ]] || [[ "${BRANCH}" = "main" ]] || [[ -n "${CHANGED_SRC_FILES}" ]] || [[ -n "${CHANGED_LIBS_FILES}" ]]; then
  echo "Code should be built during this run"
  SHOULD_BUILD_CODE=true
else
  echo "No need to build the code during this run"
  SHOULD_BUILD_CODE=
fi

echo "##[set-output name=should_build_code;]${SHOULD_BUILD_CODE}"
echo "##[set-output name=s3-artifact-basepath;]s3://liquidsoap-artifacts/${GITHUB_WORKFLOW}/${GITHUB_RUN_NUMBER}"
