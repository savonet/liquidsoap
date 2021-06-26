#!/bin/bash

set -e

if [ -n "${GITHUB_HEAD_REF}" ]; then
  BRANCH=${GITHUB_HEAD_REF}
else
  BRANCH=${GITHUB_REF#refs/heads/}
fi

echo "Detected branch: ${BRANCH}"

if [[ "${BRANCH}" =~ ^v[0-9] ]]; then
  echo "Branch is release branch"
  IS_RELEASE=true
else
  echo "Branch is not release branch"
  IS_RELEASE=
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
echo "##[set-output name=docker_release;]${DOCKER_RELEASE}"
echo "##[set-output name=sha;]${SHA}"

git fetch origin main
CHANGED_SRC_FILES=`git diff --name-only origin/main..HEAD | grep '^src/' | xargs`

if [[ "${IS_RELEASE}" = "true" ]] || [[ "${BRANCH}" = "main" ]] || [[ -n "${CHANGED_SRC_FILES}" ]]; then
  echo "Code should be built during this run"
  SHOULD_BUILD_CODE=true
else
  echo "No need to build the code during this run"
  SHOULD_BUILD_CODE=
fi

echo "##[set-output name=should_build_code;]${SHOULD_BUILD_CODE}"
