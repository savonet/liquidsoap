#!/bin/bash

set -e

if [ -n "${GITHUB_HEAD_REF}" ]; then
  BRANCH=${GITHUB_HEAD_REF}
else
  BRANCH=${GITHUB_REF#refs/heads/}
fi

echo "Detected branch: ${BRANCH}"
echo "##[set-output name=branch;]${BRANCH}"

if [[ "${BRANCH}" = "master" ]] || [[ -n "${IS_RELEASE}" ]]; then
  echo "Branch has a docker release"
  echo "##[set-output name=docker_release;]${DOCKER_RELEASE}"
else
  echo "Branch does not have a docker release"
fi

if [[ "${BRANCH}" =~ ^v[0-9] ]]; then
  echo "Branch is release branch"
  echo "##[set-output name=is_release;]${IS_RELEASE}"
else
  echo "Branch is not release branch"
fi
