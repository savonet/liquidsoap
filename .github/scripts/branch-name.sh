#!/bin/bash

set -e

if [ -n "${GITHUB_HEAD_REF}" ]; then
  BRANCH=${GITHUB_HEAD_REF}
else
  BRANCH=${GITHUB_REF#refs/heads/}
fi

echo "Detected branch: ${BRANCH}"
echo "::set-output name=branch::${BRANCH}"

if [[ "${BRANCH}" =~ ^v[0-9] ]]; then
  echo "Branch is release branch"
  echo "::set-output name=is_release::true"
  IS_RELEASE=true
else
  echo "Branch is not release branch"
fi

if [[ "${BRANCH}" = "master" ]] || [[ -n "${IS_RELEASE}" ]]; then
  echo "Branch has a docker release"
  echo "::debug::Should set a docker_release here"
  echo "::set-output name=docker_release::bla"
else
  echo "Branch does not have a docker release"
fi
