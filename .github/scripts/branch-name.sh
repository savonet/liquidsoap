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

echo "##[set-output name=branch;]${BRANCH}"
echo "##[set-output name=is_release;]${IS_RELEASE}"
