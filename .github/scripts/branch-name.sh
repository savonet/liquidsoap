#!/bin/bash

set -e

if [ "${GITHUB_HEAD_REF}" ]; then
  BRANCH=${GITHUB_HEAD_REF#refs/heads/}
else
  BRANCH=${GITHUB_REF#refs/heads/}
fi

if [[ "${BRANCH}" =~ ^v[0-9] ]]; then
  IS_RELEASE=true
else
  IS_RELEASE=
fi

echo "##[set-output name=branch;]${BRANCH}"
echo "##[set-output name=is_release;]${IS_RELEASE}"
