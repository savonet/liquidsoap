#!/bin/sh

set -e

PWD=$(dirname "$0")
BASE_DIR=$(cd "${PWD}/../.." && pwd)
RELEASE=$GITHUB_SHA

eval "$(opam config env)"

opam unpin --all -n

cd "${BASE_DIR}"

mkdir -p "./.github/opam/packages/liquidsoap-windows/liquidsoap-windows.${RELEASE}"
cp ./.github/opam/liquidsoap-windows.opam "./.github/opam/packages/liquidsoap-windows/liquidsoap-windows.${RELEASE}/opam"
sed -e "s#@COMMIT_SHORT@#$RELEASE#g" -i "./.github/opam/packages/liquidsoap-windows/liquidsoap-windows.${RELEASE}/opam"

opam remote add liquidsoap-devel ./.github/opam
