#!/bin/sh

set -e

PWD=$(dirname "$0")
BASE_DIR=$(cd "${PWD}/../.." && pwd)
RELEASE=$GITHUB_SHA

eval "$(opam config env)"

cd "${BASE_DIR}"

mkdir -p "./.github/opam/packages/liquidsoap-core-windows/liquidsoap-core-windows.${RELEASE}"
cp ./.github/opam/liquidsoap-core-windows.opam "./.github/opam/packages/liquidsoap-core-windows/liquidsoap-core-windows.${RELEASE}/opam"
sed -e "s#@COMMIT_SHORT@#$RELEASE#g" -i "./.github/opam/packages/liquidsoap-core-windows/liquidsoap-core-windows.${RELEASE}/opam"

opam remote add liquidsoap-devel ./.github/opam
