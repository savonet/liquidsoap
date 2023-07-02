#!/bin/sh

set -e

PWD=$(dirname "$0")
BASE_DIR=$(cd "${PWD}/../.." && pwd)
RELEASE=$GITHUB_SHA

git config --global user.email "toots@rastageeks.org" && git config --global user.name "Romain Beauxis"

eval "$(opam config env)"

cd /home/opam/opam-cross-windows/
git pull

for i in $(find "${BASE_DIR}/.github/opam" | grep '\.opam$'); do
  PACKAGE=$(basename "$i" | sed -e 's#\.opam$##')
  VERSION=$(grep '^version' "$i" | cut -d'"' -f 2)
  mkdir -p "/home/opam/opam-cross-windows/packages/$PACKAGE/$PACKAGE.$VERSION"
  cp "$i" "/home/opam/opam-cross-windows/packages/$PACKAGE/$PACKAGE.$VERSION/opam"
  sed -e "s#@COMMIT_SHORT@#$RELEASE#g" -i "/home/opam/opam-cross-windows/packages/$PACKAGE/$PACKAGE.$VERSION/opam"
done

git add . && git commit -m "Add custom opam files"

opam update windows

opam list --short --recursive --external --vars os-distribution=mxe,os-family=mingw --required-by="$OPAM_DEPS" > /home/opam/mxe-deps
