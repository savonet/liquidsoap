#!/bin/sh

set -e

eval $(opam config env)

cd /tmp/liquidsoap-full/liquidsoap

FILENAME=`make print-tarball-filename`

if [ ! -f "${FILENAME}" ]; then
  exit 1;
fi

echo "##[set-output name=filename;]/tmp/liquidsoap-full/liquidsoap/${FILENAME}"
echo "##[set-output name=basename;]${FILENAME}"

