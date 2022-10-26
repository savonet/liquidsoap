#!/bin/sh

set -e

eval $(opam config env)

cd /tmp/liquidsoap-full/liquidsoap

FILENAME=`make print-tarball-filename`

echo "##[set-output name=filename;]${FILENAME}"
