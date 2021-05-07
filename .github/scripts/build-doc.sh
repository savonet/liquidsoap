#!/bin/sh

set -e

cd /tmp/liquidsoap-full/liquidsoap
eval $(opam config env)
make doc
