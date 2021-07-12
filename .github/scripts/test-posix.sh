#!/bin/sh

set -e

CPU_CORES=$1

cd /tmp/liquidsoap-full/liquidsoap
eval $(opam config env)
make test -j $CPU_CORES
