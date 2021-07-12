#!/bin/sh

set -e

CPU_CORES=$1

cd /tmp/liquidsoap-full/liquidsoap
eval $(opam config env)
make doc -j $CPU_CORES
