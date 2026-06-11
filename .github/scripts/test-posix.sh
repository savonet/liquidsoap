#!/bin/sh

set -e

TARGET=$1

#export OPAMJOBS="$CPU_CORES"

cd /tmp/liquidsoap
eval "$(opam config env)"

export CLICOLOR_FORCE=1

dune build -j 4 --profile=release "${TARGET}" --error-reporting=twice --display=quiet --auto-promote
