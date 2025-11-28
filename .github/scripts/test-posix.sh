#!/bin/sh

set -e

TARGET=$1

#export OPAMJOBS="$CPU_CORES"

cd /tmp/liquidsoap-full/liquidsoap
eval "$(opam config env)"
OCAMLPATH="$(cat ../.ocamlpath)"
export OCAMLPATH

export CLICOLOR_FORCE=1

dune build -j 4 "${TARGET}" --error-reporting=twice --display=quiet --auto-promote
