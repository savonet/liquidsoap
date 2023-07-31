#!/bin/sh

set -e

#TARGET=$1

TARGET=perftest

cd /tmp/liquidsoap-full/liquidsoap
eval "$(opam config env)"
OCAMLPATH="$(cat ../.ocamlpath)"
export OCAMLPATH

export CLICOLOR_FORCE=1

dune build "${TARGET}" --error-reporting=twice --display=quiet
