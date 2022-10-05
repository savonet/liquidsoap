#!/bin/sh

set -e

INDEX=$1

cd /tmp/liquidsoap-full/liquidsoap
eval "$(opam config env)"
OCAMLPATH="$(cat ../.ocamlpath)"
export OCAMLPATH

dune build "@runtest_${INDEX}"
