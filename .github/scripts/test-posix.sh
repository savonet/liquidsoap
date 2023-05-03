#!/bin/sh

set -e

cd /tmp/liquidsoap-full/liquidsoap
eval "$(opam config env)"
OCAMLPATH="$(cat ../.ocamlpath)"
export OCAMLPATH

export CLICOLOR_FORCE=1

dune build @citest --error-reporting=twice --display=quiet
