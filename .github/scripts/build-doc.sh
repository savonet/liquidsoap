#!/bin/sh

set -e

cd /tmp/liquidsoap-full/liquidsoap
eval $(opam config env)
export OCAMLPATH=$(cat ../.ocamlpath)
opam install -y odoc
dune build @doc
