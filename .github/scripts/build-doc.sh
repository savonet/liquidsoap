#!/bin/sh

set -e

cd /tmp/liquidsoap-full/liquidsoap
eval $(opam config env)
export OCAMLPATH=`cat ../.ocamlpath`
dune build @doc
