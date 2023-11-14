#!/bin/sh

set -e

cd /tmp/liquidsoap-full/liquidsoap
eval "$(opam config env)"
OCAMLPATH="$(cat ../.ocamlpath)"
export OCAMLPATH

git clone https://github.com/smimram/ocaml-pandoc.git
cd ocaml-pandoc
opam pin -y add pandoc-include .
cd ..

opam install -y odoc
dune build @doc
dune build --profile release ./src/js/interactive_js.bc.js
