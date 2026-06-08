#!/bin/sh

set -e

cd /tmp/liquidsoap
eval "$(opam config env)"

dune build @doc
dune build --profile release ./src/js/interactive_js.bc.js
