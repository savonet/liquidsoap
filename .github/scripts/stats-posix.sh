#!/bin/sh

set -e

cd /tmp/liquidsoap-full/liquidsoap
eval "$(opam config env)"
OCAMLPATH="$(cat ../.ocamlpath)"
export OCAMLPATH

echo -n "Number of core functions: "
dune exec --display=quiet -- src/bin/liquidsoap.exe --no-stdlib --list-functions | wc -l
echo
echo -n "Number of functions: "
./liquidsoap --list-functions | wc -l
