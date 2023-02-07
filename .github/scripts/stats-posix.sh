#!/bin/sh

set -e

cd /tmp/liquidsoap-full/liquidsoap
eval "$(opam config env)"
OCAMLPATH="$(cat ../.ocamlpath)"
export OCAMLPATH

printf "Number of core functions: "
dune exec --display=quiet -- src/bin/liquidsoap.exe --no-stdlib --list-functions | wc -l
echo
printf "Number of functions: "
./liquidsoap --list-functions | wc -l
