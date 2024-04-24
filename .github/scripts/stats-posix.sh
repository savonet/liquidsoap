#!/bin/sh

set -e

cd /tmp/liquidsoap-full/liquidsoap
eval "$(opam config env)"
OCAMLPATH="$(cat ../.ocamlpath)"
export OCAMLPATH

printf "Memory usage before loading all libraries: "
dune exec --display=quiet -- src/bin/liquidsoap.exe --no-stdlib --check 'runtime.gc.full_major() print(runtime.memory.prettify_bytes(runtime.memory().process_private_memory))'

printf "Memory usage after loading all libraries: "
dune exec --display=quiet -- src/bin/liquidsoap.exe --check 'runtime.gc.full_major() print(runtime.memory().pretty.process_private_memory)'

printf "Number of core functions: "
dune exec --display=quiet -- src/bin/liquidsoap.exe --no-stdlib --list-functions | wc -l
echo
printf "Number of functions: "
./liquidsoap --list-functions | wc -l
