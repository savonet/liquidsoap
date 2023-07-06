#!/bin/sh

set -e

cd /tmp/liquidsoap-full/liquidsoap
eval "$(opam config env)"
OCAMLPATH="$(cat ../.ocamlpath)"
export OCAMLPATH

printf "Memory usage before loading all libraries: "
dune exec --display=quiet -- src/bin/liquidsoap.exe --no-stdlib --check 'runtime.gc.full_major() print(runtime.mem_usage.prettify_bytes(runtime.mem_usage().process_physical_memory))'

printf "Memory usage after loading all libraries: "
dune exec --display=quiet -- src/bin/liquidsoap.exe --check 'runtime.gc.full_major() print(runtime.memory().pretty.process_physical_memory)'

printf "Memory usage after starting the application: "
dune exec --display=quiet -- src/bin/liquidsoap.exe '
  settings.log.stdout.set(false)
  thread.run(
    delay=3.,
    (fun () -> begin
      runtime.gc.full_major()
      print(runtime.memory().pretty.process_physical_memory)
      exit(0)
    end))
  output.dummy(blank())'

printf "Number of core functions: "
dune exec --display=quiet -- src/bin/liquidsoap.exe --no-stdlib --list-functions | wc -l
echo
printf "Number of functions: "
./liquidsoap --list-functions | wc -l
