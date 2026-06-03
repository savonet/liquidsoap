#!/bin/sh -e

# shellcheck disable=SC2086,SC2068

SYSTEM=$1
shift

ML=$1
shift

OUTPUT=$1
shift

TOOLCHAIN=""

if test "${SYSTEM}" = "mingw" -o "${SYSTEM}" = "mingw64"; then
  TOOLCHAIN="-toolchain windows"
fi

ocamlfind ${TOOLCHAIN} ocamlopt \
  -linkpkg \
  -thread \
  -package ctypes.stubs \
  -package posix-socket \
  $@ ${ML} -o ${OUTPUT}
