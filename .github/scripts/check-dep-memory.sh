#!/bin/sh

set -e

TMPFILE=$1
DEP=$2
EXTERNAL=$3

export HOME=/home/opam

eval "$(opam config env)"

cd /tmp/liquidsoap-full
OCAMLPATH="$(cat .ocamlpath)"
export OCAMLPATH

if test -n "${DEP}"; then
  if test -n "${EXTERNAL}"; then
    opam install -y "${DEP}"
  else
    cd "/tmp/liquidsoap-full/ocaml-${DEP}"
    opam install -y .
  fi
fi

cd /tmp/liquidsoap-full/liquidsoap

./liquidsoap --build-config

./liquidsoap ./.github/scripts/test_resources.liq

./liquidsoap ./.github/scripts/test_resources.liq > "${TMPFILE}"
