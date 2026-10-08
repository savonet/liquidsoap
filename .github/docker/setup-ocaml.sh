#!/bin/sh
# Arguments are the ocaml-option-* packages; OCAML_VERSION and OCAML_PATCH_URL come from the environment.

set -e

opam init -y --disable-sandboxing --bare
opam switch create "$OCAML_VERSION" \
  "ocaml-variants.$OCAML_VERSION+options" "$@"
opam update -y

# OCAML_PATCH_URL is a compiler source tree for exactly OCAML_VERSION, so it has no default here.
if [ -n "$OCAML_PATCH_URL" ]; then
  opam pin add -y "ocaml-compiler.$OCAML_VERSION" "$OCAML_PATCH_URL"
fi

opam clean
