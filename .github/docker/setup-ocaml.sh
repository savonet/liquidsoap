#!/bin/sh
# Arguments are the ocaml-option-* packages; OCAML_VERSION and OCAML_PATCH_URL come from the environment.

set -e

: "${OCAML_PATCH_URL=https://github.com/toots/ocaml/archive/4552e40b1121fd64a2de0603246ed08a9c8910d8.tar.gz}"

opam init -y --disable-sandboxing --bare
opam switch create "$OCAML_VERSION" \
  "ocaml-variants.$OCAML_VERSION+options" "$@"
opam update -y

# The global-root debugging patches, ocaml/ocaml#15027. They are all #ifdef DEBUG,
# so they only show up in the runtime reached through -runtime-variant d. Build with
# an empty OCAML_PATCH_URL for a stock compiler.
if [ -n "$OCAML_PATCH_URL" ]; then
  opam pin add -y "ocaml-compiler.$OCAML_VERSION" "$OCAML_PATCH_URL"
  opam var --switch "$OCAML_VERSION" ocaml_patch_url="$OCAML_PATCH_URL"
fi

opam clean
