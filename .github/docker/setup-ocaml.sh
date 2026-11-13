#!/bin/sh
# Arguments are the ocaml-option-* packages; OCAML_VERSION and OCAML_PATCH_URL come from the environment.

set -e

: "${OCAML_PATCH_URL=https://github.com/toots/ocaml/archive/c95d4f7feccc120ca394523a7ba1b9d844631268.tar.gz}"

opam init -y --disable-sandboxing --bare
opam switch create "$OCAML_VERSION" \
  "ocaml-variants.$OCAML_VERSION+options" "$@"
opam update -y

# dune 3.25 installs in-build packages under _build/install/default/.packages,
# where dune-site does not look, so in-tree runs lose the standard library.
opam pin add -yn dune 3.24.2

# The global-root checks of ocaml/ocaml#15027, active in every runtime variant.
# Build with an empty OCAML_PATCH_URL for a stock compiler.
if [ -n "$OCAML_PATCH_URL" ]; then
  opam pin add -y "ocaml-compiler.$OCAML_VERSION" "$OCAML_PATCH_URL"
  opam var --switch "$OCAML_VERSION" ocaml_patch_url="$OCAML_PATCH_URL"
fi

opam clean
