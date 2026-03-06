#!/bin/bash
# Git merge driver for dune.inc files.
# Resolves conflicts by regenerating the file via `dune build @gendune --auto-promote`.
# Usage: configured via .gitattributes + git merge driver config.
# Args: %O=ancestor %A=ours(output) %B=theirs %L=marker-size %P=file-path
set -e

OURS="$2"
FILE_PATH="$5"

REPO_ROOT="$(git rev-parse --show-toplevel)"
ACTUAL_FILE="$REPO_ROOT/$FILE_PATH"

# Start from our version so dune sees a valid baseline
cp "$OURS" "$ACTUAL_FILE"

cd "$REPO_ROOT"
dune build @gendune --auto-promote

# Write the regenerated file back as the merge result
cp "$ACTUAL_FILE" "$OURS"
