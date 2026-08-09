#!/bin/sh
set -e

if [ -z "$1" ]; then
  echo "Usage: $0 <output-file>" >&2
  exit 1
fi

# Resolve to absolute path before changing directories
OUTPUT="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"

# The manifest is pinned and locked: an unlocked install resolved several copies of
# @codemirror/state, and bundling them all broke the editor's instanceof checks at
# runtime. npm still needs a directory it owns, so it gets a temporary one.
HERE="$(cd "$(dirname "$0")" && pwd)"

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

cp "$HERE/playground/package.json" "$HERE/playground/package-lock.json" "$TMPDIR"
cd "$TMPDIR"

npm ci --silent

# Exclude Node.js built-ins that prettier conditionally imports but doesn't use in browser
npx esbuild node_modules/liquidsoap-playground/index.js \
  --bundle \
  --format=esm \
  --platform=browser \
  --external:node:util \
  --external:node:path \
  --external:node:fs \
  --external:node:url \
  --minify \
  --outfile=playground.bundle.js

cp playground.bundle.js "$OUTPUT"

echo "Playground bundle created: $OUTPUT"
