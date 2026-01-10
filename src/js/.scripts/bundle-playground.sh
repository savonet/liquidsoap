#!/bin/sh
set -e

if [ -z "$1" ]; then
  echo "Usage: $0 <output-file>" >&2
  exit 1
fi

# Resolve to absolute path before changing directories
OUTPUT="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

cd "$TMPDIR"

npm init -y > /dev/null 2>&1
npm install --silent \
  liquidsoap-playground@1.1.1 \
  esbuild@0.24.2 \
  @babel/runtime@7.26.0

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
