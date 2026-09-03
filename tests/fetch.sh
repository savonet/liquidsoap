#!/bin/sh

# Fetch a test fixture into a cache shared by every build of this checkout, so a
# wiped _build does not mean re-downloading it. Each entry is gated by a .url
# stamp file, which also refetches when a rule points at a new URL.

set -e

KIND=$1
URL=$2
TARGET=$3
CHECK=$4

CACHE=${XDG_CACHE_HOME:-$HOME/.cache}/liquidsoap-test-fixtures
ENTRY="$CACHE/$(basename "$TARGET")"
STAMP="$ENTRY.url"

fetch() {
  case "$KIND" in
    curl) curl -fL --retry 3 --retry-all-errors -o "$1" "$URL" ;;
    rsync) rsync -avL "$URL" "$1" ;;
    git) git clone --depth 1 "$URL" "$1" ;;
    *)
      echo "$0: unknown fetch kind: $KIND" >&2
      exit 1
      ;;
  esac
  [ -z "$CHECK" ] || $CHECK "$1"
}

# The stamp is written last so a partial or corrupt download is never gated in.
if [ "$(cat "$STAMP" 2> /dev/null)" != "$URL" ]; then
  mkdir -p "$CACHE"
  rm -rf "$ENTRY" "$STAMP"
  trap 'rm -rf "$ENTRY.tmp"' EXIT
  fetch "$ENTRY.tmp"
  mv "$ENTRY.tmp" "$ENTRY"
  printf '%s\n' "$URL" > "$STAMP"
fi

cp -a "$ENTRY" "$TARGET"
