#!/bin/bash
# Usage: extract-changelog.sh <version>
# Extracts the changelog entry for a given version from CHANGES.
# Exits with error if no entry is found.

set -euo pipefail

VERSION="${1:-}"
if [ -z "$VERSION" ]; then
  echo "Usage: $0 <version>" >&2
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CHANGES="$SCRIPT_DIR/../../CHANGES"

awk -v ver="$VERSION" '
  /^[0-9]+\.[0-9]+\.[0-9]+/ { if (found) exit; if (index($0, ver) == 1) found=1; next }
  found && /^=+/ { next }
  found { lines[++n] = $0 }
  END {
    # trim leading blank lines
    start = 1
    while (start <= n && lines[start] ~ /^[[:space:]]*$/) start++
    # trim trailing blank lines
    end = n
    while (end >= start && lines[end] ~ /^[[:space:]]*$/) end--
    for (i = start; i <= end; i++) print lines[i]
  }
' "$CHANGES"
