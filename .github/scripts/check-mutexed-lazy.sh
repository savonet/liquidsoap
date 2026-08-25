#!/bin/sh
# Stdlib Lazy is unsafe to force from several threads or domains, which
# liquidsoap does. Lazy.Mutexed is the safe equivalent.

set -e

[ $# -eq 0 ] && exit 0

if matches=$(grep -nE '\bLazy\.(force|t|from_val|from_fun|is_val)\b' "$@"); then
  echo "Use Lazy.Mutexed rather than Lazy:"
  echo "${matches}"
  exit 1
fi

exit 0
