#!/usr/bin/env bash
set -euo pipefail

currentBranch=$(git rev-parse --abbrev-ref HEAD)

if [ "$currentBranch" = "main" ]; then
  git diff-tree --no-commit-id --name-only -r "HEAD..origin/main" | cut -d'/' -f 1-4 | sort -u | grep src/modules/subtree | while read -r dir; do
      echo "Changes detected in subtree $dir. Syncing..."
      git subtree push --prefix="$dir" subtree-remote main
  done
fi
