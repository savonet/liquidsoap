#!/usr/bin/env bash
# Close the mirror pull request for a synced module and delete its branch.
# Usage: drop-mirror.sh <module> <branch> <comment>

set -u

repo="savonet/ocaml-$1"
branch="$2"
comment="$3"

gh api "/repos/$repo/git/ref/heads/$branch" > /dev/null 2>&1 || exit 0

pr=$(gh pr list --repo "$repo" --head "$branch" --state open --json number --jq '.[0].number // empty')
if [ -n "$pr" ]; then
  echo "Closing $repo#$pr ($branch)"
  gh pr close --repo "$repo" "$pr" --comment "$comment" || true
fi

echo "Deleting $repo:$branch"
gh api -X DELETE "/repos/$repo/git/refs/heads/$branch" > /dev/null || true
