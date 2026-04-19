#!/usr/bin/env bash
set -euo pipefail

declare -A SUBTREES=(
       ["src/modules/metadata"]="https://github.com/savonet/ocaml-metadata main"
       ["src/modules/mem_usage"]="https://github.com/savonet/ocaml-mem_usage main"
)

while read -r _local_ref local_sha _remote_ref remote_sha; do
  if [ "$remote_sha" = "0000000000000000000000000000000000000000" ]; then
    range="$local_sha"
  else
    range="$remote_sha..$local_sha"
  fi

  changed=$(git diff --name-only "$range")

  for prefix in "${!SUBTREES[@]}"; do
    if echo "$changed" | grep -q "^${prefix}/"; then
      read -r remote branch <<< "${SUBTREES[$prefix]}"
      echo "Pushing subtree $prefix to $remote ($branch)..."
      git subtree push --prefix="$prefix" "$remote" "$branch"
    fi
  done
done
