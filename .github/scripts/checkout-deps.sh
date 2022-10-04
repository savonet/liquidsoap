#!/bin/sh

CWD="$(dirname "$0")"
BASEDIR="$(cd "$CWD/../../.." && pwd)"

for i in $(git log --reverse --pretty=format:%B origin/main..HEAD | grep '^DEPS=' | cut -d'=' -f 2  | tr ":" "\n"); do
  MODULE=$(echo "$i" | cut -d'#' -f 1)
  COMMIT=$(echo "$i" | cut -d'#' -f 2)
  echo "Checking out dep $MODULE on commit $COMMIT"

  cd "${BASEDIR}/${MODULE}" &&
    git fetch origin "$COMMIT" &&
    git reset --hard &&
    git checkout "$COMMIT" &&
    git submodule init &&
    git submodule update

done
