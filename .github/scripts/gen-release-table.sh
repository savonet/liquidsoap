#!/bin/bash

set -e

MATRIX=".github/release-matrix.json"
README="README.md"
START="<!-- release-table:start -->"
END="<!-- release-table:end -->"
REPO="https://github.com/savonet/liquidsoap"

# The table is spliced in unformatted, so prettier has to run here: leaving that
# to the prettier hook would make the two hooks rewrite each other forever.
if ! command -v prettier > /dev/null; then
  echo "gen-release-table: prettier is required" >&2
  exit 1
fi

for marker in "${START}" "${END}"; do
  if ! grep -qF "${marker}" "${README}"; then
    echo "gen-release-table: ${README} is missing ${marker}" >&2
    exit 1
  fi
done

TABLE=$(
  echo "| Branch | Latest release | Supported | Rolling Release |"
  echo "| --- | --- | --- | --- |"
  jq -r --arg repo "${REPO}" '
    def status: if . == true then "✅" elif . == false then "❌" else "🚧" end;

    .[] | [
      "| `\(.version)` | ",
      (if .latest_release == null then "🚧"
       else "[\(.latest_release)](\($repo)/releases/tag/v\(.latest_release)) (docker: `savonet/liquidsoap:v\(.latest_release)`)"
       end),
      " | \(.supported | status) | ",
      (if .branch == null then "❌"
       else "[\(.version)](\($repo)/releases/tag/rolling-release-v\(.version)) (docker: `savonet/liquidsoap:rolling-release-v\(.version)`)"
       end),
      " |"
    ] | join("")
  ' "${MATRIX}"
)

SPLICED=$(
  awk -v start="${START}" -v end="${END}" -v table="${TABLE}" '
    $0 == start { print; print table; skip = 1; next }
    $0 == end { skip = 0 }
    !skip { print }
  ' "${README}"
)

FORMATTED=$(printf '%s\n' "${SPLICED}" | prettier --stdin-filepath "${README}")

if [ "${FORMATTED}" != "$(cat "${README}")" ]; then
  printf '%s\n' "${FORMATTED}" > "${README}"
  echo "gen-release-table: updated ${README}"
fi
