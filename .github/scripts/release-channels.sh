#!/bin/bash

# Prints one row per published channel, as tag, kind, branch, description.
#
# The one place that knows what a release tag is called. build-details.sh tags a
# rolling build with it and build-repo.sh looks the release up by it; if the two
# ever spelled it differently the repository would quietly stop offering the
# channel, having found no release under the name it asked for.

set -euo pipefail

MATRIX="${1:-$(dirname "$0")/../release-matrix.json}"

jq -r '
  def published: [.[] | select(.supported != false)];
  (published | .[] | select(.latest_release != null)
    | ["v\(.latest_release)", "release", (.branch // "-"),
       "Liquidsoap \(.latest_release)"]),
  (published | .[] | select(.branch != null)
    | ["rolling-release-v\(.version)", "rolling", .branch,
       "Liquidsoap \(.version) rolling release, rebuilt on every commit"])
  | @tsv
' "${MATRIX}"
