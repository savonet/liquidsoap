#!/bin/sh

METRICS_DIR=$1
METRICS_FILE="${METRICS_DIR}/metrics.yaml"

git config --global --add safe.directory '*'
cd /tmp/liquidsoap-full/liquidsoap || exit 1
mkdir -p "${METRICS_DIR}"
{
  echo "- commit: $(git rev-parse HEAD)"
  echo "  branch: $(git rev-parse --abbrev-ref HEAD)"
  echo "  time: $(date +%s)"
} >> "${METRICS_FILE}"
