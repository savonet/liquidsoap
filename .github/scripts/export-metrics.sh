#!/bin/sh

METRICS_DIR=$1
TIME="$(date +%s)"
METRICS_FILE="${METRICS_DIR}/${TIME}.yaml"

git config --global --add safe.directory '*'
cd /tmp/liquidsoap-full/liquidsoap || exit 1
mkdir -p "${METRICS_DIR}"
touch /tmp/metrics.yaml
cat /tmp/metrics.yaml > "${METRICS_FILE}"
{
  echo "- commit: $(git rev-parse HEAD)"
  echo "  branch: $(git rev-parse --abbrev-ref HEAD)"
  echo "  time: ${TIME}"
} >> "${METRICS_FILE}"
