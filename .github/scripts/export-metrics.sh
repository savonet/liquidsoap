#!/bin/sh

BRANCH=$1
METRICS_DIR=$2
TIME="$(date +%s)"
METRICS_FILE="${METRICS_DIR}/${TIME}.yaml"

git config --global --add safe.directory '*'
cd /tmp/liquidsoap-full/liquidsoap || exit 1
mkdir -p "${METRICS_DIR}"
touch /tmp/metrics.yaml
cat /tmp/metrics.yaml > "${METRICS_FILE}"
{
  echo "- commit: $(git rev-parse HEAD)"
  echo "  branch: ${BRANCH}"
  echo "  time: ${TIME}"
} >> "${METRICS_FILE}"
