#!/usr/bin/env bash
# Scale test for output.harbor.
#
# Usage:
#   ./tests/harbor/scale-test.sh ENCODER MIME PORT [dedicated]
#
# Arguments:
#   ENCODER   Liquidsoap encoder expression, e.g. '%mp3' or '%vorbis'
#   MIME      Expected MIME type (informational only)
#   PORT      TCP port to listen on
#   dedicated If set to 'dedicated', run with dedicated_encoder=true
#
# Environment:
#   LIQUIDSOAP  Path to the liquidsoap binary (default: ./liquidsoap)

set -euo pipefail

ENCODER="${1:?ENCODER required}"
MIME="${2:?MIME required}"
PORT="${3:?PORT required}"
DEDICATED="${4:-shared}"
LIQUIDSOAP="${LIQUIDSOAP:-./liquidsoap}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

if [ "$DEDICATED" = "dedicated" ]; then
    LS_EXPR="output.harbor($ENCODER, mount=\"test\", port=$PORT, dedicated_encoder=true, sine())"
else
    LS_EXPR="output.harbor($ENCODER, mount=\"test\", port=$PORT, sine())"
fi

LOG_FILE="/tmp/liquidsoap-scale-${PORT}.log"
T0=$(date +%s%3N)
ts() { ms=$(( $(date +%s%3N) - T0 )); printf "[+%d.%ds] " "$((ms / 1000))" "$((ms % 1000 / 100))"; }

cleanup() {
    kill "$SERVER_PID" 2> /dev/null || true
}
trap cleanup EXIT

echo "$(ts)Starting liquidsoap ($DEDICATED encoder, port $PORT)..."
"$LIQUIDSOAP" "$LS_EXPR" 'log.level.set(4)' > "$LOG_FILE" 2>&1 &
SERVER_PID=$!

echo "$(ts)Waiting for port $PORT to accept connections..."
for _ in $(seq 1 30); do
    nc -z localhost "$PORT" 2> /dev/null && break
    sleep 1
done

# Wait until the encoder is ready (HTTP 200) without consuming stream data.
# Clients always start right away once connected — even with an empty burst
# buffer they receive audio from the next encoded frame — so we just need the
# encoder to be initialised before launching the load test.
echo "$(ts)Waiting for encoder to be ready on port $PORT..."
for _ in $(seq 1 60); do
    status=$(curl -s -o /dev/null -w "%{http_code}" --max-time 1 \
        "http://localhost:${PORT}/test" 2> /dev/null || true)
    [ "$status" = "200" ] && break
    sleep 0.1
done

echo "$(ts)Running concurrent ffprobe load test ($MIME)..."
if ! PORT="$PORT" node "$SCRIPT_DIR/scale-test.js"; then
    echo "--- liquidsoap log ---"
    cat "$LOG_FILE"
    exit 1
fi

if ! kill -0 "$SERVER_PID" 2> /dev/null; then
    echo "ERROR: liquidsoap crashed during the load test"
    cat "$LOG_FILE"
    exit 1
fi

echo "$(ts)PASSED: $DEDICATED encoder, mime=$MIME, port=$PORT"
