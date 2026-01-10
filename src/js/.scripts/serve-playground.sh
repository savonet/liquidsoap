#!/bin/sh
set -e

PORT="${1:-8080}"
URL="http://localhost:$PORT"

# Write directly to terminal to bypass dune's output capture
{
  echo ""
  echo "  Liquidsoap Playground"
  echo "  ====================="
  echo ""
  echo "  Open: $URL"
  echo ""
  echo "  Press Ctrl+C to stop the server"
  echo ""
} > /dev/tty 2> /dev/null || true

python3 -m http.server "$PORT"
