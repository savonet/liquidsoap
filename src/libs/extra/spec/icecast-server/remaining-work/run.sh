#!/bin/sh
# Usage: CAP=1500M run.sh --check /abs/path/to/script.liq
# Runs liquidsoap from the build tree under a memory cap and prints its peak memory and time.
cd "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)" || exit
systemd-run --user --scope -q -p MemoryMax="${CAP:-1500M}" -p MemorySwapMax=0 \
  /usr/bin/time -f "maxrss=%MKB elapsed=%es" timeout 120 \
  _build/default/src/bin/liquidsoap.exe --disable-deprecated --stdlib ./src/libs/stdlib.liq "$@"
echo "status=$?"
