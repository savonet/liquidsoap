#!/bin/sh
# Serve the documentation website locally, so doc changes can be seen before they are
# pushed. Run through dune so the reference is built first:
#
#   dune build @doc/serve-website
#
# The site itself lives in savonet/website. Point LIQUIDSOAP_WEBSITE at a checkout to use
# your own; otherwise one is cloned into doc/.website and reused.
set -e

WORKSPACE="$1"
PORT="${2:-3000}"
BUNDLE="$(pwd)/.website-bundle"

if [ -n "$LIQUIDSOAP_WEBSITE" ]; then
  WEBSITE="$LIQUIDSOAP_WEBSITE"
  if [ ! -f "$WEBSITE/scripts/sync-docs.mjs" ]; then
    echo "LIQUIDSOAP_WEBSITE=$WEBSITE does not look like a savonet/website checkout." >&2
    exit 1
  fi
else
  WEBSITE="$WORKSPACE/doc/.website"
  if [ ! -d "$WEBSITE/.git" ]; then
    echo "Cloning savonet/website into $WEBSITE"
    git clone --depth 1 https://github.com/savonet/website.git "$WEBSITE"
  else
    git -C "$WEBSITE" pull --ff-only --quiet || true
  fi
fi

# content is linked, not copied, so that editing doc/content/*.md re-syncs live. The five
# generated files come from the dune build that produced this action.
rm -rf "$BUNDLE"
mkdir -p "$BUNDLE"
ln -s "$WORKSPACE/doc/content" "$BUNDLE/content"
for f in reference reference-extras reference-deprecated protocols settings; do
  cp "$f.md" "$BUNDLE/$f.md"
done
sed -n 's/^(version \(.*\))$/\1/p' "$WORKSPACE/dune-project" > "$BUNDLE/version"

cd "$WEBSITE"
[ -d node_modules ] || npm ci

# The dev server watches every page and rspack keeps a cache open alongside; the default
# macOS limit of 256 descriptors is not enough for a documentation set this size.
ulimit -n 4096 2> /dev/null || true

# Re-syncs on every edit under doc/content; the dev server picks the result up.
# sidebars.dev.json is the last thing a sync writes, so waiting for it keeps the dev
# server from reading a half-written tree at startup.
rm -f sidebars.dev.json
node scripts/sync-docs.mjs --version dev --bundle "$BUNDLE" --watch &
SYNC=$!
trap 'kill $SYNC 2>/dev/null || true' EXIT INT TERM

while [ ! -f sidebars.dev.json ]; do
  kill -0 "$SYNC" 2> /dev/null || {
                                   echo "Documentation sync failed." >&2
                                                                          exit 1
  }
  sleep 0.5
done

{
  echo ""
  echo "  Liquidsoap documentation"
  echo "  ========================"
  echo ""
  echo "  Open:    http://localhost:$PORT"
  echo "  Editing: $WORKSPACE/doc/content  (re-syncs automatically)"
  echo "  Site:    $WEBSITE"
  echo ""
  echo "  The reference is a snapshot of this build. Re-run the target after"
  echo "  changing anything that alters it."
  echo ""
  echo "  Press Ctrl+C to stop."
  echo ""
} > /dev/tty 2> /dev/null || true

npm start -- --port "$PORT"
