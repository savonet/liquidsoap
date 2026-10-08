#!/bin/sh

set -e

CPU_CORES="$1"

export CPU_CORES

eval "$(opam config env)"

export LIQUIDSOAP_INSTALL_NO_OPTIONAL_FAIL=true

echo "::group::Checking out CI commit"

if [ -d /tmp/liquidsoap ]; then
  cd /tmp/liquidsoap
else
  git clone --depth 1 https://github.com/savonet/liquidsoap.git /tmp/liquidsoap
  cd /tmp/liquidsoap
fi

git fetch --depth 1 origin "$GITHUB_SHA"
git checkout "$GITHUB_SHA"

echo "::endgroup::"

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/share/pkgconfig/pkgconfig

echo "::group::Cleaning up cache"

rm -rf /var/cache/liquidsoap/* "$HOME"/.cache/liquidsoap/*

echo "::endgroup::"

echo "::group::Compiling"

dune build --profile=release

echo "::endgroup::"

echo "::group::Print build config"

dune exec --profile=release -- liquidsoap --build-config

echo "::endgroup::"
