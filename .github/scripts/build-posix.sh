#!/bin/sh

set -e

CPU_CORES="$1"

export CPU_CORES

eval "$(opam config env)"

echo "::group::Preparing bindings"

cd /tmp/liquidsoap-full

git remote set-url origin https://github.com/savonet/liquidsoap-full.git
git fetch --recurse-submodules=no && git checkout origin/master -- Makefile.git
git reset --hard
git pull

git pull
make clean
make public
make update

echo "::endgroup::"

echo "::group::Checking out CI commit"

cd /tmp/liquidsoap-full/liquidsoap

git fetch origin "$GITHUB_SHA"
git checkout "$GITHUB_SHA"
mv .github /tmp
rm -rf ./*
mv /tmp/.github .
git reset --hard

echo "::endgroup::"

echo "::group::Setting up specific dependencies"

opam update
opam pin -y add re 1.13.2
# tsdl-ttf 0.7 regressed the Linux dlopen path back to the unversioned
# libSDL2_ttf.so, which is only in the dev package on Alpine. Pin to 0.6
# until https://github.com/sanette/tsdl-ttf/issues/14 is resolved.
opam pin -y add tsdl-ttf 0.6
opam upgrade -y posix-socket
opam install -y domain_shims syslog

cd /tmp/liquidsoap-full/liquidsoap

./.github/scripts/checkout-deps.sh

cd /tmp/liquidsoap-full

export PKG_CONFIG_PATH=/usr/share/pkgconfig/pkgconfig

echo "::endgroup::"

echo "::group::Cleaning up cache"

rm -rf /var/cache/liquidsoap/* "$HOME"/.cache/liquidsoap/*

echo "::endgroup::"

echo "::group::Compiling"

cd /tmp/liquidsoap-full

test -f PACKAGES || cp PACKAGES.default PACKAGES

# Workaround
touch liquidsoap/configure

./configure --prefix=/usr \
  --includedir="\${prefix}/include" \
  --mandir="\${prefix}/share/man" \
  --infodir="\${prefix}/share/info" \
  --sysconfdir=/etc \
  --localstatedir=/var \
  --with-camomile-data-dir=/usr/share/liquidsoap/camomile \
  CFLAGS=-g

# Workaround
rm liquidsoap/configure

OCAMLPATH="$(cat .ocamlpath)"
export OCAMLPATH

cd /tmp/liquidsoap-full/liquidsoap
dune build --profile=release

echo "::endgroup::"

echo "::group::Print build config"

dune exec -- liquidsoap --build-config

echo "::endgroup::"
