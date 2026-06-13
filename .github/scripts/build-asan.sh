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

echo "::group::Setting up specific dependencies"

opam update

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/share/pkgconfig/pkgconfig

opam pin -y add re 1.13.2
if [ -z "${SKIP_SDL}" ]; then
  # tsdl-ttf 0.7 regressed the Linux dlopen path back to the unversioned
  # libSDL2_ttf.so, which is only in the dev package on Alpine. Pin to 0.6
  # until https://github.com/sanette/tsdl-ttf/issues/14 is resolved.
  opam pin -y add tsdl-ttf 0.6
fi
opam upgrade -y posix-socket
opam install -y domain_shims syslog dune.3.23.1

for pkg in alsa ao faad fdkaac frei0r jack ladspa lame lo mad mem_usage metadata mm portaudio pulseaudio samplerate shine soundtouch srt; do
  opam pin -ny add https://github.com/savonet/ocaml-${pkg}.git
done
opam install -y --no-depexts \
  alsa ao faad fdkaac frei0r jack ladspa lame lo mad mem_usage metadata mm \
  portaudio pulseaudio samplerate shine soundtouch srt

echo "::endgroup::"

echo "::group::Cleaning up cache"

rm -rf /var/cache/liquidsoap/* "$HOME"/.cache/liquidsoap/*

echo "::endgroup::"

echo "::group::Compiling"

dune build --profile=release

echo "::endgroup::"

echo "::group::Print build config"

dune exec -- liquidsoap --build-config

echo "::endgroup::"
