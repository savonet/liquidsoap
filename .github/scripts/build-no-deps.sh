#!/bin/sh

set -e

CPU_CORES="$1"
EXCLUDE_DEPS="$2"
GITHUB_SHA="$3"

export CPU_CORES
export LIQ_BUILD_MIN=true
export HOME=/home/opam

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)

eval "$(opam config env)"

echo "::group::Preparing build"

cd /tmp/liquidsoap-full/liquidsoap
git remote set-url origin https://github.com/savonet/liquidsoap.git
git fetch origin "${GITHUB_SHA}"
git checkout "${GITHUB_SHA}"

cd /tmp/liquidsoap-full

git remote set-url origin https://github.com/savonet/liquidsoap-full.git
git fetch --recurse-submodules=no
git checkout origin/master -- Makefile.git

make public

git reset --hard
git submodule foreach 'git reset --hard'
git pull

cp PACKAGES.minimal PACKAGES

opam update
opam pin -yn .
opam install -y saturn_lockfree.0.4.1 ppx_hash
# shellcheck disable=SC2086
opam info -f "depopts:" liquidsoap-core | grep -v osx-secure-transport | xargs opam remove -y inotify ffmpeg-avutil cohttp-lwt-unix prometheus-app ${EXCLUDE_DEPS}
opam install -y mem_usage
echo "::endgroup::"
cd liquidsoap

"${SCRIPT_DIR}/build-posix.sh" "${CPU_CORES}" "${PLATFORM}"
