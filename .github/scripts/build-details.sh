#!/bin/bash

set -e

if [ -n "${GITHUB_HEAD_REF}" ]; then
  BRANCH="${GITHUB_HEAD_REF#refs_heads_}"
else
  BRANCH="${GITHUB_REF}"
fi

BRANCH="${BRANCH#refs_heads_}"
BRANCH="${BRANCH#refs/heads/}"
BRANCH="${BRANCH#refs/tags/}"
BRANCH="${BRANCH//\//_}"

echo "Detected branch: ${BRANCH}"

if [ "${IS_FORK}" == "true" ]; then
  echo "Branch is from a fork"
  IS_FORK=true
fi

if [[ "${IS_FORK}" != "true" && ("${BRANCH}" =~ ^rolling-release\-v[0-9]\.[0-9]\.x || "${BRANCH}" =~ ^v[0-9]\.[0-9]\.[0-9]) ]]; then
  echo "Branch is release branch"
  IS_RELEASE=true

  echo "Branch has a docker release"
  DOCKER_RELEASE=true
else
  echo "Branch is not release branch"
  IS_RELEASE=

  echo "Branch does not have a docker release"
  DOCKER_RELEASE=
fi

BUILD_OS='["debian_trixie", "debian_bookworm", "ubuntu_plucky", "ubuntu_noble", "alpine"]'
BUILD_PLATFORM='["amd64", "arm64"]'
BUILD_INCLUDE='[{"platform": "amd64", "runs-on": "depot-ubuntu-24.04-4", "alpine-arch": "x86_64", "docker-debian-os": "bookworm"}, {"platform": "arm64", "runs-on": "depot-ubuntu-24.04-arm-4", "alpine-arch": "aarch64", "docker-debian-os": "bookworm"}]'

SHA=$(git rev-parse --short HEAD)

if [[ "${BRANCH}" =~ "rolling-release-" ]]; then
  echo "Branch is rolling release"
  IS_ROLLING_RELEASE=true
else
  IS_ROLLING_RELEASE=
fi

if [ "${IS_FORK}" != "true" ] && [ "${IS_RELEASE}" != "true" ] && [ "${IS_ROLLING_RELEASE}" != "true" ]; then
  echo "Save tests traces"
  SAVE_TRACES=true
else
  echo "Disable tests traces upload"
  SAVE_TRACES=
fi

if [ "${IS_RELEASE}" != "true" ] || [ "${IS_ROLLING_RELEASE}"  == "true" ]; then
  echo "Build is a snapshot"
  IS_SNAPSHOT=true
else
  IS_SNAPSHOT=
fi

MINIMAL_EXCLUDE_DEPS="alsa ao bjack camlimages dssi faad fdkaac flac frei0r gd graphics irc-client-unix ladspa lame lastfm lilv lo mad magic ogg opus osc-unix portaudio pulseaudio samplerate shine soundtouch speex srt tls theora tsdl sqlite3 vorbis sdl-liquidsoap"

echo "Ocaml version to build: 4.14.2,"
OCAML_VERSION='["4.14.2"]'

echo "OCaml docker release version: 4.14.2"
OCAML_DOCKER_RELEASE_VERSION="4.14.2"

{
  echo "branch=${BRANCH}"
  echo "is_release=${IS_RELEASE}"
  echo "build_os=${BUILD_OS}"
  echo "build_platform=${BUILD_PLATFORM}"
  echo "build_include=${BUILD_INCLUDE}"
  echo "docker_release=${DOCKER_RELEASE}"
  echo "is_rolling_release=${IS_ROLLING_RELEASE}"
  echo "sha=${SHA}"
  echo "s3-artifact-basepath=s3://liquidsoap-artifacts/${GITHUB_WORKFLOW}/${GITHUB_RUN_NUMBER}"
  echo "is_fork=${IS_FORK}"
  echo "minimal_exclude_deps=${MINIMAL_EXCLUDE_DEPS}"
  echo "save_traces=${SAVE_TRACES}"
  echo "is_snapshot=${IS_SNAPSHOT}"
  echo "ocaml_version=${OCAML_VERSION}"
  echo "ocaml_docker_release_version=${OCAML_DOCKER_RELEASE_VERSION}"
} >> "${GITHUB_OUTPUT}"
