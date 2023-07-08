#!/bin/bash

set -e

if [ -n "${GITHUB_HEAD_REF}" ]; then
  BRANCH="${GITHUB_HEAD_REF}"
else
  BRANCH="${GITHUB_REF#refs/heads/}"
fi

echo "Detected branch: ${BRANCH}"

if [ "${IS_FORK}" == "true" ]; then
  echo "Branch is from a fork"
  IS_FORK=true
fi

if [[ "${IS_FORK}" != "true" && ("${BRANCH}" =~ "rolling-release-" || "${BRANCH}" =~ ^v[0-9]) ]]; then
  echo "Branch is release branch"
  IS_RELEASE=true

  echo "Building on all architectures"
  BUILD_OS='["debian_trixie", "debian_bookworm", "ubuntu_jammy", "ubuntu_lunar", "alpine"]'
  BUILD_PLATFORM='["amd64", "arm64", "armhf"]'
  #BUILD_PLATFORM='["amd64", "arm64"]'
  BUILD_INCLUDE='[{"platform": "amd64", "runs-on": "ubuntu-latest", "alpine-arch": "x86_64", "docker-platform": "linux/amd64"}, {"platform": "arm64", "runs-on": "self-hosted", "alpine-arch": "aarch64", "docker-platform": "linux/arm64"}, {"platform": "armhf", "runs-on": "self-hosted", "alpine-arch": "armv7", "docker-platform": "linux/arm/v7"}]'
  #BUILD_INCLUDE='[{"platform": "amd64", "runs-on": "ubuntu-latest", "alpine-arch": "x86_64", "docker-platform": "linux/amd64"}, {"platform": "arm64", "runs-on": "self-hosted", "alpine-arch": "aarch64", "docker-platform": "linux/arm64"}]'

  echo "Enabling opam build"
  echo "build_opam=true" >> "${GITHUB_OUTPUT}"

  echo "Branch has a docker release"
  DOCKER_RELEASE=true
else
  echo "Branch is not release branch"
  IS_RELEASE=

  echo "Building on amd64 only"
  BUILD_OS='["debian_trixie", "debian_bookworm", "ubuntu_jammy", "ubuntu_lunar", "alpine"]'
  BUILD_PLATFORM='["amd64"]'
  BUILD_INCLUDE='[{"platform": "amd64", "runs-on": "ubuntu-latest", "alpine-arch": "x86_64", "docker-platform": "linux/amd64"}]'

  echo "Not enabling opam build"
  echo "Branch does not have a docker release"
  DOCKER_RELEASE=
fi

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

MINIMAL_EXCLUDE_DEPS="alsa ao bjack camlimages dssi faad fdkaac flac frei0r gd graphics gstreamer imagelib irc-client-unix ladspa lame lastfm lilv lo mad magic ogg opus osc-unix portaudio pulseaudio samplerate shine soundtouch speex srt taglib tls theora tsdl vorbis"

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
} >> "${GITHUB_OUTPUT}"
