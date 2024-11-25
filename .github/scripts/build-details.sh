#!/bin/bash

set -e

if [ -n "${GITHUB_HEAD_REF}" ]; then
  BRANCH="${GITHUB_HEAD_REF}"
else
  BRANCH="${GITHUB_REF#refs/heads/}"
fi

BRANCH="${BRANCH//\//_}"

echo "Detected branch: ${BRANCH}"

if [ "${IS_FORK}" == "true" ]; then
  echo "Branch is from a fork"
  IS_FORK=true
fi

if [[ "${IS_FORK}" != "true" && ("${BRANCH}" =~ ^rolling-release\-v[0-9]\.[0-9]\.x || "${BRANCH}" =~ ^v[0-9]\.[0-9]\.[0-9]) ]]; then
  echo "Branch is release branch"
  IS_RELEASE=true
  DOCKER_TAG="savonet/liquidsoap:${BRANCH}"
else
  echo "Branch is not release branch"
  IS_RELEASE=
  DOCKER_TAG="savonet/liquidsoap-ci-build:${BRANCH}"
fi

BUILD_OS='["debian_trixie", "debian_bookworm", "ubuntu_oracular", "ubuntu_noble", "alpine"]'
BUILD_PLATFORM='["amd64", "arm64"]'
BUILD_INCLUDE='[{"platform": "amd64", "runs-on": "ubuntu-latest", "alpine-arch": "x86_64", "docker-platform": "linux/amd64", "docker-debian-os": "bookworm"}, {"platform": "arm64", "runs-on": ["self-hosted", "build"], "alpine-arch": "aarch64", "docker-platform": "linux/arm64", "docker-debian-os": "bookworm"}]'

echo "Docker tag: ${DOCKER_TAG}"

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

MINIMAL_EXCLUDE_DEPS="alsa ao bjack camlimages dssi faad fdkaac flac frei0r gd graphics gstreamer imagelib irc-client-unix ladspa lame lastfm lilv lo mad magic ogg opus osc-unix portaudio pulseaudio samplerate shine soundtouch speex srt tls theora tsdl sqlite3 vorbis"

{
  echo "branch=${BRANCH}"
  echo "is_release=${IS_RELEASE}"
  echo "build_os=${BUILD_OS}"
  echo "build_platform=${BUILD_PLATFORM}"
  echo "build_include=${BUILD_INCLUDE}"
  echo "docker_tag=${DOCKER_TAG}"
  echo "is_rolling_release=${IS_ROLLING_RELEASE}"
  echo "sha=${SHA}"
  echo "s3-artifact-basepath=s3://liquidsoap-artifacts/${GITHUB_WORKFLOW}/${GITHUB_RUN_NUMBER}"
  echo "is_fork=${IS_FORK}"
  echo "minimal_exclude_deps=${MINIMAL_EXCLUDE_DEPS}"
  echo "save_traces=${SAVE_TRACES}"
  echo "depot_project=wz546czd90"
  echo "is_snapshot=${IS_SNAPSHOT}"
} >> "${GITHUB_OUTPUT}"
