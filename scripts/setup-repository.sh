#!/bin/sh

# Configures the Liquidsoap apt, apk or dnf repository. Published as
# https://repo.liquidsoap.info/setup.sh.
#
# The script asks which release to install. To pick one up front, or to run it
# somewhere without a terminal, pass --channel.

set -eu

BASE="${LIQUIDSOAP_REPO_URL:-https://repo.liquidsoap.info}"
CHANNEL="${LIQUIDSOAP_CHANNEL:-}"

if [ "${1:-}" = "--channel" ]; then
  CHANNEL="${2:-}"
  if [ -z "${CHANNEL}" ]; then
    echo "--channel needs a value; see ${BASE}/channels.txt" >&2
    exit 1
  fi
fi

command -v curl > /dev/null || {
  echo "curl is required" >&2
  exit 1
}

fetch() {
  curl -fsSL "$1" -o "$2" && return 0
  echo "nothing published at $1" >&2
  exit 1
}

# shellcheck disable=SC1091 # provided by the distribution
. /etc/os-release

# Where each channel publishes this system's packages, and the name of the list
# of channels that have them.
if [ -d /etc/apt/sources.list.d ]; then
  if [ -z "${VERSION_CODENAME:-}" ]; then
    echo "cannot tell which release this is: /etc/os-release has no VERSION_CODENAME" >&2
    exit 1
  fi
  TARGET="deb/${VERSION_CODENAME}"
elif [ -d /etc/apk ]; then
  TARGET="alpine/$(apk --print-arch)"
elif [ -d /etc/yum.repos.d ]; then
  TARGET="fedora/${VERSION_ID}"
else
  echo "no apt, apk or dnf here: see https://liquidsoap.info/doc-dev/install.html" >&2
  exit 1
fi

if ! curl -fsSL "${BASE}/targets/${TARGET}.txt" -o /tmp/liquidsoap-channels; then
  echo "no release has packages for ${TARGET}; see ${BASE}" >&2
  exit 1
fi

# Piped to a shell, so stdin is the script itself: the menu has to talk to the
# terminal directly, and has to have an answer when there is no terminal.
if [ -z "${CHANNEL}" ]; then
  # Opened rather than tested: /dev/tty is there in a container with no terminal
  # and only fails when something tries to use it. The subshell keeps that
  # failure from taking the script with it.
  if (exec 3<> /dev/tty) 2> /dev/null; then
    exec 3<> /dev/tty
    n=1
    while IFS="$(printf '\t')" read -r channel description; do
      printf '  %d) %-26s %s\n' "${n}" "${channel}" "${description}" >&3
      n=$((n + 1))
    done < /tmp/liquidsoap-channels
    printf 'Which release? [1] ' >&3
    read -r answer <&3
    exec 3>&-
  else
    answer=1
  fi

  CHANNEL=$(sed -n "${answer:-1}p" /tmp/liquidsoap-channels | cut -f1)
  if [ -z "${CHANNEL}" ]; then
    echo "no such choice; see ${BASE}/targets/${TARGET}.txt" >&2
    exit 1
  fi
  echo "Using ${CHANNEL}."
elif ! cut -f1 /tmp/liquidsoap-channels | grep -qxF "${CHANNEL}"; then
  echo "${CHANNEL} has no packages for ${TARGET}; see ${BASE}/targets/${TARGET}.txt" >&2
  exit 1
fi

case "${TARGET}" in
  deb/*)
    install -d /etc/apt/keyrings
    fetch "${BASE}/liquidsoap.asc" /etc/apt/keyrings/liquidsoap.asc
    fetch "${BASE}/${CHANNEL}/${TARGET}/liquidsoap.sources" \
      /etc/apt/sources.list.d/liquidsoap.sources
    apt-get update
    echo "Done. Install with: apt-get install liquidsoap"
    ;;
  alpine/*)
    fetch "${BASE}/liquidsoap.rsa.pub" /etc/apk/keys/liquidsoap.rsa.pub
    # Rewritten rather than appended, so re-running the script switches channel
    # instead of leaving two of them for apk to choose between.
    sed -i "\\#^${BASE}/#d" /etc/apk/repositories
    echo "${BASE}/${CHANNEL}/alpine" >> /etc/apk/repositories
    apk update
    echo "Done. Install with: apk add liquidsoap"
    ;;
  fedora/*)
    fetch "${BASE}/${CHANNEL}/${TARGET}/liquidsoap.repo" \
      /etc/yum.repos.d/liquidsoap.repo
    # Imports the key and verifies the signed index up front, as apt-get update does.
    dnf -y makecache --repo liquidsoap
    echo "Done. Install with: dnf install liquidsoap"
    ;;
esac
