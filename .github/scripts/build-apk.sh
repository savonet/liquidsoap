#!/bin/sh

set -e

cd /tmp/liquidsoap

APK_VERSION=$(opam show -f version ./opam/liquidsoap.opam | cut -d'-' -f 1)

export LIQUIDSOAP_BUILD_TARGET=posix
APKDEST="/tmp/apkout"

# apk derives a package's URL from its name and version alone, so the name has to
# be the same across builds and architectures for the repository to work. What
# used to distinguish it moves to the asset file name, which apk never sees, and
# to the version. `_rc` sorts below the plain release, so 2.4.6 supersedes every
# 2.4.6 rolling build. Alpine versions admit no commit sha; `liquidsoap
# --build-config` names the commit.
APK_SUFFIX="${ALPINE_TAG}-${ALPINE_ARCH}"

if [ -n "${IS_ROLLING_RELEASE}" ]; then
  APK_PACKAGE="liquidsoap"
  APK_VERSION="${APK_VERSION}_rc${BUILD_STAMP}"
elif [ -n "${IS_RELEASE}" ]; then
  APK_PACKAGE="liquidsoap"
else
  ALPINE_BRANCH=$(echo "${BRANCH}" | tr '[:upper:]' '[:lower:]' | sed -e 's#[^0-9^a-z^A-Z^.^-]#-#g')
  APK_PACKAGE="liquidsoap-${ALPINE_BRANCH}"
fi

# build-repo.sh reads the canonical name back out of .PKGINFO and redirects it
# here.
collect_apk() {
  find "$APKDEST" -name "*.apk" | while read -r apk; do
    mv "$apk" "${LIQ_TMP_DIR}/$(basename "$apk" .apk)-${APK_SUFFIX}.apk"
  done
}

echo "::group:: build ${APK_PACKAGE}.."

cd /tmp

sed -e "s#@APK_PACKAGE@#${APK_PACKAGE}#" /tmp/liquidsoap/.github/alpine/APKBUILD.in |
  sed -e "s#@APK_VERSION@#${APK_VERSION}#" |
  sed -e "s#@APK_RELEASE@#${APK_RELEASE}#" \
    > APKBUILD

cp "/tmp/liquidsoap/.github/alpine/liquidsoap.post-install" "${APK_PACKAGE}.post-install"

mkdir -p "$APKDEST"
abuild -P "$APKDEST"

collect_apk

echo "::endgroup::"

if [ "${ARCH}" = "amd64" ]; then
  echo "::group:: save build config for ${APK_PACKAGE}.."

  eval "$(opam config env)"
  /tmp/liquidsoap/liquidsoap --build-config > "${LIQ_TMP_DIR}/${APK_PACKAGE}-${APK_VERSION}-r${APK_RELEASE}-${APK_SUFFIX}.config"

  echo "::endgroup::"
fi

rm -rf APKBUILD "$APKDEST"

echo "::group:: building ${APK_PACKAGE}-minimal.."

export LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS="$MINIMAL_EXCLUDE_DEPS"

# shellcheck disable=SC2086
opam remove -y --assume-depexts $MINIMAL_EXCLUDE_DEPS

eval "$(opam config env)"

cd /tmp/liquidsoap
./.github/scripts/build-posix.sh 1

cd /tmp

sed -e "s#@APK_PACKAGE@#${APK_PACKAGE}-minimal#" /tmp/liquidsoap/.github/alpine/APKBUILD-minimal.in |
  sed -e "s#@APK_VERSION@#${APK_VERSION}#" |
  sed -e "s#@APK_RELEASE@#${APK_RELEASE}#" \
    > APKBUILD

cp "/tmp/liquidsoap/.github/alpine/liquidsoap.post-install" "${APK_PACKAGE}-minimal.post-install"

mkdir -p "$APKDEST"
abuild -P "$APKDEST"

collect_apk

echo "::endgroup::"

if [ "${ARCH}" = "amd64" ]; then
  echo "::group:: save build config for ${APK_PACKAGE}-minimal.."

  /tmp/liquidsoap/liquidsoap --build-config > "${LIQ_TMP_DIR}/${APK_PACKAGE}-minimal-${APK_VERSION}-r${APK_RELEASE}-${APK_SUFFIX}.config"
fi

echo "::endgroup::"

{
  echo "basename=${APK_PACKAGE}-${APK_VERSION}-r${APK_RELEASE}-${APK_SUFFIX}.apk"
  echo "basename-minimal=${APK_PACKAGE}-minimal-${APK_VERSION}-r${APK_RELEASE}-${APK_SUFFIX}.apk"
} >> "${GITHUB_OUTPUT}"
