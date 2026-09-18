#!/bin/bash

# Assembles the apt and apk repositories for every published channel into the
# site directory $1. Run from the repository root.
#
# The packages themselves are not republished: they stay the release assets they
# already are, and _redirects sends clients to them. That keeps the site at a few
# hundred kilobytes and the package bytes on GitHub's release CDN, which is where
# they are downloaded from today anyway. apt and apk both follow a cross-host
# redirect for a package; neither accepts an absolute URL in its index.
#
# Nothing here is incremental: the site is rebuilt from the releases every time,
# so a stale index has nowhere to survive.
#
# Needs gh, jq, dpkg-dev, apt-utils, docker, and a gpg secret key.

set -euo pipefail

SITE="${1:-site}"
BASE_URL="${REPO_BASE_URL:-https://repo.liquidsoap.info}"
RELEASE_REPO="${RELEASE_REPO:-savonet/liquidsoap}"
PACKAGE_BASE_URL="${PACKAGE_BASE_URL:-https://github.com/${RELEASE_REPO}/releases/download}"
MATRIX=".github/release-matrix.json"

# Signing is not optional: apt refuses an unsigned repository outright and apk
# refuses an unsigned index, so a missing key has to stop the publish rather than
# ship something every client rejects at its first update. The apk private key
# has to be named liquidsoap.rsa, because abuild-sign records its basename in the
# index and clients look for that name under /etc/apk/keys.
gpg --list-secret-keys --with-colons | grep -q '^sec:' \
  || { echo "build-repo: no gpg secret key" >&2; exit 1; }
ABUILD_KEY="${ABUILD_KEY:?build-repo: ABUILD_KEY must point at liquidsoap.rsa}"
[ "$(basename "${ABUILD_KEY}")" = "liquidsoap.rsa" ] \
  || { echo "build-repo: ABUILD_KEY must be named liquidsoap.rsa" >&2; exit 1; }

WORK=$(mktemp -d)
trap 'rm -rf "${WORK}"' EXIT

rm -rf "${SITE}"
mkdir -p "${SITE}"
gpg --export --armor > "${SITE}/liquidsoap.asc"
openssl rsa -in "${ABUILD_KEY}" -pubout > "${SITE}/liquidsoap.rsa.pub" 2> /dev/null

: > "${SITE}/_redirects"
: > "${SITE}/channels.txt"

# A channel per supported series: the published release, and the rolling branch
# that leads to the next one. Bumping latest_release in the matrix is all it
# takes to add a channel, which is already part of releasing. Stable first, so
# the installer's default is a release rather than a rolling build.
channels() {
  jq -r '
    def published: [.[] | select(.supported != false)];
    (published | .[] | select(.latest_release != null)
      | "v\(.latest_release)\tLiquidsoap \(.latest_release)"),
    (published | .[] | select(.branch != null)
      | "rolling-release-v\(.version)\tLiquidsoap \(.version) rolling release, rebuilt on every commit")
  ' "${MATRIX}"
}

# Branches build several OCaml versions; only one of them can be the liquidsoap
# package, or the two collide under the same name. The newest wins, so a branch
# that drops or adds an OCaml version needs no edit here.
newest_ocaml() {
  # shellcheck disable=SC2012 # names only, and they are ours
  ls "$1" | sed -n 's/.*-ocaml\([0-9][0-9.]*\)[-.].*/\1/p' | sort -V -u | tail -1
}

build_deb() {
  local channel="$1" downloads="$2" ocaml="$3" stage="${WORK}/deb" found=
  rm -rf "${stage}"

  for deb in "${downloads}"/*.deb; do
    [ -e "${deb}" ] || continue
    case "${deb}" in *-dbgsym_*) continue ;; esac
    local version
    version=$(dpkg-deb -f "${deb}" Version)
    case "${version}" in *"-ocaml${ocaml}-"*) ;; *) continue ;; esac

    # The distribution a package was built for is in its version --
    # 1:2.5.0-debian-trixie-ocaml5.5.0-2 -- and is the codename /etc/os-release
    # reports, which is what setup.sh looks the directory up by.
    local codename
    codename=$(printf '%s' "${version}" | sed -n 's/.*-\(debian\|ubuntu\)-\([a-z]*\)-ocaml.*/\2/p')
    [ -n "${codename}" ] || { echo "build-repo: no codename in ${version}" >&2; exit 1; }

    mkdir -p "${stage}/${codename}/pool"
    ln -sf "${deb}" "${stage}/${codename}/pool/$(basename "${deb}")"
    found=1
  done
  [ -n "${found}" ] || { echo "build-repo: ${channel} has no .deb for ocaml ${ocaml}" >&2; exit 1; }

  for dir in "${stage}"/*/; do
    local codename out
    codename=$(basename "${dir}")
    out="${SITE}/${channel}/deb/${codename}"
    mkdir -p "${out}"
    (
      cd "${dir}"
      # Scanned from pool/, so Filename is the path _redirects matches on.
      dpkg-scanpackages --multiversion pool > Packages
      gzip -9kf Packages
      rm -rf pool
      apt-ftparchive -o APT::FTPArchive::Release::Origin=liquidsoap \
        -o APT::FTPArchive::Release::Label="Liquidsoap ${channel}" release . > Release
    )
    cp "${dir}/Packages" "${dir}/Packages.gz" "${out}/"
    # InRelease only, and Release goes with it: leaving an unsigned Release
    # behind gives apt a second thing to fetch that nothing vouches for.
    gpg --batch --yes --clearsign -o "${out}/InRelease" "${dir}/Release"

    cat > "${out}/liquidsoap.sources" <<EOF
Types: deb
URIs: ${BASE_URL}/${channel}/deb/${codename}
Suites: ./
Signed-By: /etc/apt/keyrings/liquidsoap.asc
EOF
    # Asset names are unique across distributions and architectures already,
    # because the version they carry names both, so one splat covers the pool.
    printf '/%s/deb/%s/pool/*  %s/%s/:splat  302\n' \
      "${channel}" "${codename}" "${PACKAGE_BASE_URL}" "${channel}" >> "${SITE}/_redirects"
  done
}

build_apk() {
  local channel="$1" downloads="$2" ocaml="$3" stage="${WORK}/apk" found=
  rm -rf "${stage}"

  for apk in "${downloads}"/*.apk; do
    [ -e "${apk}" ] || continue
    case "${apk}" in *"-ocaml${ocaml}-"*) ;; *) continue ;; esac

    local arch name version canonical
    arch=$(tar -xOf "${apk}" .PKGINFO | sed -n 's/^arch = //p')
    name=$(tar -xOf "${apk}" .PKGINFO | sed -n 's/^pkgname = //p')
    version=$(tar -xOf "${apk}" .PKGINFO | sed -n 's/^pkgver = //p')
    canonical="${name}-${version}.apk"

    mkdir -p "${stage}/${arch}"
    ln -sf "${apk}" "${stage}/${arch}/${canonical}"
    # apk builds a package's URL from its name and version, so the asset's own
    # name never reaches a client and the redirect has to put it back.
    printf '/%s/alpine/%s/%s  %s/%s/%s  302\n' \
      "${channel}" "${arch}" "${canonical}" \
      "${PACKAGE_BASE_URL}" "${channel}" "$(basename "${apk}")" >> "${SITE}/_redirects"
    found=1
  done
  [ -n "${found}" ] || { echo "build-repo: ${channel} has no .apk for ocaml ${ocaml}" >&2; exit 1; }

  for dir in "${stage}"/*/; do
    local arch out
    arch=$(basename "${dir}")
    out="${SITE}/${channel}/alpine/${arch}"
    mkdir -p "${out}"
    # apk-tools indexes and abuild-sign signs, neither of which the runner has.
    docker run --rm \
      -v "$(cd "${dir}" && pwd -P):/pkgs:ro" \
      -v "${WORK}:/out" -v "${ABUILD_KEY}:/liquidsoap.rsa:ro" \
      alpine:3 sh -ec '
        apk add --no-cache -q abuild
        cd /pkgs && apk index -o /out/APKINDEX.tar.gz ./*.apk
        abuild-sign -k /liquidsoap.rsa /out/APKINDEX.tar.gz'
    mv "${WORK}/APKINDEX.tar.gz" "${out}/"
  done
}

while IFS=$'\t' read -r channel description; do
  [ -n "${channel}" ] || continue

  if ! gh release view "${channel}" -R "${RELEASE_REPO}" --json isDraft > "${WORK}/release.json" 2> /dev/null; then
    echo "build-repo: skipping ${channel}, no release in ${RELEASE_REPO}"
    continue
  fi
  # A draft release's assets are not public, so redirecting to them would give
  # every client a 404 at install time.
  if [ "$(jq -r .isDraft "${WORK}/release.json")" = "true" ]; then
    echo "build-repo: skipping ${channel}, release is still a draft"
    continue
  fi

  echo "build-repo: building ${channel}.."
  downloads="${WORK}/downloads/${channel}"
  mkdir -p "${downloads}"
  gh release download "${channel}" -R "${RELEASE_REPO}" -D "${downloads}" \
    -p '*.deb' -p '*.apk' --clobber

  ocaml=$(newest_ocaml "${downloads}")
  [ -n "${ocaml}" ] || { echo "build-repo: no ocaml version in ${channel} assets" >&2; exit 1; }
  echo "build-repo: ${channel} ships ocaml ${ocaml}"

  build_deb "${channel}" "${downloads}" "${ocaml}"
  build_apk "${channel}" "${downloads}" "${ocaml}"

  printf '%s\t%s\n' "${channel}" "${description}" >> "${SITE}/channels.txt"
done < <(channels)

[ -s "${SITE}/channels.txt" ] || { echo "build-repo: no channel could be built" >&2; exit 1; }

sed -e "s#@BASE@#${BASE_URL}#g" .github/scripts/setup.sh.in > "${SITE}/setup.sh"
chmod +x "${SITE}/setup.sh"

# Written from the channels that were built rather than by hand, so the page
# cannot name one that does not exist.
{
  printf '<!doctype html><meta charset=utf-8><title>Liquidsoap packages</title>'
  printf '<meta name=viewport content="width=device-width,initial-scale=1">'
  printf '<style>body{font:15px/1.6 system-ui,sans-serif;margin:3rem auto;max-width:46rem;padding:0 1rem}'
  printf 'pre{background:#f4f4f4;padding:.8rem;overflow-x:auto}</style>'
  printf '<h1>Liquidsoap packages</h1>'
  printf '<p>Debian, Ubuntu and Alpine repositories for <a href="https://liquidsoap.info">Liquidsoap</a>.</p>'
  printf '<pre>curl -fsSL %s/setup.sh | sudo sh</pre>' "${BASE_URL}"
  printf '<p>The script asks which release to install. To pick one up front:</p>'
  printf '<pre>curl -fsSL %s/setup.sh | sudo sh -s -- --channel CHANNEL</pre><h2>Channels</h2><ul>' "${BASE_URL}"
  while IFS=$'\t' read -r channel description; do
    printf '<li><code>%s</code> — %s</li>' "${channel}" "${description}"
  done < "${SITE}/channels.txt"
  printf '</ul>'
} > "${SITE}/index.html"

find "${SITE}" -type f | sort
