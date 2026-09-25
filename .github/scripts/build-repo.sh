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

fail() {
  echo "build-repo: $1" >&2
  exit 1
}

# Signing is not optional: apt refuses an unsigned repository outright and apk
# refuses an unsigned index, so a missing key has to stop the publish rather than
# ship something every client rejects at its first update. The apk private key
# has to be named liquidsoap.rsa, because abuild-sign records its basename in the
# index and clients look for that name under /etc/apk/keys.
grep -q '^sec:' < <(gpg --list-secret-keys --with-colons) || fail "no gpg secret key"
ABUILD_KEY="${ABUILD_KEY:?build-repo: ABUILD_KEY must point at liquidsoap.rsa}"
[ "$(basename "${ABUILD_KEY}")" = "liquidsoap.rsa" ] ||
  fail "ABUILD_KEY must be named liquidsoap.rsa"

WORK=$(mktemp -d)
trap 'rm -rf "${WORK}"' EXIT

# Kept outside the scratch directory when asked, so the verification pass can
# serve the packages the redirects point at without downloading them twice.
DOWNLOAD_DIR="${DOWNLOAD_DIR:-${WORK}/downloads}"
mkdir -p "${DOWNLOAD_DIR}"

rm -rf "${SITE}"
mkdir -p "${SITE}"
gpg --export --armor > "${SITE}/liquidsoap.asc"
openssl rsa -in "${ABUILD_KEY}" -pubout > "${SITE}/liquidsoap.rsa.pub"
# An empty public key signs and serves perfectly well, and rejects every package
# on the client.
[ -s "${SITE}/liquidsoap.rsa.pub" ] || fail "could not derive the public key"
SITE_ABS=$(cd "${SITE}" && pwd -P)

: > "${SITE}/_redirects"
: > "${SITE}/channels.txt"

# Branches build several OCaml versions; only one of them can be the liquidsoap
# package, or the two collide under the same name. The newest wins, so a branch
# that drops or adds an OCaml version needs no edit here.
newest_ocaml() {
  # shellcheck disable=SC2012 # names only, and they are ours
  ls "$1" | sed -n 's/.*-ocaml\([0-9][0-9.]*\)[-.].*/\1/p' | sort -V -u | tail -1
}

# The release also carries debug symbols and the sanitizer build, neither of which
# a repository should offer, so the two packages a user installs are named rather
# than inferred.
publishable() {
  case "$1" in liquidsoap | liquidsoap-minimal) return 0 ;; *) return 1 ;; esac
}

# The one place that decides what a channel publishes. A channel built before
# packages carried stable names selects nothing and is skipped rather than
# published half working; it returns on its own once that branch is rebuilt.
select_packages() {
  local downloads="$1" ocaml="$2" name
  : > "${WORK}/debs"
  : > "${WORK}/apks"

  for deb in "${downloads}"/*.deb; do
    [ -e "${deb}" ] || continue
    name=$(dpkg-deb -f "${deb}" Package)
    publishable "${name}" || continue
    case "$(dpkg-deb -f "${deb}" Version)" in
      *"-ocaml${ocaml}-"*) printf '%s\n' "${deb}" >> "${WORK}/debs" ;;
    esac
  done

  for apk in "${downloads}"/*.apk; do
    [ -e "${apk}" ] || continue
    case "${apk}" in *"-ocaml${ocaml}-"*) ;; *) continue ;; esac
    name=$(tar -xOf "${apk}" .PKGINFO | sed -n 's/^pkgname = //p')
    publishable "${name}" || continue
    printf '%s\n' "${apk}" >> "${WORK}/apks"
  done

  [ -s "${WORK}/debs" ] && [ -s "${WORK}/apks" ]
}

# Listed from what was built, so the page cannot name a distribution or an
# architecture a channel does not carry.
dir_names() {
  find "$1" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' |
    sort | paste -sd, - | sed 's/,/, /g'
}

deb_arches() {
  grep -h '^Architecture: ' "${SITE}/$1"/deb/*/Packages |
    sed 's/^Architecture: //' | sort -u | paste -sd, - | sed 's/,/, /g'
}

index_deb_dir() {
  cd "$1"
  # Scanned from pool/, so Filename is the path _redirects matches on.
  dpkg-scanpackages --multiversion pool > Packages
  # Counted rather than assumed: an index that lists nothing publishes and
  # updates perfectly well, and only fails at install time.
  local expected indexed
  expected=$(find pool -name '*.deb' | wc -l)
  indexed=$(grep -c '^Package:' Packages || true)
  [ "${indexed}" = "${expected}" ] ||
    fail "indexed ${indexed} of ${expected} packages"
  gzip -9kf Packages
  rm -rf pool
  apt-ftparchive -o APT::FTPArchive::Release::Origin=liquidsoap \
    -o APT::FTPArchive::Release::Label="Liquidsoap $2" release . > Release
}

build_deb() {
  local channel="$1" stage="${WORK}/deb"
  rm -rf "${stage}"

  while IFS= read -r deb; do
    local version
    version=$(dpkg-deb -f "${deb}" Version)

    # The distribution a package was built for is in its version --
    # 1:2.5.0-debian-trixie-ocaml5.5.0-2 -- and is the codename /etc/os-release
    # reports, which is what setup.sh looks the directory up by.
    local codename
    codename=$(printf '%s' "${version}" | sed -n 's/.*-\(debian\|ubuntu\)-\([a-z]*\)-ocaml.*/\2/p')
    [ -n "${codename}" ] || fail "no codename in ${version}"

    mkdir -p "${stage}/${codename}/pool"
    ln -f "${deb}" "${stage}/${codename}/pool/$(basename "${deb}")"
  done < "${WORK}/debs"

  for dir in "${stage}"/*/; do
    local codename out
    codename=$(basename "${dir}")
    out="${SITE}/${channel}/deb/${codename}"
    mkdir -p "${out}"
    (index_deb_dir "${dir}" "${channel}")
    cp "${dir}/Packages" "${dir}/Packages.gz" "${out}/"
    # InRelease only, and Release goes with it: leaving an unsigned Release
    # behind gives apt a second thing to fetch that nothing vouches for.
    gpg --batch --yes --clearsign -o "${out}/InRelease" "${dir}/Release"

    cat > "${out}/liquidsoap.sources" << EOF
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
  local channel="$1" stage="${WORK}/apk"
  rm -rf "${stage}"

  while IFS= read -r apk; do
    local arch name version canonical
    arch=$(tar -xOf "${apk}" .PKGINFO | sed -n 's/^arch = //p')
    name=$(tar -xOf "${apk}" .PKGINFO | sed -n 's/^pkgname = //p')
    version=$(tar -xOf "${apk}" .PKGINFO | sed -n 's/^pkgver = //p')
    canonical="${name}-${version}.apk"

    mkdir -p "${stage}/${arch}"
    ln -f "${apk}" "${stage}/${arch}/${canonical}"
    # apk builds a package's URL from its name and version, so the asset's own
    # name never reaches a client and the redirect has to put it back.
    printf '/%s/alpine/%s/%s  %s/%s/%s  302\n' \
      "${channel}" "${arch}" "${canonical}" \
      "${PACKAGE_BASE_URL}" "${channel}" "$(basename "${apk}")" >> "${SITE}/_redirects"
  done < "${WORK}/apks"

  for dir in "${stage}"/*/; do
    local arch out
    arch=$(basename "${dir}")
    out="${SITE}/${channel}/alpine/${arch}"
    mkdir -p "${out}"
    # apk-tools indexes and abuild-sign signs, neither of which the runner has.
    # The key is trusted in there too: apk index verifies every package's
    # signature, which is what makes a mis-signed build fail here rather than on
    # a user's machine.
    docker run --rm \
      -v "$(cd "${dir}" && pwd -P):/pkgs:ro" \
      -v "${SITE_ABS}/liquidsoap.rsa.pub:/etc/apk/keys/liquidsoap.rsa.pub:ro" \
      -v "${WORK}:/out" -v "${ABUILD_KEY}:/liquidsoap.rsa:ro" \
      alpine:3 sh -ec '
        apk add --no-cache -q abuild
        cd /pkgs
        expected=$(find . -name "*.apk" | wc -l)
        apk index -o /out/APKINDEX.tar.gz ./*.apk
        # apk index warns and still succeeds on a package it cannot read, so the
        # index has to be counted: an empty one signs and serves just as well.
        indexed=$(tar -xzOf /out/APKINDEX.tar.gz APKINDEX | grep -c "^P:" || true)
        if [ "$indexed" != "$expected" ]; then
          echo "build-repo: indexed $indexed of $expected packages" >&2
          exit 1
        fi
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
  downloads="${DOWNLOAD_DIR}/${channel}"
  mkdir -p "${downloads}"
  gh release download "${channel}" -R "${RELEASE_REPO}" -D "${downloads}" \
    -p '*.deb' -p '*.apk' --clobber

  ocaml=$(newest_ocaml "${downloads}")
  [ -n "${ocaml}" ] || fail "no ocaml version in ${channel} assets"
  echo "build-repo: ${channel} ships ocaml ${ocaml}"

  if ! select_packages "${downloads}" "${ocaml}"; then
    echo "build-repo: skipping ${channel}, no packages with stable names"
    continue
  fi

  build_deb "${channel}"
  build_apk "${channel}"

  printf '%s\t%s\n' "${channel}" "${description}" >> "${SITE}/channels.txt"
done < <(.github/scripts/release-channels.sh | cut -f1,4)

[ -s "${SITE}/channels.txt" ] || fail "no channel could be built"

# Without it, Pages answers every missing path with index.html and a 200, so
# setup.sh would install the page as a sources file.
printf 'Not found\n' > "${SITE}/404.html"

sed -e "s#https://repo.liquidsoap.info#${BASE_URL}#g" scripts/setup-repository.sh > "${SITE}/setup.sh"
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
  printf '<pre>curl -fsSL %s/setup.sh | sudo sh -s -- --channel CHANNEL</pre>' "${BASE_URL}"
  printf '<p>We build for the current Debian stable and testing, the current Ubuntu LTS'
  printf ' and latest release, and Alpine edge.</p><h2>Channels</h2>'
  while IFS=$'\t' read -r channel description; do
    printf '<h3><code>%s</code></h3><p>%s</p><ul>' "${channel}" "${description}"
    printf '<li>Debian and Ubuntu: %s — %s</li>' \
      "$(dir_names "${SITE}/${channel}/deb")" "$(deb_arches "${channel}")"
    printf '<li>Alpine: %s</li></ul>' "$(dir_names "${SITE}/${channel}/alpine")"
  done < "${SITE}/channels.txt"
} > "${SITE}/index.html"

find "${SITE}" -type f | sort
