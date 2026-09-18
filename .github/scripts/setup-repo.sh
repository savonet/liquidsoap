#!/bin/bash

# Sets up everything the package repository needs that can be set up from a
# terminal: the two signing keys, the repository secrets, the Cloudflare Pages
# project and its custom domain. Run it once, from a checkout, with gh logged in.
# It is safe to run again.
#
# Keep the key directory somewhere safe. Replacing the alpine key breaks every
# machine that already trusts the old one, which is every machine that has run
# setup.sh.
#
# Set CLOUDFLARE_API_TOKEN to have the Pages half done too. The token itself has
# to come from the dashboard, under My Profile / API Tokens, with the
# "Cloudflare Pages: Edit" permission.

set -euo pipefail

KEY_DIR="${1:-${HOME}/.liquidsoap-repo-keys}"
REPO="${REPO:-savonet/liquidsoap}"
PROJECT="${PROJECT:-liquidsoap-repo}"
DOMAIN="${DOMAIN:-repo.liquidsoap.info}"

for tool in gh gpg openssl; do
  command -v "${tool}" > /dev/null || {
    echo "${tool} is required" >&2
    exit 1
  }
done

mkdir -p "${KEY_DIR}"
chmod 700 "${KEY_DIR}"

GPG_KEY="${KEY_DIR}/liquidsoap.gpg"
ABUILD_KEY="${KEY_DIR}/liquidsoap.rsa"

if [ ! -f "${GPG_KEY}" ]; then
  echo "Creating the apt signing key.."
  export GNUPGHOME="${KEY_DIR}/gnupg"
  mkdir -p "${GNUPGHOME}"
  chmod 700 "${GNUPGHOME}"
  # No passphrase and no expiry: a workflow cannot answer a prompt, and a key
  # that expires takes every published repository down with it.
  gpg --batch --passphrase '' \
    --quick-generate-key 'Liquidsoap <contact@liquidsoap.info>' rsa4096 sign never
  gpg --armor --export-secret-keys > "${GPG_KEY}"
  chmod 600 "${GPG_KEY}"
fi

if [ ! -f "${ABUILD_KEY}" ]; then
  echo "Creating the alpine signing key.."
  openssl genrsa -out "${ABUILD_KEY}" 4096 2> /dev/null
  chmod 600 "${ABUILD_KEY}"
fi

echo "Uploading signing secrets to ${REPO}.."
gh secret set REPO_GPG_KEY -R "${REPO}" < "${GPG_KEY}"
gh secret set REPO_ABUILD_KEY -R "${REPO}" < "${ABUILD_KEY}"

if [ -z "${CLOUDFLARE_API_TOKEN:-}" ]; then
  echo
  echo "CLOUDFLARE_API_TOKEN is not set, so the Pages project is up to you:"
  echo "  - create a token at https://dash.cloudflare.com/profile/api-tokens with"
  echo "    the 'Cloudflare Pages: Edit' permission, then run this script again"
  exit 0
fi

for tool in curl jq npx; do
  command -v "${tool}" > /dev/null || {
    echo "${tool} is required for the Cloudflare half" >&2
    exit 1
  }
done

# Every response carries .success, and a failed call still answers 200, so each
# one is checked rather than trusted.
api() {
  local method="$1" path="$2" body="${3:-}"
  local response
  response=$(curl -s -X "${method}" \
    -H "Authorization: Bearer ${CLOUDFLARE_API_TOKEN}" \
    -H "Content-Type: application/json" \
    ${body:+-d "${body}"} \
    "https://api.cloudflare.com/client/v4${path}")
  if [ "$(printf '%s' "${response}" | jq -r '.success')" != "true" ]; then
    printf '%s\n' "${response}" | jq -r '.errors' >&2
    return 1
  fi
  printf '%s' "${response}"
}

ACCOUNT_ID="${CLOUDFLARE_ACCOUNT_ID:-$(api GET /accounts | jq -r '.result[0].id')}"
if [ -z "${ACCOUNT_ID}" ] || [ "${ACCOUNT_ID}" = "null" ]; then
  echo "could not determine the account id; set CLOUDFLARE_ACCOUNT_ID" >&2
  exit 1
fi
export CLOUDFLARE_ACCOUNT_ID="${ACCOUNT_ID}"

if api GET "/accounts/${ACCOUNT_ID}/pages/projects/${PROJECT}" > /dev/null 2>&1; then
  echo "Pages project ${PROJECT} is already there."
else
  echo "Creating the Pages project ${PROJECT}.."
  npx --yes wrangler pages project create "${PROJECT}" --production-branch main
fi

if api GET "/accounts/${ACCOUNT_ID}/pages/projects/${PROJECT}/domains" |
    jq -e --arg domain "${DOMAIN}" '.result[] | select(.name == $domain)' > /dev/null; then
  echo "${DOMAIN} is already attached to ${PROJECT}."
else
  echo "Attaching ${DOMAIN} to ${PROJECT}.."
  api POST "/accounts/${ACCOUNT_ID}/pages/projects/${PROJECT}/domains" \
    "{\"name\":\"${DOMAIN}\"}" > /dev/null
fi

echo "Uploading Cloudflare secrets to ${REPO}.."
printf '%s' "${CLOUDFLARE_API_TOKEN}" | gh secret set CLOUDFLARE_API_TOKEN -R "${REPO}"
printf '%s' "${ACCOUNT_ID}" | gh secret set CLOUDFLARE_ACCOUNT_ID -R "${REPO}"

echo
echo "Keys are in ${KEY_DIR}. Back that directory up."
gh secret list -R "${REPO}" | grep -E 'REPO_|CLOUDFLARE_' || true
echo
echo "One record left, wherever liquidsoap.info DNS lives:"
echo "  ${DOMAIN}  CNAME  ${PROJECT}.pages.dev"
echo "Only the subdomain is involved. If the domain is on Cloudflare, attaching"
echo "the custom domain above already created it."
