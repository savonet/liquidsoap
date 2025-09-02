FROM debian:13-slim AS downloader

ARG DEB_FILE
ARG DEB_DEBUG_FILE
COPY $DEB_FILE /downloads/liquidsoap.deb
COPY $DEB_DEBUG_FILE /downloads/liquidsoap-debug.deb

ARG DEB_MULTIMEDIA_KEYRING="https://www.deb-multimedia.org/pool/main/d/deb-multimedia-keyring/deb-multimedia-keyring_2024.9.1_all.deb"
ARG DEB_MULTIMEDIA_KEYRING_SHA256SUM="8dc6cbb266c701cfe58bd1d2eb9fe2245a1d6341c7110cfbfe3a5a975dcf97ca"

RUN set -eux; \
      apt-get update; \
      apt-get install -y --no-install-recommends \
        ca-certificates \
        wget \
      ; \
      wget "$DEB_MULTIMEDIA_KEYRING" -O /downloads/deb-multimedia-keyring.deb; \
      echo "$DEB_MULTIMEDIA_KEYRING_SHA256SUM  /downloads/deb-multimedia-keyring.deb" | sha256sum -c -;

FROM debian:13-slim

ARG DEBIAN_FRONTEND=noninteractive

# For ffmpeg with libfdk-aac
RUN --mount=type=bind,from=downloader,source=/downloads,target=/downloads \
    set -eux; \
      apt-get update; \
      apt-get install -y --no-install-recommends \
        /downloads/deb-multimedia-keyring.deb \
        ca-certificates \
      ; \
      cat <<EOF > /etc/apt/sources.list.d/dmo.sources
Types: deb
URIs: https://www.deb-multimedia.org
Suites: trixie
Components: main non-free
Signed-By: /usr/share/keyrings/deb-multimedia-keyring.pgp
Enabled: yes
EOF

RUN --mount=type=bind,from=downloader,source=/downloads,target=/downloads \
    set -eux; \
      apt-get update; \
      apt-get install -y --no-install-recommends \
        /downloads/liquidsoap.deb \
        /downloads/liquidsoap-debug.deb \
      ; \
      rm -rf \
        /var/lib/apt/lists \
        /var/lib/dpkg/status-old \
      ;

USER liquidsoap

RUN liquidsoap --cache-stdlib

ENTRYPOINT ["/usr/bin/liquidsoap"]
