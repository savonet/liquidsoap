FROM debian:13-slim AS downloader

ARG DEB_FILE
ARG DEB_DEBUG_FILE
COPY $DEB_FILE /downloads/liquidsoap.deb
COPY $DEB_DEBUG_FILE /downloads/liquidsoap-debug.deb

FROM debian:13-slim

ARG DEBIAN_FRONTEND=noninteractive

RUN --mount=type=bind,from=downloader,source=/downloads,target=/downloads \
    set -eux; \
      apt-get update; \
      apt-get install -y --no-install-recommends \
        ca-certificates \
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
