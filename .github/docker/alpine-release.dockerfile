FROM alpine:edge AS downloader

ARG APK_FILE

COPY $APK_FILE /downloads/liquidsoap.apk

FROM alpine:edge

RUN --mount=type=bind,from=downloader,source=/downloads,target=/downloads \
    set -eux; \
      echo 'https://dl-cdn.alpinelinux.org/alpine/edge/testing' >> /etc/apk/repositories; \
      apk add --allow-untrusted --no-cache \
        /downloads/liquidsoap.apk \
      ;

USER liquidsoap

RUN liquidsoap --cache-stdlib

ENTRYPOINT ["/usr/bin/liquidsoap"]
