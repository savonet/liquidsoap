FROM alpine:edge AS base

ENTRYPOINT bash

MAINTAINER The Savonet Team <contact@liquidsoap.info>

ARG OCAML_VERSION=4.14.2
ARG OCAML_PATCH_URL

USER root

RUN echo "https://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories && \
    echo "https://dl-cdn.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories && \
    apk update && \
    apk add --no-cache \
      aspcud autoconf automake bash build-base curl git \
      openssh-client openssl unzip gnupg sudo musl-dbg rsync

RUN printf "\ny\n" | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"

RUN adduser -D opam

USER opam

COPY .github/docker/setup-ocaml.sh /tmp/setup-ocaml.sh

RUN sh /tmp/setup-ocaml.sh

COPY .github/docker/ext-packages /tmp/ext-packages

# Alpine packages gd and has no dssi.
RUN (echo gd; grep -vx dssi /tmp/ext-packages) > /tmp/packages

# flac is pinned to git for a fix to %ogg(%flac) pages that has no release yet.
RUN eval $(opam env) && \
    for p in ogg flac vorbis opus; do opam pin add -n -y $p git+https://github.com/savonet/ocaml-xiph.git || exit 1; done && \
    for p in lo lilv frei0r; do opam pin add -n -y $p git+https://github.com/savonet/ocaml-$p.git || exit 1; done && \
    opam list --short --external --resolve="$(xargs < /tmp/packages | tr ' ' ','),liquidsoap" > /tmp/deps

USER root

RUN cat /tmp/deps | xargs apk add --no-cache

USER opam

RUN \
    eval $(opam config env) && \
    opam install --no-depexts -y liquidsoap $(xargs < /tmp/packages) && \
    opam uninstall --no-depexts -y liquidsoap-lang && \
    opam clean

USER root

RUN echo 'Defaults secure_path="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"' > /etc/sudoers.d/secure_path

FROM alpine:edge
COPY --from=base / /
