FROM alpine:edge AS base

ENTRYPOINT bash

MAINTAINER The Savonet Team <contact@liquidsoap.info>

ARG OCAML_VERSION=4.14.2

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

RUN \
    opam init -y --disable-sandboxing --compiler=$OCAML_VERSION && \
    opam update -y && \
    opam clean

ARG LIQUIDSOAP_SHA=main

WORKDIR /tmp

RUN git clone https://github.com/savonet/liquidsoap.git && \
    cd liquidsoap && git fetch origin "$LIQUIDSOAP_SHA" && git checkout "$LIQUIDSOAP_SHA"

# Pin each synced module directory and liquidsoap itself
RUN find /tmp/liquidsoap/src/modules/synced -maxdepth 1 -mindepth 1 -type d | \
    while read dir; do opam pin add -y --no-action "$dir"; done && \
    cd /tmp/liquidsoap && opam pin add -y --no-action .

# Build the package list from .opam files in synced modules
RUN find /tmp/liquidsoap/src/modules/synced -name '*.opam' ! -name '*.opam.template' | \
    xargs -I{} basename {} .opam | grep -Ev "^(speex|theora|dssi)$" > /tmp/packages

ENV EXT_PACKAGES="$EXTRA_PACKAGES camomile ocurl irc-client-unix osc-unix gd inotify prometheus-liquidsoap tsdl sdl-liquidsoap tls-liquidsoap syslog memtrace ssl posix-time2 yaml js_of_ocaml js_of_ocaml-ppx re sqlite3 pandoc-include odoc"

RUN eval $(opam env) && opam list --short --external --resolve="`echo $EXT_PACKAGES | sed -e 's# #,#g'`,`cat /tmp/packages | while read i; do printf "$i,"; done`,liquidsoap" > /tmp/deps

USER root

RUN cat /tmp/deps | xargs apk add --no-cache

USER opam

RUN \
    eval $(opam config env) && \
    PACKAGES=`cat /tmp/packages | xargs echo` && \
    opam install --no-depexts -y liquidsoap $PACKAGES $EXT_PACKAGES && \
    opam uninstall --no-depexts -y liquidsoap-lang $PACKAGES ffmpeg-avutil && \
    opam pin list --short | xargs -r opam pin remove -y && \
    rm -rf /tmp/liquidsoap && \
    opam clean

USER root

RUN echo 'Defaults secure_path="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"' > /etc/sudoers.d/secure_path

FROM alpine:edge
COPY --from=base / /
