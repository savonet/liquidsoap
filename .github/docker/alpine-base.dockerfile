FROM alpine:edge AS base

ENTRYPOINT bash

MAINTAINER The Savonet Team <savonet-users@lists.sourceforge.net>

ARG OCAML_VERSION=4.14.2

USER root

RUN echo "https://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories && \
    echo "https://dl-cdn.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories && \
    apk update && \
    apk add --no-cache \
      aspcud autoconf automake bash build-base curl git \
      openssh-client openssl unzip gnupg sudo musl-dbg rsync

RUN printf "\ny\n" |  bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"

RUN adduser -D opam

USER opam

RUN \
    opam init -y --disable-sandboxing --compiler=$OCAML_VERSION && \
    opam update -y && \
    opam clean

WORKDIR /tmp

RUN git clone https://github.com/savonet/liquidsoap-full.git

WORKDIR /tmp/liquidsoap-full

RUN make init && make update

RUN cat PACKAGES.default | grep '^ocaml' | grep -v dssi > /tmp/modules && \
    cat /tmp/modules | while read i; do find $i | grep '\.opam$'; done | while read i; do basename $i | cut -d'.' -f 1; done > /tmp/packages

RUN \
    cat /tmp/modules | while read module; do \
        cd $module && opam pin add -y --no-action . && cd .. \
      fi; \
    done && cd liquidsoap && opam pin add -y --no-action .

ENV EXT_PACKAGES="$EXTRA_PACKAGES camomile ocurl irc-client-unix osc-unix gd inotify prometheus-liquidsoap tsdl sdl-liquidsoap tls-liquidsoap syslog memtrace mem_usage ssl posix-time2 yaml js_of_ocaml js_of_ocaml-ppx re sqlite3 pandoc-include odoc"

RUN eval $(opam env) && opam list --short --external --resolve="`echo $EXT_PACKAGES | sed -e 's# #,#g'`,`cat /tmp/packages | while read i; do printf "$i,"; done`,liquidsoap" > /tmp/deps

USER root

RUN cat /tmp/deps | xargs apk add --no-cache

USER opam

RUN \
    eval $(opam config env) && \
    PACKAGES=`cat /tmp/packages | xargs echo` && \
    opam install --no-depexts -y liquidsoap $PACKAGES $EXT_PACKAGES && \
    opam uninstall --no-depexts -y liquidsoap-lang $PACKAGES ffmpeg-avutil && \
    opam clean

FROM alpine:edge
COPY --from=base / /
