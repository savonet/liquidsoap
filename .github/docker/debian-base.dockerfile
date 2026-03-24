ARG BASE_IMAGE
FROM $BASE_IMAGE AS base

ENTRYPOINT bash

MAINTAINER The Savonet Team <savonet-users@lists.sourceforge.net>

ARG OS
ARG DISTRIBUTION
ARG OCAML_VERSION=4.14.2
ARG ARCHITECTURE
ARG EXTRA_PACKAGES

ENV DEBIAN_FRONTEND=noninteractive

USER root

# For libfdk-aac-dev
RUN if [ "$OS" = "debian" ]; then \
      echo "deb http://deb.$OS.org/$OS $DISTRIBUTION non-free" >> /etc/apt/sources.list; \
    fi

# We need an up-to date ffmpeg on bookworm
RUN if [ "$DISTRIBUTION" = "forky" -o "$DISTRIBUTION" = "bookworm" ]; then \
      apt-get update && apt install -y ca-certificates && \
      echo "deb https://www.deb-multimedia.org $DISTRIBUTION main non-free" >> /etc/apt/sources.list && \
      apt-get update -oAcquire::AllowInsecureRepositories=true && \
      apt-get install -y --allow-unauthenticated deb-multimedia-keyring; \
    fi

RUN \
    apt-get update && \
    apt-get install -y --no-install-recommends aspcud autoconf automake rsync \
            build-essential ca-certificates curl debhelper devscripts sudo opam \
            ffmpeg pandoc fakeroot git openssh-client unzip gnupg dirmngr apt-transport-https && \
    apt-get -y autoclean && \
    apt-get -y clean

RUN adduser opam

USER opam

RUN \
    opam init -y --disable-sandboxing --bare && \
    opam switch create $OCAML_VERSION ocaml-variants.$OCAML_VERSION+options ocaml-option-flambda && \
    opam update -y && \
    opam clean

WORKDIR /tmp

RUN git clone https://github.com/smimram/ocaml-pandoc.git ocaml-pandoc && \
    opam pin -y add ocaml-pandoc

RUN git clone https://github.com/savonet/liquidsoap-full.git

WORKDIR /tmp/liquidsoap-full

RUN make init && make update

RUN cat PACKAGES.default | grep '^ocaml' > /tmp/modules && \ 
    cat /tmp/modules | while read i; do find $i | grep '\.opam$'; done | while read i; do basename $i | cut -d'.' -f 1; done > /tmp/packages

RUN \
    cat /tmp/modules | while read module; do \
        cd $module && opam pin add -y --no-action . && cd .. \
      fi; \
    done && cd liquidsoap && opam pin add -y --no-action .

ENV EXT_PACKAGES="$EXTRA_PACKAGES camomile ocurl irc-client-unix osc-unix inotify prometheus-liquidsoap tsdl sdl-liquidsoap tls-liquidsoap syslog memtrace mem_usage ssl posix-time2 yaml js_of_ocaml js_of_ocaml-ppx re sqlite3 pandoc-include odoc"

RUN eval $(opam env) && opam list --short --external --resolve="`echo $EXT_PACKAGES | sed -e 's# #,#g'`,`cat /tmp/packages | while read i; do printf "$i,"; done`,liquidsoap" > /tmp/deps

USER root

RUN \
    cat /tmp/deps | xargs apt-get install -y --no-install-recommends && \
    apt-get -y autoclean && apt-get -y clean

USER opam

RUN \
    eval $(opam config env) && \
    PACKAGES=`cat /tmp/packages | xargs echo` && \
    opam install --no-depexts -y liquidsoap $PACKAGES $EXT_PACKAGES && \
    opam uninstall --no-depexts -y liquidsoap-lang $PACKAGES ffmpeg-avutil && \
    opam clean

FROM $BASE_IMAGE
COPY --from=base / /
