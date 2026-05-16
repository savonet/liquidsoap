ARG BASE_IMAGE

# Stage 1: OCaml compiler
FROM $BASE_IMAGE AS ocaml

MAINTAINER The Savonet Team <savonet-users@lists.sourceforge.net>

ARG OCAML_VERSION=4.14.2

ENV DEBIAN_FRONTEND=noninteractive

USER root

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
            build-essential ca-certificates curl git unzip && \
    apt-get -y autoclean && apt-get -y clean

RUN printf "\ny\n" | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"

RUN useradd -m opam

USER opam

RUN opam init -y --disable-sandboxing --bare && \
    opam switch create $OCAML_VERSION ocaml-variants.$OCAML_VERSION+options ocaml-option-flambda && \
    opam update -y && \
    opam clean

# Stage 2: Clone liquidsoap-full and pin all packages
FROM ocaml AS pinned

USER root

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
            build-essential ca-certificates curl git pkg-config unzip && \
    apt-get -y autoclean && apt-get -y clean

WORKDIR /tmp

USER opam

RUN git clone https://github.com/smimram/ocaml-pandoc.git ocaml-pandoc && \
    opam pin -y add ocaml-pandoc

RUN git clone https://github.com/savonet/liquidsoap-full.git

WORKDIR /tmp/liquidsoap-full

RUN make init && make update

RUN cat PACKAGES.default | grep '^ocaml' > /tmp/modules && \
    cat /tmp/modules | while read i; do find $i | grep '\.opam$'; done | while read i; do basename $i | cut -d'.' -f 1; done > /tmp/packages

RUN cat /tmp/modules | while read module; do \
        cd $module && opam pin add -y --no-action . && cd ..; \
    done && cd liquidsoap && opam pin add -y --no-action .

# Stage 3: Install ffmpeg-liquidsoap and static opam packages
FROM pinned AS static-packages

ENV STATIC_PACKAGES="fdkaac ffmpeg flac lame ogg opus shine srt vorbis"
ENV PKG_CONFIG_PATH=/usr/local/lib/pkgconfig

USER root

# Static FFmpeg build for Liquidsoap
RUN apt-get update && apt-get install -y ca-certificates curl gnupg && \
    curl -fsSL https://liquidsoap.info/ffmpeg-static-build/key.asc \
      | gpg --dearmor -o /etc/apt/trusted.gpg.d/liquidsoap-ffmpeg.gpg && \
    echo "deb https://liquidsoap.info/ffmpeg-static-build stable main" \
      > /etc/apt/sources.list.d/liquidsoap-ffmpeg.list && \
    apt-get update && apt-get install -y ffmpeg-liquidsoap libffi-dev

# Wrap ld to inject -Bsymbolic when building shared objects.
# Needed on ARM64: FFmpeg/x265 NEON assembly uses non-GOT ADRP relocations
# against globally-visible symbols, which ld rejects when making a .so.
RUN mv /usr/bin/ld /usr/bin/ld.real && \
    printf '#!/bin/sh\nfor a; do [ "$a" = "-shared" ] && exec /usr/bin/ld.real -Bsymbolic "$@"; done\nexec /usr/bin/ld.real "$@"\n' \
      > /usr/bin/ld && \
    chmod +x /usr/bin/ld

USER opam

RUN eval $(opam env) && \
    opam install --no-depexts -y $STATIC_PACKAGES && \
    opam clean

USER root

RUN mv /usr/bin/ld.real /usr/bin/ld

# Stage 4: Install remaining external and opam dependencies
FROM static-packages AS build

ENV EXT_PACKAGES="camomile ocurl irc-client-unix osc-unix inotify prometheus-liquidsoap tsdl sdl-liquidsoap tls-liquidsoap syslog memtrace mem_usage ssl posix-time2 yaml js_of_ocaml js_of_ocaml-ppx re sqlite3 pandoc-include odoc"

USER opam

RUN eval $(opam env) && \
    STATIC_RE=$(echo $STATIC_PACKAGES | tr ' ' '|') && \
    PKGS=$(cat /tmp/packages | grep -Ev "^($STATIC_RE)$" | while read i; do printf "$i,"; done) && \
    opam list --short --external --resolve="`echo $EXT_PACKAGES | sed -e 's# #,#g'`,$PKGS,liquidsoap" > /tmp/deps

USER root

RUN apt-get update && \
    apt-get install -y --no-install-recommends aspcud autoconf automake rsync \
            build-essential ca-certificates curl debhelper devscripts sudo \
            fakeroot git openssh-client pkg-config unzip pandoc \
            gnupg dirmngr apt-transport-https && \
    cat /tmp/deps | xargs apt-get install -y --no-install-recommends && \
    apt-get -y autoclean && apt-get -y clean

USER opam

RUN eval $(opam env) && \
    PACKAGES=$(cat /tmp/packages | grep -Ev "^(speex|theora)$" | xargs echo) && \
    opam install --no-depexts -y liquidsoap $PACKAGES $EXT_PACKAGES && \
    opam uninstall --no-depexts -y liquidsoap-lang $PACKAGES ffmpeg-avutil && \
    opam clean

FROM $BASE_IMAGE
ENTRYPOINT bash
COPY --from=build / /
