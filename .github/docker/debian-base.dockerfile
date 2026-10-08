ARG BASE_IMAGE

# Stage 1: OCaml compiler
FROM $BASE_IMAGE AS ocaml

MAINTAINER The Savonet Team <contact@liquidsoap.info>

ARG OCAML_VERSION=4.14.2
ARG OCAML_PATCH_URL

ENV DEBIAN_FRONTEND=noninteractive

USER root

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
            build-essential ca-certificates curl git rsync unzip && \
    apt-get -y autoclean && apt-get -y clean

RUN printf "\ny\n" | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"

RUN useradd -m opam

USER opam

COPY .github/docker/setup-ocaml.sh /tmp/setup-ocaml.sh

RUN sh /tmp/setup-ocaml.sh ocaml-option-flambda

# Stage 2: Install ffmpeg-liquidsoap and static opam packages
FROM ocaml AS static-packages

ENV STATIC_RE="^(fdkaac|ffmpeg|flac|lame|ogg|opus|shine|srt|vorbis)(<.*)?\$"
ENV PKG_CONFIG_PATH=/usr/local/lib/pkgconfig

USER root

# Static FFmpeg build for Liquidsoap
RUN apt-get update && apt-get install -y ca-certificates curl gnupg pkg-config && \
    curl -fsSL https://liquidsoap.info/ffmpeg-static-build/key.asc \
      | gpg --dearmor -o /etc/apt/trusted.gpg.d/liquidsoap-ffmpeg.gpg && \
    echo "deb https://liquidsoap.info/ffmpeg-static-build stable main" \
      > /etc/apt/sources.list.d/liquidsoap-ffmpeg.list && \
    apt-get update && apt-get install -y libffi-dev ffmpeg-liquidsoap ffmpeg-liquidsoap-tools

# Wrap ld to inject -Bsymbolic when building shared objects.
# Needed on ARM64: FFmpeg/x265 NEON assembly uses non-GOT ADRP relocations
# against globally-visible symbols, which ld rejects when making a .so.
RUN mv /usr/bin/ld /usr/bin/ld.real && \
    printf '#!/bin/sh\nfor a; do [ "$a" = "-shared" ] && exec /usr/bin/ld.real -Bsymbolic "$@"; done\nexec /usr/bin/ld.real "$@"\n' \
      > /usr/bin/ld && \
    chmod +x /usr/bin/ld

COPY .github/docker/ext-packages /tmp/ext-packages

USER opam

RUN eval $(opam env) && \
    opam install --no-depexts -y $(grep -E "$STATIC_RE" /tmp/ext-packages | xargs) && \
    opam clean

USER root

# Stage 3: Install remaining external and opam dependencies
FROM static-packages AS build

USER opam

RUN eval $(opam env) && \
    opam pin add -n -y lo git+https://github.com/savonet/ocaml-lo.git && \
    PKGS=$(grep -Ev "$STATIC_RE" /tmp/ext-packages | xargs | tr ' ' ',') && \
    opam list --short --external --resolve="$PKGS,liquidsoap" > /tmp/deps

USER root

RUN apt-get update && \
    apt-get install -y --no-install-recommends aspcud autoconf automake rsync \
            build-essential ca-certificates curl debhelper devscripts sudo \
            fakeroot git openssh-client pkg-config unzip \
            gnupg dirmngr apt-transport-https && \
    cat /tmp/deps | xargs apt-get install -y --no-install-recommends && \
    apt-get -y autoclean && apt-get -y clean

RUN arch=$(dpkg --print-architecture) && \
    curl -fsSL "https://github.com/jgm/pandoc/releases/download/3.10/pandoc-3.10-1-${arch}.deb" -o /tmp/pandoc.deb && \
    dpkg -i /tmp/pandoc.deb && \
    rm /tmp/pandoc.deb

USER opam

RUN eval $(opam env) && \
    opam install --no-depexts -y liquidsoap $(xargs < /tmp/ext-packages) && \
    opam uninstall --no-depexts -y liquidsoap-lang && \
    opam clean

USER root

RUN echo 'Defaults secure_path="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"' > /etc/sudoers.d/secure_path

FROM $BASE_IMAGE
ENTRYPOINT bash
COPY --from=build / /
