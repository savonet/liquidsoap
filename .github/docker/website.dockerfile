FROM ghcr.io/savonet/liquidsoap:ci-v2-debian_trixie-5.4.0

MAINTAINER The Savonet Team <contact@liquidsoap.info>

USER root

# The following could also be interesting but compilation of the website takes
# forever: amb-plugins caps cmt csladspa fomp lsp-plugins-ladspa lsp-plugins-lv2

RUN apt-get update && apt-get -y dist-upgrade && apt-get -y install openssh-client && apt-get install -y abgate calf-plugins swh-lv2 swh-plugins tap-plugins zam-plugins frei0r-plugins

# This is until the next image rebuild:
RUN apt-get -y install libcurl4-gnutls-dev

USER opam

WORKDIR /tmp

RUN git clone --recurse-submodules=no https://github.com/savonet/liquidsoap-full.git

WORKDIR /tmp/liquidsoap-full

# Clone liquidsoap into the location the website Makefile expects
RUN rm -rf liquidsoap && \
    git clone https://github.com/savonet/liquidsoap.git

RUN eval "$(opam config env)" && \
    export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/share/pkgconfig/pkgconfig && \
    cd liquidsoap && \
    dune build && \
    dune build --release src/js

WORKDIR /tmp/liquidsoap-full/website

RUN eval "$(opam config env)" && opam install -y odoc && make clean && git pull && make dist
