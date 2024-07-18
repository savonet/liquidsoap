FROM savonet/liquidsoap-ci:debian_bookworm_amd64

MAINTAINER The Savonet Team <savonet-users@lists.sourceforge.net>

USER root

# The following could also be interesting but compilation of the website takes
# forever: amb-plugins caps cmt csladspa fomp lsp-plugins-ladspa lsp-plugins-lv2

RUN apt-get update && apt-get -y dist-upgrade && apt-get -y install openssh-client && apt-get install -y abgate calf-plugins swh-lv2 swh-plugins tap-plugins zam-plugins frei0r-plugins

# This is until the next image rebuild:
RUN apt-get -y install libcurl4-gnutls-dev

USER opam

# This is until the next image rebuild:
RUN eval "$(opam config env)" && opam install -y ocurl js_of_ocaml js_of_ocaml-ppx

WORKDIR /tmp/liquidsoap-full

RUN rm -rf website/savonet.github.io

RUN git remote set-url origin https://github.com/savonet/liquidsoap-full.git && \
    git fetch --recurse-submodules=no && git checkout origin/master -- Makefile.git && \
    git reset --hard && \
    git pull && \
    git submodule init ocaml-metadata && \
    git submodule update ocaml-metadata && \
    make public

RUN eval "$(opam config env)" && make clean

RUN eval "$(opam config env)" && cd ocaml-metadata && opam install -y . fileutils

RUN eval "$(opam config env)" && \
    git clone https://github.com/savonet/ocaml-posix.git && \
    cd ocaml-posix && \
    opam install -y .

RUN cd liquidsoap && \
    mv .git /tmp && \
    rm -rf * && \
    mv /tmp/.git . && \
    git reset --hard

RUN make public && make update

# TODO: Remove gstreamer from liquidsoap-full
RUN cat PACKAGES.default | grep -v gstreamer > PACKAGES

RUN eval "$(opam config env)" && \
    export PKG_CONFIG_PATH=/usr/share/pkgconfig/pkgconfig && \
    touch liquidsoap/configure && \
    ./configure --enable-graphics && \
    rm liquidsoap/configure && \
    export OCAMLPATH="$(cat .ocamlpath)" && \
    cd liquidsoap && \
    dune build && \
    dune build --release src/js

WORKDIR /tmp/liquidsoap-full/website

RUN eval "$(opam config env)" && opam install -y odoc && make clean  && git pull && make dist
