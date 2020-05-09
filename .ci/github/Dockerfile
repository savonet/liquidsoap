ARG DOCKER_TAG
FROM savonet/liquidsoap-full:$DOCKER_TAG

MAINTAINER The Savonet Team <savonet-users@lists.sourceforge.net>

# This looks like a bug..
ARG DOCKER_TAG
ARG GITHUB_SHA

USER root

USER opam

WORKDIR /tmp/liquidsoap-full

RUN eval $(opam config env) && git pull && make clean && make update && \
     cd liquidsoap && git reset --hard && git checkout master && git pull && \
     git fetch origin $GITHUB_SHA && git checkout $GITHUB_SHA && \
     ./.ci/checkout-deps.sh && \
     cd .. && ./bootstrap && \
     export PKG_CONFIG_PATH=/usr/share/pkgconfig/pkgconfig && \
     ./configure --prefix=/usr --includedir=\${prefix}/include --mandir=\${prefix}/share/man \
                 --infodir=\${prefix}/share/info --sysconfdir=/etc --localstatedir=/var \
                 --with-camomile-data-dir=/usr/share/liquidsoap/camomile && \
     make clean && make && cd liquidsoap && make doc

WORKDIR /tmp/liquidsoap-full/liquidsoap

RUN eval $(opam config env) && make test
