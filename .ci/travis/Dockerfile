ARG DOCKER_TAG
FROM savonet/liquidsoap-full:$DOCKER_TAG

MAINTAINER The Savonet Team <savonet-users@lists.sourceforge.net>

# This looks like a bug..
ARG DOCKER_TAG
ARG TRAVIS_BRANCH
ARG TRAVIS_COMMIT
ARG TRAVIS_COMMIT_SHORT
ARG TRAVIS_PULL_REQUEST
ARG TRAVIS_PULL_REQUEST_BRANCH
ARG TRAVIS_BUILD_NUMBER

USER root

RUN apt-get update && apt-get -y install ffmpeg gstreamer1.0-libav gstreamer1.0-plugins-ugly gstreamer1.0-plugins-good gstreamer1.0-plugins-base gstreamer1.0-plugins-bad

USER opam

WORKDIR /tmp/liquidsoap-full

RUN eval $(opam config env) && git pull && make clean && make update && \
     cd liquidsoap && git reset --hard && git checkout master && git pull && \
     git fetch origin $TRAVIS_COMMIT && git checkout $TRAVIS_COMMIT && \
     ./.ci/checkout-deps.sh && \
     cd .. && ./bootstrap && \
     export PKG_CONFIG_PATH=/usr/share/pkgconfig/pkgconfig && \
     ./configure --prefix=/usr --includedir=\${prefix}/include --mandir=\${prefix}/share/man \
                 --infodir=\${prefix}/share/info --sysconfdir=/etc --localstatedir=/var \
                 --with-camomile-data-dir=/usr/share/liquidsoap/camomile && \
     make clean && make && cd liquidsoap && make test && make doc

WORKDIR /tmp/liquidsoap-full/liquidsoap

RUN eval $(opam config env) && make test

USER root

ADD .ci/travis/debian/ /tmp/liquidsoap-full/liquidsoap/debian

ADD .ci/travis/build-deb.sh /tmp/liquidsoap-full/liquidsoap/

RUN chown -R opam /tmp/liquidsoap-full/liquidsoap/debian

USER opam

WORKDIR /tmp/liquidsoap-full/liquidsoap

RUN ./build-deb.sh "$TRAVIS_COMMIT_SHORT" "$TRAVIS_BRANCH" "$TRAVIS_PULL_REQUEST_BRANCH" "$TRAVIS_PULL_REQUEST" "$DOCKER_TAG" "$TRAVIS_BUILD_NUMBER"
