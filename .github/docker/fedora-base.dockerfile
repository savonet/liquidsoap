ARG BASE_IMAGE=fedora:44

FROM $BASE_IMAGE AS base

MAINTAINER The Savonet Team <contact@liquidsoap.info>

ARG OCAML_VERSION=5.5.1
ARG OCAML_PATCH_URL=https://github.com/toots/ocaml/archive/4552e40b1121fd64a2de0603246ed08a9c8910d8.tar.gz

USER root

RUN dnf install -y \
      bzip2 curl diffutils findutils gcc gcc-c++ git gnupg2 make \
      openssh-clients openssl patch pkgconf-pkg-config rpm-build rsync \
      sudo unzip which && \
    dnf clean all

RUN printf "\ny\n" | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"

RUN useradd -m opam

USER opam

COPY .github/docker/setup-ocaml.sh /tmp/setup-ocaml.sh

RUN sh /tmp/setup-ocaml.sh

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
    xargs -I{} basename {} .opam | grep -Ev "^(speex|theora|dssi|shine)$" > /tmp/packages

COPY .github/docker/ext-packages /tmp/ext-packages

RUN eval $(opam env) && EXT_PACKAGES=$(xargs < /tmp/ext-packages) && opam list --short --external --resolve="`echo $EXT_PACKAGES | sed -e 's# #,#g'`,`cat /tmp/packages | while read i; do printf "$i,"; done`,liquidsoap" > /tmp/deps

USER root

RUN cat /tmp/deps | xargs dnf install -y && dnf clean all

USER opam

RUN \
    eval $(opam config env) && \
    EXT_PACKAGES=$(xargs < /tmp/ext-packages) && \
    PACKAGES=`cat /tmp/packages | xargs echo` && \
    opam install --no-depexts -y liquidsoap $PACKAGES $EXT_PACKAGES && \
    opam uninstall --no-depexts -y liquidsoap-lang $PACKAGES ffmpeg-avutil && \
    opam pin list --short | grep -v '^ocaml-compiler$' | xargs -r opam pin remove -y && \
    rm -rf /tmp/liquidsoap && \
    opam clean

# The compiler pin has to survive the pin cleanup above, or the patched compiler is
# silently replaced by the release one.
RUN test -z "$OCAML_PATCH_URL" || \
    (eval $(opam env) && opam pin list --short | grep -qx ocaml-compiler)

USER root

RUN echo 'Defaults secure_path="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"' > /etc/sudoers.d/secure_path

FROM $BASE_IMAGE
ENTRYPOINT bash
COPY --from=base / /
