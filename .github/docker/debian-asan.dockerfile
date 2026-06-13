ARG BASE_IMAGE=debian:testing

# Stage 1: OCaml compiler with AddressSanitizer option
FROM $BASE_IMAGE AS ocaml

MAINTAINER The Savonet Team <contact@liquidsoap.info>

ARG OCAML_VERSION=5.4.0

ENV DEBIAN_FRONTEND=noninteractive
ENV ASAN_OPTIONS="detect_leaks=0:detect_stack_use_after_return=0:detect_container_overflow=0:protect_shadow_gap=0"

USER root

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        build-essential ca-certificates curl git rsync unzip && \
    apt-get -y autoclean && apt-get -y clean

RUN printf "\ny\n" | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"

RUN useradd -m opam

USER opam

RUN opam init -y --disable-sandboxing --bare && \
    opam switch create $OCAML_VERSION ocaml-variants.$OCAML_VERSION+options ocaml-option-address-sanitizer && \
    opam update -y && \
    opam clean

# Stage 2: Clone liquidsoap and pin all synced modules
FROM ocaml AS pinned

USER root

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        build-essential ca-certificates curl git pkg-config unzip && \
    apt-get -y autoclean && apt-get -y clean

ARG LIQUIDSOAP_SHA=main

WORKDIR /tmp

USER opam

RUN git clone https://github.com/smimram/ocaml-pandoc.git ocaml-pandoc && \
    opam pin -y add ocaml-pandoc

RUN git clone https://github.com/savonet/liquidsoap.git && \
    cd liquidsoap && git fetch origin "$LIQUIDSOAP_SHA" && git checkout "$LIQUIDSOAP_SHA"

RUN find /tmp/liquidsoap/src/modules/synced -maxdepth 1 -mindepth 1 -type d | \
    while read dir; do opam pin add -y --no-action "$dir"; done && \
    cd /tmp/liquidsoap && opam pin add -y --no-action .

# Stage 3: Install APT and opam dependencies
FROM pinned AS build

# All synced module opam package names
ENV OPTIONAL_OPAM_PACKAGES="\
    alsa ao faad fdkaac frei0r jack ladspa lame lo mad mem_usage metadata mm \
    portaudio pulseaudio samplerate shine soundtouch srt \
    ffmpeg ffmpeg-av ffmpeg-avcodec ffmpeg-avdevice ffmpeg-avfilter ffmpeg-avutil ffmpeg-swresample ffmpeg-swscale \
    flac ogg opus speex theora vorbis"

ENV EXT_PACKAGES="\
    camomile ocurl irc-client-unix osc-unix inotify prometheus-liquidsoap \
    tls-liquidsoap syslog memtrace ssl posix-time2 \
    yaml js_of_ocaml js_of_ocaml-ppx re sqlite3 pandoc-include"

ENV APT_PACKAGES="\
    aspcud autoconf automake rsync build-essential ca-certificates curl \
    debhelper devscripts sudo fakeroot git openssh-client pkg-config unzip \
    gnupg dirmngr apt-transport-https \
    gcc g++ \
    libavcodec-dev libavformat-dev libavutil-dev libavfilter-dev \
        libswresample-dev libswscale-dev libavdevice-dev ffmpeg \
    libasound2-dev \
    libao-dev \
    libfaad-dev \
    libfdk-aac-dev \
    frei0r-plugins-dev \
    libjack-jackd2-dev \
    ladspa-sdk \
    libmp3lame-dev \
    liblo-dev \
    libmad0-dev \
    portaudio19-dev \
    libpulse-dev \
    libsamplerate0-dev \
    libshine-dev \
    libsoundtouch-dev \
    libsrt-dev \
    libflac-dev libogg-dev libopus-dev libspeex-dev libtheora-dev libvorbis-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libsqlite3-dev \
    libyaml-dev"

USER root

# Enable non-free (for libfdk-aac-dev) and Debian debug symbol repository
RUN apt-get update && \
    apt-get install -y --no-install-recommends lsb-release && \
    sed -i 's/^Components: main$/Components: main contrib non-free non-free-firmware/' \
        /etc/apt/sources.list.d/debian.sources && \
    echo "deb http://debug.mirrors.debian.org/debian-debug/ $(lsb_release -cs)-debug main" \
        > /etc/apt/sources.list.d/debug.list && \
    apt-get update

RUN apt-get install -y --no-install-recommends $APT_PACKAGES && \
    apt-get -y autoclean && apt-get -y clean

# Debug symbols: install -dbgsym for lib* packages that have one
RUN pkgs=$(dpkg -l $APT_PACKAGES 2>/dev/null | awk '/^ii/ && $2 ~ /^lib/{print $2"-dbgsym"}') && \
    available=$(apt-cache show $pkgs 2>/dev/null | awk '/^Package:/{print $2}') && \
    apt-get install -y --no-install-recommends $available

RUN arch=$(dpkg --print-architecture) && \
    curl -fsSL "https://github.com/jgm/pandoc/releases/download/3.10/pandoc-3.10-1-${arch}.deb" -o /tmp/pandoc.deb && \
    dpkg -i /tmp/pandoc.deb && \
    rm /tmp/pandoc.deb

USER opam

RUN eval $(opam env) && \
    opam install --no-depexts -y --deps-only liquidsoap $OPTIONAL_OPAM_PACKAGES $EXT_PACKAGES && \
    rm -rf /tmp/liquidsoap /tmp/ocaml-pandoc && \
    opam clean

USER root

RUN echo 'Defaults secure_path="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"' \
    > /etc/sudoers.d/secure_path

FROM $BASE_IMAGE
ENTRYPOINT bash
COPY --from=build / /
