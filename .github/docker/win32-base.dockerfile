ARG DEBIAN_VERSION=bookworm

FROM debian:${DEBIAN_VERSION}-slim AS builder

ARG OCAML_VERSION=5.5.0

# MXE revision. This is pinned rather than tracked: MXE decides a package is
# stale by comparing its install stamp against the mtime of the recipe, so a
# moving tree rebuilds the toolchain and every C library under it, and the
# library versions we ship change without anything in this repo changing.
ARG MXE_GIT_TAG=71cb7a3c56c0fde5d6aa806ad7623240e9128d57

ARG DEBIAN_FRONTEND=noninteractive

ENV MXE_DIR=/usr/src/mxe
ENV CROSS_TRIPLE=x86_64-w64-mingw32.static

# MXE's own requirements, from its docs/index.html. wine runs the built
# executable, xvfb gives the initial wineboot a display, zip bundles the
# release and gosu drops the CI step to the opam user.
RUN apt-get update && \
    apt-get install --no-install-recommends --yes \
      autoconf automake autopoint bash bison bzip2 ca-certificates flex \
      g++ g++-multilib gettext git gperf intltool libc6-dev-i386 \
      libclang-dev libgdk-pixbuf-2.0-dev libgl-dev libltdl-dev libpcre2-dev \
      libssl-dev libtool-bin libxml-parser-perl lzip make openssl p7zip-full \
      patch perl python3 python3-mako python3-packaging python3-pkg-resources \
      python3-setuptools python-is-python3 ruby sed sqlite3 unzip wget \
      xz-utils \
      curl gosu wine xvfb xauth zip && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

RUN git clone https://github.com/mxe/mxe.git "${MXE_DIR}" && \
    git -C "${MXE_DIR}" checkout "${MXE_GIT_TAG}"

# settings.mk is gitignored, so it is ours to own. The default target is
# narrowed so that a bare `make` cannot start building all of MXE.
RUN printf '%s\n' \
      "MXE_TARGETS := ${CROSS_TRIPLE}" \
      'MXE_USE_CCACHE :=' \
      'LOCAL_PKG_LIST := cc' \
      '.DEFAULT local-pkg-list:' \
      'local-pkg-list: $(LOCAL_PKG_LIST)' \
      > "${MXE_DIR}/settings.mk"

# The cross toolchain, on its own layer: it is the slowest thing here and it
# only moves when MXE_GIT_TAG does.
RUN cd "${MXE_DIR}" && make cc

# C libraries needed by the Windows opam packages. Each is a separate layer so
# that adding one does not rebuild the others; ffmpeg is last because it is the
# slowest and pulls the largest dependency chain.
RUN cd "${MXE_DIR}" && make openssl
RUN cd "${MXE_DIR}" && make curl
RUN cd "${MXE_DIR}" && make libsrt
RUN cd "${MXE_DIR}" && make jack
RUN cd "${MXE_DIR}" && make portaudio
RUN cd "${MXE_DIR}" && make libsamplerate
RUN cd "${MXE_DIR}" && make dlfcn-win32
RUN cd "${MXE_DIR}" && make libao
RUN cd "${MXE_DIR}" && make ogg
RUN cd "${MXE_DIR}" && make flac
RUN cd "${MXE_DIR}" && make vorbis
RUN cd "${MXE_DIR}" && make opus
RUN cd "${MXE_DIR}" && make faad2
RUN cd "${MXE_DIR}" && make lame
RUN cd "${MXE_DIR}" && make libmad
RUN cd "${MXE_DIR}" && make fdk-aac
RUN cd "${MXE_DIR}" && make ffmpeg

# The cross environment the final image also exports. The opam packages are
# built in this stage, and without it their configure scripts find no
# x86_64-w64-mingw32.static tools and quietly build for the host. MXE's bin
# directory is appended, never prepended, so it cannot shadow the host tools
# opam itself needs.
ENV PATH="${PATH}:/usr/src/mxe/usr/bin"
ENV WINEARCH=win64
ENV CMAKE_TOOLCHAIN_FILE="/usr/src/mxe/usr/x86_64-w64-mingw32.static/share/cmake/mxe-conf.cmake"
# PKG_CONFIG_PATH_default_windows is this repo's own dune-context convention;
# an opam package's configure script only reads the real one.
ENV PKG_CONFIG_PATH="/usr/src/mxe/usr/x86_64-w64-mingw32.static/lib/pkgconfig/"
ENV TOOLPREF64="/usr/src/mxe/usr/bin/x86_64-w64-mingw32.static-"
ENV PKG_CONFIG_PATH_default_windows="/usr/src/mxe/usr/x86_64-w64-mingw32.static/lib/pkgconfig/"
ENV PKG_CONFIG_default_windows="/usr/src/mxe/usr/bin/x86_64-w64-mingw32.static-pkg-config"

RUN printf "\ny\n" | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"

RUN useradd -g staff --create-home opam

USER opam

# Local opam overlay: camomile-embedded is not in opam-cross-windows.
COPY --chown=opam .github/opam /home/opam/opam-overlay

RUN xvfb-run -a wineboot --init

RUN opam init --auto-setup --disable-sandboxing --compiler="${OCAML_VERSION}" && \
    opam repository add windows https://github.com/ocaml-cross/opam-cross-windows.git && \
    opam repository add archive git+https://github.com/ocaml/opam-repository-archive && \
    opam repository add liquidsoap-devel /home/opam/opam-overlay

RUN eval $(opam env) && \
    opam install -y ocaml-windows && \
    opam clean

# Pre-install opam dependencies so the CI build step only compiles liquidsoap.
RUN eval $(opam env) && \
    opam install --deps-only -y /home/opam/opam-overlay/liquidsoap-windows.opam && \
    opam clean

FROM scratch
COPY --from=builder / /
ENV PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/src/mxe/usr/bin"
ENV WINEARCH="win64"
ENV CMAKE_TOOLCHAIN_FILE="/usr/src/mxe/usr/x86_64-w64-mingw32.static/share/cmake/mxe-conf.cmake"
ENV CROSS_TRIPLE="x86_64-w64-mingw32.static"
ENV PKG_CONFIG_PATH_default_windows="/usr/src/mxe/usr/x86_64-w64-mingw32.static/lib/pkgconfig/"
ENV PKG_CONFIG_default_windows="/usr/src/mxe/usr/bin/x86_64-w64-mingw32.static-pkg-config"
USER opam
