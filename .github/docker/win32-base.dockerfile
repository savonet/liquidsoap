ARG OCAML_VERSION=5.4.1
FROM ghcr.io/ocaml-cross/windows-x64-base:${OCAML_VERSION} AS builder

# Install missing build-time prerequisites for MXE packages.
# python3-packaging is required by glib (a build dep of curl and others).
RUN apt-get update && \
    apt-get install -y --no-install-recommends python3-packaging && \
    apt-get clean

# Update MXE to get the latest package recipes (e.g. ffmpeg 7.1.1).
RUN cd /usr/src/mxe && git fetch origin && git reset --hard origin/master

# Build MXE packages required by the Windows opam packages.
# Each package is a separate layer to maximise Docker cache reuse.
# ffmpeg is last because it is the slowest build and the most stable,
# so its cache is only invalidated when strictly necessary.

RUN cd /usr/src/mxe && make openssl
RUN cd /usr/src/mxe && make curl
RUN cd /usr/src/mxe && make libsrt
RUN cd /usr/src/mxe && make jack
RUN cd /usr/src/mxe && make portaudio
RUN cd /usr/src/mxe && make libsamplerate
RUN cd /usr/src/mxe && make dlfcn-win32
RUN cd /usr/src/mxe && make libao
RUN cd /usr/src/mxe && make ogg
RUN cd /usr/src/mxe && make flac
RUN cd /usr/src/mxe && make vorbis
RUN cd /usr/src/mxe && make opus
RUN cd /usr/src/mxe && make faad2
RUN cd /usr/src/mxe && make lame
RUN cd /usr/src/mxe && make libmad
RUN cd /usr/src/mxe && make fdk-aac
RUN cd /usr/src/mxe && make ffmpeg

# Install xvfb to provide a virtual display for the initial Wine setup.
RUN apt-get update && apt-get install -y --no-install-recommends xvfb xauth && apt-get clean

# Install the latest opam release, answering prompts non-interactively.
RUN printf "\ny\n" | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"

# Pre-install opam dependencies so the CI build step only compiles liquidsoap.

USER opam

COPY .github/opam/liquidsoap-windows.opam /tmp/liquidsoap-windows.opam

RUN xvfb-run -a wineboot --init

RUN eval $(opam env) && \
    opam repository set-url windows https://github.com/ocaml-cross/opam-cross-windows.git && \
    opam repo add archive git+https://github.com/ocaml/opam-repository-archive && \
    opam update && \
    opam install --deps-only -y /tmp/liquidsoap-windows.opam && \
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
