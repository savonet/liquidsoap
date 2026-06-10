ARG OCAML_VERSION=5.4.1
FROM ghcr.io/ocaml-cross/windows-x64-base:${OCAML_VERSION}

# Build MXE packages required by the Windows opam packages.
# Each package is a separate layer to maximise Docker cache reuse.
# ffmpeg is last because it is the slowest build and the most stable,
# so its cache is only invalidated when strictly necessary.

RUN cd /usr/src/mxe && make openssl
RUN cd /usr/src/mxe && make curl
RUN cd /usr/src/mxe && make libsrt
RUN cd /usr/src/mxe && make portaudio
RUN cd /usr/src/mxe && make libsamplerate
RUN cd /usr/src/mxe && make dlfcn-win32
RUN cd /usr/src/mxe && make libao
RUN cd /usr/src/mxe && make ffmpeg

# Pre-install opam dependencies so the CI build step only compiles liquidsoap.

USER opam

COPY .github/opam/liquidsoap-windows.opam /tmp/liquidsoap-windows.opam

RUN eval $(opam env) && \
    opam install --deps-only -y /tmp/liquidsoap-windows.opam && \
    opam clean
