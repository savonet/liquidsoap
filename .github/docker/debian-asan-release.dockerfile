FROM debian:sid

ARG DEB_FILE
COPY ${DEB_FILE} /tmp/liquidsoap.deb

ENV DEBIAN_FRONTEND=noninteractive
ENV ASAN_OPTIONS="detect_leaks=0:detect_stack_use_after_return=0:detect_container_overflow=0:protect_shadow_gap=0"

RUN sed -i 's/^Components: main$/Components: main contrib non-free non-free-firmware/' \
        /etc/apt/sources.list.d/debian.sources && \
    apt-get update && \
    apt-get install -y --no-install-recommends /tmp/liquidsoap.deb && \
    rm /tmp/liquidsoap.deb && \
    apt-get -y clean

ENV ASAN_OPTIONS=""

ENTRYPOINT ["/usr/bin/liquidsoap"]
