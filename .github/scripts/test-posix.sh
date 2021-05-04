#!/bin/sh

set -e

sudo -i -u opam /bin/sh << EOF
cd /tmp/liquidsoap-full/liquidsoap
eval $(opam config env)
make test
EOF
