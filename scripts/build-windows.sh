#!/bin/sh

OPAM_PKG=cry-windows,faad-windows,flac-windows,mad-windows,lame-windows,opus-windows,samplerate-windows,speex-windows,taglib-windows,theora-windows,vorbis-windows,winsvc-windows,ssl-windows,yojson-windows,duppy-windows,mm-windows,dtools-windows,pcre-windows,camomile-windows

for system in win32 win64; do
  docker build -t savonet/liquidsoap-${system} --build-arg OPAM_PKG=${OPAM_PKG} -f Dockerfile.${system} .
  id=$(docker create savonet/liquidsoap-${system})
  docker cp $id:/tmp/liquidsoap-${system}.zip ..
  docker rm -v $id
done
