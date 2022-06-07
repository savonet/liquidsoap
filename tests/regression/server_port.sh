#!/bin/sh

LIQUIDSOAP=$1

nc -l 2000 &
pid=$!

function cleanup() {
  kill $pid;
}

trap cleanup EXIT

${LIQUIDSOAP} --check 'harbor.http.register(port=2000,method="GET","/foo",(fun (~protocol=_,~data=_,~headers=_,_) -> "test"))'
