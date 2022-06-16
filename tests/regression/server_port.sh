#!/bin/sh

nc -l 2000 &
pid=$!

cleanup() {
  kill $pid
}

trap cleanup EXIT

../../src/bin/liquidsoap.exe --no-stdlib ../../src/libs/stdlib.liq --check 'harbor.http.register(port=2000,method="GET","/foo",(fun (~protocol=_,~data=_,~headers=_,_) -> "test"))'
