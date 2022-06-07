#!/bin/sh

BASEDIR=`dirname $0`
PWD=`cd $BASEDIR && pwd`

nc -l 2000 &
pid=$!

function cleanup() {
  kill $pid;
}

trap cleanup EXIT

${PWD}/../../src/liquidsoap --no-stdlib --check 'harbor.http.register(port=2000,method="GET","/foo",(fun (~protocol=_,~data=_,~headers=_,_) -> "test"))'
