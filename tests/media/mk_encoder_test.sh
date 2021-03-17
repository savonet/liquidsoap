#!/bin/sh

BASEPATH=$0
BASEDIR=`dirname $0`
CWD=`cd $BASEDIR && pwd`

FORMAT=$1
SOURCE=$2

cat "${CWD}/test_encoder.liq.in" | sed -e "s#@FORMAT@#${FORMAT}#g" | sed -e "s#@SOURCE@#${SOURCE}#g"

