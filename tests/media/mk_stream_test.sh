#!/bin/sh

BASEPATH=$0
BASEDIR=`dirname $0`
CWD=`cd $BASEDIR && pwd`

FILE=$1
FORMAT=`echo $FILE | cut -d '.' -f 1 | sed -e "s#@#%#g"`

MODE=$2

cat "${CWD}/test_stream_${MODE}.liq.in" | sed -e "s#@FORMAT@#${FORMAT}#g" >| "${CWD}/test_stream_${MODE}.liq"
