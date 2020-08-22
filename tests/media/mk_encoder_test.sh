#!/bin/sh

BASEPATH=$0
BASEDIR=`dirname $0`
CWD=`cd $BASEDIR && pwd`

FILE=$1
FORMAT=`echo $FILE | cut -d '.' -f 1 | sed -e "s#@#%#g" | sed -e 's#DOT#.#g'`

SOURCE=$2

cat "${CWD}/test_encoder.liq.in" | sed -e "s#@FORMAT@#${FORMAT}#g" | sed -e "s#@SOURCE@#${SOURCE}#g" >| "${CWD}/test_encoder.liq"

