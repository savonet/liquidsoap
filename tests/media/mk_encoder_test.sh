#!/bin/sh

FORMAT=$1
SOURCE=$2
FILE=$3

cat "test_encoder.liq.in" | sed -e "s#@FORMAT@#${FORMAT}#g" | sed -e "s#@SOURCE@#${SOURCE}#g" | sed -e "s#@FILE@#${FILE}#g"
