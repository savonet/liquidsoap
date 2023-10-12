#!/bin/sh

FORMAT="$1"
SOURCE="$2"
FILE="$3"

sed -e "s#@FORMAT@#${FORMAT}#g" "encoder_${SOURCE}.liq.in" |
  sed -e "s#@FILE@#${FILE}#g"
