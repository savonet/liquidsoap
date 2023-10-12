#!/bin/sh

FORMAT="$1"
SOURCE="$2"
FILE="$3"

sed -e "s#@FORMAT@#${FORMAT}#g" "encoder.liq.in" |
  sed -e "s#@SOURCE@#${SOURCE}#g" |
  sed -e "s#@FILE@#${FILE}#g"
