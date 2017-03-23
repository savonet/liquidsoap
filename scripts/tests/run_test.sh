#!/bin/bash

LIQ=$1
TEST=$2

echo -en "Running test \033[1m${TEST}\033[0m... "
(cat ${TEST} | ${LIQ} -q ../pervasives.liq -  >/dev/null 2>&1 && echo -e "\033[0;32m[ok]\033[0m") || echo -e "\033[0;31m[failed]\033[0m"
