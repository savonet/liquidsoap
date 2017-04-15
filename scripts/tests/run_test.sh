#!/bin/bash

CMD=$1
TEST=$2

echo -en "Running test \033[1m${TEST}\033[0m... "
cat ${TEST} | ${CMD}  >/dev/null 2>&1

STATUS=$?

if [ "${STATUS}" == "0" ]; then
  echo -e "\033[0;32m[ok]\033[0m"
fi

if [ "${STATUS}" == "1" ]; then
  echo -e "\033[0;31m[failed]\033[0m"
fi

if [ "${STATUS}" == "2" ]; then
  echo -e "\033[1;33m[skipped]\033[0m"
fi
