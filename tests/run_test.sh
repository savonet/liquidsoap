#!/bin/bash

BASEPATH=$0
BASEDIR=`dirname $0`
PWD=`cd $BASEDIR && pwd`

CMD=$1
TEST=$2
TEST_NAME=$3

if [ -z "${TEST_NAME}" ]; then
  TEST_NAME=${TEST}
fi

LOG_FILE=`mktemp`

trap cleanup 0 1 2

cleanup() {
  rm -rf "${LOG_FILE}"
}

echo -en "Running test \033[1m${TEST_NAME}\033[0m... "

${CMD} < "${PWD}/${TEST}"  > "${LOG_FILE}" 2>&1

STATUS=$?

if [ "${STATUS}" == "0" ]; then
  echo -e "\033[0;32m[ok]\033[0m"
  exit 0
fi

if [ "${STATUS}" == "2" ]; then
    echo -e "\033[1;33m[skipped]\033[0m"
    exit 0
fi

echo -e "\033[0;31m[failed]\033[0m"
cat "${LOG_FILE}"
exit 1
