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

TIMEOUT=90
LOG_FILE=`mktemp`
LIQ_PID=
MAIN_PID=$$
WATCH_PID=

trap cleanup 0 1 2

cleanup() {
  rm -rf "${LOG_FILE}"
}

print_timeout() {
  echo -e "\033[1;34m[timeout]\033[0m"
  cat "${LOG_FILE}"
  exit 1
}

timeout() {
  kill -9 $WAITD_PID >/dev/null 2>&1
  kill -9 $PID >/dev/null 2>&1 && print_timeout
}

watchit() {
  sleep $TIMEOUT &
  wait $!
  kill -ALRM $MAIN_PID
}

watchit &
WATCH_PID=$!
trap timeout ALRM

echo -en "Running test \033[1m${TEST_NAME}\033[0m... "

${CMD} < "${PWD}/${TEST}"  > "${LOG_FILE}" 2>&1 &

PID=$!
wait $PID
STATUS=$?

kill -ALRM $WATCH_PID 2>/dev/null
wait $WATCH_PID

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
