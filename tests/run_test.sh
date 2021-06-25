#!/bin/bash

BASEPATH=$0
BASEDIR=`dirname $0`
PWD=`cd $BASEDIR && pwd`

TIMEOUT=10m

run_test() {
  PWD=$1
  CMD=$2
  TEST=$3
  TEST_NAME=$4

  if [ -z "${TEST_NAME}" ]; then
    TEST_NAME=${TEST}
  fi

  LOG_FILE=`mktemp`

  trap cleanup 0 1 2

  cleanup() {
    rm -rf "${LOG_FILE}"
  }

  on_timeout() {
    echo -e "\033[1;34m[timeout]\033[0m"
    cat "${LOG_FILE}"
    exit 1
  }

  trap on_timeout 15

  echo -en "Running test \033[1m${TEST_NAME}\033[0m... "

  ${CMD} < "${PWD}/${TEST}"  > "${LOG_FILE}" 2>&1 &

  PID=$!
  wait $PID
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
}

export -f run_test

on_term() {
  exit 1
}

trap on_term INT

timeout -s 15 "${TIMEOUT}" bash -c "run_test \"$PWD\" \"$1\" \"$2\" \"$3\"" &

pid=$!
wait $pid
