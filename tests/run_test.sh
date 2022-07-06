#!/bin/bash

BASEPATH=$0
BASEDIR=`dirname $0`

TIMEOUT=10m

run_test() {
  CMD=$1
  TEST=$2
  TEST_NAME=$3

  if [ -z "${TEST_NAME}" ]; then
    TEST_NAME=${TEST}
  fi

  TEST_NAME=`echo ${TEST_NAME} | sed -e 's#%#%%#g'`

  LOG_FILE=`mktemp`

  START_TIME="$(date +%s)"

  if [ -n "${GITHUB_ACTIONS}" ]; then
    WARNING_PREFIX="::warning file=${TEST},title=Test skipped::"
    ERROR_PREFIX="::error file=${TEST},title=Test failed::"
  fi

  trap cleanup 0 1 2

  cleanup() {
    rm -rf "${LOG_FILE}"
  }

  on_timeout() {
    T="$(($(date +%s)-START_TIME))"
    printf "${ERROR_PREFIX}Ran test \033[1m${TEST_NAME}\033[0m: \033[1;34m[timeout]\033[0m (Test time: %02dm:%02ds)\n" "$((T/60))" "$((T%60))"
    cat "${LOG_FILE}"
    kill -9 "$PID"
    exit 1
  }

  trap on_timeout 15

  ${CMD} < "${TEST}" > "${LOG_FILE}" 2>&1

  STATUS=$?
  T="$(($(date +%s)-START_TIME))"

  if [ "${STATUS}" == "0" ]; then
    printf "Ran test \033[1m${TEST_NAME}\033[0m: \033[0;32m[ok]\033[0m (Test time: %02dm:%02ds)\n" "$((T/60))" "$((T%60))"
    if [ -n "${LIQ_VERBOSE_TEST}" ]; then
      cat "${LOG_FILE}"
    fi
    exit 0
  fi

  if [ "${STATUS}" == "2" ]; then
      printf "${WARNING_PREFIX}Ran test \033[1m${TEST_NAME}\033[0m: \033[1;33m[skipped]\033[0m\n"
      exit 0
  fi

  printf "${ERROR_PREFIX}Ran test \033[1m${TEST_NAME}\033[0m: \033[0;31m[failed]\033[0m (Test time: %02dm:%02ds)\n" "$((T/60))" "$((T%60))"
  cat "${LOG_FILE}"
  exit 1
}

export -f run_test

on_term() {
  exit 1
}

trap on_term INT

timeout -s 15 "${TIMEOUT}" bash -c "run_test \"$1\" \"$2\" \"$3\""
