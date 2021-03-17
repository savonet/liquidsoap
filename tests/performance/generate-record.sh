#!/bin/sh

echo "#!../src/liquidsoap ../libs/pervasives.liq --force-start"
echo '%include "performance.liq"'
echo "def sum () ="
echo "r = ()"
for i in `seq 5000`; do echo "let r.a$i = $i"; done
echo "n = ref(0)"
for i in `seq 5000`; do echo "n := !n + r.a$i"; done
echo "end"
echo 'time("sum of fields", sum)'
echo "exit(0)"
