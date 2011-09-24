#!/bin/sh

(echo $1 ; echo exit) | netcat localhost 1234 | sed -e 's/\r//g' |
while read l ; do
  if test "$l" = "END" ; then exit 0 ; fi ;
  echo $l
done
