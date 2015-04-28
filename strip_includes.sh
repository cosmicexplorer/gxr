#!/bin/bash

TMPFILE=".lock.$1"

if [ "$1" = "" ]; then
  echo "Give an input file!" >&2
  exit 1
else
  output="$(echo $1 | sed -e "s/\.c$//g" -e "s/\.cpp$//g" -e "s/\.h$//g" -e \
  "s/\.cxx$//g" -e "s/\.cc$//g")"
  if [ "$(echo $1 | sed -e "s/\.c$//g" -e "s/\.cpp$//g")" = "$1" ]; then
    echo "Input file must end in .c or .cpp." >&2
    exit 1
  fi
fi

if [ -f $TMPFILE ]; then
  echo "File already locked: remove $TMPFILE" >&2
  exit 1
fi

# mild race condition here which hopefully shouldn't matter

cat $1 | sed -e 's/^#include.*$//g' > $TMPFILE
./test $TMPFILE
rm $TMPFILE
