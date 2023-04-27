#!/usr/bin/env bash

EXAMPLES="11 21 12 22 11s 21s 12s 22s 11i 21i 12i 22i"

if [ "$*" != "" ]; then
  EXAMPLES="$*"
fi

for v in $EXAMPLES; do
  echo "Running: $v"
  gtime \
  erl -noshell -pa "$PWD"/ebin -eval "cuter:run(example, foo$v, [[17]], 25)" \
      -s init stop > LOG."$v"
done
