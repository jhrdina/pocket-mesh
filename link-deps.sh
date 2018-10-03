#!/bin/bash

set -x

function f {
  newName=`printf "$1" | sed 's/-/_/g'`

  if [ ! -d "$newName" ]; then
    mkdir "$newName"
  fi
  cd "$newName" && \

  if [ ! -f jbuild ]; then
    echo "(library ((name $newName)))" > jbuild
  fi
  find "../node_modules/$1/src/" \
    -type f \
    '(' \
    -name '*.re' -or \
    -name '*.rei' -or \
    -name '*.ml' -or \
    -name '*.mli' \
    ')' \
    -exec ln -s {} \;
  cd ..
}

f rex-json
f reason-websocket