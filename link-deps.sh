#!/bin/bash

set -x

function f {
  newName=`printf "$2" | sed 's/-/_/g'`

  if [ ! -d "$newName" ]; then
    mkdir "$newName"
  fi
  cd "$newName" && \

  if [ ! -f jbuild ]; then
    echo "(library ((name $newName)))" > jbuild
  fi
  find "../$1/$2/src/" \
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

# f node_modules rex-json
# f node_modules reason-websocket
#f '../../bs-black-tea' bs-black-tea

# cd bin/black_tea_internals
cd bin
find "../../../bs-black-tea/bs-black-tea/src/internals" \
  -type f \
  '(' \
  -name '*.re' -or \
  -name '*.rei' -or \
  -name '*.ml' -or \
  -name '*.mli' \
  ')' \
  -exec ln -s {} \;
cd ..