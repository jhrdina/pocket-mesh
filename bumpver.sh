#!/bin/bash

usage="$(basename "$0") [[-h] | [-i | -c | -p] VERSION]
Updates packages versions to VERSION and optionally installs the packages

where:
    -h  Show this help text
    -i  Also install the packages
    -c  Creates version bump commit in Git
    -p  Pushes tag and current branch to Git
    "

install=false
commit=false
push=false
version=""
dir=`pwd`

# Parse args

if [ "$1" == "-h" ]; then
  echo "$usage"
  exit
fi

if [ "$1" == "-i" ]; then
  install=true
  shift
fi

if [ "$1" == "-c" ]; then
  commit=true
  shift
fi

if [ "$1" == "-p" ]; then
  push=true
  shift
fi

if [ "$1" != "" ]; then
  version=$1
else
  echo "$usage"
  exit
fi

# ============================================
# Update versions
# ============================================

function replaceVersionInPackageJson {
  sed -ri "s/(^\s*\"version\"\s*:\s*\")[^\"]*(\"\s*,\s*$)/\1$1\2/g" $2
}

function replaceVersionInOpam {
  sed -ri "s/(^\s*version\s*:\s*\")[^\"]*(\"\s*$)/\1$1\2/g" $2
}

function replaceDepVersionInPackageJson {
  sed -ri "s/(^\s*\"$1\"\s*:\s*\"[^ ]*-)[^\"-]+(\.tgz\"\s*,\s*$)/\1$2\2/g" $3
}

replaceVersionInPackageJson "$version" ./peer/package.json

replaceVersionInPackageJson "$version" ./peer-material-ui/package.json
replaceDepVersionInPackageJson pocket-mesh-peer "$version" ./peer-material-ui/package.json

replaceVersionInPackageJson "$version" ./signal-server/package.json
replaceVersionInOpam "$version" ./signal-server/pocket-mesh-signal-server.opam

# ============================================
# Install
# ============================================

if [ $install == true ]; then
  echo -e "Installing...\n"

  function npack {
    npm pack && mv *.tgz ../
  }

  cd "$dir/peer"
  npack

  cd "$dir/peer-material-ui"
  npm i
  npm run build
  peerMaterialUiBuildRes=$?
  npack

  cd "$dir/signal-server"
  opam install .

  echo

  if [ $peerMaterialUiBuildRes != "0" ]; then
    echo "!!! WARNING: Could not build peer-material-ui"
  else
    echo "Install success."
  fi
fi

if [ $commit == true ]; then
  echo -e "Creating commit...\n"

  cd "$dir"
  git add .
  git commit -m "Bump version"
  git tag -f "$version"
fi

if [ $push == true ]; then
  cd "$dir"
  echo -e "Pushing current branch..."
  git push origin
  echo -e "Pushing tags..."
  git push --tags origin
fi