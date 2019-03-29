#!/bin/bash

PKG_REPO="$HOME/npm-pkgs"

usage="$(basename "$0") [[-h] | [-i | -c | -p] VERSION]
Updates packages versions to VERSION and optionally installs the packages

where:
    -h  Show this help text
    -v  Print current version of peer package and exit
    -i  Also install the packages
    -t  Also update tree-burst demo project
    -c  Create version bump commit in Git
    -p  Push tag and current branch to Git
    "

install=false
commit=false
push=false
version=""
dir=`pwd`

updateTreeBurst=false
treeBurstPath="$dir/../tree-burst"

# Parse args

OPTIND=1

while getopts "h?vicpt" opt; do
  case "$opt" in
  h|\?)
    echo "$usage"
    exit
    ;;
  v)
    grep '"version":' ./peer/package.json
    exit
    ;;
  i)
    install=true
    ;;
  c)
    commit=true
    ;;
  p)
    push=true
    ;;
  t)
    updateTreeBurst=true
    ;;
  esac
done

shift $((OPTIND-1))
[ "${1:-}" = "--" ] && shift

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

if [ $updateTreeBurst = true ]; then
  replaceDepVersionInPackageJson pocket-mesh-peer "$version" "$treeBurstPath/package.json"
  replaceDepVersionInPackageJson pocket-mesh-peer-material-ui "$version" "$treeBurstPath/package.json"
fi

# ============================================
# Install
# ============================================

if [ $install == true ]; then
  echo -e "Installing...\n"

  function npack {
    npm pack && mv *.tgz "$PKG_REPO/"
  }
  function bak {
    cp package.json package.json.bak
  }
  function rest {
    mv package.json.bak package.json
  }

  cd "$dir/peer"
  npm i
  npack

  cd "$dir/peer-material-ui"
  npm i
  bak
  npm i -f pocket-mesh-peer
  rest
  npm run clean
  npm run build
  peerMaterialUiBuildRes=$?
  npack

  cd "$dir/signal-server"
  opam install .

  if [ $updateTreeBurst = true ]; then
    cd "$treeBurstPath"
    bak
    npm i -f pocket-mesh-peer pocket-mesh-peer-material-ui
    rest
  fi

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