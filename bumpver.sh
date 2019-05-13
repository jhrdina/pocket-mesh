#!/bin/bash

source scripts-common.sh

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

npmReplaceVersion "$version" ./peer/package.json

npmReplaceVersion "$version" ./peer-material-ui/package.json
npmDepReplaceVersion pocket-mesh-peer "$version" ./peer-material-ui/package.json

npmReplaceVersion "$version" ./signal-server/package.json
opamReplaceVersion "$version" ./signal-server/pocket-mesh-signal-server.opam

if [ $updateTreeBurst = true ]; then
  npmDepReplaceVersion pocket-mesh-peer "$version" "$treeBurstPath/package.json"
  npmDepReplaceVersion pocket-mesh-peer-material-ui "$version" "$treeBurstPath/package.json"
fi

# ============================================
# Install
# ============================================

if [ $install == true ]; then
  echo -e "Installing...\n"

  cd "$dir/peer"
  npmInstallAndPublishLocal

  cd "$dir/peer-material-ui"
  npmInstallPkgs pocket-mesh-peer
  npmInstall
  npmCleanBuild
  peerMaterialUiBuildRes=$?
  npmPublishLocal

  cd "$dir/signal-server"
  opam install .

  if [ $updateTreeBurst = true ]; then
    cd "$treeBurstPath"
    npmInstallPkgs pocket-mesh-peer pocket-mesh-peer-material-ui
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