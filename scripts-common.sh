#!/bin/bash

dir=`pwd`
treeBurstPath="$dir/../tree-burst"
PKG_REPO="$HOME/npm-pkgs"

# npmDepSwitchToLocal my-pkg package.json
function npmDepSwitchToLocal {
  sed -ri "s/(^\s*\"$1\"\s*:\s*\")([^ ]*)([0-9]+\.[0-9]+\.[0-9]+)([^\"]*)(\"\s*,\s*$)/\1file:~\/npm-pkgs\/$1-\3.tgz\5/g" $2
}

# npmDepSwitchToPublic my-pkg package.json
function npmDepSwitchToPublic {
  sed -ri "s/(^\s*\"$1\"\s*:\s*\")([^ ]*)([0-9]+\.[0-9]+\.[0-9]+)([^\"]*)(\"\s*,\s*$)/\1\^\3\5/g" $2
}

# Dangerous
function npmPublishLocal {
  npm pack \
  && mv *.tgz "$PKG_REPO/"
}

# Dangerous
function npmPublish {
  npm publish
}

function npmBackup {
  cp package.json package.json.bak
}

function npmRestore {
  mv package.json.bak package.json
}

function npmInstall {
  npm install
}

function npmReplaceVersion {
  sed -ri "s/(^\s*\"version\"\s*:\s*\")[^\"]*(\"\s*,\s*$)/\1$1\2/g" $2
}

function opamReplaceVersion {
  sed -ri "s/(^\s*version\s*:\s*\")[^\"]*(\"\s*$)/\1$1\2/g" $2
}

function npmCleanBuild {
  npm run clean
  npm run build
}

function npmDepReplaceVersion {
  # sed -ri "s/(^\s*\"$1\"\s*:\s*\"[^ ]*-)[^\"-]+(\.tgz\"\s*,\s*$)/\1$2\2/g" $3
  sed -ri "s/(^\s*\"$1\"\s*:\s*\"[^ ]*)[0-9]+\.[0-9]+\.[0-9]+([^\"]*\"\s*,\s*$)/\1$2\2/g" $3
}

# Compound

function npmInstallPkgs {
  npmBackup
  npm i -f $@
  npmRestore
}

function npmInstallAndPublishLocal {
  npmInstall \
  && npmPublishLocal
}

function npmInstallAndPublish {
  npmInstall \
  && npmPublish
}

# npmSwitchVersionsTo [local_|public]
function npmSwitchVersionsTo {
  replaceFun='npmDepSwitchToLocal'
  if [ "$1" == "public" ]; then
    replaceFun='npmDepSwitchToPublic'
  fi

  $replaceFun pocket-mesh-peer ./peer-material-ui/package.json
  $replaceFun pocket-mesh-peer "$treeBurstPath/package.json"
  $replaceFun pocket-mesh-peer-material-ui "$treeBurstPath/package.json"
}

function npmPackAllLocal {
  npmSwitchVersionsTo local_

  cd "$dir/peer" \
  && npmInstallAndPublishLocal

  cd "$dir/peer-material-ui" \
  && npmInstallPkgs pocket-mesh-peer \
  && npmInstallAndPublishLocal

  cd "$dir/signal-server" \
  && opam install .

  cd "$treeBurstPath" \
  && npmInstallPkgs pocket-mesh-peer pocket-mesh-peer-material-ui
}

function npmPublishAll {
  npmSwitchVersionsTo public

  cd "$dir/peer" \
  && npmInstallAndPublish

  cd "$dir/peer-material-ui" \
  && npmInstallPkgs pocket-mesh-peer \
  && npmInstallAndPublish

  cd "$dir/signal-server" \
  && opam install .

  cd "$treeBurstPath" \
  && npmInstallPkgs pocket-mesh-peer pocket-mesh-peer-material-ui
}