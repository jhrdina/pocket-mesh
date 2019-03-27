#!/bin/sh

echo Building jhrdina/pocket-mesh:build

docker build -t jhrdina/pocket-mesh:build . -f Dockerfile.build

docker container create --name extract jhrdina/pocket-mesh:build
docker container cp extract:/home/opam/app/signal-server/_build/default/src/SignalServer.exe ./out
docker container rm -f extract

echo Building jhrdina/pocket-mesh-signal-server:latest

docker build --no-cache -t jhrdina/pocket-mesh-signal-server:latest .
buildSuccess=$?

if [ $buildSuccess == "0" ]; then
  echo "Signal server image successfully created."
  echo ""
  echo "You can run the created image using"
  echo "  docker run jhrdina/pocket-mesh-signal-server:latest"
fi