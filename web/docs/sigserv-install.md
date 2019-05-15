---
id: sigserv-install
title: Installation
---

To run your own signal server, you have two options:

1. Get a [**pre-compiled binary**](https://github.com/jhrdina/pocket-mesh/releases) and go directly to the [next section](sigserv-config) or
2. Compile it manually from sources.

The following text describes the signal server's build process.

## Prerequisites

Our signal server uses OPAM 2 package manager to manage its OCaml dependencies and build process. In Ubuntu 18.04+ you can install OPAM and other dependencies via

```sh
sudo apt-get install m4 pkg-config libgmp-dev libssl-dev
sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update
sudo apt-get install opam
```

Our project is designed to work with OCaml compiler version `4.06.1`. You can switch to it using

```sh
opam init --bare # only if you haven't run OPAM 2 yet
eval $(opam env)
opam switch create 4.06.1
eval $(opam env)
opam update # updates repository
```

## Building

First you need to download sources of the current release and extract it

```sh
curl -L https://github.com/jhrdina/pocket-mesh/archive/0.8.4.zip -o pocket-mesh.zip
unzip pocket-mesh.zip
```

then go to signal-server directory and run the build process:

```sh
cd pocket-mesh-0.8.4/signal-server
opam install .
make
```

OPAM will download, compile and install all necessary dependencies and also compile server itself. The resulting binary is located at `_build/default/src/SignalServer.exe`.