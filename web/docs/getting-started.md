---
id: getting-started
title: Getting started
sidebar_label: Getting Started
---

PocketMesh is a framework for building webapps with P2P real-time collaboration support. It is built upon

- **WebRTC** - protocol that enables direct P2P connection between two browsers
- **CRDT** (conflict-free replicated datatypes) - datatypes that can ensure seamless merging of changes across replicas and provides eventual consistency
- **TEA** - The Elm Architecture - architecture for managing global state

The easiest way to create a new project that uses the framework is to use the [TreeBurst](https://github.com/jhrdina/tree-burst) example project. To fetch it and install all its dependencies simply run

```sh
git clone --depth=1 https://github.com/jhrdina/tree-burst.git my-project
cd my-project
npm install
```

## Initial config

Application's basic configuration is located in the `src/Config.re` file. You can use it to set the signal server URL and WebRTC ICE servers

```reason
/* src/Config.re */
let signalServerUrl = "wss://example.com:6421";
let iceServers: list(PM.InitConfig.iceServer) = [
  Basic("stun:stun.l.google.com:19302"),
  WithCredentials(
    "turn:example.com",
    {credential: "somePassword", username: "someUsername"},
  ),
];
```

or you can use the default configuration (Google's STUN server and a local signal server at port 7777) using:

```sh
mv src/Config.re.default src/Config.re
```

## Running the compiler


Now can run the BuckleScript compiler that watches your `.re` files and compiles corresponding JS files as soon as you change them:

```sh
npm start
```

and in a new terminal tab run a live-reload webpack server

```sh
npm run server
```

Finally, if you visit http://localhost:8000, you should be able to see the TreeBurst demo app.


## _Advanced:_ Manual installation

If, for some reason, you want to install PocketMesh Peer Library from scratch to your existing project, you can do so by running

```sh
npm install pocket-mesh-peer
```

and by adding the library into your `bsconfig.json`:

```json
"bs-dependencies": ["pocket-mesh-peer"],
```

Head to the [next section](core-usage) for instructions on how to include it to your project or browse through the [TreeBurst source code](https://github.com/jhrdina/tree-burst).