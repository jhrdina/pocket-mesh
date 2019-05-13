# Pocket Mesh

![version](https://img.shields.io/npm/v/pocket-mesh-peer.svg?style=flat-square)

Pocket Mesh is a framework for creating collaborative P2P webapps.

[**Website**](https://pocket-mesh.hrdinajan.cz) &nbsp;&nbsp;&nbsp; [Getting started](https://pocket-mesh.hrdinajan.cz/getting-started) &nbsp;&nbsp;&nbsp; [API Docs](https://pocket-mesh.hrdinajan.cz/api)

It is written completely in ReasonML and consists of three subprojects:

- [Peer Library](peer#readme) – endpoint library with all the P2P functionality (no GUI),
- [Peer Material UI](peer-material-ui#readme) – optional library with pre-built ReasonReact GUI components for common tasks such as groups management, rendering a list of friends, setting permissions, etc.,
- [Signal Server](signal-server#readme) – native signal server that is needed in the phase of connection establishment and for online state notifications.

## Example project

We've prepared a demo project that shows the framework in action:

[**TreeBurst**](https://tree-burst.hrdinajan.cz) – peer-to-peer mind-map editor ([source code](https://github.com/jhrdina/tree-burst))
