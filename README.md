# Pocket Mesh

Pocket Mesh is a framework for creating collaborative P2P webapps.

[**Website**](https://pocket-mesh.hrdinajan.cz) &nbsp;&nbsp;&nbsp; [Getting started](https://pocket-mesh.hrdinajan.cz/getting-started) &nbsp;&nbsp;&nbsp; [API Docs](https://pocket-mesh.hrdinajan.cz/api)

It is written completely in ReasonML and consists of three subprojects:

- [Peer Library](peer) – endpoint library with all the P2P functionality (no GUI),
- [Peer Material UI](peer-material-ui) – optional library with pre-built ReasonReact GUI components for common tasks such as groups management, rendering a list of friends, setting permissions, etc.,
- [Signal Server](signal-server) – native signal server that is needed in the phase of connection establishment and for online state notifications.

## Example project

We've prepared a demo project that shows the framework in action:

[**TreeBurst**](https://tree-burst.hrdinajan.cz) – peer-to-peer mind-map editor ([source code](https://github.com/jhrdina/tree-burst))
