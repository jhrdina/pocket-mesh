# Pocket Mesh Peer

Peer library for Pocket Mesh framework written in ReasonML and compiled to JavaScript using BuckleScript.

## Installation

Add the package into your project using

```
npm install pocket-mesh-peer
```

and add a corresponding item to the `bsconfig.json`:

```json
"bs-dependencies": ["pocket-mesh-peer"],
```

## Usage

[**Getting Started**](https://pocket-mesh.hrdinajan.cz/getting-started)&nbsp;&nbsp;&nbsp;&nbsp; [Reference Guide](https://pocket-mesh.hrdinajan.cz/api)&nbsp;&nbsp;&nbsp;&nbsp; [Demo project](https://github.com/jhrdina/tree-burst)

## Manual Build, Examples

### Build

```
npm run build
```

### Build + Watch

```
npm run start
```

### Run Examples

After the modules are compiled, we need to create JS bundles with examples using

```
npm run webpack
```

Open `example/StoreDemo.html` or any other example in your web browser. You don't need a running web server.
