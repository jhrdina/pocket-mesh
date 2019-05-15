---
id: gui-install
title: GUI Library Installation
sidebar_label: Installation
---

Framework ships with an optional set of pre-built ReasonReact GUI components for common tasks such as group management, rendering a list of friends, setting permissions and other functionality implemented in the PocketMesh Peer Library.

> **Note:** If you have started with TreeBurst demo app as a base, the following libraries are already pre-installed.

You can install the GUI library to your project using

```sh
npm install pocket-mesh-peer-material-ui
```

and by adding a corresponding item to the `bsconfig.json`:

```json
"bs-dependencies": ["pocket-mesh-peer-material-ui"],
```

The components are built on top of [Material-UI](https://material-ui.com/), a popular React implementation of the Google's [Material Design Guidelines](https://material.io/design/), and its Reason bindings [jsiebern/bs-material-ui](https://github.com/jsiebern/bs-material-ui). If you don't have them already installed in your project, you can do so by issuing

```sh
npm install @jsiebern/bs-material-ui @material-ui/core @material-ui/icons github:jhrdina/material-ui-styles#build
```