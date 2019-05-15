---
id: gui-usage
title: GUI Library Usage
sidebar_label: Usage
---

The easiest way to start with the GUI library is to use the TreeBurst example project, see [Getting started](getting-started).

The GUI library has its own state separated from the core functionality of the peer library. Therefore you need to add it to your target application state as demonstrated in the [`tree-burst/RootModel.re`](https://github.com/jhrdina/tree-burst/blob/master/src/RootModel.re).

After your GUI state is successfully connected, you can start inserting required ReasonReact components to your project. See the [next section](gui-components) for information about the available components.