---
id: gui-components
title: Available GUI Components
sidebar_label: Available Components
---

Framework ships with optional GUI components for ReasonReact that were built using a popular Material-UI library. It has an open API so feel free to use any of the components.

We could find two main approaches on how to use it:

1. Use the whole GUI without modifications via `GlobalIcon` and `PeerScreens` components.
2. Cherry-pick different partial components and compose a custom GUI of it.

## GlobalIcon: The P2P settings entrypoint

<img src="/img/gui_icon_tooltip.png" width="360" height="118.5">

Global icon is usually the main entrance point to the P2P settings provided by our library. It communicates the state of _connection to signal server_ and _connection to other peers_. You can also get more information by hovering above it.

```reason
/* tree-burst/EditorScreen.re */
<PMGui.GlobalIcon
  dbState
  runtimeState
  groupId={model.openedGroup}
/>
```

## PeerScreens: All P2P settings screens in one component

`PeerScreens` is a component that wraps all the screens for the P2P settings provided by this library.

You can simply put this component next to your app's screens, activate it after a click on `GlobalIcon` and you don't need to care about anything else.

```reason
/* tree-burst/App.re */
let render = (model, pushMsg) =>
  switch (model.route) {
  | Editor => <EditorScreen model pushMsg />
  | SomeOtherAppScreen =>
    <SomeOtherAppScreen />
  | P2PSettings =>
    <PMGui.PeerScreens
      core={model.p2p |> PM.State.classify}
      model={model.p2pGui}
      pushMsg={msg => msg |> RootModel.p2pGuiMsgToMsg |> pushMsg}
    />
  }
```

### `GroupsListTab`

<!-- <img src="/img/gui_groups.png" width="360" height="404" class="img-border"> -->

<img src="/img/gui_groups.png" width="250" class="img-border">

Tab with a list of local groups and options to manage it. Reusable components:

- **`GroupRow`** - List item with a alias of a group, preview of its members, etc.


### `GroupScreen`

<!-- <img src="/img/gui_group.png" width="352.5" height="457.5" class="img-border"> -->

<img src="/img/gui_group.png" width="250" class="img-border">

Group screen can be used to manage group alias, copy its ID and add or remove its members.

There is at least one reusable component in here:

- **`IdBox`** - Dashed box for displaying various IDs with the "Copy" button.


### `PeersListTab`

<img src="/img/gui_peers.png" width="250" class="img-border">

Tab with friends list and their statuses. There are several useful components used in this page:

- **`PeerRowWithData`** - Single peer row with its name and status indicator.
- **`PeerStatusIndicator`** - The variable animated status indicator alone.


### `GeneralTab`

<!-- <img src="/img/gui_general.png" width="360" height="404" class="img-border"> -->

<img src="/img/gui_general.png" width="250" class="img-border">

Tab with general settings: User's identity management and signal server URL settings.


