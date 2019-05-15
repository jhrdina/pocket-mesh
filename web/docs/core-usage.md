---
id: core-usage
title: Peer Library Usage
sidebar_label: Usage
---

The peer library handles all the P2P functionality, including

- local _identity_ generating and persistence,
- _peers_ list manipulation and persistence,
- _groups_ management and persistence in IndexedDB,
- tracking of _online statuses_ of your friends using signal server,
- _automatic P2P connection establishment_ with the friends that are online (signal server is used in the establishment phase, afterwards the communication goes directly P2P),
- automatic _retrying_ of attempts in case of failure or timeout,
- connection _cancelling_ in case of loss of interest and resources deallocation,
- automatic debounced _exchange and merging of content changes_.

## Including the library state into your app state

The whole library is built upon [TEA (The Elm Architecture)](https://guide.elm-lang.org/architecture/) and its custom implementation for Reason called [bs-black-tea](https://github.com/jhrdina/bs-black-tea). The architecture manages the global state in a manner similar to Redux, but better (because of Reason, of course 😉).

To add the library to your application you need to include its `init`, `update` and `subscriptions` functions, as shown in [`tree-burst/RootModel.re`](https://github.com/jhrdina/tree-burst/blob/master/src/RootModel.re):

```reason
let init = () => {
  let (p2p, p2pCmd) =
    PM.init(
      PM.InitConfig.make(
        ~contentInitializer=
          crdt =>
            /* This function can be used to init content of the default
               group. */,
        ~signalServerUrl=Config.signalServerUrl,
        ~iceServers=Config.iceServers,
        (),
      ),
    );
  (
    {p2p, /* Rest of app's initial state */},
    Cmd.batch([
      p2pCmd |> Cmd.map(p2pMsgToMsg),
      /* Rest of app's cmds */
    ]),
  );
};

let update = (model, msg) =>
  switch (msg) {
  | P2PMsg(p2pMsg) =>
    let (p2p, p2pCmd) = PM.update(model.p2p, p2pMsg);
    ({...model, p2p}, p2pCmd |> Cmd.map(p2pMsgToMsg));

  | /* Rest of app's messages handling */ => /* do whatever */
  | _ => (model, Cmd.none)
  };

let subscriptions = model =>
  PM.subscriptions(model.p2p) |> Sub.map(p2pMsgToMsg)

```

## First start

Immediately after the first start of your application a **new local identity** (its private and public keys) is generated and saved into the IndexedDB. First _initial group_ with content specified by the `contentInitializer` function (see above) is created and _connection with signal server is established_.

## Interacting with the library

For all information about all the model structure and available fields of the peer library, see the [API Reference](api) and sources of [TreeBurst](https://github.com/jhrdina/tree-burst) example project.