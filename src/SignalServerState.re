let defaultSignalServerUrl = "ws://localhost:7777";

/* HELPERS */

let cmdConnectToSignalServer = (thisPeer, peers) =>
  SignalServerCmds.connect(
    defaultSignalServerUrl,
    thisPeer,
    peers |> Peers.getAllIds,
    Msgs.connectToSignalServerSuccess,
    Msgs.signalServerMessage,
    Msgs.signalServerConnectionError,
  );

let applyPeerStatusChanges = (onlinePeers, changes) =>
  List.fold_left(
    (prevOnlinePeers, change) =>
      /* TODO: Care only about those I have in my contacts */
      switch (change) {
      | Message.WentOnline(peerId) =>
        prevOnlinePeers |> PeerId.Set.add(peerId)
      | WentOffline(peerId) =>
        prevOnlinePeers |> PeerId.Set.filter(oldPeer => oldPeer !== peerId)
      },
    onlinePeers,
    changes,
  );

let timeoutRetryCmd = attemptsMade =>
  Cmds.timeout(
    Msgs.signalServerRetryConnection,
    Retry.getTimeoutMs(attemptsMade),
  );

/* INIT, UPDATE */

let initialModel = Types.Connecting;

let init = (thisPeer, peers) => (
  initialModel,
  cmdConnectToSignalServer(thisPeer, peers),
);

let update = (thisPeer, peers, model: Types.signalServerState, msg) =>
  switch (msg, model) {
  | (Msgs.ConnectToSignalServerSuccess(connection), _) => (
      Types.SigningIn(connection),
      Cmds.none,
    )

  | (
      SignalServerConnectionError,
      FailedRetryingAt(time, attemptsMade, lastErr),
    ) => (
      Types.FailedRetryingAt(time, attemptsMade + 1, lastErr),
      timeoutRetryCmd(attemptsMade),
    )

  | (SignalServerConnectionError, _signalServerState) => (
      FailedRetryingAt("", 1, ""),
      timeoutRetryCmd(1),
    )

  | (SignalServerRetryConnection, FailedRetryingAt(_, _, _)) => (
      model,
      cmdConnectToSignalServer(thisPeer, peers),
    )

  | (SignalServerMessage(Ok(onlinePeers)), SigningIn(conn)) => (
      Connected(conn, onlinePeers),
      Cmds.none,
    )

  | (
      SignalServerMessage(WatchedPeersChanged(changes)),
      Connected(conn, onlinePeers),
    ) => (
      Connected(conn, applyPeerStatusChanges(onlinePeers, changes)),
      Cmds.none,
    )

  | (_, signalServerState) => (signalServerState, Cmds.none)
  };