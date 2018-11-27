/**
  Signalling server connection state representation, its changes and related
  global messages handling.
 */

/* TYPES */
type t = {
  url: string,
  connectionState: Types.signalServerState,
};

/* HELPERS */

let cmdConnectToSignalServer = (thisPeer, peers, url) =>
  SignalServerCmds.connect(
    url,
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

/* INIT, UPDATE */

let initialConnectionState = Types.Connecting;

let init = (thisPeer, peers, url) => (
  {url, connectionState: initialConnectionState},
  cmdConnectToSignalServer(thisPeer, peers, url),
);

let update = (thisPeer, peers, {url, connectionState}, msg) => {
  let (newConnectionState, cmd) =
    switch (msg, connectionState) {
    | (Msgs.ConnectToSignalServerSuccess(connection), _) => (
        Types.SigningIn(connection),
        Cmds.none,
      )

    | (
        SignalServerConnectionError,
        FailedRetryingAt(_, attemptsMade, lastErr),
      ) =>
      let newFailedAttempts = attemptsMade + 1;
      let timeoutMs = Retry.getTimeoutMs(newFailedAttempts);
      (
        Types.FailedRetryingAt(
          timeoutMs |> Retry.msToSec,
          newFailedAttempts,
          lastErr,
        ),
        Cmds.timeout(Msgs.signalServerRetryConnection, timeoutMs),
      );

    | (SignalServerConnectionError, _signalServerState) =>
      let failedAttempts = 1;
      let timeoutMs = Retry.getTimeoutMs(failedAttempts);
      (
        FailedRetryingAt(timeoutMs |> Retry.msToSec, failedAttempts, ""),
        Cmds.timeout(Msgs.signalServerRetryConnection, timeoutMs),
      );

    | (SignalServerRetryConnection, FailedRetryingAt(_, _, _)) => (
        connectionState,
        cmdConnectToSignalServer(thisPeer, peers, url),
      )

    | (SignalServerMessage(Ok(onlinePeers)), SigningIn(conn)) => (
        Connected(conn, onlinePeers),
        Cmds.none,
      )

    | (UpdateSignalServerUrl(url), _) => (
        Connecting,
        cmdConnectToSignalServer(thisPeer, peers, url),
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

  let newUrl =
    switch (msg) {
    | UpdateSignalServerUrl(newUrl) => newUrl
    | _ => url
    };

  ({connectionState: newConnectionState, url: newUrl}, cmd);
};