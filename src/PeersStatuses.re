open BlackTea;

type peerStatus =
  | Online
  | Offline;

type t = {
  allPeersIds: PeerId.Set.t,
  onlinePeers: PeerId.Set.t,
};

// type Msgs.t +=
//   | StatusesChanged(PeerId.Map.t(peerStatus));

/* HELPERS */

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

let addPeerIdsWithStatus = (set, status, changes) =>
  PeerId.Set.fold(
    (peerId, changes) => changes |> PeerId.Map.add(peerId, status),
    set,
    changes,
  );

let diffStatuses = (onlineSet1, onlineSet2) => {
  let removed = PeerId.Set.diff(onlineSet1, onlineSet2);
  let added = PeerId.Set.diff(onlineSet2, onlineSet1);
  PeerId.Map.empty
  |> addPeerIdsWithStatus(removed, Offline)
  |> addPeerIdsWithStatus(added, Online);
};

/* QUERIES */

let getPeerStatus = (peerId, t) =>
  PeerId.Set.mem(peerId, t) ? Online : Offline;

let getOnlinePeers = t => t.onlinePeers;

/* MAIN */

let init = () =>
  // TODO: Derive peers immediately
  {allPeersIds: PeerId.Set.empty, onlinePeers: PeerId.Set.empty};

let update = (~thisPeer: ThisPeer.t, ~peers, msg, model) => {
  let (model, cmd) =
    switch (msg) {
    | SignalServerState.GotMessage(Unsigned(Ok(onlinePeers))) => (
        {...model, onlinePeers},
        Cmds.none,
      )

    | SignalServerState.GotMessage(Unsigned(WatchedPeersChanged(changes))) => (
        {
          ...model,
          onlinePeers: applyPeerStatusChanges(model.onlinePeers, changes),
        },
        Cmds.none,
      )
    | _ => (model, Cmds.none)
    };

  let newAllPeersIds = peers |> Peers.getAllIds;
  let (model, derivedCmd) =
    if (newAllPeersIds != model.allPeersIds) {
      (
        {...model, allPeersIds: newAllPeersIds},
        Cmd.msg(
          SignalVerifier.SignAndSendMsg(
            PeerToServer(thisPeer.id, ChangeWatchedPeers(newAllPeersIds)),
          ),
        ),
      );
    } else {
      (model, Cmd.none);
    };
  (model, Cmd.batch([cmd, derivedCmd]));
};