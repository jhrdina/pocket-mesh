open BlackTea;

type peerStatus =
  | Online
  | Offline;

type t = {
  lastAllPeersIds: PeerId.Set.t,
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
  t.onlinePeers |> PeerId.Set.mem(peerId) ? Online : Offline;

let getOnlinePeers = t => t.onlinePeers;

/* MAIN */

let init = () =>
  // TODO: Derive peers immediately
  {lastAllPeersIds: PeerId.Set.empty, onlinePeers: PeerId.Set.empty};

let update =
    (
      ~thisPeer: ThisPeer.t,
      ~peers,
      ~signalChannel: SignalChannel.t,
      msg,
      model,
    ) => {
  let (model, cmd) =
    switch (msg) {
    | SignalChannel.GotMessage(Unsigned(Ok(onlinePeers))) => (
        {...model, onlinePeers},
        Cmds.none,
      )

    | SignalChannel.GotMessage(Unsigned(WatchedPeersChanged(changes))) => (
        {
          ...model,
          onlinePeers: applyPeerStatusChanges(model.onlinePeers, changes),
        },
        Cmds.none,
      )
    | _ => (model, Cmds.none)
    };

  // Derive
  //   peers i got info they are online
  //   INTERSECTION
  //   peers

  let allPeersIds = peers |> Peers.getAllIds;
  let (model, derivedCmd) =
    switch (signalChannel.connectionState) {
    | Connecting => (
        {lastAllPeersIds: allPeersIds, onlinePeers: PeerId.Set.empty},
        Cmd.none,
      )
    | Connected(_) when allPeersIds != model.lastAllPeersIds => (
        {
          onlinePeers:
            PeerId.Set.symmetric_diff(
              model.lastAllPeersIds,
              allPeersIds,
              ~f=
                (diffRes, onlinePeers) =>
                  switch (diffRes) {
                  // Remove peer from online peers if it was removed from friends
                  | Left(peerId) => onlinePeers |> PeerId.Set.remove(peerId)
                  | Right(_) => onlinePeers
                  },
              ~acc=model.onlinePeers,
            ),
          lastAllPeersIds: allPeersIds,
        },
        Cmd.msg(
          SignalVerifier.SignAndSendMsg(
            PeerToServer(thisPeer.id, ChangeWatchedPeers(allPeersIds)),
          ),
        ),
      )
    | Connected(_) => (model, Cmd.none)
    };

  (model, Cmd.batch([cmd, derivedCmd]));
};