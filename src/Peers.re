/**
  - Container for storing and querying peers list
    - indexed by connection state for extra performance
      (not measured, therefore it might not be necessary)
  - Controller that handles Peers list related global messages
    - handles peers list changes
    - ensures changes are saved to IDB

  TODO: Separate?
 */

type byConnStates = {
  notInGroup: PeerId.Set.t,
  inGroupWaitingForOnlineSignal: PeerId.Set.t,
  inGroupOnlineCreatingSdpOffer: PeerId.Set.t,
  inGroupOnlineWaitingForAcceptor: PeerId.Set.t,
  inGroupOnlineFailedRetrying: PeerId.Set.t,
  onlineCreatingSdpAnswer: PeerId.Set.t,
  onlineWaitingForInitiator: PeerId.Set.t,
  inGroupConnected: PeerId.Set.t,
};

type t = {
  peers: PeerId.Map.t(Peer.t),
  byConnectionState: byConnStates,
};

let empty = {
  peers: PeerId.Map.empty,
  byConnectionState: {
    notInGroup: PeerId.Set.empty,
    inGroupWaitingForOnlineSignal: PeerId.Set.empty,
    inGroupOnlineCreatingSdpOffer: PeerId.Set.empty,
    inGroupOnlineWaitingForAcceptor: PeerId.Set.empty,
    inGroupOnlineFailedRetrying: PeerId.Set.empty,
    onlineCreatingSdpAnswer: PeerId.Set.empty,
    onlineWaitingForInitiator: PeerId.Set.empty,
    inGroupConnected: PeerId.Set.empty,
  },
};

let _mapByConnectionStateIndex = (connState, f, index) =>
  switch (connState) {
  | Peer.NotInGroup(_) => {...index, notInGroup: f(index.notInGroup)}
  | InGroupWaitingForOnlineSignal => {
      ...index,
      inGroupWaitingForOnlineSignal: f(index.inGroupWaitingForOnlineSignal),
    }
  | InGroupOnlineCreatingSdpOffer(_) => {
      ...index,
      inGroupOnlineCreatingSdpOffer: f(index.inGroupOnlineCreatingSdpOffer),
    }
  | InGroupOnlineWaitingForAcceptor(_) => {
      ...index,
      inGroupOnlineWaitingForAcceptor:
        f(index.inGroupOnlineWaitingForAcceptor),
    }
  | InGroupOnlineFailedRetryingAt(_) => {
      ...index,
      inGroupOnlineFailedRetrying: f(index.inGroupOnlineFailedRetrying),
    }
  | OnlineCreatingSdpAnswer(_, _) => {
      ...index,
      onlineCreatingSdpAnswer: f(index.onlineCreatingSdpAnswer),
    }
  | OnlineWaitingForInitiator(_, _, _) => {
      ...index,
      onlineWaitingForInitiator: f(index.onlineWaitingForInitiator),
    }
  | Connected(_) => {...index, inGroupConnected: f(index.inGroupConnected)}
  };

let findOpt = (peerId, t) =>
  switch (t.peers |> PeerId.Map.find(peerId)) {
  | peer => Some(peer)
  | exception Not_found => None
  };

let add = (id, peer, t) => {
  peers: PeerId.Map.add(id, peer, t.peers),
  byConnectionState:
    switch (findOpt(id, t)) {
    | Some(oldPeer) when oldPeer.connectionState !== peer.connectionState =>
      t.byConnectionState
      |> _mapByConnectionStateIndex(
           oldPeer.connectionState,
           PeerId.Set.remove(id),
         )
      |> _mapByConnectionStateIndex(peer.connectionState, PeerId.Set.add(id))
    | Some(_) => t.byConnectionState
    | None =>
      t.byConnectionState
      |> _mapByConnectionStateIndex(peer.connectionState, PeerId.Set.add(id))
    },
};

let remove = (id, t) => {
  peers: PeerId.Map.remove(id, t.peers),
  byConnectionState:
    switch (findOpt(id, t)) {
    | Some(peer) =>
      t.byConnectionState
      |> _mapByConnectionStateIndex(
           peer.connectionState,
           PeerId.Set.remove(id),
         )
    | None => t.byConnectionState
    },
};

let map = (f, t) =>
  PeerId.Map.fold(
    (id, peer, acc) => add(id, f(peer), acc),
    t.peers,
    empty,
  );

let fold = (f, acc, t) =>
  PeerId.Map.fold((_id, peer, acc) => f(acc, peer), t.peers, acc);

let findAllIdsWithConnectionState = (connState, t) => {
  let index = t.byConnectionState;
  switch (connState) {
  | Peer.NotInGroup(_) => index.notInGroup
  | InGroupWaitingForOnlineSignal => index.inGroupWaitingForOnlineSignal
  | InGroupOnlineCreatingSdpOffer(_) => index.inGroupOnlineCreatingSdpOffer
  | InGroupOnlineWaitingForAcceptor(_) => index.inGroupOnlineWaitingForAcceptor
  | InGroupOnlineFailedRetryingAt(_) => index.inGroupOnlineFailedRetrying
  | OnlineCreatingSdpAnswer(_) => index.onlineCreatingSdpAnswer
  | OnlineWaitingForInitiator(_) => index.onlineWaitingForInitiator
  | Connected(_) => index.inGroupConnected
  };
};

let getAllIds = t =>
  PeerId.Map.fold(
    (peerId, _v, peerIdSet) => peerIdSet |> PeerId.Set.add(peerId),
    t.peers,
    PeerId.Set.empty,
  );

/* Helpers */

let findByConnectionState = (connState, t) => {
  let idSet = t |> findAllIdsWithConnectionState(connState);
  switch (idSet |> PeerId.Set.choose) {
  | peerId => t |> findOpt(peerId)
  | exception Not_found => None
  };
};

let sendSignalMessage = (thisPeer: ThisPeer.t, msg, ssConn) =>
  Cmds.wrapPromise(
    () =>
      SignalServerCmds.signAndSendMsg(
        PeerToServer(thisPeer.id, msg),
        thisPeer.privateKey,
        ssConn,
      ),
    _ => Msgs.noop,
    Msgs.cryptoFatalError,
  );

/* PERSISTENCY */

let toDb = peers => peers |> PeerId.Map.map(peer => peer |> Peer.toDb);
let saveToDb = (maybeDb, model) =>
  switch (maybeDb) {
  | Some(db) =>
    Db.setPeers(model.peers |> toDb, _ => Msgs.noop, _ => Msgs.noop, db)
  | None => Cmds.none
  };

/* UPDATE */

let init = (dbAndDbPeers, peerGroups, signalServerState) =>
  switch (dbAndDbPeers) {
  | Some((_db, Some(dbPeers))) =>
    let (newPeers, cmdsList) =
      PeerId.Map.fold(
        (_, dbPeer: Types.peerInDb, (peers, cmdsList)) => {
          let inGroup = PeerGroups.isPeerInAGroup(dbPeer.id, peerGroups);
          let signalState =
            Peer.peerSignalStateOfSignalServerState(
              dbPeer.id,
              signalServerState,
            );
          let (newPeer, newPeerCmd) =
            Peer.initFromDb(dbPeer, inGroup, signalState);
          (peers |> add(dbPeer.id, newPeer), [newPeerCmd, ...cmdsList]);
        },
        dbPeers,
        (empty, []),
      );
    (newPeers, Cmds.batch(cmdsList));
  | Some((db, None)) =>
    let newPeers = empty;
    (newPeers, saveToDb(Some(db), newPeers));
  | None => (empty, Cmds.none)
  };

let updatePeer = (peerId, peerMsg, thisPeer, peers) =>
  switch (peers |> findOpt(peerId)) {
  | Some(peer) =>
    let (newPeer, cmd) = Peer.update(thisPeer, peer, peerMsg);
    (peers |> add(peer.id, newPeer), cmd);
  | None => (peers, Cmds.none)
  };

let update =
    (
      db,
      thisPeer: ThisPeer.t,
      signalServerState: Types.signalServerState,
      peerGroups,
      peers,
      msg: Msgs.t,
    ) =>
  switch (msg, signalServerState) {
  | (AddPeer(id, alias), signalServerState) =>
    /* Ensure peer exists */
    let (newPeer, newPeerCmd) =
      switch (peers |> findOpt(id)) {
      | None =>
        let peerSignalState =
          Peer.peerSignalStateOfSignalServerState(id, signalServerState);
        Peer.init(
          ~id,
          ~publicKey=None,
          ~alias,
          ~inGroup=false,
          ~peerSignalState,
        );
      | Some(existingPeer) => (existingPeer, Cmds.none)
      };

    let newPeers = peers |> add(id, newPeer);

    let addWatchCmd =
      switch (peers |> findOpt(id), signalServerState) {
      | (None, Connected(conn, _)) =>
        sendSignalMessage(
          thisPeer,
          ChangeWatchedPeers(peers |> getAllIds |> PeerId.Set.add(id)),
          conn,
        )
      | _ => Cmds.none
      };

    (
      newPeers,
      Cmds.batch([newPeerCmd, addWatchCmd, saveToDb(db, newPeers)]),
    );

  | (UpdatePeer(id, alias), _) =>
    updatePeer(id, UpdateAlias(alias), thisPeer, peers)
  | (RemovePeer(id), signalServerState) =>
    let (newPeers, newPeersCmd) =
      switch (peers |> findOpt(id)) {
      | Some(peer) =>
        /* TODO: Send some more appropriate message */
        let (_newPeer, cmd) =
          Peer.update(thisPeer, peer, RemovedFromLastGroup);
        (peers |> remove(peer.id), cmd);
      | None => (peers, Cmds.none)
      };

    let removeWatchCmd =
      switch (peers |> findOpt(id), signalServerState) {
      | (None, Connected(conn, _)) =>
        sendSignalMessage(
          thisPeer,
          ChangeWatchedPeers(peers |> getAllIds |> PeerId.Set.remove(id)),
          conn,
        )
      | _ => Cmds.none
      };

    (newPeers, Cmds.batch([newPeersCmd, removeWatchCmd]));

  | (RemoveGroup(groupId), _) =>
    /* Check if the group was the last one for members */

    let peersGroupsWithoutGroup =
      peerGroups |> PeerGroups.removeGroup(groupId);

    switch (peerGroups |> PeerGroups.findOpt(groupId)) {
    | Some(group) =>
      let (newPeers, cmdsList) =
        PeerGroup.foldPeersInGroup(
          ((peers, cmdsList), {id: peerId, _}) =>
            if (!PeerGroups.isPeerInAGroup(peerId, peersGroupsWithoutGroup)) {
              let (newPeers, cmd) =
                peers |> updatePeer(peerId, RemovedFromLastGroup, thisPeer);
              (newPeers, [cmd, ...cmdsList]);
            } else {
              (peers, cmdsList);
            },
          (peers, []),
          group,
        );
      (newPeers, Cmds.batch(cmdsList));
    | None => (peers, Cmds.none)
    };

  | (AddPeerToGroup(id, _groupId, _perms), _) =>
    peers |> updatePeer(id, AddedToGroup, thisPeer)

  | (RemovePeerFromGroup(peerId, groupId), _) =>
    /* Check if the group was the last one for member */
    let peersGroupsWithoutTheGroup =
      peerGroups |> PeerGroups.removeGroup(groupId);

    if (!PeerGroups.isPeerInAGroup(peerId, peersGroupsWithoutTheGroup)) {
      peers |> updatePeer(peerId, RemovedFromLastGroup, thisPeer);
    } else {
      (peers, Cmds.none);
    };

  | (SignalServerMessage(Unsigned(Ok(onlinePeers))), SigningIn(ssConn)) =>
    let (newPeers, cmdList) =
      PeerId.Set.fold(
        (peerId, (newPeers, cmds)) =>
          switch (peers |> findOpt(peerId)) {
          | Some(peer) =>
            let (newPeer, cmd) =
              Peer.update(thisPeer, peer, WentOnline(ssConn));
            (newPeers |> add(peerId, newPeer), [cmd, ...cmds]);
          | None => (newPeers, cmds)
          },
        onlinePeers,
        (peers, []),
      );
    (newPeers, Cmds.batch(cmdList));

  | (SignalServerConnectionError, signalServerState) =>
    switch (signalServerState) {
    | FailedRetryingAt(_) => (peers, Cmds.none)
    | _ =>
      let (newPeers, cmdList) =
        fold(
          ((newPeers, cmds), peer) => {
            let (newPeer, cmd) = Peer.update(thisPeer, peer, WentOffline);
            (newPeers |> add(peer.id, newPeer), [cmd, ...cmds]);
          },
          (peers, []),
          peers,
        );
      (newPeers, Cmds.batch(cmdList));
    }

  | (
      SignalServerMessage(Unsigned(WatchedPeersChanged(changes))),
      Connected(ssConn, _),
    ) =>
    /* updatePeers(onlinePeers, thisPeers, msg) */
    let (newPeers, cmdList) =
      List.fold_left(
        (
          (newPeers, cmds),
          (Message.WentOnline(peerId) | WentOffline(peerId)) as change,
        ) =>
          switch (peers |> findOpt(peerId)) {
          | Some(peer) =>
            let msgForPeer =
              switch (change) {
              | WentOnline(_) => Peer.WentOnline(ssConn)
              | WentOffline(_) => WentOffline
              };
            let (newPeer, cmd) = Peer.update(thisPeer, peer, msgForPeer);
            (newPeers |> add(peerId, newPeer), [cmd, ...cmds]);
          | None => (newPeers, cmds)
          },
        (peers, []),
        changes,
      );
    (newPeers, Cmds.batch(cmdList));

  | (
      SignalServerMessage(
        Signed(
          signature,
          PeerToPeer(src, tg, (Offer(sdp) | Answer(sdp)) as sdpMsg),
        ),
      ),
      _,
    ) =>
    /* TODO: Check signature */
    let msgForPeer =
      switch (sdpMsg) {
      | Offer(sdp) => Peer.ReceivedSdp(Offer, sdp)
      | Answer(sdp) => ReceivedSdp(Answer, sdp)
      | _ => raise(Types.InternalError)
      };
    peers |> updatePeer(src, msgForPeer, thisPeer);

  | (RtcOfferReady(rtcConn, sdp, acceptorId), _) =>
    peers |> updatePeer(acceptorId, RtcOfferReady(rtcConn, sdp), thisPeer)

  | (RtcAnswerReady(rtcConn, sdp, peerId), _) =>
    peers |> updatePeer(peerId, RtcAnswerReady(rtcConn, sdp), thisPeer)

  | (RtcConnected(_rtcConn, peerId), _) =>
    peers |> updatePeer(peerId, RtcConnected, thisPeer)

  | (RtcClose(_rtcConn, peerId), _) =>
    peers |> updatePeer(peerId, RtcClose, thisPeer)

  | (RtcRetryConnection(peerId), _) =>
    peers |> updatePeer(peerId, RtcRetryConnection, thisPeer)

  /* DEBUG */

  | (_, _) => (peers, Cmds.none)
  };

let foldActiveConnections = (f, acc, peers) =>
  fold(
    (acc, peer) =>
      switch (peer |> Peer.getActiveConnection) {
      | Some(rtcConn) => f(acc, peer.id, rtcConn)
      | None => acc
      },
    acc,
    peers,
  );