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

let map = (f, t) =>
  PeerId.Map.fold(
    (id, peer, acc) => add(id, f(peer), acc),
    t.peers,
    empty,
  );

let fold = (f, t, n) => PeerId.Map.fold(_ => f, t.peers, n);

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

/* PERSISTENCY */

let toDb = peers => peers |> PeerId.Map.map(peer => peer |> Peer.toDb);
let saveToDb = (db, model) =>
  Db.setPeers(model.peers |> toDb, _ => Msgs.noop, _ => Msgs.noop, db);

/* UPDATE */

let init = (db, peerGroups, signalServerState) =>
  fun
  | Some(dbPeers) => {
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
    }
  | None => {
      let newPeers = empty;
      (newPeers, saveToDb(db, newPeers));
    };

let update =
    (
      db,
      thisPeer,
      signalServerState: Types.signalServerState,
      peers,
      msg: Msgs.t,
    ) => {
  let updatePeer = (peerId, peerMsg) =>
    switch (peers |> findOpt(peerId)) {
    | Some(peer) =>
      let (newPeer, cmd) = Peer.update(thisPeer, peer, peerMsg);
      (peers |> add(peer.id, newPeer), cmd);
    | None => (peers, Cmds.none)
    };

  switch (msg, signalServerState) {
  | (AddPeerWithIdAndPublicKeyToGroup(id, key, _groupId), signalServerState) =>
    /* Ensure peer exists */
    let (newPeer, newPeerCmd) =
      switch (peers |> findOpt(id)) {
      | Some(existingPeer) =>
        Peer.update(thisPeer, existingPeer, AddedToGroup)

      | None =>
        let peerSignalState =
          Peer.peerSignalStateOfSignalServerState(id, signalServerState);
        Peer.init(
          ~id,
          ~publicKey=key,
          ~nickName="",
          ~inGroup=true,
          ~peerSignalState,
        );
      };

    let addWatchCmd =
      switch (peers |> findOpt(id), signalServerState) {
      | (None, Connected(conn, _)) =>
        SignalServerCmds.sendMsg(
          ChangeWatchedPeers({
            src: thisPeer.id,
            watch: peers |> getAllIds |> PeerId.Set.add(id),
            /* TODO: Sign */
            signature: "",
          }),
          conn,
        )
      | _ => Cmds.none
      };

    let newPeers = peers |> add(id, newPeer);

    (
      newPeers,
      Cmds.batch([newPeerCmd, addWatchCmd, saveToDb(db, newPeers)]),
    );

  | (SignalServerMessage(Ok(onlinePeers)), SigningIn(ssConn)) =>
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
          (peer, (newPeers, cmds)) => {
            let (newPeer, cmd) = Peer.update(thisPeer, peer, WentOffline);
            (newPeers |> add(peer.id, newPeer), [cmd, ...cmds]);
          },
          peers,
          (peers, []),
        );
      (newPeers, Cmds.batch(cmdList));
    }

  | (
      SignalServerMessage(WatchedPeersChanged(changes)),
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

  | (SignalServerMessage((Offer(payload) | Answer(payload)) as sdpMsg), _) =>
    let msgForPeer =
      Peer.ReceivedSdp(
        switch (sdpMsg) {
        | Offer(_) => Peer.Offer(payload)
        | Answer(_) => Answer(payload)
        | _ => raise(Types.InternalError)
        },
      );
    updatePeer(payload.src, msgForPeer);

  | (RtcOfferReady(rtcConn, sdp, acceptorId), _) =>
    updatePeer(acceptorId, RtcOfferReady(rtcConn, sdp))

  | (RtcAnswerReady(rtcConn, sdp, peerId), _) =>
    updatePeer(peerId, RtcAnswerReady(rtcConn, sdp))

  | (RtcConnected(_rtcConn, peerId), _) => updatePeer(peerId, RtcConnected)

  | (RtcClose(_rtcConn, peerId), _) => updatePeer(peerId, RtcClose)

  | (RtcRetryConnection(peerId), _) =>
    updatePeer(peerId, RtcRetryConnection)

  /* DEBUG */

  | (_, _) => (peers, Cmds.none)
  };
};

let foldActiveConnections = (f, peers, acc) =>
  fold(
    (peer, acc) =>
      switch (peer |> Peer.getActiveConnection) {
      | Some(rtcConn) => f(peer.id, rtcConn, acc)
      | None => acc
      },
    peers,
    acc,
  );