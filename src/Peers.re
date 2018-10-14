type byConnStates = {
  noNeedToConnect: PeerId.Set.t,
  waitingForOnlineSignal: PeerId.Set.t,
  creatingSdpOffer: PeerId.Set.t,
  waitingForSdpAnswer: PeerId.Set.t,
  failedRetrying: PeerId.Set.t,
  connected: PeerId.Set.t,
};

type t = {
  peers: PeerId.Map.t(Peer.t),
  byConnectionState: byConnStates,
};

let empty = {
  peers: PeerId.Map.empty,
  byConnectionState: {
    noNeedToConnect: PeerId.Set.empty,
    waitingForOnlineSignal: PeerId.Set.empty,
    creatingSdpOffer: PeerId.Set.empty,
    waitingForSdpAnswer: PeerId.Set.empty,
    failedRetrying: PeerId.Set.empty,
    connected: PeerId.Set.empty,
  },
};

let _mapByConnectionStateIndex = (connState, f, index) =>
  switch (connState) {
  | Peer.NoNeedToConnect => {
      ...index,
      noNeedToConnect: f(index.noNeedToConnect),
    }
  | WaitingForOnlineSignal => {
      ...index,
      waitingForOnlineSignal: f(index.waitingForOnlineSignal),
    }
  | CreatingSdpOffer => {
      ...index,
      creatingSdpOffer: f(index.creatingSdpOffer),
    }
  | WaitingForSdpAnswer(_) => {
      ...index,
      waitingForSdpAnswer: f(index.waitingForSdpAnswer),
    }
  | FailedRetryingAt(_, _, _) => {
      ...index,
      failedRetrying: f(index.failedRetrying),
    }
  | Connected(_) => {...index, connected: f(index.connected)}
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

let findAllIdsWithConnectionState = (connState, t) => {
  let index = t.byConnectionState;
  switch (connState) {
  | Peer.NoNeedToConnect => index.noNeedToConnect
  | WaitingForOnlineSignal => index.waitingForOnlineSignal
  | CreatingSdpOffer => index.creatingSdpOffer
  | WaitingForSdpAnswer(_) => index.waitingForSdpAnswer
  | FailedRetryingAt(_, _, _) => index.failedRetrying
  | Connected(_) => index.connected
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