open Collections;

type byConnStates = {
  noNeedToConnect: PeerIdSet.t,
  waitingForOnlineSignal: PeerIdSet.t,
  creatingSdpOffer: PeerIdSet.t,
  waitingForSdpAnswer: PeerIdSet.t,
  failedRetrying: PeerIdSet.t,
  connected: PeerIdSet.t,
};

type t = {
  peers: PeerIdMap.t(Peer.t),
  byConnectionState: byConnStates,
};

let empty = {
  peers: PeerIdMap.empty,
  byConnectionState: {
    noNeedToConnect: PeerIdSet.empty,
    waitingForOnlineSignal: PeerIdSet.empty,
    creatingSdpOffer: PeerIdSet.empty,
    waitingForSdpAnswer: PeerIdSet.empty,
    failedRetrying: PeerIdSet.empty,
    connected: PeerIdSet.empty,
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
  switch (t.peers |> PeerIdMap.find(peerId)) {
  | peer => Some(peer)
  | exception Not_found => None
  };

let add = (id, peer, t) => {
  peers: PeerIdMap.add(id, peer, t.peers),
  byConnectionState:
    switch (findOpt(id, t)) {
    | Some(oldPeer) when oldPeer.connectionState !== peer.connectionState =>
      t.byConnectionState
      |> _mapByConnectionStateIndex(
           oldPeer.connectionState,
           PeerIdSet.remove(id),
         )
      |> _mapByConnectionStateIndex(peer.connectionState, PeerIdSet.add(id))
    | Some(_) => t.byConnectionState
    | None =>
      t.byConnectionState
      |> _mapByConnectionStateIndex(peer.connectionState, PeerIdSet.add(id))
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

/* Helpers */

let findByConnectionState = (connState, t) => {
  let idSet = t |> findAllIdsWithConnectionState(connState);
  switch (idSet |> PeerIdSet.choose) {
  | peerId => t |> findOpt(peerId)
  | exception Not_found => None
  };
};