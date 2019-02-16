/* CONSTANTS */

let defaultSignalServerUrl: string;

/* MODULES */

module Crdt: Automerge.CommonAPI;

module InitConfig: {
  type t = {
    contentInitializer: Crdt.t => Crdt.t,
    signalServerUrl: string,
  };

  let make:
    (
      ~contentInitializer: Crdt.t => Crdt.t=?,
      ~signalServerUrl: string=?,
      unit
    ) =>
    t;
};

module Peer: {
  module Id: {
    type t;
    let toString: t => string;
    let ofString: string => option(t);
  };

  type t;

  let id: t => Id.t;
  let alias: t => string;
};

module Peers: {
  type t;
  let findOpt: (Peer.Id.t, t) => option(Peer.t);
  let fold: (('acc, Peer.t) => 'acc, 'acc, t) => 'acc;
};

// module PeersConnections: {
//   type connectionState =
//     | NotInGroup(signalState)
//     | InGroupWaitingForOnlineSignal
//     | InGroupOnlineInitiatingConnection(int)
//     | OnlineAcceptingConnection(bool)
//     | InGroupOnlineFailedRetryingAt(int, int, string)
//     | Connected(bool, signalState);

//   let connectionState: t => connectionState;
// };

// module PeersStatuses: {
//   type signalState =
//     | Online
//     | Offline;
// };

module PeerInGroup: {
  type membersChangingPerms =
    | ReadMembers
    | WriteMembers;
  type groupPermissions =
    | ReadContentAndMembers
    | WriteContent(membersChangingPerms);
  type t;

  let id: t => Peer.Id.t;
  let permissions: t => groupPermissions;
};

module PeersGroup: {
  module Id: {
    type t;
    let toString: t => string;
    let ofString: string => option(t);
  };

  type t;

  let id: t => Id.t;
  let content: t => Crdt.t;
  let findPeerInGroupOpt: (Peer.Id.t, t) => option(PeerInGroup.t);
  let foldPeersInGroup: (('acc, PeerInGroup.t) => 'acc, 'acc, t) => 'acc;
};

module PeersGroups: {
  type t;
  let findOpt: (PeersGroup.Id.t, t) => option(PeersGroup.t);
  let fold: (('acc, PeersGroup.t) => 'acc, 'acc, t) => 'acc;
};

module ThisPeer: {
  type t;
  let id: t => Peer.Id.t;
};

module SignalServer: {
  type t;
  type connectionState =
    | Connecting
    | Connected;

  let url: t => string;
  let connectionState: t => connectionState;
};

module Msg: {
  type t;

  /* Peers */
  let addPeer: (Peer.Id.t, string) => t;
  let updatePeer: (Peer.Id.t, string) => t;
  let removePeer: Peer.Id.t => t;

  /* Groups */
  let addGroup: (PeersGroup.Id.t, string, Crdt.t => Crdt.t) => t;
  let updateGroupAlias: (PeersGroup.Id.t, string) => t;
  let updateGroupContent: (PeersGroup.Id.t, Crdt.t) => t;
  let removeGroup: PeersGroup.Id.t => t;

  /* Peers in groups */
  let addPeerToGroup:
    (Peer.Id.t, PeersGroup.Id.t, PeerInGroup.groupPermissions) => t;
  let updatePeerPermissions:
    (Peer.Id.t, PeersGroup.Id.t, PeerInGroup.groupPermissions) => t;
  let removePeerFromGroup: (Peer.Id.t, PeersGroup.Id.t) => t;

  /* Others */
  let updateSignalServerUrl: string => t;
  // let removeThisPeerAndAllData: t;
};

module DbState: {
  type t;
  let groups: t => PeersGroups.t;
  let peers: t => Peers.t;
  let thisPeer: t => ThisPeer.t;
};

module RuntimeState: {
  type t;
  let signalServer: t => SignalServer.t;
};

module State: {
  type t;
  type taggedT =
    | WaitingForDbAndIdentity(SignalServer.t)
    | HasIdentity(DbState.t, RuntimeState.t);

  let classify: t => taggedT;
};

let init: InitConfig.t => (State.t, BlackTea.Cmd.t(Msg.t));
let update: (State.t, Msg.t) => (State.t, BlackTea.Cmd.t(Msg.t));
let subscriptions: State.t => BlackTea.Sub.t(Msg.t);