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

  type signalState =
    | Online
    | Offline;
  type connectionState =
    | NotInGroup(signalState)
    | InGroupWaitingForOnlineSignal
    | InGroupOnlineInitiatingConnection(int)
    | OnlineAcceptingConnection(bool)
    | InGroupOnlineFailedRetryingAt(int, int, string)
    | Connected(bool, signalState);

  type t;

  let id: t => Id.t;
  let alias: t => string;
  let connectionState: t => connectionState;
};

module Peers: {
  type t;
  let findOpt: (Peer.Id.t, t) => option(Peer.t);
  let fold: (('acc, Peer.t) => 'acc, 'acc, t) => 'acc;
};

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
    | SigningIn
    | FailedRetryingAt(int, int, string)
    | Connected;

  let url: t => string;
  let connectionState: t => connectionState;
};

module StateWithId: {
  type t;
  let groups: t => PeersGroups.t;
  let peers: t => Peers.t;
  let thisPeer: t => ThisPeer.t;
  let signalServer: t => SignalServer.t;
};

module Msg: {
  type t;

  /* Peers */
  let addPeer: (PeerId.t, string) => t;
  let updatePeer: (PeerId.t, string) => t;
  let removePeer: PeerId.t => t;

  /* Groups */
  let addGroup: (PeerGroup.Id.t, string, Crdt.t => Crdt.t) => t;
  let updateGroupAlias: (PeerGroup.Id.t, string) => t;
  let updateGroupContent: (PeerGroup.Id.t, PeerGroup.AM.t) => t;
  let removeGroup: PeerGroup.Id.t => t;

  /* Peers in groups */
  let addPeerToGroup:
    (PeerId.t, PeerGroup.Id.t, PeerGroup.groupPermissions) => t;
  let updatePeerPermissions:
    (PeerId.t, PeerGroup.Id.t, PeerGroup.groupPermissions) => t;
  let removePeerFromGroup: (PeerId.t, PeerGroup.Id.t) => t;

  /* Others */
  let updateSignalServerUrl: string => t;
  let removeThisPeerAndAllData: t;
};

module State: {
  type t;
  type taggedT =
    | OpeningDB
    | LoadingDBData
    | GeneratingIdentity
    | FatalError(exn)
    | HasIdentity(StateWithId.t);

  let classify: t => taggedT;
};

let init: InitConfig.t => (State.t, BlackTea.Cmd.t(Msg.t));
let update: (State.t, Msg.t) => (State.t, BlackTea.Cmd.t(Msg.t));