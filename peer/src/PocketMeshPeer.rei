/* CONSTANTS */

let defaultSignalServerUrl: string;

/* MODULES */

module Peer: {
  module Id: {
    type t;
    let compare: (t, t) => int;
    let equal: (t, t) => bool;
    let toString: t => string;
    let ofString: string => option(t);
    let ofStringExn: string => t;

    module Map: OcamlDiff.Map.S with type key = t;
    module Set: OcamlDiff.Set.S with type elt = t;
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

module Crdt: Automerge.CommonAPI with type actorId = Peer.Id.t;

module InitConfig: {
  type t = {
    defaultGroupAlias: string,
    contentInitializer: Crdt.t => Crdt.t,
    signalServerUrl: string,
  };

  let make:
    (
      ~contentInitializer: Crdt.t => Crdt.t=?,
      ~signalServerUrl: string=?,
      ~defaultGroupAlias: string=?,
      unit
    ) =>
    t;
};

module PeersConnections: {
  type t;
  type connectionState;

  type taggedConnectionState =
    | InitiatingConnection
    | AcceptingConnection
    | Connected;

  let classifyConnectionState: connectionState => taggedConnectionState;

  let getPeerConnectionState: (Peer.Id.t, t) => option(connectionState);

  let fold:
    (('acc, Peer.Id.t, taggedConnectionState) => 'acc, 'acc, t) => 'acc;
};

module PeersStatuses: {
  type t;
  type peerStatus =
    | Online
    | Offline;

  let getPeerStatus: (Peer.Id.t, t) => peerStatus;
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
  let alias: t => string;
  let content: t => Crdt.t;
  let findPeerInGroupOpt: (Peer.Id.t, t) => option(PeerInGroup.t);
  let foldPeersInGroup: (('acc, PeerInGroup.t) => 'acc, 'acc, t) => 'acc;
};

module IdGenerator: {
  let generate: unit => string;
  let generateCmd: (string => 'msg) => BlackTea.Cmd.t('msg);
  let generateGroupId: unit => PeersGroup.Id.t;
  let generateGroupIdCmd: (PeersGroup.Id.t => 'msg) => BlackTea.Cmd.t('msg);
};

module PeersGroups: {
  type t;
  let findOpt: (PeersGroup.Id.t, t) => option(PeersGroup.t);
  let fold: (('acc, PeersGroup.t) => 'acc, 'acc, t) => 'acc;
  let isPeerInAGroup: (Peer.Id.t, t) => bool;
};

module ThisPeer: {
  type t;
  let id: t => Peer.Id.t;
};

module SignalChannel: {
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
  let removeThisPeerAndAllData: t;
};

module DbState: {
  type t;
  let groups: t => PeersGroups.t;
  let peers: t => Peers.t;
  let thisPeer: t => ThisPeer.t;
};

module RuntimeState: {
  type t;
  let signalChannel: t => SignalChannel.t;
  let peersStatuses: t => PeersStatuses.t;
  let peersConnections: t => PeersConnections.t;
  let initConfig: t => InitConfig.t;
};

module State: {
  type t;
  type taggedT =
    | WaitingForDbAndIdentity(SignalChannel.t)
    | HasIdentity(DbState.t, RuntimeState.t);

  let classify: t => taggedT;
};

let init: InitConfig.t => (State.t, BlackTea.Cmd.t(Msg.t));
let update: (State.t, Msg.t) => (State.t, BlackTea.Cmd.t(Msg.t));
let subscriptions: State.t => BlackTea.Sub.t(Msg.t);