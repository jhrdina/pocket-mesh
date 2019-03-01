/* CONSTANTS */

let defaultSignalServerUrl = RuntimeState.defaultSignalServerUrl;

/* MODULES */

module Crdt = PeerGroup.AM;
module InitConfig = RuntimeState.InitConfig;

module Peer = {
  module Id = PeerId;

  type t = Peer.t;

  /* Helpers */

  let id = t => t.Peer.id;
  let alias = t => t.Peer.alias;
};

module PeersConnections = {
  include PeersConnections;

  type taggedConnectionState =
    | InitiatingConnection
    | AcceptingConnection
    | Connected;

  let classifyConnectionState =
    fun
    | PeersConnections.CreatingSdpOffer(_)
    | WaitingForAcceptor(_) => InitiatingConnection
    | CreatingSdpAnswer(_)
    | WaitingForInitiator(_) => AcceptingConnection
    | Connected(_) => Connected;
};

module PeersStatuses = PeersStatuses;

module Peers = {
  type t = Peers.t;
  let findOpt = Peers.findOpt;
  let fold = Peers.fold;
};

module PeerInGroup = {
  type membersChangingPerms =
    PeerGroup.membersChangingPerms = | ReadMembers | WriteMembers;
  type groupPermissions =
    PeerGroup.groupPermissions =
      | ReadContentAndMembers | WriteContent(membersChangingPerms);

  type t = PeerGroup.peerInGroup;

  let id = (t: t) => t.id;
  let permissions = (t: t) => t.permissions;
};

module PeersGroup = PeerGroup;

module PeersGroups = PeersGroups;

module ThisPeer = ThisPeer;

module SignalChannel = {
  type t = SignalChannel.t;
  type connectionState =
    | Connecting
    | Connected;

  let url = (t: t) => t.url;

  let connectionState = (t: t) =>
    switch (t.connectionState) {
    | Connecting => Connecting
    | Connected(_) => Connected
    };
};

module DbState = {
  type t = DbState.t;
  let groups = t => t.DbState.peersGroups;
  let peers = t => t.DbState.peers;
  let thisPeer = t => t.DbState.thisPeer;
};

module RuntimeState = {
  type t = RuntimeState.t;
  let signalChannel = t => t.RuntimeState.signalChannel;
  let peersConnections = t => t.RuntimeState.peersConnections;
  let peersStatuses = t => t.RuntimeState.peersStatuses;
};

module State = {
  type t = Store.t;
  type taggedT =
    | WaitingForDbAndIdentity(SignalChannel.t)
    | HasIdentity(DbState.t, RuntimeState.t);

  let classify =
    fun
    | Store.WaitingForDbAndIdentity(_, _, signalChannel) =>
      WaitingForDbAndIdentity(signalChannel)
    | HasIdentity(_, dbState, runtimeState) =>
      HasIdentity(dbState, runtimeState);
};

module Msg = {
  type t = Msgs.t;
  let addPeer = PocketMeshPeer.Peers.addPeer;
  let updatePeer = PocketMeshPeer.Peers.updateAlias;
  let removePeer = PocketMeshPeer.Peers.removePeer;
  let updateSignalServerUrl = PocketMeshPeer.SignalChannel.updateUrl;

  let removePeerFromGroup = PocketMeshPeer.PeersGroups.removePeerFromGroupMsg;
  let updatePeerPermissions = PocketMeshPeer.PeersGroups.updatePeerPermissionsMsg;
  let addPeerToGroup = PocketMeshPeer.PeersGroups.addPeerToGroupMsg;
  let removeGroup = PocketMeshPeer.PeersGroups.removeGroupMsg;
  let updateGroupContent = PocketMeshPeer.PeersGroups.updateGroupContentMsg;
  let updateGroupAlias = PocketMeshPeer.PeersGroups.updateGroupAliasMsg;
  let addGroup = PocketMeshPeer.PeersGroups.addGroupMsg;
  // let removeThisPeerAndAllData
};

let init = Store.init;
let update = Store.update;
let subscriptions = Store.subscriptions;