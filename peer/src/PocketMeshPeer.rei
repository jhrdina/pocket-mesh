/** Library for implementing the peer node. Contains all the important P2P functionality. */;

/* MODULES */

/** Represents one peer in the list of peers (friends), see {!Peers}. */
module Peer: {
  /** Module for representing peer's ID and collections built around the ID ({!Map}, {!Set}, ...). */
  module Id: {
    /** The type for ID.*/
    type t;

    /** [compare(a, b)] lexicographically compares ids [a] and [b]. */
    let compare: (t, t) => int;

    /** [equal(a, b)] tests whether the ids [a] and [b] contain equal IDs. */
    let equal: (t, t) => bool;

    /** [toString(id)] returns string representation of the ID. */
    let toString: t => string;

    /** [ofString(idStr)] creates a new ID out of [idStr] string. [idStr] must have a non-zero length. Returns [None] in case of invalid ID. */
    let ofString: string => option(t);

    /** Same as {! ofString} but raises {! InvalidPeerIdString} exception in case of invalid ID. */
    let ofStringExn: string => t;

    /** Universal Map that uses peer ID as a key. */
    module Map: OcamlDiff.Map.S with type key = t;

    /** Set of peer IDs. */
    module Set: OcamlDiff.Set.S with type elt = t;
  };

  /** The type representing a peer. */
  type t;

  /** [id(p)] returns an ID of the peer [p]. */
  let id: t => Id.t;

  /** [alias(p)] returns a local alias of the peer [p]. */
  let alias: t => string;
};

/** Represents a list of peers (friends). */
module Peers: {
  type t;

  /** [findOpt(id, p)] tries to find a peer with the specified [id]. Returns [None] if not found. */
  let findOpt: (Peer.Id.t, t) => option(Peer.t);

  /** [fold(f, acc, t)] can be used to iterate through all peers and accumulate a value with a starting value of [acc]. */
  let fold: (('acc, Peer.t) => 'acc, 'acc, t) => 'acc;
};

/** Replicated data type that represents application-specific content for a group of peers. */
module Crdt: Automerge.CommonAPI with type actorId = Peer.Id.t;

/** Initial configuration of the library that is passed into the {! init} function. */
module InitConfig: {
  /** Default signal server (probably localhost:7777) that is used if no signal server is specified in {!make}. */
  let defaultSignalServerUrl: string;
  let defaultGroupAlias: string;

  type iceCredentials = {
    username: string,
    credential: string,
  };

  /** Type representing one ICE server connection parameters. */
  type iceServer =
    | Basic(string)
    | WithCredentials(string, iceCredentials);

  type t = {
    /** Alias for the group that is automatically created on the first start. */
    defaultGroupAlias: string,
    /** Function that is used to initialize content of a newly created group. */
    contentInitializer: Crdt.t => Crdt.t,
    /** Default URL address of a WebSocket signal server. */
    signalServerUrl: string,
    /** List of ICE servers (STUN, TURN, ...) */
    iceServers: list(iceServer),
  };

  /** Creates a new configuration. See {! t} for description of the fields. If [contentInitializer] is missing, content of a newly created groups is left empty. If [signalServerUrl] is missing, default server specified in {!defaultSignalServerUrl} constant is used. If [defaultGroupAlias] is not specified, "My group" is used. */
  let make:
    (
      ~contentInitializer: Crdt.t => Crdt.t=?,
      ~signalServerUrl: string=?,
      ~defaultGroupAlias: string=?,
      ~iceServers: list(iceServer)=?,
      unit
    ) =>
    t;
};

/** Manages P2P connections and their state. */
module PeersConnections: {
  type t;

  /** Opaque variant type used internally to represent the current connection state. Use {!classifyConnectionState} to get something to pattern-match on.*/
  type connectionState;

  /** Simplified representation of P2P connection state ideal for pattern-matching. */
  type taggedConnectionState =
    | /** Local peer in the role of initiator is trying to establish P2P connection with the peer and/or is waiting for its response. */
      InitiatingConnection
    | /** Local peer has responded to the initiator and now is waiting for the other peer to finish the connection procedure. */
      AcceptingConnection
    | /** P2P connection is established. */
      Connected;

  /** Converts opaque internal representation of the P2P connection state into the simplified type suitable for pattern-matching. */
  let classifyConnectionState: connectionState => taggedConnectionState;

  /** [getPeerConnectionState(p, t)] returns state of P2P connection for a peer with ID [p] or returns [None] if there is no initiative to connect to [p]. */
  let getPeerConnectionState: (Peer.Id.t, t) => option(connectionState);

  /** [fold(f, acc, t)] can be used to iterate through all peers with P2P connection initiative and accumulate a value with a starting value of [acc]. */
  let fold:
    (('acc, Peer.Id.t, taggedConnectionState) => 'acc, 'acc, t) => 'acc;
};

/** Stores online statuses of all friends. */
module PeersStatuses: {
  type t;
  type peerStatus =
    | /** Peer is connected to signal server. */
      Online
    | /** Peer is NOT connected to signal server. */
      Offline;

  /** [getPeerStatus(p, t)] returns online status for your friend with ID [p]. If the peer is not your friend, [Offline] is returned. */
  let getPeerStatus: (Peer.Id.t, t) => peerStatus;
};

/** Record describing membership of a peer in a group. Currently only stores his permissions. */
module PeerInGroup: {
  /** Permissions for the group's list of members */
  type membersChangingPerms =
    | /** Local peer won't accept members list changes from the peer. */
      ReadMembers
    | /** Local peer will accept members list changes from the peer. */
      WriteMembers;

  /** Permissions for the peer */
  type groupPermissions =
    | /** The peer can only read group's content and members list. */
      ReadContentAndMembers
    | /** Local peer will accept content changes. */
      WriteContent(
        membersChangingPerms,
      );

  type t;

  /** Returns ID of the member of the group. */
  let id: t => Peer.Id.t;

  /** Returns permissions. */
  let permissions: t => groupPermissions;
};

/** Module that represents one group of peers with its replicated content. */
module PeersGroup: {
  /** ID of a group of peers. */
  module Id: {
    /** Type for an ID of a group of peers. */
    type t;

    /** Converts group ID to string. */
    let toString: t => string;

    /** Tries to create group ID from string. It must have a non-zero length. */
    let ofString: string => option(t);
  };

  /** Type for a group of peers. */
  type t;

  /** Returns ID of the group. */
  let id: t => Id.t;

  /** Returns local alias of the group. */
  let alias: t => string;

  /** Returns replicated data structure that stores application-specific content of the group. */
  let content: t => Crdt.t;

  /** Tries to find a member of the group by its ID. */
  let findPeerInGroupOpt: (Peer.Id.t, t) => option(PeerInGroup.t);

  /** [foldPeersInGroup(f, acc, t)] can be used to iterate through members of the group and accumulate a value. */
  let foldPeersInGroup: (('acc, PeerInGroup.t) => 'acc, 'acc, t) => 'acc;
};

/** Generates various globally unique identifiers. */
module IdGenerator: {
  /** Generates a globally unique identifier. */
  let generate: unit => string;

  /** TEA Cmd that generates a globally unique identifier. */
  let generateCmd: (string => 'msg) => BlackTea.Cmd.t('msg);

  /** Generates a globally unique group ID. */
  let generateGroupId: unit => PeersGroup.Id.t;

  /** TEA Cmd that generates a globally unique group ID. */
  let generateGroupIdCmd: (PeersGroup.Id.t => 'msg) => BlackTea.Cmd.t('msg);
};

/** Represents a list of all local groups. */
module PeersGroups: {
  /** Type for list of groups of peers. */
  type t;

  /** Tries to find a group with specified id. */
  let findOpt: (PeersGroup.Id.t, t) => option(PeersGroup.t);

  /** [fold(f, acc, t)] can be used to iterate through all groups and accumulate a value. */
  let fold: (('acc, PeersGroup.t) => 'acc, 'acc, t) => 'acc;

  /** [isPeerInAGroup(p, t)] returns [true] if peer [p] is member of one or more groups. */
  let isPeerInAGroup: (Peer.Id.t, t) => bool;
};

/** All info about the local peer. */
module ThisPeer: {
  type t;

  /** Returns ID of the local peer. */
  let id: t => Peer.Id.t;
};

/** All info about the signal channel. */
module SignalChannel: {
  type t;

  /** State of the connection to signal server. */
  type connectionState =
    | /** Local node is trying to connect to the server. Failed attempts are automatically retried with exponential backoff. */
      Connecting
    | /** Connection to the signal server is established. */
      Connected;

  /** Returns the currently used URL address of the signal server. */
  let url: t => string;

  /** Returns description of connection state to the signal server. */
  let connectionState: t => connectionState;
};

/** Library message type and constructors for the public messages. Messages are meant to be passed to the {!update} function. */
module Msg: {
  /** Opaque variant type that represents library messages, both private and public. */
  type t;

  /* Peers */

  /** [addPeer(id, alias)] message adds a new peer with ID [id] and local alias [alias] into the list of peers (friends). If a peer with the same ID is already added, nothing happens. */
  let addPeer: (Peer.Id.t, string) => t;

  /** [updatePeer(id, alias)] message updates local alias of peer with specified ID. If no such peer is found, nothing happens. */
  let updatePeer: (Peer.Id.t, string) => t;

  /** [removePeer(id)] message removes peer from the list of peers (friends) and from all groups. */
  let removePeer: Peer.Id.t => t;

  /* Groups */

  /** [addGroup(id, alias, initializer)] message adds a new group with ID [id] and local alias [alias] and initializes its replicated content using using [initializer]. If you want to keep it empty, simply pass [a => a] function.*/
  let addGroup: (PeersGroup.Id.t, string, Crdt.t => Crdt.t) => t;

  /** Message that updates local alias of the specified group. */
  let updateGroupAlias: (PeersGroup.Id.t, string) => t;

  /** Message that updates contents of the group. Changes are debounced before propagating them to other group members. */
  let updateGroupContent: (PeersGroup.Id.t, Crdt.t) => t;

  /** Message that removes group with specified ID. */
  let removeGroup: PeersGroup.Id.t => t;

  /* Peers in groups */

  /** Message that adds peer to the group with specified permissions. If the member already exists, its permissions get overwritten. */
  let addPeerToGroup:
    (Peer.Id.t, PeersGroup.Id.t, PeerInGroup.groupPermissions) => t;

  /** Message that updates permissions of the peer in the group. If the peer is not a member of the group, nothing happens. */
  let updatePeerPermissions:
    (Peer.Id.t, PeersGroup.Id.t, PeerInGroup.groupPermissions) => t;

  /** Message that removes peer from the group. */
  let removePeerFromGroup: (Peer.Id.t, PeersGroup.Id.t) => t;

  /* Others */

  /** Message that updates signal server URL and connects to it. */
  let updateSignalServerUrl: string => t;

  /** Message that removes local identity and all data of the local replica (groups, their contents, list of friends, ...) and generates a clean new identity. */
  let removeThisPeerAndAllData: t;
};

/** Represents the part of library state that is persisted in the local database. */
module DbState: {
  type t;

  /** Returns a list of local groups of peers. */
  let groups: t => PeersGroups.t;

  /** Returns a list of peers (friends). */
  let peers: t => Peers.t;

  /** Returns info about the local peer. */
  let thisPeer: t => ThisPeer.t;
};

/** Represents runtime state of the library that is lost after closing the app. */
module RuntimeState: {
  type t;

  /** Returns state of signal channel. */
  let signalChannel: t => SignalChannel.t;

  /** Returns online statuses of peers (friends). */
  let peersStatuses: t => PeersStatuses.t;

  /** Returns states of P2P connections. */
  let peersConnections: t => PeersConnections.t;

  /** Returns the initial config. */
  let initConfig: t => InitConfig.t;
};

/** Represents the whole state of the library. */
module State: {
  /** Internal representation of the library state. Use {!classify} to pattern-match on its contents. */
  type t;

  /** Simplified state of the library suitable for pattern-matching. */
  type taggedT =
    | WaitingForDbAndIdentity(SignalChannel.t)
    | HasIdentity(DbState.t, RuntimeState.t);

  /** Converts opaque internal representation of the library state into a simplified variant type suitable for pattern-matching. */
  let classify: t => taggedT;
};

/** Initializes state of the library. */
let init: InitConfig.t => (State.t, BlackTea.Cmd.t(Msg.t));

/** Updates state of the library. */
let update: (State.t, Msg.t) => (State.t, BlackTea.Cmd.t(Msg.t));

/** Subscriptions of the library. */
let subscriptions: State.t => BlackTea.Sub.t(Msg.t);