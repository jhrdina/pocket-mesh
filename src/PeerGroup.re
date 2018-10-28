type docType = {. "a": int};

module Automerge =
  Automerge.Make({
    type t = docType;
  });

type permission =
  | Read
  | Write
  | ReadWrite;

type groupPermissions = {
  content: permission,
  membersList: permission,
};

type peerInGroup = {
  id: PeerId.t,
  permissions: groupPermissions,
};

type t = {
  id: string,
  /* TODO: Better container */
  peers: list(peerInGroup),
  content: docType,
};

let make = (id, actorId) => {
  id,
  peers: [],
  content: Automerge.init(~actorId, ()),
};

let addPeer = (peer, t: t) => {...t, peers: [peer, ...t.peers]};

let containsPeer = (peerId, t) =>
  List.exists(
    (peerInGroup: peerInGroup) => peerInGroup.id === peerId,
    t.peers,
  );