type t = Types.peerGroup;

let make = (id, actorId) => {
  Types.id,
  peers: [],
  content: Types.Automerge.init(~actorId, ()),
};

let addPeer = (peer, t: t) => {...t, peers: [peer, ...t.peers]};