open Json.Infix;

/**
  - Container for storing and querying peers list
  - Controller that handles Peers list related global messages
    - handles peers list CRUD
 */

type t = {byId: PeerId.Map.t(Peer.t)};

type Msgs.t +=
  | AddPeer(PeerId.t, string) /* Ps */
  | UpdateAlias(PeerId.t, string) /* Ps */
  | UpdatePublicKey(PeerId.t, option(SimpleCrypto.key)) /* Ps */
  | RemovePeer(PeerId.t); /* Ps, PG */
/* OUT */

let addPeer = (id, alias) => AddPeer(id, alias);
let updateAlias = (id, alias) => UpdateAlias(id, alias);
let removePeer = id => RemovePeer(id);

let empty = {byId: PeerId.Map.empty};

let findOpt = (peerId, t) =>
  switch (t.byId |> PeerId.Map.find(peerId)) {
  | peer => Some(peer)
  | exception Not_found => None
  };

let add = (peer: Peer.t, t) => {
  byId: PeerId.Map.add(peer.id, peer, t.byId),
};

let remove = (id, t) => {byId: PeerId.Map.remove(id, t.byId)};

let filter = (f, t) => {
  byId: PeerId.Map.filter((_id, peer) => f(peer), t.byId),
};

let map = (f, t) =>
  PeerId.Map.fold((_id, peer, acc) => add(f(peer), acc), t.byId, empty);

let fold = (f, acc, t) =>
  PeerId.Map.fold((_id, peer, acc) => f(acc, peer), t.byId, acc);

let getAllIds = t =>
  PeerId.Map.fold(
    (peerId, _v, peerIdSet) => peerIdSet |> PeerId.Set.add(peerId),
    t.byId,
    PeerId.Set.empty,
  );

/* HELPERS */

let mapPeer = (peerId, updater, peers) =>
  switch (peers |> findOpt(peerId)) {
  | Some(peer) =>
    /* TODO: Protect id */
    peers |> add(updater(peer))
  | None => peers
  };

/* UPDATE */

let init = () => empty;

let update = (peers, msg: Msgs.t) =>
  switch (msg) {
  | AddPeer(id, alias) =>
    /* Ensure peer exists */
    let newPeer = peers |> findOpt(id) |? {Peer.id, alias, publicKey: None};
    let newPeers = peers |> add(newPeer);
    (newPeers, Cmds.none);

  | UpdateAlias(id, alias) => (
      mapPeer(id, p => {...p, alias}, peers),
      Cmds.none,
    )

  | UpdatePublicKey(id, publicKey) => (
      mapPeer(id, p => {...p, publicKey}, peers),
      Cmds.none,
    )

  | RemovePeer(id) =>
    let newPeers =
      switch (peers |> findOpt(id)) {
      | Some(peer) => peers |> remove(peer.id)
      | None => peers
      };
    (newPeers, Cmds.none);

  /* No action */
  | _ => (peers, Cmds.none)
  };