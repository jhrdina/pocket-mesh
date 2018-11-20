open Rex_json.Json.Infix;

/* MODULES */

module AM: Automerge.CommonAPI = Automerge.UniJs;

module Id = {
  module Impl: {
    type t;
    let compare: (t, t) => int;
    let toString: t => string;
    let ofString: string => option(t);
    let ofStringExn: string => t;
  } = {
    type t = string;
    let compare = compare;
    let toString = t => t;
    /* TODO: Make sure it is not empty */
    let ofString = str => Some(str);
    let ofStringExn = str =>
      switch (ofString(str)) {
      | Some(t) => t
      | None => raise(Invalid_argument("Cannot convert"))
      };
  };

  include Impl;
  module Map = Map.Make(Impl);
  module Set = Set.Make(Impl);
};

/* TYPES */

type membersChangingPerms =
  | ReadMembers
  | WriteMembers;

type groupPermissions =
  | ReadContentAndMembers
  | WriteContent(membersChangingPerms);

type peerInGroup = {
  id: PeerId.t,
  permissions: groupPermissions,
};

type t = {
  id: Id.t,
  alias: string,
  peers: AM.t,
  content: AM.t,
};

/* CREATING */

let make = (id, thisPeerId, alias, initContent) => {
  let actorId = AM.ActorId.ofString(thisPeerId |> PeerId.toString);
  {
    id,
    alias,
    peers: AM.make(actorId),
    content: AM.make(actorId) |> initContent,
  };
};

/* ENCODING/DECODING */

let encodePermissions =
  fun
  | ReadContentAndMembers => "crmr"
  | WriteContent(ReadMembers) => "crwmr"
  | WriteContent(WriteMembers) => "crwmrw";

let decodePermissions =
  fun
  | "crmr" => Some(ReadContentAndMembers)
  | "crwmr" => Some(WriteContent(ReadMembers))
  | "crwmrw" => Some(WriteContent(WriteMembers))
  | _ => None;

let encode = peerGroup =>
  Json.(
    Object([
      ("id", String(peerGroup.id |> Id.toString)),
      ("alias", String(peerGroup.alias)),
      ("peers", String(peerGroup.peers |> AM.save)),
      ("content", String(peerGroup.content |> AM.save)),
    ])
  );

let decode = json =>
  Json.(
    switch (
      json |> get("id") |?> string |?> Id.ofString,
      json |> get("alias") |?> string,
      json |> get("peers") |?> string |?> AM.load,
      json |> get("content") |?> string |?> AM.load,
    ) {
    | (Some(id), Some(alias), Some(peers), Some(content)) =>
      Some({id, alias, peers, content})
    | _ => None
    }
  );

/* MEMBERS MANAGEMENT */

let addPeer = (peer: peerInGroup, t: t) => {
  let newPeers =
    t.peers
    |> AM.change("Add peer", root =>
         AM.Json.(
           root
           |> Map.add(
                peer.id |> PeerId.toString,
                Map.create()
                |> Map.add("id", string(peer.id |> PeerId.toString))
                |> Map.add(
                     "permissions",
                     string(encodePermissions(peer.permissions)),
                   )
                |> Map.toJson,
              )
         )
       );
  {...t, peers: newPeers};
};

let removePeer = (peerId, t: t) => {
  let newPeers =
    t.peers
    |> AM.change("Remove peer", root =>
         AM.Json.(root |> Map.remove(peerId |> PeerId.toString))
       );
  {...t, peers: newPeers};
};

/* QUERIES */

let id = t => t.id;
let content = t => t.content;

let containsPeer = (peerId, t) =>
  switch (t.peers |> AM.root |> AM.Json.Map.get(peerId |> PeerId.toString)) {
  | Some(_peerInGroup) => true
  | None => false
  };

let decodePeer = crdtPeerMap =>
  AM.Json.(
    switch (
      crdtPeerMap |> Map.get("id") |?> asString |?> PeerId.ofString,
      crdtPeerMap |> Map.get("permissions") |?> asString |?> decodePermissions,
    ) {
    | (Some(id), Some(permissions)) => Some({id, permissions})
    | _ => None
    }
  );

/* TODO: Handle conflicts */

let findPeerInGroupOpt = (peerId, t) =>
  AM.Json.(
    t.peers
    |> AM.root
    |> Map.get(peerId |> PeerId.toString)
    |?> Map.ofJson
    |?> decodePeer
  );

let getPeerPermissions = (peerId, t) =>
  findPeerInGroupOpt(peerId, t) |?>> (peer => peer.permissions);

let foldPeersInGroup = (f, acc, t) =>
  AM.Json.(
    Map.fold(
      (_peerId, peerJson, acc) =>
        switch (peerJson |> Map.ofJson |?> decodePeer) {
        | Some(peer) => f(acc, peer)
        | None => acc
        },
      t.peers |> AM.root,
      acc,
    )
  );