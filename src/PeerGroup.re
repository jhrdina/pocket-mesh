open Rex_json.Json.Infix;

/* MODULES */

module AM: Automerge.CommonAPI = Automerge.UniJs;

module Id = {
  module Impl = {
    type t = string;
    let compare = compare;
  };

  include Impl;
  module Map = Map.Make(Impl);
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
  peers: AM.t,
  content: AM.t,
};

/* CREATING */

let make = (id, actorId) => {
  let actorId = AM.ActorId.ofString(actorId);
  {
    id,
    peers: AM.make(actorId),
    content:
      AM.make(actorId)
      |> AM.change("Init", root =>
           AM.Json.(root |> Map.add("items", List.create() |> List.toJson))
         ),
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
      ("id", String(peerGroup.id)),
      ("peers", String(peerGroup.peers |> AM.save)),
      ("content", String(peerGroup.content |> AM.save)),
    ])
  );

let decode = json =>
  Json.(
    switch (
      json |> get("id") |?> string,
      json |> get("peers") |?> string |?> AM.load,
      json |> get("content") |?> string |?> AM.load,
    ) {
    | (Some(id), Some(peers), Some(content)) => Some({id, peers, content})
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
                peer.id,
                Map.create()
                |> Map.add("id", string(peer.id))
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

/* QUERIES */

let containsPeer = (peerId, t) =>
  switch (t.peers |> AM.root |> AM.Json.Map.get(peerId)) {
  | Some(_peerInGroup) => true
  | None => false
  };

let getPeerPermissions = (peerId, t) =>
  AM.Json.(
    t.peers
    |> AM.root
    |> Map.get(peerId)
    |?> Map.ofJson
    |?> Map.get("permissions")
    |?> asString
    |?> decodePermissions
  );

let foldPeers = (f, acc, t) =>
  AM.Json.(
    Map.fold(
      (_peerId, peerJson, acc) => {
        let peerMap = peerJson |> Map.ofJson;
        switch (
          peerMap |?> Map.get("id") |?> asString,
          peerMap
          |?> Map.get("permissions")
          |?> asString
          |?> decodePermissions,
        ) {
        | (Some(id), Some(permissions)) => f(acc, {id, permissions})
        | _ => acc
        };
      },
      t.peers |> AM.root,
      acc,
    )
  );