open Rex_json.Json.Infix;

type docType = {. "a": int};
module AM: Automerge.CommonAPI = Automerge.UniJs;

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

module Id = {
  module Impl = {
    type t = string;
    let compare = compare;
  };

  include Impl;
  module Map = Map.Make(Impl);
};

type t = {
  id: Id.t,
  /* TODO: Better container */
  peers: AM.t,
  content: AM.t,
};

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

let addPeer = (peer: peerInGroup, t: t) => {
  ...t,
  peers:
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
       ),
};

let containsPeer = (peerId, t) =>
  switch (t.peers |> AM.root |> AM.Json.Map.get(peerId)) {
  | Some(_peerInGroup) => true
  | None => false
  };

let getPeerInGroup = (peerId, t) => {
  open AM.Json;
  let peer = t.peers |> AM.root |> Map.get(peerId) |?> Map.ofJson;
  switch (
    peer |?> Map.get("permissions") |?> asString |?> decodePermissions,
    peer |?> Map.get("id") |?> asString,
  ) {
  | (Some(permissions), Some(id)) => Some({id, permissions})
  | _ => None
  };
};

/* ENCODING/DECODING */

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