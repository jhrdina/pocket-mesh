type t = list(PeerGroup.t);

let empty = [];

let addPeerGroup = (peerGroup, t) => [peerGroup, ...t];
let getFirstId: t => option(string) =
  fun
  | [first, ..._] => Some(first.id)
  | [] => None;

let update = (id, updateFn, t) =>
  t
  |> List.map((item: PeerGroup.t) => item.id === id ? updateFn(item) : item);

let addPeerToGroup = (peerId, groupId, t) =>
  t
  |> update(groupId, group =>
       group
       |> PeerGroup.addPeer({
            id: peerId,
            /* TODO: Really? */
            permissions: {
              content: ReadWrite,
              membersList: ReadWrite,
            },
          })
     );

/* DEBUG */

let createDefaultPeerGroups = myId =>
  empty
  |> addPeerGroup(
       PeerGroup.make("aaa", myId)
       |> PeerGroup.addPeer({
            id: myId,
            permissions: {
              content: ReadWrite,
              membersList: ReadWrite,
            },
          }),
     );

/* UPDATE */

let init = thisPeerId =>
  empty
  |> addPeerGroup(
       PeerGroup.make("aaa", thisPeerId)
       |> PeerGroup.addPeer({
            id: thisPeerId,
            permissions: {
              content: ReadWrite,
              membersList: ReadWrite,
            },
          }),
     );

let update = (model, msg) =>
  switch (msg, model) {
  | (Msgs.AddPeerWithIdAndPublicKeyToGroup(id, key, groupId), peerGroups) =>
    peerGroups |> addPeerToGroup(id, groupId)
  | (_, peerGroups) => peerGroups
  };