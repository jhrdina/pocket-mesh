/* TODO: Better container */
let empty = [];

let addPeerGroup = (peerGroup, t) => [peerGroup, ...t];
let getFirstId: Types.peerGroups => option(string) =
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

/* UPDATE */

let saveToDb = (db, model) =>
  Db.setPeerGroups(model, _ => Msgs.noop, _ => Msgs.noop, db);

let init = (db, thisPeerId, maybeDbPeerGroups) =>
  switch (maybeDbPeerGroups) {
  | Some(dbPeerGroups) => (dbPeerGroups, Cmds.none)
  | None =>
    let newPeerGroups =
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
    (newPeerGroups, saveToDb(db, newPeerGroups));
  };

let update = (db, model, msg) =>
  switch (msg, model) {
  | (Msgs.AddPeerWithIdAndPublicKeyToGroup(id, key, groupId), peerGroups) =>
    let newPeerGroups = peerGroups |> addPeerToGroup(id, groupId);
    (newPeerGroups, saveToDb(db, newPeerGroups));
  | (_, peerGroups) => (peerGroups, Cmds.none)
  };

/* QUERIES */

let isPeerInAGroup = (peerId, t) =>
  List.exists(peerGroup => peerGroup |> PeerGroup.containsPeer(peerId), t);