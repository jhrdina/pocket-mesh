open Json.Infix;
open BlackTea;

/**
  - Container for storing and querying groups of peers
  - Controller that handles PeersGroups-related global messages:
    - handles PeersGroups changes
    - logic that negotiates/applies changes with different peers
    - ensures changes are saved to IDB

  TODO: Separate?
 */

/* CONSTANTS */
let defaultGroupId = PeerGroup.Id.ofStringExn("aaa");

/* TYPES */
type t = PeerGroup.Id.Map.t(PeerGroup.t);

type Msgs.t +=
  | GeneratedInitialGroupId(PeerGroup.Id.t)
  | AddGroup(PeerGroup.Id.t, string, (PeerGroup.AM.t => PeerGroup.AM.t))
  | UpdateGroupAlias(PeerGroup.Id.t, string)
  | UpdateGroupContent(PeerGroup.Id.t, PeerGroup.AM.t)
  | ApplyContentChanges(PeerGroup.Id.t, PeerGroup.AM.ChangeSet.t)
  | ApplyMembersChanges(PeerGroup.Id.t, PeerGroup.AM.ChangeSet.t)
  | RemoveGroup(PeerGroup.Id.t)
  /* Peers in groups */
  | AddPeerToGroup(PeerId.t, PeerGroup.Id.t, PeerGroup.groupPermissions)
  | UpdatePeerPermissions(
      PeerId.t,
      PeerGroup.Id.t,
      PeerGroup.groupPermissions,
    )
  | RemovePeerFromGroup(PeerId.t, PeerGroup.Id.t);

let generatedInitialGroupIdMsg = groupId => GeneratedInitialGroupId(groupId);
let addGroupMsg = (groupId, alias, init) => AddGroup(groupId, alias, init);
let updateGroupAliasMsg = (groupId, alias) =>
  UpdateGroupAlias(groupId, alias);
let updateGroupContentMsg = (groupId, content) =>
  UpdateGroupContent(groupId, content);
let removeGroupMsg = groupId => RemoveGroup(groupId);
let addPeerToGroupMsg = (peerId, groupId, permissions) =>
  AddPeerToGroup(peerId, groupId, permissions);
let updatePeerPermissionsMsg = (peerId, groupId, permissions) =>
  UpdatePeerPermissions(peerId, groupId, permissions);
let removePeerFromGroupMsg = (peerId, groupId) =>
  RemovePeerFromGroup(peerId, groupId);

/* MODIFIERS */

let empty = PeerGroup.Id.Map.empty;

let addPeerGroup = (peerGroup: PeerGroup.t) =>
  PeerGroup.Id.Map.add(peerGroup.id, peerGroup);

let updateGroup = (id, updateFn: PeerGroup.t => PeerGroup.t, t) =>
  switch (t |> PeerGroup.Id.Map.find(id)) {
  | peerGroup => t |> PeerGroup.Id.Map.add(id, updateFn(peerGroup))
  | exception Not_found => t
  };

let removeGroup = PeerGroup.Id.Map.remove;

let fold = (f, acc, t) =>
  PeerGroup.Id.Map.fold((_id, group, acc) => f(acc, group), t, acc);

let addPeerToGroupWithPerms = (peerId, groupId, perms) =>
  updateGroup(groupId, PeerGroup.addPeer({id: peerId, permissions: perms}));

let addPeerToGroup = (peerId, groupId, t) =>
  addPeerToGroupWithPerms(peerId, groupId, ReadContentAndMembers, t);

let removePeerFromAllGroups = (peerId, t) =>
  fold(
    (newPeersGroups, group) =>
      if (group |> PeerGroup.containsPeer(peerId)) {
        let newGroup = group |> PeerGroup.removePeer(peerId);
        newPeersGroups |> addPeerGroup(newGroup);
      } else {
        newPeersGroups;
      },
    t,
    t,
  );

/* QUERIES */

let isPeerInAGroup = (peerId, t) =>
  /* WARNING: Maybe slow */
  PeerGroup.Id.Map.exists(_groupId => PeerGroup.containsPeer(peerId), t);

let getGroupsForPeer = (peerId, t) =>
  /* WARNING: Maybe slow */
  PeerGroup.Id.Map.filter(_groupId => PeerGroup.containsPeer(peerId), t);

let findOpt = (groupId, t) =>
  switch (PeerGroup.Id.Map.find(groupId, t)) {
  | group => Some(group)
  | exception Not_found => None
  };

/* SERIALIZATION */

let encode = peersGroups =>
  Json.(
    Array(
      peersGroups
      |> fold(
           (groupsList, peerGroup) =>
             [peerGroup |> PeerGroup.encode, ...groupsList],
           [],
         ),
    )
  );

let decode = json =>
  Json.(
    json
    |> array
    |?> List.fold_left(
          (newPeersGroups, jsonItem) =>
            newPeersGroups
            |?> (
              newPeersGroups =>
                jsonItem
                |> PeerGroup.decode
                |?>> (peerGroup => newPeersGroups |> addPeerGroup(peerGroup))
            ),
          Some(empty),
        )
  );

/* UPDATE */

let init = () => (
  empty,
  IdGenerator.generateGroupIdCmd(generatedInitialGroupIdMsg),
);

let update = (~thisPeer: ThisPeer.t, ~initContent, ~defaultAlias, msg, model) =>
  switch (msg) {
  | GeneratedInitialGroupId(id) => (
      model
      |> addPeerGroup(
           PeerGroup.make(id, thisPeer.id, defaultAlias, initContent)
           |> PeerGroup.addPeer({
                id: thisPeer.id,
                permissions: WriteContent(WriteMembers),
              }),
         ),
      Cmd.none,
    )
  | AddGroup(id, alias, initContent) =>
    switch (model |> findOpt(id)) {
    | None => (
        model
        |> addPeerGroup(
             PeerGroup.make(id, thisPeer.id, alias, initContent)
             |> PeerGroup.addPeer({
                  id: thisPeer.id,
                  permissions: WriteContent(WriteMembers),
                }),
           ),
        Cmd.none,
      )

    | Some(_) => (
        model,
        Cmds.log(
          "Cannot add group "
          ++ (id |> PeerGroup.Id.toString)
          ++ ": Group already exists",
        ),
      )
    }

  | UpdateGroupAlias(id, alias) => (
      model |> updateGroup(id, g => {...g, alias}),
      Cmd.none,
    )

  | UpdateGroupContent(id, content) => (
      model |> updateGroup(id, g => {...g, content}),
      Cmds.none,
    )

  | ApplyContentChanges(id, changes) => (
      model |> updateGroup(id, PeerGroup.applyContentChanges(changes)),
      Cmd.none,
    )

  | ApplyMembersChanges(id, changes) => (
      model |> updateGroup(id, PeerGroup.applyMembersChanges(changes)),
      Cmd.none,
    )

  | RemoveGroup(id) => (model |> removeGroup(id), Cmd.none)
  | AddPeerToGroup(peerId, groupId, perms) =>
    /* TODO: Check if the peer is in the friends list */
    (model |> addPeerToGroupWithPerms(peerId, groupId, perms), Cmd.none)
  | UpdatePeerPermissions(peerId, groupId, perms) =>
    /* TODO: Check if the peer is in the friends list */
    switch (model |> findOpt(groupId) |?>> PeerGroup.containsPeer(peerId)) {
    | Some(true) => (
        model |> addPeerToGroupWithPerms(peerId, groupId, perms),
        Cmd.none,
      )
    | Some(false)
    | None => (model, Cmds.none)
    }
  | RemovePeerFromGroup(peerId, groupId) =>
    let newPeersGroups =
      model
      |> updateGroup(groupId, group => group |> PeerGroup.removePeer(peerId));
    (newPeersGroups, Cmd.none);
  | Peers.RemovePeer(peerId) =>
    let groups = model |> getGroupsForPeer(peerId);
    let newPeersGroups =
      groups
      |> fold(
           (newPeersGroups, group) =>
             newPeersGroups
             |> addPeerGroup(group |> PeerGroup.removePeer(peerId)),
           model,
         );
    /* let newPeersGroups = model |> removePeerFromAllGroups(peerId); */
    (newPeersGroups, Cmd.none);
  | _ => (model, Cmd.none)
  };