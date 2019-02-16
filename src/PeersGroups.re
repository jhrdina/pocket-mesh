open Rex_json;
open Json.Infix;
open BlackTea;

/**
  - Container for storing and querying groups of peers
  - Controller that handles PeerGroups-related global messages:
    - handles PeerGroups changes
    - logic that negotiates/applies changes with different peers
    - ensures changes are saved to IDB

  TODO: Separate?
 */

/* CONSTANTS */
let defaultGroupId = PeerGroup.Id.ofStringExn("aaa");

/* TYPES */
type t = PeerGroup.Id.Map.t(PeerGroup.t);

type Msgs.t +=
  | AddGroup(PeerGroup.Id.t, string, (PeerGroup.AM.t => PeerGroup.AM.t))
  | UpdateGroupAlias(PeerGroup.Id.t, string)
  | UpdateGroupContent(PeerGroup.Id.t, PeerGroup.AM.t)
  | RemoveGroup(PeerGroup.Id.t)
  /* Peers in groups */
  | AddPeerToGroup(PeerId.t, PeerGroup.Id.t, PeerGroup.groupPermissions)
  | UpdatePeerPermissions(
      PeerId.t,
      PeerGroup.Id.t,
      PeerGroup.groupPermissions,
    )
  | RemovePeerFromGroup(PeerId.t, PeerGroup.Id.t)
  /* OUT */
  | PeersGroupsChanged(PeerGroup.Id.Set.t);

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

let addPeerGroup = (peerGroup: PeerGroup.t, t) =>
  PeerGroup.Id.Map.add(peerGroup.id, peerGroup, t);

let updateGroup = (id, updateFn: PeerGroup.t => PeerGroup.t, t) =>
  switch (t |> PeerGroup.Id.Map.find(id)) {
  | peerGroup => t |> PeerGroup.Id.Map.add(id, updateFn(peerGroup))
  | exception Not_found => t
  };

let removeGroup = PeerGroup.Id.Map.remove;

let fold = (f, acc, t) =>
  PeerGroup.Id.Map.fold((_id, group, acc) => f(acc, group), t, acc);

let addPeerToGroupWithPerms = (peerId, groupId, perms, t) =>
  t
  |> updateGroup(groupId, group =>
       group |> PeerGroup.addPeer({id: peerId, permissions: perms})
     );

let addPeerToGroup = (peerId, groupId, t) =>
  addPeerToGroupWithPerms(peerId, groupId, ReadContentAndMembers, t);

let removePeerFromAllGroups = (peerId, t) =>
  fold(
    (newPeerGroups, group) =>
      if (group |> PeerGroup.containsPeer(peerId)) {
        let newGroup = group |> PeerGroup.removePeer(peerId);
        newPeerGroups |> addPeerGroup(newGroup);
      } else {
        newPeerGroups;
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

let getGroupsStatusesForPeer = (peerId, peerGroups) =>
  fold(
    (groupStatuses, group) =>
      switch (P2PMsg.getGroupStatusForPeer(peerId, group)) {
      | Some(groupStatus) =>
        groupStatuses |> PeerGroup.Id.Map.add(group.id, groupStatus)
      | None => groupStatuses
      },
    PeerGroup.Id.Map.empty,
    peerGroups |> getGroupsForPeer(peerId),
  );

/* SERIALIZATION */

let encode = peerGroups =>
  Json.(
    Array(
      peerGroups
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
          (newPeerGroups, jsonItem) =>
            newPeerGroups
            |?> (
              newPeerGroups =>
                jsonItem
                |> PeerGroup.decode
                |?>> (peerGroup => newPeerGroups |> addPeerGroup(peerGroup))
            ),
          Some(empty),
        )
  );

/* CMDS */

// let saveToDb = (maybeDb, model) =>
//   switch (maybeDb) {
//   | Some(db) =>
//     Db.setPeerGroups(model |> encode, _ => Msgs.noop, _ => Msgs.noop, db)
//   | None => Cmds.none
//   };

/* UPDATE */

let connectionStarted = (peerId, rtcConn, peerGroups) => {
  let groupsStatuses = getGroupsStatusesForPeer(peerId, peerGroups);
  let msgForPeer = P2PMsg.ChangesOffer(groupsStatuses);
  rtcConn->RTCCmds.send(
    String(msgForPeer |> P2PMsg.encode |> Json.stringify),
  );
};

let receivedChangesOffer = (peerId, rtcConn, remoteGroupsStatuses, peerGroups) => {
  let localGroupsStatuses = getGroupsStatusesForPeer(peerId, peerGroups);
  let maybeRequests =
    P2PMsg.maybeCreateRequestsForMissingChanges(
      remoteGroupsStatuses,
      localGroupsStatuses,
    );
  switch (maybeRequests) {
  | Some(requests) =>
    let msgForPeer = P2PMsg.ChangesRequest(requests);
    RTCCmds.send(
      rtcConn,
      String(msgForPeer |> P2PMsg.encode |> Json.stringify),
    );
  | None => Cmds.none
  };
};

let maybeGetGroupsChangesForRequest =
    (peerId, groupsChangesRequest, peerGroups) => {
  let groupsChanges =
    /* TODO: THINK: Isn't there some better meta-operation? */
    PeerGroup.Id.Map.fold(
      (groupId, groupChangesRequest, groupsChanges) =>
        peerGroups
        |> findOpt(groupId)
        |?> P2PMsg.maybeGetGroupChangesForRequest(
              peerId,
              groupChangesRequest,
            )
        |?>> (
          groupChanges =>
            groupsChanges |> PeerGroup.Id.Map.add(groupId, groupChanges)
        )
        |? groupsChanges,
      groupsChangesRequest,
      PeerGroup.Id.Map.empty,
    );
  groupsChanges |> PeerGroup.Id.Map.is_empty ? None : Some(groupsChanges);
};

let receivedChangesRequest = (peerId, rtcConn, requestedChanges, peerGroups) => {
  let maybeChanges =
    maybeGetGroupsChangesForRequest(peerId, requestedChanges, peerGroups);
  switch (maybeChanges) {
  | Some(changes) =>
    let msgForPeer: P2PMsg.t = Changes(changes);
    RTCCmds.send(
      rtcConn,
      String(msgForPeer |> P2PMsg.encode |> Json.stringify),
    );
  | None => Cmds.none
  };
};

let receivedChanges = (peerId, groupsChanges, peerGroups) =>
  PeerGroup.Id.Map.fold(
    (groupId, groupChanges, newPeerGroups) =>
      peerGroups
      |> findOpt(groupId)
      |?> P2PMsg.maybeGetPeerGroupWithAppliedChanges(peerId, groupChanges)
      |?>> (
        peerGroup => newPeerGroups |> updateGroup(groupId, _ => peerGroup)
      )
      |? newPeerGroups,
    groupsChanges,
    peerGroups,
  );

let initDefaultPeerGroups = (thisPeerId, initContent) =>
  empty
  |> addPeerGroup(
       /* TODO: Really a fixed ID? */
       PeerGroup.make(defaultGroupId, thisPeerId, "aaa", initContent)
       |> PeerGroup.addPeer({
            id: thisPeerId,
            permissions: WriteContent(WriteMembers),
          }),
     );

let init = (~thisPeer: ThisPeer.t, ~initContent) =>
  initDefaultPeerGroups(thisPeer.id, initContent);

let update = (~thisPeer: ThisPeer.t, msg, model) =>
  switch (msg) {
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

  | UpdateGroupAlias(id, alias) =>
    switch (model |> findOpt(id)) {
    | Some(peersGroup) => (
        model |> addPeerGroup({...peersGroup, alias}),
        Cmd.none,
      )
    | None => (model, Cmds.none)
    }

  | UpdateGroupContent(id, content) =>
    switch (model |> findOpt(id)) {
    | Some(peersGroup) => (
        model |> addPeerGroup({...peersGroup, content}),
        Cmds.none,
      )
    | None => (model, Cmds.none)
    }

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
    let newPeerGroups =
      model
      |> updateGroup(groupId, group => group |> PeerGroup.removePeer(peerId));
    (newPeerGroups, Cmd.none);
  | Peers.RemovePeer(peerId) =>
    let groups = model |> getGroupsForPeer(peerId);
    let newPeerGroups =
      groups
      |> fold(
           (newPeerGroups, group) =>
             newPeerGroups
             |> addPeerGroup(group |> PeerGroup.removePeer(peerId)),
           model,
         );
    /* let newPeerGroups = model |> removePeerFromAllGroups(peerId); */
    (newPeerGroups, Cmd.none);
  | _ => (model, Cmd.none)
  };