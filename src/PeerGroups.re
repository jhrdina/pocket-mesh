open Rex_json;
open Json.Infix;

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
type t = Types.peerGroups;

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
           (groupsList, peerGroup) => [
             peerGroup |> PeerGroup.encode,
             ...groupsList,
           ],
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

let saveToDb = (maybeDb, model) =>
  switch (maybeDb) {
  | Some(db) =>
    Db.setPeerGroups(model |> encode, _ => Msgs.noop, _ => Msgs.noop, db)
  | None => Cmds.none
  };

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

let receivedMessageFromPeer =
    (peerId, rtcConn, db, message: P2PMsg.t, peerGroups) =>
  switch (message) {
  | ChangesOffer(changesOffer) => (
      peerGroups,
      receivedChangesOffer(peerId, rtcConn, changesOffer, peerGroups),
    )
  | ChangesRequest(requestedChanges) => (
      peerGroups,
      receivedChangesRequest(peerId, rtcConn, requestedChanges, peerGroups),
    )
  | Changes(groupsChanges) =>
    let newPeerGroups = receivedChanges(peerId, groupsChanges, peerGroups);
    (newPeerGroups, saveToDb(db, newPeerGroups));
  };

let receivedStringMessageFromPeer =
    (peerId, rtcConn, db, stringMessage, peerGroups) =>
  stringMessage
  |> JsonUtils.parseOpt
  |?> P2PMsg.decode
  |?>> (msg => receivedMessageFromPeer(peerId, rtcConn, db, msg, peerGroups))
  |? (
    peerGroups,
    Cmds.log(
      "Failed to parse message from peer " ++ (peerId |> PeerId.toString),
    ),
  );

let offerChangesDebounced = groupsIds =>
  Debouncer.debounceCmd(
    Msgs.OfferChangesFromGroupsDebounced(groupsIds),
    Msgs.offerChangesDebouncerMsg,
    3000,
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

let init = (dbAndDbPeerGroups, thisPeerId, initContent) =>
  switch (dbAndDbPeerGroups |?>> (((db, data)) => (db, data |?> decode))) {
  | Some((_db, Some(dbPeerGroups))) => (dbPeerGroups, Cmds.none)
  | Some((db, None)) =>
    /* TODO: Don't immediately clear the DB if parsing fails. */
    /* TODO: Move saveToDb somewhere else and simplify */
    let newPeerGroups = initDefaultPeerGroups(thisPeerId, initContent);
    (newPeerGroups, saveToDb(Some(db), newPeerGroups));
  | None =>
    /* DB not available (probably a private mode) */
    let newPeerGroups = initDefaultPeerGroups(thisPeerId, initContent);
    (newPeerGroups, Cmds.none);
  };

let update = (db, thisPeerId, peerGroups, msg) =>
  switch (msg) {
  /* GLOBAL MESSAGES */
  | Msgs.AddGroup(id, alias, initContent) =>
    switch (peerGroups |> findOpt(id)) {
    | None =>
      let newPeerGroups =
        peerGroups
        |> addPeerGroup(
             PeerGroup.make(id, thisPeerId, alias, initContent)
             |> PeerGroup.addPeer({
                  id: thisPeerId,
                  permissions: WriteContent(WriteMembers),
                }),
           );
      (newPeerGroups, saveToDb(db, newPeerGroups));
    | Some(_) => (
        peerGroups,
        Cmds.log(
          "Cannot add group "
          ++ (id |> PeerGroup.Id.toString)
          ++ ": Group already exists",
        ),
      )
    }

  | UpdateGroupAlias(id, alias) =>
    switch (peerGroups |> findOpt(id)) {
    | Some(peersGroup) =>
      let newPeerGroups = peerGroups |> addPeerGroup({...peersGroup, alias});
      (newPeerGroups, saveToDb(db, newPeerGroups));
    | None => (peerGroups, Cmds.none)
    }

  | UpdateGroupContent(id, content) =>
    switch (peerGroups |> findOpt(id)) {
    | Some(peersGroup) =>
      let newPeerGroups =
        peerGroups |> addPeerGroup({...peersGroup, content});

      (
        newPeerGroups,
        Cmds.batch([
          offerChangesDebounced(PeerGroup.Id.Set.singleton(id)),
          saveToDb(db, newPeerGroups),
        ]),
      );
    | None => (peerGroups, Cmds.none)
    }

  | RemoveGroup(id) =>
    let newPeerGroups = peerGroups |> removeGroup(id);
    (newPeerGroups, saveToDb(db, newPeerGroups));

  | AddPeerToGroup(peerId, groupId, perms) =>
    /* TODO: Check if the peer is in the friends list */
    let newPeerGroups =
      peerGroups |> addPeerToGroupWithPerms(peerId, groupId, perms);
    (
      newPeerGroups,
      Cmds.batch([
        offerChangesDebounced(PeerGroup.Id.Set.singleton(groupId)),
        saveToDb(db, newPeerGroups),
      ]),
    );

  | UpdatePeerPermissions(peerId, groupId, perms) =>
    /* TODO: Check if the peer is in the friends list */
    switch (
      peerGroups |> findOpt(groupId) |?>> PeerGroup.containsPeer(peerId)
    ) {
    | Some(true) =>
      let newPeerGroups =
        peerGroups |> addPeerToGroupWithPerms(peerId, groupId, perms);
      (
        newPeerGroups,
        Cmds.batch([
          offerChangesDebounced(PeerGroup.Id.Set.singleton(groupId)),
          saveToDb(db, newPeerGroups),
        ]),
      );
    | Some(false)
    | None => (peerGroups, Cmds.none)
    }

  | RemovePeerFromGroup(peerId, groupId) =>
    let newPeerGroups =
      peerGroups
      |> updateGroup(groupId, group => group |> PeerGroup.removePeer(peerId));
    (
      newPeerGroups,
      Cmds.batch([
        offerChangesDebounced(PeerGroup.Id.Set.singleton(groupId)),
        saveToDb(db, newPeerGroups),
      ]),
    );

  | RemovePeer(peerId) =>
    let groups = peerGroups |> getGroupsForPeer(peerId);
    let (changedGroupsIds, newPeerGroups) =
      groups
      |> fold(
           ((changedGroupsIds, newPeerGroups), group) => {
             let newPeerGroups =
               newPeerGroups
               |> addPeerGroup(group |> PeerGroup.removePeer(peerId));
             let changedGroupsIds =
               changedGroupsIds |> PeerGroup.Id.Set.add(group.id);
             (changedGroupsIds, newPeerGroups);
           },
           (PeerGroup.Id.Set.empty, peerGroups),
         );
    /* let newPeerGroups = peerGroups |> removePeerFromAllGroups(peerId); */
    (
      newPeerGroups,
      Cmds.batch([
        offerChangesDebounced(changedGroupsIds),
        saveToDb(db, newPeerGroups),
      ]),
    );

  /* INTERNAL MESSAGES */

  | RtcConnected(rtcConn, peerId) => (
      peerGroups,
      connectionStarted(peerId, rtcConn, peerGroups),
    )

  | RtcGotData(rtcConn, peerId, String(data)) =>
    receivedStringMessageFromPeer(peerId, rtcConn, db, data, peerGroups)

  | _ => (peerGroups, Cmds.none)
  };