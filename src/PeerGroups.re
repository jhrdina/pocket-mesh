open Json.Infix;

/* TODO: Better container */
let empty = [];

let addPeerGroup = (peerGroup, t) => [peerGroup, ...t];
let getFirstId: Types.peerGroups => option(string) =
  fun
  | [first, ..._] => Some(first.id)
  | [] => None;

let updateGroup = (id, updateFn, t) =>
  t
  |> List.rev_map((item: PeerGroup.t) =>
       item.id === id ? updateFn(item) : item
     );

let addPeerToGroupWithPerms = (peerId, groupId, perms, t) =>
  t
  |> updateGroup(groupId, group =>
       group |> PeerGroup.addPeer({id: peerId, permissions: perms})
     );

let addPeerToGroup = (peerId, groupId, t) =>
  addPeerToGroupWithPerms(peerId, groupId, ReadContentAndMembers, t);

/* QUERIES */

let isPeerInAGroup = (peerId, t) =>
  List.exists(PeerGroup.containsPeer(peerId), t);

let getGroupsForPeer = (peerId, t) =>
  List.filter(PeerGroup.containsPeer(peerId), t);

let findOpt = (groupId, t) =>
  switch (List.find((group: PeerGroup.t) => group.id == groupId, t)) {
  | group => Some(group)
  | exception Not_found => None
  };

let getGroupsStatusesForPeer = (peerId, peerGroups) =>
  List.fold_left(
    (groupStatuses, group) =>
      switch (P2PMsg.getGroupStatusForPeer(peerId, group)) {
      | Some(groupStatus) =>
        groupStatuses |> PeerGroup.Id.Map.add(group.id, groupStatus)
      | None => groupStatuses
      },
    PeerGroup.Id.Map.empty,
    peerGroups |> getGroupsForPeer(peerId),
  );

/* ENCODING/DECODING */

let encode = peerGroups =>
  Json.(
    Array(
      peerGroups |> List.rev_map(peerGroup => peerGroup |> PeerGroup.encode),
    )
  );

let decode = json =>
  Json.(
    switch (json |> array) {
    | Some(jsonArr) =>
      jsonArr
      |> List.fold_left(
           (jsonArr, jsonItem) =>
             switch (jsonArr) {
             | Some(jsonArr) =>
               switch (jsonItem |> PeerGroup.decode) {
               | Some(peerGroup) => Some([peerGroup, ...jsonArr])
               | None => None
               }
             | None => None
             },
           Some([]),
         )

    | None => None
    }
  );

/* CMDS */

let saveToDb = (db, model) =>
  Db.setPeerGroups(model |> encode, _ => Msgs.noop, _ => Msgs.noop, db);

/* UPDATE */

let connectionStarted = (peerId, rtcConn, peerGroups) => {
  let groupsStatuses = getGroupsStatusesForPeer(peerId, peerGroups);
  let msgForPeer = P2PMsg.ChangesOffer(groupsStatuses);
  rtcConn->RTCCmds.send(msgForPeer |> P2PMsg.encode |> Json.stringify);
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
    RTCCmds.send(rtcConn, msgForPeer |> P2PMsg.encode |> Json.stringify);
  | None => Cmds.none
  };
};

let maybeGetGroupsChangesForRequest =
    (peerId, groupsChangesRequest, peerGroups) => {
  let groupsChanges =
    /* TODO: THINK: Isn't there some better meta-operation? */
    PeerGroup.Id.Map.fold(
      (groupId, groupChangesRequest, groupsChanges) =>
        switch (peerGroups |> findOpt(groupId)) {
        | Some(group) =>
          let maybeGroupChanges =
            P2PMsg.maybeGetGroupChangesForRequest(
              peerId,
              groupChangesRequest,
              group,
            );
          switch (maybeGroupChanges) {
          | Some(groupChanges) =>
            groupsChanges |> PeerGroup.Id.Map.add(group.id, groupChanges)
          | None => groupsChanges
          };

        | None => groupsChanges
        },
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
    RTCCmds.send(rtcConn, msgForPeer |> P2PMsg.encode |> Json.stringify);
  | None => Cmds.none
  };
};

let receivedChanges = (peerId, groupsChanges, peerGroups) =>
  PeerGroup.Id.Map.fold(
    (groupId, groupChanges, newPeerGroups) =>
      switch (peerGroups |> findOpt(groupId)) {
      | Some(group) =>
        let maybeNewPeerGroup =
          P2PMsg.maybeGetPeerGroupWithAppliedChanges(
            peerId,
            groupChanges,
            group,
          );
        switch (maybeNewPeerGroup) {
        | Some(peerGroup) =>
          newPeerGroups |> updateGroup(group.id, _ => peerGroup)
        | None => newPeerGroups
        };
      | None => newPeerGroups
      },
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
    (peerId, rtcConn, db, stringMessage, peerGroups) => {
  let json =
    switch (stringMessage |> Json.parse) {
    | json => Some(json)
    | exception _ => None
    };
  switch (json |?> P2PMsg.decode) {
  | Some(msg) => receivedMessageFromPeer(peerId, rtcConn, db, msg, peerGroups)
  | None => (
      peerGroups,
      Cmds.log("Failed to parse message from peer " ++ peerId),
    )
  };
};

let init = (db, thisPeerId, maybeDbPeerGroups) =>
  switch (maybeDbPeerGroups |?> decode) {
  | Some(dbPeerGroups) => (dbPeerGroups, Cmds.none)
  | None =>
    let newPeerGroups =
      empty
      |> addPeerGroup(
           /* TODO: Really a fixed ID? */
           PeerGroup.make("aaa", thisPeerId)
           |> PeerGroup.addPeer({
                id: thisPeerId,
                permissions: WriteContent(WriteMembers),
              }),
         );
    (newPeerGroups, saveToDb(db, newPeerGroups));
  };

let update = (db, peerGroups, msg) =>
  switch (msg) {
  | Msgs.AddPeerWithIdAndPublicKeyToGroup(id, _key, groupId) =>
    let newPeerGroups = peerGroups |> addPeerToGroup(id, groupId);
    (newPeerGroups, saveToDb(db, newPeerGroups));
  | RtcConnected(rtcConn, peerId) => (
      peerGroups,
      connectionStarted(peerId, rtcConn, peerGroups),
    )
  | RtcGotData(rtcConn, peerId, data) =>
    receivedStringMessageFromPeer(peerId, rtcConn, db, data, peerGroups)

  | AddPeerToGroupWithPerms(peerId, groupId, strPerms) =>
    /* TODO: Check if the peer is in the friends list */
    switch (PeerGroup.decodePermissions(strPerms)) {
    | Some(perms) =>
      let newPeerGroups =
        peerGroups |> addPeerToGroupWithPerms(peerId, groupId, perms);
      (newPeerGroups, saveToDb(db, newPeerGroups));
    | None => (
        peerGroups,
        Cmds.log("Invalid permissions, try crmr, crwmr, or crwmrw"),
      )
    }

  /* TODO: Debug, remove */
  | AddItem(text) =>
    let newPeerGroups =
      peerGroups
      |> updateGroup("aaa", peerGroup =>
           {
             ...peerGroup,
             content:
               peerGroup.content
               |> PeerGroup.AM.change("Add item", root =>
                    PeerGroup.AM.Json.(
                      switch (root |> Map.get("items") |?> List.ofJson) {
                      | Some(list) =>
                        root
                        |> Map.add(
                             "items",
                             list
                             |> List.prepend(string(text))
                             |> List.toJson,
                           )
                      | None => root
                      }
                    )
                  ),
           }
         );
    (newPeerGroups, saveToDb(db, newPeerGroups));

  /* TODO: Debug, remove */
  | PrintData =>
    switch (peerGroups |> findOpt("aaa")) {
    | Some(peerGroup) => (
        peerGroups,
        Cmds.log(
          PeerGroup.AM.(peerGroup.content |> root |> Json.Map.get("items")),
        ),
      )
    | None => (peerGroups, Cmds.none)
    }

  | _ => (peerGroups, Cmds.none)
  };