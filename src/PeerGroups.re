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
  |> List.map((item: PeerGroup.t) => item.id === id ? updateFn(item) : item);

let addPeerToGroupWithPerms = (peerId, groupId, perms, t) =>
  t
  |> updateGroup(groupId, group =>
       group |> PeerGroup.addPeer({id: peerId, permissions: perms})
     );

let addPeerToGroup = (peerId, groupId, t) =>
  addPeerToGroupWithPerms(peerId, groupId, ReadContentAndMembers, t);

/* QUERIES */

let isPeerInAGroup = (peerId, t) =>
  List.exists(peerGroup => peerGroup |> PeerGroup.containsPeer(peerId), t);

let getGroupsForPeer = (peerId, t) =>
  List.fold_left(
    (groupsForPeer, group) =>
      if (group |> PeerGroup.containsPeer(peerId)) {
        [group, ...groupsForPeer];
      } else {
        groupsForPeer;
      },
    /* TODO: Better container */
    [],
    t,
  );

let findOpt = (groupId, t) =>
  switch (List.find((group: PeerGroup.t) => group.id == groupId, t)) {
  | group => Some(group)
  | exception Not_found => None
  };

let getGroupsStatusesForPeer = (peerId, peerGroups) =>
  List.fold_left(
    (groupStatuses, group) =>
      switch (group |> PeerGroup.getPeerInGroup(peerId)) {
      | Some(peerInGroup) =>
        let groupStatus: P2PMsg.groupStatus = {
          id: group.id,
          clock: group.content |> PeerGroup.AM.getClock,
          permissions: peerInGroup.permissions,
          permissionsClock: group.peers |> PeerGroup.AM.getClock,
        };
        groupStatuses |> PeerGroup.Id.Map.add(group.id, groupStatus);
      | None => groupStatuses
      },
    PeerGroup.Id.Map.empty,
    peerGroups |> getGroupsForPeer(peerId),
  );

/* ENCODING/DECODING */

let encode = peerGroups =>
  Json.(
    Array(peerGroups |> List.map(peerGroup => peerGroup |> PeerGroup.encode))
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

/* UPDATE */

let saveToDb = (db, model) =>
  Db.setPeerGroups(model |> encode, _ => Msgs.noop, _ => Msgs.noop, db);

let connectionStarted = (peerId, rtcConn, peerGroups) => {
  let groupsStatuses = getGroupsStatusesForPeer(peerId, peerGroups);
  let msgForPeer = P2PMsg.ChangesOffer(groupsStatuses);
  rtcConn->RTCCmds.send(msgForPeer |> P2PMsg.encode |> Json.stringify);
};

let requestChangesFromPeer = peerId => {};

let maybeCreateRequestForMissingChanges:
  (P2PMsg.groupStatus, P2PMsg.groupStatus) =>
  option(P2PMsg.groupChangesRequest) =
  (remoteGroupStatus, localGroupStatus) => {
    let wantsContent =
      switch (localGroupStatus.P2PMsg.permissions) {
      | WriteContent(_)
          when
            !
              PeerGroup.AM.Clock.lessOrEqual(
                remoteGroupStatus.clock,
                localGroupStatus.clock,
              ) =>
        true
      | WriteContent(_)
      | ReadContentAndMembers => false
      };

    let wantsMembers =
      switch (localGroupStatus.permissions) {
      | WriteContent(WriteMembers)
          when
            !
              PeerGroup.AM.Clock.lessOrEqual(
                remoteGroupStatus.permissionsClock,
                localGroupStatus.permissionsClock,
              ) =>
        true
      | WriteContent(WriteMembers)
      | WriteContent(ReadMembers)
      | ReadContentAndMembers => false
      };

    switch (wantsContent, wantsMembers) {
    | (true, true) =>
      Some(
        ContentAndMembers(
          localGroupStatus.clock,
          localGroupStatus.permissionsClock,
        ),
      )
    | (true, false) => Some(Content(localGroupStatus.clock))
    | (false, true) => Some(Members(localGroupStatus.permissionsClock))
    | (false, false) => None
    };
  };

let maybeCreateRequestsForMissingChanges =
    (remoteGroupsStatuses, localGroupsStatuses) => {
  let requests =
    PeerGroup.Id.Map.fold(
      (groupId, remoteGroupStatus, requestedChanges) =>
        switch (localGroupsStatuses |> PeerGroup.Id.Map.find(groupId)) {
        | localGroupStatus =>
          let maybeRequest =
            maybeCreateRequestForMissingChanges(
              remoteGroupStatus,
              localGroupStatus,
            );
          switch (maybeRequest) {
          | Some(request) =>
            requestedChanges |> PeerGroup.Id.Map.add(groupId, request)
          | None => requestedChanges
          };
        | exception Not_found => requestedChanges
        },
      remoteGroupsStatuses,
      PeerGroup.Id.Map.empty,
    );
  !PeerGroup.Id.Map.is_empty(requests) ? Some(requests) : None;
};

let receivedChangesOffer = (peerId, rtcConn, offeredChanges, peerGroups) => {
  let localGroupsStatuses = getGroupsStatusesForPeer(peerId, peerGroups);
  let maybeRequests =
    maybeCreateRequestsForMissingChanges(offeredChanges, localGroupsStatuses);
  switch (maybeRequests) {
  | Some(requests) =>
    let msgForPeer = P2PMsg.ChangesRequest(requests);
    RTCCmds.send(rtcConn, msgForPeer |> P2PMsg.encode |> Json.stringify);
  | None => Cmds.none
  };
};

let maybeGetGroupChangesForRequest =
    (peerId, groupChangesRequest: P2PMsg.groupChangesRequest, group) =>
  if (PeerGroup.containsPeer(peerId, group)) {
    /* If peer is in a group it automatically has at least read permissions. */
    let maybeContentChanges =
      switch (groupChangesRequest) {
      | Content(clock)
      | ContentAndMembers(clock, _) =>
        Some(group.content |> PeerGroup.AM.getChangesFromTime(clock))
      | Members(_) => None
      };

    let maybeMembersChanges =
      switch (groupChangesRequest) {
      | Members(clock)
      | ContentAndMembers(_, clock) =>
        Some(group.peers |> PeerGroup.AM.getChangesFromTime(clock))
      | Content(_) => None
      };

    switch (maybeContentChanges, maybeMembersChanges) {
    | (Some(contentChanges), Some(membersChanges)) =>
      Some(P2PMsg.ContentAndMembers(contentChanges, membersChanges))
    | (Some(contentChanges), None) => Some(Content(contentChanges))
    | (None, Some(membersChanges)) => Some(Members(membersChanges))
    | (None, None) => None
    };
  } else {
    None;
  };

let maybeGetGroupsChangesForRequest = (peerId, requestedChanges, peerGroups) => {
  let groupsChanges =
    /* TODO: THINK: Isn't there some better meta-operation? */
    PeerGroup.Id.Map.fold(
      (groupId, groupChangesRequest, groupsChanges) =>
        switch (peerGroups |> findOpt(groupId)) {
        | Some(group) =>
          let maybeGroupChanges =
            maybeGetGroupChangesForRequest(
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
      requestedChanges,
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

let maybeGetPeerGroupWithAppliedChanges =
    (peerId, groupChanges: P2PMsg.groupChanges, peerGroup) =>
  switch (PeerGroup.getPeerInGroup(peerId, peerGroup)) {
  | Some(peerInGroup) =>
    let maybeNewContent =
      switch (peerInGroup.permissions, groupChanges) {
      | (WriteContent(_), Content(changes) | ContentAndMembers(changes, _)) =>
        Some(peerGroup.content |> PeerGroup.AM.applyChanges(changes))
      | (WriteContent(_), Members(_))
      | (ReadContentAndMembers, _) => None
      };

    let maybeNewMembers =
      switch (peerInGroup.permissions, groupChanges) {
      | (
          WriteContent(WriteMembers),
          Members(changes) | ContentAndMembers(_, changes),
        ) =>
        Some(peerGroup.peers |> PeerGroup.AM.applyChanges(changes))
      | (WriteContent(WriteMembers), Content(_))
      | (ReadContentAndMembers | WriteContent(ReadMembers), _) => None
      };

    switch (maybeNewContent, maybeNewMembers) {
    | (Some(newContent), Some(newMembers)) =>
      Some({...peerGroup, content: newContent, peers: newMembers})
    | (Some(newContent), None) => Some({...peerGroup, content: newContent})
    | (None, Some(newMembers)) => Some({...peerGroup, peers: newMembers})
    | (None, None) => None
    };
  | None => None
  };

let receivedChanges = (peerId, groupsChanges, peerGroups) =>
  PeerGroup.Id.Map.fold(
    (groupId, groupChanges, newPeerGroups) =>
      switch (peerGroups |> findOpt(groupId)) {
      | Some(group) =>
        let maybeNewPeerGroup =
          maybeGetPeerGroupWithAppliedChanges(peerId, groupChanges, group);
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
  | Msgs.AddPeerWithIdAndPublicKeyToGroup(id, key, groupId) =>
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