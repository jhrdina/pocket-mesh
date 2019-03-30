open Json.Infix;
open BlackTea;

type t = {
  lastPeersConnections: PeersConnections.t,
  lastPeersGroups: PeersGroups.t,
  lastPeersGroupsDebounced: PeersGroups.t,
  offerChangesDebouncer: Debouncer.t(Msgs.t),
};

type Msgs.t +=
  | OfferChangesDebouncerMsg(Debouncer.msg(Msgs.t))
  | OfferChangesFromGroupsDebounced;

let offerChangesDebouncerMsg = msg => OfferChangesDebouncerMsg(msg);
let offerChangesFromGroupsDebounced = () => OfferChangesFromGroupsDebounced;

let offerChangesDebounced = () =>
  Debouncer.debounceCmd(
    OfferChangesFromGroupsDebounced,
    offerChangesDebouncerMsg,
    3000,
  );

// Helpers

let getGroupStatusForPeer:
  (PeerId.t, PeerGroup.t) => option(P2PMsg.groupStatus) =
  (peerId, group) =>
    group
    |> PeerGroup.getPeerPermissions(peerId)
    |?>> (
      permissions => {
        P2PMsg.id: group.id,
        clock: group.content |> PeerGroup.AM.getClock,
        permissions,
        permissionsClock: group.peers |> PeerGroup.AM.getClock,
      }
    );

let getGroupsStatusesForPeer = (peerId, peersGroups) =>
  PeersGroups.fold(
    (groupStatuses, group) =>
      switch (getGroupStatusForPeer(peerId, group)) {
      | Some(groupStatus) =>
        groupStatuses |> PeerGroup.Id.Map.add(group.id, groupStatus)
      | None => groupStatuses
      },
    PeerGroup.Id.Map.empty,
    peersGroups |> PeersGroups.getGroupsForPeer(peerId),
  );

// ================================================
// Connection started
// ================================================

let connectionStarted = (peerId, peersGroups) => {
  let groupsStatuses = getGroupsStatusesForPeer(peerId, peersGroups);
  let msgForPeer = P2PMsg.ChangesOffer(groupsStatuses);
  Cmd.msg(
    PeersConnections.Send(
      peerId,
      String(msgForPeer |> P2PMsg.encode |> Json.stringify),
    ),
  );
};

// ================================================
// Received ChangesOffer
// ================================================

let maybeCreateRequestForMissingChanges:
  (P2PMsg.groupStatus, P2PMsg.groupStatus) =>
  option(P2PMsg.groupChangesRequest) =
  (remoteGroupStatus, localGroupStatus) => {
    let wantsContent =
      switch (localGroupStatus.permissions) {
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

let receivedChangesOffer = (peerId, remoteGroupsStatuses, peersGroups) => {
  let localGroupsStatuses = getGroupsStatusesForPeer(peerId, peersGroups);
  let maybeRequests =
    maybeCreateRequestsForMissingChanges(
      remoteGroupsStatuses,
      localGroupsStatuses,
    );
  switch (maybeRequests) {
  | Some(requests) =>
    let msgForPeer = P2PMsg.ChangesRequest(requests);
    Cmd.msg(
      PeersConnections.Send(
        peerId,
        String(msgForPeer |> P2PMsg.encode |> Json.stringify),
      ),
    );
  | None => Cmds.none
  };
};

// ================================================
// Received ChangesRequest
// ================================================

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

let maybeGetGroupsChangesForRequest =
    (peerId, groupsChangesRequest, peersGroups) => {
  let groupsChanges =
    /* TODO: THINK: Isn't there some better meta-operation? */
    PeerGroup.Id.Map.fold(
      (groupId, groupChangesRequest, groupsChanges) =>
        peersGroups
        |> PeersGroups.findOpt(groupId)
        |?> maybeGetGroupChangesForRequest(peerId, groupChangesRequest)
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

let receivedChangesRequest = (peerId, requestedChanges, peersGroups) => {
  let maybeChanges =
    maybeGetGroupsChangesForRequest(peerId, requestedChanges, peersGroups);
  switch (maybeChanges) {
  | Some(changes) =>
    let msgForPeer: P2PMsg.t = Changes(changes);
    Cmd.msg(
      PeersConnections.Send(
        peerId,
        String(msgForPeer |> P2PMsg.encode |> Json.stringify),
      ),
    );
  | None => Cmds.none
  };
};

// ================================================
// Received Changes
// ================================================

let applyValidChanges = (peerId, groupChanges: P2PMsg.groupChanges, peerGroup) =>
  switch (PeerGroup.getPeerPermissions(peerId, peerGroup)) {
  | Some(permissions) =>
    let applyContentChangesCmd =
      switch (permissions, groupChanges) {
      | (WriteContent(_), Content(changes) | ContentAndMembers(changes, _)) =>
        Cmd.msg(PeersGroups.ApplyContentChanges(peerGroup.id, changes))
      | (WriteContent(_), Members(_))
      | (ReadContentAndMembers, _) => Cmd.none
      };

    let applyMembersChangesCmd =
      switch (permissions, groupChanges) {
      | (
          WriteContent(WriteMembers),
          Members(changes) | ContentAndMembers(_, changes),
        ) =>
        Cmd.msg(PeersGroups.ApplyMembersChanges(peerGroup.id, changes))
      | (WriteContent(WriteMembers), Content(_))
      | (ReadContentAndMembers | WriteContent(ReadMembers), _) => Cmd.none
      };

    Cmd.batch([applyContentChangesCmd, applyMembersChangesCmd]);

  | None => Cmd.none
  };

let receivedChanges = (peerId, groupsChanges, peersGroups) =>
  PeerGroup.Id.Map.fold(
    (groupId, groupChanges, cmdList) =>
      peersGroups
      |> PeersGroups.findOpt(groupId)
      |?>> applyValidChanges(peerId, groupChanges)
      |?>> (cmd => [cmd, ...cmdList])
      |? cmdList,
    groupsChanges,
    [],
  )
  |> Cmd.batch;

// ================================================

let receivedMessageFromPeer = (peerId, message: P2PMsg.t, peersGroups) =>
  switch (message) {
  | ChangesOffer(changesOffer) =>
    receivedChangesOffer(peerId, changesOffer, peersGroups)
  | ChangesRequest(requestedChanges) =>
    receivedChangesRequest(peerId, requestedChanges, peersGroups)
  | Changes(groupsChanges) =>
    receivedChanges(peerId, groupsChanges, peersGroups)
  };

let receivedStringMessageFromPeer = (peerId, stringMessage, peersGroups) =>
  stringMessage
  |> JsonUtils.parseOpt
  |?> P2PMsg.decode
  |?>> (msg => receivedMessageFromPeer(peerId, msg, peersGroups))
  |? Cmds.log(
       "Failed to parse message from peer " ++ (peerId |> PeerId.toString),
     );

let derive = (~peersGroups, ~peersConnections, model) => {
  let connStartedCmd =
    PeerId.Map.symmetric_diff(
      model.lastPeersConnections,
      peersConnections,
      ~f=
        ((peerId, diffRes), acc) =>
          if (diffRes |> PeersConnections.diffGotConnected) {
            [connectionStarted(peerId, peersGroups), ...acc];
          } else {
            acc;
          },
      ~veq=(===),
      ~acc=[],
    )
    |> Cmd.batch;

  let offerChangesCmd =
    model.lastPeersGroups !== peersGroups ? offerChangesDebounced() : Cmd.none;

  (
    {
      ...model,
      lastPeersGroups: peersGroups,
      lastPeersConnections: peersConnections,
    },
    Cmd.batch([connStartedCmd, offerChangesCmd]),
  );
};

let init = (~peersGroups, ~peersConnections) => {
  let model = {
    lastPeersConnections: PeerId.Map.empty,
    lastPeersGroups: PeersGroups.empty,
    lastPeersGroupsDebounced: PeersGroups.empty,
    offerChangesDebouncer: Debouncer.init(),
  };
  derive(~peersGroups, ~peersConnections, model);
};

let addConnectedMembersIdsFromGroupToSet =
    (~peersConnections: PeersConnections.t, group, set) =>
  group
  |> PeerGroup.foldPeersInGroup(
       (ids, peerInGroup) =>
         switch (peersConnections |> PeerId.Map.findOpt(peerInGroup.id)) {
         | Some(Connected(_)) => ids |> PeerId.Set.add(peerInGroup.id)
         | Some(_)
         | None => ids
         },
       set,
     );

let maybeCreateIndependentChangesRequest:
  (option(PeerGroup.groupPermissions), P2PMsg.groupStatus) =>
  option(P2PMsg.groupChangesRequest) =
  (oldPermissions, localGroupStatus) => {
    let wantsContent =
      switch (oldPermissions, localGroupStatus.permissions) {
      | (None | Some(ReadContentAndMembers), WriteContent(_)) => true
      | (_, _) => false
      };

    let wantsMembers =
      switch (oldPermissions, localGroupStatus.permissions) {
      | (
          None | Some(ReadContentAndMembers | WriteContent(ReadMembers)),
          WriteContent(WriteMembers),
        ) =>
        true
      | (Some(WriteContent(WriteMembers)), WriteContent(WriteMembers))
      | (_, WriteContent(ReadMembers))
      | (_, ReadContentAndMembers) => false
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

let addGroupChangesRequests =
    (~peersConnections, ~oldGroup, group, peersToBeRequestedChanges) =>
  group
  |> PeerGroup.foldPeersInGroup(
       (peers, peerInGroup) =>
         if (peersConnections
             |> PeersConnections.isPeerConnected(peerInGroup.id)) {
           getGroupStatusForPeer(peerInGroup.id, group)
           |?> maybeCreateIndependentChangesRequest(
                 oldGroup |?> PeerGroup.getPeerPermissions(peerInGroup.id),
               )
           |?>> (
             request => {
               let requestsForPeer =
                 peers
                 |> PeerId.Map.findOpt(peerInGroup.id)
                 |? PeerGroup.Id.Map.empty
                 |> PeerGroup.Id.Map.add(group |> PeerGroup.id, request);

               peers |> PeerId.Map.add(peerInGroup.id, requestsForPeer);
             }
           )
           |? peers;
         } else {
           peers;
         },
       peersToBeRequestedChanges,
     );

let update = (~peersGroups, ~peersConnections, msg, model) => {
  let (model, cmd) =
    switch (msg) {
    | PeersConnections.GotData(peerId, String(data)) => (
        model,
        receivedStringMessageFromPeer(peerId, data, peersGroups),
      )

    | OfferChangesDebouncerMsg(msg) =>
      let (offerChangesDebouncer, offerChangesDebouncerCmd) =
        Debouncer.update(
          d => d,
          model.offerChangesDebouncer,
          msg,
          offerChangesDebouncerMsg,
        );
      ({...model, offerChangesDebouncer}, offerChangesDebouncerCmd);

    | OfferChangesFromGroupsDebounced =>
      let (peersToBeOfferedChanges, peersToBeRequestedChanges) =
        PeerGroup.Id.Map.symmetric_diff(
          model.lastPeersGroupsDebounced,
          peersGroups,
          ~f=
            (
              (_groupId, diffRes),
              (peersToBeOfferedChanges, peersToBeRequestedChanges) as acc,
            ) =>
              switch (diffRes) {
              | Left(_removedGroup) => acc

              | Right(addedGroup) =>
                // Simply notify all the members
                let peersToBeOfferedChanges =
                  addConnectedMembersIdsFromGroupToSet(
                    ~peersConnections,
                    addedGroup,
                    peersToBeOfferedChanges,
                  );

                let peersToBeRequestedChanges =
                  addGroupChangesRequests(
                    ~peersConnections,
                    ~oldGroup=None,
                    addedGroup,
                    peersToBeRequestedChanges,
                  );

                (peersToBeOfferedChanges, peersToBeRequestedChanges);

              | Unequal(oldGroup, newGroup)
                  when
                    oldGroup.peers !== newGroup.peers
                    || oldGroup.content !== newGroup.content =>
                let peersToBeOfferedChanges =
                  addConnectedMembersIdsFromGroupToSet(
                    ~peersConnections,
                    newGroup,
                    peersToBeOfferedChanges,
                  );

                let peersToBeRequestedChanges =
                  addGroupChangesRequests(
                    ~peersConnections,
                    ~oldGroup=Some(oldGroup),
                    newGroup,
                    peersToBeRequestedChanges,
                  );

                (peersToBeOfferedChanges, peersToBeRequestedChanges);

              | Unequal(_, _) => acc
              },
          ~veq=(===),
          ~acc=(PeerId.Set.empty, PeerId.Map.empty),
        );
      let offerChangesCmd =
        PeerId.Set.fold(
          (peerId, cmdList) => {
            Js.log("active connection to " ++ PeerId.toString(peerId));
            let groupsStatuses =
              getGroupsStatusesForPeer(peerId, peersGroups);
            let msgForPeer = P2PMsg.ChangesOffer(groupsStatuses);
            let sendOfferCmd =
              Cmd.msg(
                PeersConnections.Send(
                  peerId,
                  String(msgForPeer |> P2PMsg.encode |> Json.stringify),
                ),
              );

            let sendRequestCmd =
              switch (peersToBeRequestedChanges |> PeerId.Map.findOpt(peerId)) {
              | Some(requests) =>
                let msgForPeer = P2PMsg.ChangesRequest(requests);
                Cmd.msg(
                  PeersConnections.Send(
                    peerId,
                    String(msgForPeer |> P2PMsg.encode |> Json.stringify),
                  ),
                );
              | None => Cmd.none
              };

            [sendOfferCmd, sendRequestCmd, ...cmdList];
          },
          peersToBeOfferedChanges,
          [],
        )
        |> Cmds.batch;
      // let offerChangesCmd = Cmds.log("OfferChangesDebounced");
      ({...model, lastPeersGroupsDebounced: peersGroups}, offerChangesCmd);
    | _ => (model, Cmd.none)
    };

  let (model, deriveCmd) = derive(~peersGroups, ~peersConnections, model);
  (model, Cmd.batch([cmd, deriveCmd]));
};