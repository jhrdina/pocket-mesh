open BlackTea;
open BsUuid;

// TYPES

type model = {addGroupDialogOpen: bool};
type Msg.t +=
  | ClickedAddGroup
  | ClosedAddGroupDialog(AddGroupDialog.closeResult)
  | GeneratedNewGroupId(Uuid.V4.t);

let generatedNewGroupId = groupId => GeneratedNewGroupId(groupId);

module GuiMsg = Msg;

// HELPERS

let genUuidCmd = uuidToMsg =>
  Cmd.call(callbacks => callbacks^.enqueue(Uuid.V4.create() |> uuidToMsg));

// UPDATE

let init = () => {addGroupDialogOpen: false};

let update = (~contentInitializer, msg, model) => {
  switch (msg) {
  | ClosedAddGroupDialog(Cancel) => ({addGroupDialogOpen: false}, Cmd.none)
  | ClosedAddGroupDialog(CreateNew) => (
      {addGroupDialogOpen: false},
      genUuidCmd(generatedNewGroupId),
    )
  | GeneratedNewGroupId(groupUuid) =>
    let cmd =
      switch (groupUuid |> Uuid.V4.toString |> PM.PeersGroup.Id.ofString) {
      | Some(groupId) =>
        Cmd.batch([
          Cmd.msg(Route.ChangeRoute(Group(groupId))),
          Cmd.msg(
            Msg.ReqP2PMsg(PM.Msg.addGroup(groupId, "", contentInitializer)),
          ),
        ])
      | None => Cmd.none
      };
    (model, cmd);
  | ClosedAddGroupDialog(AddExisting(groupId)) => (
      {addGroupDialogOpen: false},
      Cmd.batch([
        Cmd.msg(Route.ChangeRoute(Group(groupId))),
        Cmd.msg(Msg.ReqP2PMsg(PM.Msg.addGroup(groupId, "", c => c))),
      ]),
    )
  | ClickedAddGroup => ({addGroupDialogOpen: true}, Cmd.none)
  | _ => (model, Cmd.none)
  };
};

let render =
    (
      ~dbState: PM.DbState.t,
      ~runtimeState: PM.RuntimeState.t,
      ~model,
      ~pushMsg,
    ) =>
  MaterialUi.(
    <>
      <List>
        {PM.(
           dbState
           |> DbState.groups
           |> PeersGroups.fold(
                (arr, group) => {
                  let groupId = group |> PeersGroup.id;

                  let (membersPreview, membersCount) =
                    group
                    |> PeersGroup.foldPeersInGroup(
                         ((members, count), peerInGroup) => {
                           let peerId = peerInGroup |> PeerInGroup.id;
                           let peerIdStr = peerId |> Peer.Id.toString;
                           let displayedName =
                             switch (
                               dbState
                               |> DbState.peers
                               |> Peers.findOpt(peerId)
                             ) {
                             | Some(peer) =>
                               GuiUtils.getPeerVisibleName(
                                 ~idMaxChars=6,
                                 peer,
                               )
                             | None =>
                               Js.log(
                                 "Weird: member "
                                 ++ peerIdStr
                                 ++ " is missing in peers",
                               );
                               peerIdStr |> GuiUtils.truncate(6);
                             };
                           ([displayedName, ...members], count + 1);
                         },
                         ([], 0),
                       );

                  let groupRowEl =
                    <GroupRow
                      key={groupId |> PeersGroup.Id.toString}
                      alias={group |> GuiUtils.getPeerGroupVisibleName}
                      membersPreview
                      membersCount
                      onOpenClick={_ =>
                        pushMsg(GuiMsg.ClickedOpenGroup(groupId))
                      }
                      onClick={_ =>
                        pushMsg(Route.ChangeRoute(Group(groupId)))
                      }
                    />;
                  Js.Array.concat(arr, [|groupRowEl|]);
                },
                [||],
              )
           |> GuiUtils.elementArrayWithDefaultMsg("No groups added.")
         )}
      </List>
      <AddGroupDialog
        open_={model.addGroupDialogOpen}
        onClose={res => pushMsg(ClosedAddGroupDialog(res))}
      />
    </>
  );

let renderFab = (~className, ~pushMsg) =>
  MaterialUi.(
    <Fab color=`Secondary className onClick={_ => pushMsg(ClickedAddGroup)}>
      <Icons.Add />
    </Fab>
  );