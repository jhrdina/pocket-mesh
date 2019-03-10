open BlackTea;

// TYPES

type model = {addPeerDialogOpen: bool};
type Msg.t +=
  | ClickedAddGroup
  | ClosedAddGroupDialog(AddPeerDialog.closeResult);

// UPDATE

let init = () => {addPeerDialogOpen: false};

let update = (msg, model) => {
  switch (msg) {
  | ClosedAddGroupDialog(Cancel) => ({addPeerDialogOpen: false}, Cmd.none)
  | ClosedAddGroupDialog(Ok(peerId)) => (
      {addPeerDialogOpen: false},
      Cmd.batch([
        Cmd.msg(Route.ChangeRoute(Peer(peerId))),
        Cmd.msg(Msg.ReqP2PMsg(PM.Msg.addPeer(peerId, ""))),
      ]),
    )
  | ClickedAddGroup => ({addPeerDialogOpen: true}, Cmd.none)
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
    <List>
      {PM.(
         dbState
         |> DbState.groups
         |> PeersGroups.fold(
              (arr, group) => {
                let groupId = group |> PeersGroup.id;
                let groupIdStr = groupId |> PeersGroup.Id.toString;

                let displayedName =
                  group |> PeersGroup.alias != "" ?
                    group |> PeersGroup.alias : groupIdStr;

                let (membersPreview, membersCount) =
                  group
                  |> PeersGroup.foldPeersInGroup(
                       ((members, count), peerInGroup) => {
                         let peerId = peerInGroup |> PeerInGroup.id;
                         let peerIdStr = peerId |> Peer.Id.toString;
                         let displayedName =
                           switch (
                             dbState |> DbState.peers |> Peers.findOpt(peerId)
                           ) {
                           | Some(peer) =>
                             peer |> Peer.alias != "" ?
                               peer |> Peer.alias :
                               GuiUtils.truncate(peerIdStr, 6)
                           | None =>
                             Js.log(
                               "Weird: member "
                               ++ peerIdStr
                               ++ " is missing in peers",
                             );
                             GuiUtils.truncate(peerIdStr, 6);
                           };
                         ([displayedName, ...members], count + 1);
                       },
                       ([], 0),
                     );

                let groupRowEl =
                  <GroupRow
                    key=groupIdStr
                    alias=displayedName
                    membersPreview
                    membersCount
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
  );

let renderFab = (~className, ~pushMsg) =>
  MaterialUi.(
    <Fab color=`Secondary className onClick={_ => pushMsg(ClickedAddGroup)}>
      <Icons.Add />
    </Fab>
  );