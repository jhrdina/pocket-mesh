open BlackTea;

// TYPES

type model = {addPeerDialogOpen: bool};
type Msg.t +=
  | ClickedAddPeer
  | ClosedAddPeerDialog(AddPeerDialog.closeResult);

// UPDATE

let init = () => {addPeerDialogOpen: false};

let update = (msg, model) => {
  switch (msg) {
  | ClosedAddPeerDialog(Cancel) => ({addPeerDialogOpen: false}, Cmd.none)
  | ClosedAddPeerDialog(Ok(peerId)) => (
      {addPeerDialogOpen: false},
      Cmd.batch([
        Cmd.msg(Route.ChangeRoute(Peer(peerId))),
        Cmd.msg(Msg.ReqP2PMsg(PM.Msg.addPeer(peerId, ""))),
      ]),
    )
  | ClickedAddPeer => ({addPeerDialogOpen: true}, Cmd.none)
  | _ => (model, Cmd.none)
  };
};

let render =
    (
      ~dbState: PM.DbState.t,
      ~runtimeState: PM.RuntimeState.t,
      ~model,
      ~pushMsg,
    ) => {
  MaterialUi.(
    <div>
      <List>
        {PM.(
           dbState
           |> DbState.peers
           |> Peers.fold(
                (arr, peer) => {
                  let peerId = peer |> Peer.id;

                  let connectionState =
                    switch (
                      (
                        runtimeState
                        |> RuntimeState.peersConnections
                        |> PeersConnections.getPeerConnectionState(peerId)
                      )
                      ->Belt.Option.map(
                          PeersConnections.classifyConnectionState,
                        )
                    ) {
                    | None => PeerStatusIndicator.Offline
                    | Some(AcceptingConnection)
                    | Some(InitiatingConnection) => Connecting
                    | Some(Connected) => Connected
                    };

                  let signalState =
                    switch (
                      runtimeState
                      |> RuntimeState.peersStatuses
                      |> PeersStatuses.getPeerStatus(peerId)
                    ) {
                    | Online => PeerStatusIndicator.Online
                    | Offline => PeerStatusIndicator.Offline
                    };

                  let inGroup =
                    dbState
                    |> DbState.groups
                    |> PeersGroups.isPeerInAGroup(peerId);

                  let peerRowEl =
                    <PeerRow
                      key={peerId |> PM.Peer.Id.toString}
                      onClick={_ =>
                        pushMsg(Route.ChangeRoute(Peer(peerId)))
                      }
                      peerId
                      alias={peer |> Peer.alias}
                      connectionState
                      signalState
                      inGroup
                    />;
                  Js.Array.concat(arr, [|peerRowEl|]);
                },
                [||],
              )
           |> GuiUtils.elementArrayWithDefaultMsg("No friends added.")
         )}
      </List>
      <AddPeerDialog
        open_={model.addPeerDialogOpen}
        onClose={res => pushMsg(ClosedAddPeerDialog(res))}
      />
    </div>
  );
};

let renderFab = (~className, ~pushMsg) =>
  MaterialUi.(
    <Fab color=`Secondary className onClick={_ => pushMsg(ClickedAddPeer)}>
      <Icons.PersonAdd />
    </Fab>
  );