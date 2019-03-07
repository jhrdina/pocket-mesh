let elementArrayWithDefaultMsg = (text, arr) =>
  arr |> Array.length > 0 ? arr |> ReasonReact.array : <LonelyMessage text />;

let render = (~core: PocketMeshPeer.State.taggedT, ~pushMsg) => {
  MaterialUi.(
    <List>
      {switch (core) {
       | WaitingForDbAndIdentity(_) => <LonelyMessage text="Loading..." />
       | HasIdentity(dbState, runtimeState) =>
         PocketMeshPeer.(
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
                      onClick={_ =>
                        pushMsg(Route.ChangeRoute(Peer(peerId)))
                      }
                      alias={peer |> Peer.alias}
                      connectionState
                      signalState
                      inGroup
                    />;
                  Js.Array.concat(arr, [|peerRowEl|]);
                },
                [||],
              )
           |> elementArrayWithDefaultMsg("No friends added.")
         )
       }}
    </List>
  );
};

let renderFab = (~className) =>
  MaterialUi.(<Fab color=`Secondary className> <Icons.PersonAdd /> </Fab>);