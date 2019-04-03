let component = ReasonReact.statelessComponent("PeerRowWithData");
let make = (~peerId, ~onClick, ~dbState, ~runtimeState, _children) => {
  ...component,
  render: _self => {
    PM.(
      {
        let connectionState =
          switch (
            (
              runtimeState
              |> RuntimeState.peersConnections
              |> PeersConnections.getPeerConnectionState(peerId)
            )
            ->Belt.Option.map(PeersConnections.classifyConnectionState)
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
          dbState |> DbState.groups |> PeersGroups.isPeerInAGroup(peerId);

        let displayedName = GuiUtils.getPeerVisibleName(~dbState, peerId);

        <PeerRow
          key={peerId |> PM.Peer.Id.toString}
          onClick
          name=displayedName
          connectionState
          signalState
          inGroup
        />;
      }
    );
  },
};