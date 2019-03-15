open BlackTea;
open Infix;

// TYPES

type model = {alias: string};
type Msg.t +=
  | ChangedAlias(string)
  | ClickedDelete;

// UPDATE

let init = (~dbState, peerId) => (
  {
    alias:
      dbState
      |> PM.DbState.peers
      |> PM.Peers.findOpt(peerId)
      |?>> PM.Peer.alias
      |? "",
  },
  Cmd.none,
);

let update = (~peerId, msg, model: model) => {
  switch (msg) {
  | ChangedAlias(alias) => ({...model, alias}, Cmd.none)
  | ClickedDelete => (
      model,
      Cmd.batch([
        Cmd.msg(Route.ChangeRoute(Main(Peers))),
        Cmd.msg(Msg.ReqP2PMsg(PM.Msg.removePeer(peerId))),
      ]),
    )
  | _ => (model, Cmd.none)
  };
};

let subscriptions = model => {
  Sub.none;
};

// VIEW

let component = ReasonReact.statelessComponent("PeerScreen");

let useStyles =
  MuiStylesHooks.makeWithTheme(theme =>
    [
      {name: "root", styles: ReactDOMRe.Style.make()},
      {
        name: "appBar",
        styles:
          ReactDOMRe.Style.make(
            ~backgroundColor=MaterialUi.Colors.Grey.c200,
            ~color=
              MaterialUi.Theme.(
                theme
                |> Theme.paletteGet
                |> Palette.textGet
                |> TypeText.secondaryGet
              ),
            (),
          ),
      },
      {
        name: "toolbar",
        styles:
          ReactDOMRe.Style.make(~paddingLeft="0", ~paddingRight="0", ()),
      },
      {
        name: "titleInput",
        styles:
          ReactDOMRe.Style.make(
            ~flex="1",
            ~borderRadius="4px",
            ~padding="0 8px",
            ~backgroundColor=MaterialUi.Colors.Grey.c50,
            ~fontWeight="500",
            ~fontSize="1.125rem",
            (),
          ),
      },
      {
        name: "peerIdBox",
        styles: ReactDOMRe.Style.make(~margin="11px 16px", ()),
      },
    ]
  );

let make =
    (
      ~peerId,
      ~dbState,
      ~runtimeState: PM.RuntimeState.t,
      ~model,
      ~pushMsg,
      _children,
    ) => {
  let peerAliasStr =
    dbState
    |> PM.DbState.peers
    |> PM.Peers.findOpt(peerId)
    |?>> PM.Peer.alias
    |? "";

  let signalStateStr =
    switch (
      runtimeState
      |> PM.RuntimeState.peersStatuses
      |> PM.PeersStatuses.getPeerStatus(peerId)
    ) {
    | Online => "Online"
    | Offline => "Offline"
    };

  let connState =
    (
      runtimeState
      |> PM.RuntimeState.peersConnections
      |> PM.PeersConnections.getPeerConnectionState(peerId)
    )
    ->Belt.Option.map(PM.PeersConnections.classifyConnectionState);

  let connStateStr =
    switch (connState) {
    | None => "Not established"
    | Some(InitiatingConnection)
    | Some(AcceptingConnection) => "Connecting..."
    | Some(Connected) => "Connected"
    };

  let connStateStrSecondary =
    switch (connState) {
    | None =>
      if (!(
            dbState
            |> PM.DbState.groups
            |> PM.PeersGroups.isPeerInAGroup(peerId)
          )) {
        "Connection is not needed because peer is not added in any group.";
      } else if (runtimeState
                 |> PM.RuntimeState.peersStatuses
                 |> PM.PeersStatuses.getPeerStatus(peerId) == Offline) {
        "Connection cannot be established because the peer is offline.";
      } else {
        "";
      }

    | Some(InitiatingConnection) => "Initiating connection to the peer..."
    | Some(AcceptingConnection) => "Accepting connection request from the peer..."
    | Some(Connected) => "Direct P2P connection is established."
    };

  {
    ...component,
    render: _self => {
      MaterialUi.(
        <UseHook
          hook=useStyles
          render={classes =>
            <div className=classes##root>
              <AppBar position=`Static className={classes##appBar}>
                <Toolbar variant=`Dense className=classes##toolbar>
                  <IconButton
                    color=`Inherit
                    onClick={_ => pushMsg(Route.ChangeRoute(Main(Peers)))}>
                    <Icons.ArrowBack />
                  </IconButton>
                  <InputBase
                    autoFocus=true
                    placeholder="Peer alias"
                    className=classes##titleInput
                    value={`String(peerAliasStr)}
                    onChange={e =>
                      pushMsg(
                        Msg.ReqP2PMsg(
                          PM.Msg.updatePeer(
                            peerId,
                            e->ReactEvent.Form.target##value,
                          ),
                        ),
                      )
                    }
                  />
                  <div>
                    <IconButton
                      color=`Inherit onClick={_ => pushMsg(ClickedDelete)}>
                      <Icons.Delete />
                    </IconButton>
                  </div>
                </Toolbar>
              </AppBar>
              <SectionTitle text="Peer ID" />
              <IdBox
                className=classes##peerIdBox
                id={peerId |> PM.Peer.Id.toString}
              />
              <SectionTitle text="Status" />
              <SectionValue text=signalStateStr />
              <SectionTitle text="P2P connection" />
              <ListItem>
                <ListItemText
                  primary={connStateStr |> ReasonReact.string}
                  secondary={connStateStrSecondary |> ReasonReact.string}
                />
              </ListItem>
            </div>
          }
        />
      );
    },
  };
};