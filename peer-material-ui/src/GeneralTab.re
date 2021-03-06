open BlackTea;

type model = {signalServerDialogOpen: bool};

type Msg.t +=
  | ClosedSignalServerSettingsDialog(SignalServerSettingsDialog.closeResult)
  | ClickedOpenSignalServerSettings;

let iconSize = "34px";
let useStyles =
  MuiStylesHooks.make([
    {
      name: "icon",
      styles: ReactDOMRe.Style.make(~width=iconSize, ~height=iconSize, ()),
    },
    {
      name: "listItemIcon",
      styles: ReactDOMRe.Style.make(~marginRight="0", ()),
    },
  ]);

let init = () => {signalServerDialogOpen: false};

let update = (msg, model) =>
  switch (msg) {
  | ClosedSignalServerSettingsDialog(Cancel) => (
      {signalServerDialogOpen: false},
      Cmd.none,
    )
  | ClosedSignalServerSettingsDialog(Ok(settings)) => (
      {signalServerDialogOpen: false},
      Cmd.msg(Msg.ReqP2PMsg(PM.Msg.updateSignalServerUrl(settings.url))),
    )
  | ClickedOpenSignalServerSettings => (
      {signalServerDialogOpen: true},
      Cmd.none,
    )
  | _ => (model, Cmd.none)
  };

let render = (~dbState, ~runtimeState, ~model, ~pushMsg) => {
  let signalChannel = runtimeState |> PM.RuntimeState.signalChannel;
  let signalServerUrl = signalChannel |> PM.SignalChannel.url;
  let signalServerConnState =
    signalChannel |> PM.SignalChannel.connectionState;

  let signalStateStr =
    switch (signalServerConnState) {
    | Connecting => "Trying to connect to " ++ signalServerUrl ++ "..."
    | Connected => "Connected to " ++ signalServerUrl
    };

  let thisPeerIdStr =
    PM.(dbState |> DbState.thisPeer |> ThisPeer.id |> Peer.Id.toString);

  MaterialUi.(
    <UseHook
      hook=useStyles
      render={classes =>
        <List>
          <ListItem
            button=true onClick={_ => pushMsg(Route.ChangeRoute(ThisPeer))}>
            <ListItemIcon className=classes##listItemIcon>
              <GlobalIcon
                className=classes##icon
                dbState
                runtimeState
                highlight=ThisPeer
              />
            </ListItemIcon>
            <ListItemText
              primary={"My identity" |> ReasonReact.string}
              secondary={thisPeerIdStr |> ReasonReact.string}
              secondaryTypographyProps={"noWrap": true}
            />
          </ListItem>
          <ListItem
            button=true
            onClick={_ => pushMsg(ClickedOpenSignalServerSettings)}>
            <ListItemIcon className=classes##listItemIcon>
              <GlobalIcon
                dbState
                runtimeState
                className=classes##icon
                highlight=SignalServer
              />
            </ListItemIcon>
            <ListItemText
              primary={"Signal server" |> ReasonReact.string}
              secondary={signalStateStr |> ReasonReact.string}
            />
          </ListItem>
          <SignalServerSettingsDialog
            settings={url: signalServerUrl}
            open_={model.signalServerDialogOpen}
            onClose={closeResult =>
              pushMsg(ClosedSignalServerSettingsDialog(closeResult))
            }
          />
        </List>
      }
    />
  );
};

let renderFab = (~className as _) => ReasonReact.null;