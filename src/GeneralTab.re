let iconSize = "34px";

let muiStyles: MaterialUi.Theme.t => list(MaterialUi.WithStyles.style) =
  _theme => [
    {
      name: "icon",
      styles: ReactDOMRe.Style.make(~width=iconSize, ~height=iconSize, ()),
    },
    {
      name: "listItemIcon",
      styles: ReactDOMRe.Style.make(~marginRight="0", ()),
    },
  ];

let render = () => {
  let signalServerUrl = "wss://signal.example.com";
  let signalState: GlobalIcon.signalState = Offline;
  let peerState: GlobalIcon.peerState = Online;

  let signalStateStr =
    switch (signalState) {
    | Offline => "Trying to connect to " ++ signalServerUrl ++ "..."
    | Online => "Connected to " ++ signalServerUrl
    };

  let thisPeerIdStr = "YXNkZmZmc2Rmc2FzZGZzZGZzZnNkZnNkZg==";

  MaterialUi.(
    <MaterialUi.WithStyles
      classesWithTheme=muiStyles
      render={classes =>
        <List>
          <ListItem button=true>
            <ListItemIcon className=classes##listItemIcon>
              <GlobalIcon
                className=classes##icon
                signalState
                peerState
                highlight=ThisPeer
              />
            </ListItemIcon>
            <ListItemText
              primary={"My identity" |> ReasonReact.string}
              secondary={thisPeerIdStr |> ReasonReact.string}
            />
          </ListItem>
          <ListItem button=true>
            <ListItemIcon className=classes##listItemIcon>
              <GlobalIcon
                className=classes##icon
                signalState
                peerState
                highlight=SignalServer
              />
            </ListItemIcon>
            <ListItemText
              primary={"Signal server" |> ReasonReact.string}
              secondary={signalStateStr |> ReasonReact.string}
            />
          </ListItem>
        </List>
      }
    />
  );
};

let renderFab = (~className as _) => ReasonReact.null;