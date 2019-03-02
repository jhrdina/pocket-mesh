type settings = {url: string};

type retainedProps = {settings};

type closeResult =
  | Cancel
  | Ok(settings);

let useStyles =
  MuiStylesHooks.makeWithTheme(_theme =>
    [{name: "root", styles: ReactDOMRe.Style.make()}]
  );

type state = settings;

type action =
  | ChangedUrl(string)
  | ChangedSettingsInProps(settings);

let component =
  ReasonReact.reducerComponentWithRetainedProps("SignalServerSettingsDialog");

let make = (~settings: settings, ~open_, ~onClose, _children) => {
  ...component,

  initialState: () => settings,

  reducer: (action, s) =>
    ReasonReact.Update(
      switch (action) {
      | ChangedSettingsInProps(s) => s
      | ChangedUrl(url) => {...s, url}
      },
    ),

  retainedProps: {
    settings: settings,
  },

  didUpdate: ({oldSelf, newSelf}) =>
    if (oldSelf.retainedProps.settings !== newSelf.retainedProps.settings) {
      newSelf.send(ChangedSettingsInProps(newSelf.retainedProps.settings));
    },
  render: self =>
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={_classes =>
          <Dialog open_ onClose={_ => onClose(Cancel)}>
            <DialogTitle>
              {"Signal Server Settings" |> ReasonReact.string}
            </DialogTitle>
            <DialogContent>
              <TextField
                value={`String(self.state.url)}
                onChange={evt =>
                  self.send(ChangedUrl(ReactEvent.Form.target(evt)##value))
                }
                autoFocus=true
                margin=`Dense
                label={"URL" |> ReasonReact.string}
                fullWidth=true
              />
            </DialogContent>
            // </DialogContentText>
            //   {"Some text" |> ReasonReact.string}
            // <DialogContentText>
            <DialogActions>
              <Button onClick={_ => onClose(Cancel)}>
                {"Cancel" |> ReasonReact.string}
              </Button>
              <Button
                onClick={_ => onClose(Ok({url: "TODO"}))} color=`Secondary>
                {"Apply" |> ReasonReact.string}
              </Button>
            </DialogActions>
          </Dialog>
        }
      />
    ),
};