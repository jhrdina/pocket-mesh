type closeResult =
  | Cancel
  | Ok(PM.Peer.Id.t);

let useStyles =
  MuiStylesHooks.makeWithTheme(_theme =>
    [{name: "root", styles: ReactDOMRe.Style.make()}]
  );

type state = {
  peerIdStr: string,
  peerIdTouched: bool,
};

type action =
  | ChangedPeerIdStr(string)
  | Closed;

let initialState = {peerIdStr: "", peerIdTouched: false};

let validatePeerId = str =>
  switch (PM.Peer.Id.ofString(str)) {
  | Some(peerId) => Belt.Result.Ok(peerId)
  | None => Belt.Result.Error("Peer ID cannot be empty.")
  };

let handleClose = (self: ReasonReact.self('a, 'b, 'c), onClose, result) => {
  onClose(result);
  self.send(Closed);
};

let component = ReasonReact.reducerComponent("AddPeerDialog");

let make = (~open_, ~onClose, _children) => {
  ...component,

  initialState: () => initialState,

  reducer: (action, s) =>
    ReasonReact.Update(
      switch (action) {
      | Closed => initialState
      | ChangedPeerIdStr(peerIdStr) => {peerIdStr, peerIdTouched: true}
      },
    ),

  render: self => {
    let peerIdResult = validatePeerId(self.state.peerIdStr);
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={_classes =>
          <Dialog
            open_ onClose={(_, _) => handleClose(self, onClose, Cancel)}>
            <DialogTitle> {"Add peer" |> ReasonReact.string} </DialogTitle>
            <DialogContent>
              <TextField
                value={`String(self.state.peerIdStr)}
                onChange={evt =>
                  self.send(
                    ChangedPeerIdStr(ReactEvent.Form.target(evt)##value),
                  )
                }
                autoFocus=true
                margin=`Dense
                label={"Peer ID" |> ReasonReact.string}
                fullWidth=true
                error={
                  peerIdResult->Belt.Result.isError && self.state.peerIdTouched
                }
                helperText={
                  switch (peerIdResult, self.state.peerIdTouched) {
                  | (Error(text), true) => text |> ReasonReact.string
                  | (Error(_), false)
                  | (Ok(_), _) => ReasonReact.null
                  }
                }
              />
            </DialogContent>
            // </DialogContentText>
            //   {"Some text" |> ReasonReact.string}
            // <DialogContentText>
            <DialogActions>
              <Button onClick={_ => handleClose(self, onClose, Cancel)}>
                {"Cancel" |> ReasonReact.string}
              </Button>
              <Button
                disabled={peerIdResult->Belt.Result.isError}
                onClick={_ =>
                  switch (peerIdResult) {
                  | Ok(peerId) => handleClose(self, onClose, Ok(peerId))
                  | Error(_) => ()
                  }
                }>
                {"Add" |> ReasonReact.string}
              </Button>
            </DialogActions>
          </Dialog>
        }
      />
    );
  },
};