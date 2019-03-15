type closeResult =
  | Cancel
  | Ok(PM.Peer.Id.t);

let useStyles =
  MuiStylesHooks.makeWithTheme(_theme =>
    [
      {name: "root", styles: ReactDOMRe.Style.make()},
      {
        name: "spaceBelow",
        styles: ReactDOMRe.Style.make(~marginBottom="8px", ()),
      },
    ]
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

module Styles = {
  open Css;
  let dialogContent =
    style([
      children([marginBottom(`px(8))]),
      lastChild([marginBottom(`zero)]),
    ]);
};

let component = ReasonReact.reducerComponent("AddGroupDialog");

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
        render={classes =>
          <Dialog
            open_ onClose={(_, _) => handleClose(self, onClose, Cancel)}>
            <DialogTitle> {"Add group" |> ReasonReact.string} </DialogTitle>
            <DialogContent className=Styles.dialogContent>
              <Button
                variant=`Contained
                color=`Secondary
                fullWidth=true
                className=classes##spaceBelow>
                {"Add an existing group" |> ReasonReact.string}
              </Button>
              <Button variant=`Contained color=`Primary fullWidth=true>
                {"Create empty group" |> ReasonReact.string}
              </Button>
            </DialogContent>
          </Dialog>
        }
      />
    );
  },
};