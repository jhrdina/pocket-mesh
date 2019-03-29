type closeResult =
  | Cancel
  | AddExisting(PM.PeersGroup.Id.t)
  | CreateNew;

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

type idInputState = {
  text: string,
  touched: bool,
};

type state =
  | SelectingType
  | EnteringExistingId(idInputState);

type action =
  | ClickedAddExisting
  | ChangedGroupIdStr(string)
  | Closed;

let initialState = SelectingType;

let validateGroupId = str =>
  switch (PM.PeersGroup.Id.ofString(str)) {
  | Some(groupId) => Belt.Result.Ok(groupId)
  | None => Belt.Result.Error("Group ID cannot be empty.")
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
    switch (action, s) {
    | (Closed, _) => ReasonReact.Update(initialState)
    | (ClickedAddExisting, SelectingType) =>
      ReasonReact.Update(EnteringExistingId({text: "", touched: false}))
    | (ChangedGroupIdStr(text), EnteringExistingId(_)) =>
      ReasonReact.Update(EnteringExistingId({text, touched: true}))

    | (ChangedGroupIdStr(_), _)
    | (ClickedAddExisting, EnteringExistingId(_)) => ReasonReact.NoUpdate
    },

  render: self => {
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <Dialog
            open_ onClose={(_, _) => handleClose(self, onClose, Cancel)}>
            {switch (self.state) {
             | SelectingType =>
               <>
                 <DialogTitle>
                   {"Add group" |> ReasonReact.string}
                 </DialogTitle>
                 <DialogContent className=Styles.dialogContent>
                   <Button
                     variant=`Contained
                     color=`Secondary
                     fullWidth=true
                     className=classes##spaceBelow
                     onClick={_ => self.send(ClickedAddExisting)}>
                     {"Add an existing group" |> ReasonReact.string}
                   </Button>
                   <Button
                     variant=`Contained
                     color=`Primary
                     fullWidth=true
                     onClick={_ => handleClose(self, onClose, CreateNew)}>
                     {"Create empty group" |> ReasonReact.string}
                   </Button>
                 </DialogContent>
               </>
             | EnteringExistingId(idInputState) =>
               let groupIdResult = validateGroupId(idInputState.text);
               <>
                 <DialogTitle>
                   {"Add an existing group" |> ReasonReact.string}
                 </DialogTitle>
                 <DialogContent>
                   <TextField
                     value={`String(idInputState.text)}
                     onChange={evt =>
                       self.send(
                         ChangedGroupIdStr(
                           ReactEvent.Form.target(evt)##value,
                         ),
                       )
                     }
                     autoFocus=true
                     margin=`Dense
                     label={"Group ID" |> ReasonReact.string}
                     fullWidth=true
                     error={
                       groupIdResult->Belt.Result.isError
                       && idInputState.touched
                     }
                     helperText={
                       switch (groupIdResult, idInputState.touched) {
                       | (Error(text), true) => text |> ReasonReact.string
                       | (Error(_), false)
                       | (Ok(_), _) => ReasonReact.null
                       }
                     }
                   />
                   <Typography
                     variant=`Body2
                     style={ReactDOMRe.Style.make(~marginTop="8px", ())}>
                     //  <Icons.Info />

                       {"To fetch contents of the group, remember to add one or more members with write permissions. They will be used to obtain the initial replica."
                        |> ReasonReact.string}
                     </Typography>
                 </DialogContent>
                 <DialogActions>
                   <Button onClick={_ => handleClose(self, onClose, Cancel)}>
                     {"Cancel" |> ReasonReact.string}
                   </Button>
                   <Button
                     disabled={groupIdResult->Belt.Result.isError}
                     onClick={_ =>
                       switch (groupIdResult) {
                       | Ok(groupId) =>
                         handleClose(self, onClose, AddExisting(groupId))
                       | Error(_) => ()
                       }
                     }>
                     {"Add" |> ReasonReact.string}
                   </Button>
                 </DialogActions>
               </>;
             }}
          </Dialog>
        }
      />
    );
  },
};