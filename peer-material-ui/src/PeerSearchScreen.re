type closeResult =
  | Cancel
  | Ok(PM.Peer.Id.t);

type state = {
  searchText: string,
  searchTextFieldRef: ref(option(ReasonReact.reactRef)),
};

type action =
  | ChangedSearchText(string)
  | ClickedClearSearchText;

let idBoxBorder = "1px dashed #ccc";

let useStyles =
  MuiStylesHooks.makeWithTheme(theme =>
    [
      {
        name: "wrapper",
        styles: ReactDOMRe.Style.make(~position="relative", ()),
      },
      {
        name: "appBar",
        styles:
          ReactDOMRe.Style.make(
            ~backgroundColor="white",
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
      {name: "titleInput", styles: ReactDOMRe.Style.make(~flex="1", ())},
    ]
  );

let getMatchingPeers = (~peers, searchText) =>
  if (searchText != "") {
    let searchText = searchText |> Js.String.toLocaleLowerCase;
    peers
    |> PM.Peers.fold(
         (arr, peer) =>
           if (peer
               |> PM.Peer.alias
               |> Js.String.toLocaleLowerCase
               |> Js.String.includes(searchText)
               || peer
               |> PM.Peer.id
               |> PM.Peer.Id.toString
               |> Js.String.toLocaleLowerCase
               |> Js.String.includes(searchText)) {
             arr |> Array.append([|peer|]);
           } else {
             arr;
           },
         [||],
       );
  } else {
    peers
    |> PM.Peers.fold((arr, peer) => arr |> Array.append([|peer|]), [||]);
  };

let setSearchTextFieldRef = (theRef, {ReasonReact.state}) => {
  state.searchTextFieldRef := Js.Nullable.toOption(theRef);
};

let component = ReasonReact.reducerComponent("PeerSearchScreen");

let make =
    (~dbState, ~runtimeState, ~open_, ~onClose, ~className="", _children) => {
  ...component,

  initialState: () => {searchText: "", searchTextFieldRef: ref(None)},

  reducer: (action, s) =>
    switch (action) {
    | ChangedSearchText(t) => ReasonReact.Update({...s, searchText: t})
    | ClickedClearSearchText =>
      ReasonReact.UpdateWithSideEffects(
        {...s, searchText: ""},
        _self =>
          switch (s.searchTextFieldRef^) {
          | Some(actualRef) => actualRef->ReasonReact.refToJsObj##focus()
          | None => ()
          },
      )
    },

  render: self => {
    let isEmpty = false;
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <Dialog
            fullScreen=true
            open_
            onClose={(_, _) => onClose(Cancel)}
            hideBackdrop=true>
            <div
              className={[classes##wrapper, className] |> String.concat(" ")}>
              <AppBar position=`Static className={classes##appBar}>
                <Toolbar variant=`Dense className=classes##toolbar>
                  <IconButton
                    color=`Inherit
                    className={classes##toolbarLeftBtn}
                    onClick={_ => onClose(Cancel)}>
                    <Icons.ArrowBack />
                  </IconButton>
                  <InputBase
                    placeholder="Search for a new member"
                    autoFocus=true
                    value={`String(self.state.searchText)}
                    inputRef={`Callback(self.handle(setSearchTextFieldRef))}
                    onChange={event =>
                      self.send(
                        ChangedSearchText(
                          ReactEvent.Form.target(event)##value,
                        ),
                      )
                    }
                    className=classes##titleInput
                  />
                  {if (self.state.searchText != "") {
                     <div className=classes##toolbarRightBlock>
                       <IconButton
                         color=`Inherit
                         onClick={_ => self.send(ClickedClearSearchText)}>
                         <Icons.Clear />
                       </IconButton>
                     </div>;
                   } else {
                     ReasonReact.null;
                   }}
                </Toolbar>
              </AppBar>
              <List>
                {getMatchingPeers(
                   ~peers=dbState |> PM.DbState.peers,
                   self.state.searchText,
                 )
                 |> Array.map(peer => {
                      let peerId = peer |> PM.Peer.id;
                      <PeerRowWithData
                        key={peerId |> PM.Peer.Id.toString}
                        peerId
                        dbState
                        runtimeState
                        onClick={_ => onClose(Ok(peerId))}
                      />;
                    })
                 |> GuiUtils.elementArrayWithDefaultMsg(
                      "No matching peers found.",
                    )}
              </List>
            </div>
          </Dialog>
        }
      />
    );
  },
};