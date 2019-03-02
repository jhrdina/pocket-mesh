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
      {
        name: "noRecordsMessage",
        styles: ReactDOMRe.Style.make(~textAlign="center", ()),
      },
    ]
  );

let setSearchTextFieldRef = (theRef, {ReasonReact.state}) => {
  state.searchTextFieldRef := Js.Nullable.toOption(theRef);
};

let component = ReasonReact.reducerComponent("PeerSearchScreen");

let make = (~className="", _children) => {
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
          <div
            className={[classes##wrapper, className] |> String.concat(" ")}>
            <AppBar position=`Static className={classes##appBar}>
              <Toolbar variant=`Dense className=classes##toolbar>
                <IconButton color=`Inherit className={classes##toolbarLeftBtn}>
                  <Icons.ArrowBack />
                </IconButton>
                <InputBase
                  placeholder="Search for a new member"
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
              {if (isEmpty) {
                 <Typography
                   variant=`Body2 className=classes##noRecordsMessage>
                   {"No matching peers found." |> ReasonReact.string}
                 </Typography>;
               } else {
                 <PeerRow
                   signalState=Online
                   inGroup=true
                   connectionState=Connected
                   alias="Carl Buchta"
                 />;
               }}
            </List>
          </div>
        }
      />
    );
  },
};