type tab =
  | Groups
  | Peers
  | General;

type state = {
  activeTab: tab,
  generalTab: GeneralTab.state,
};

exception InternalError;

type MainScreenAction.t +=
  | ChangedActiveTab(tab);

let useStyles =
  MuiStylesHooks.makeWithTheme(_theme =>
    [
      {
        name: "wrapper",
        styles: ReactDOMRe.Style.make(~position="relative", ()),
      },
      {
        name: "toolbarLeftBtn",
        styles:
          ReactDOMRe.Style.make(~marginLeft="-18px", ~marginRight="10px", ()),
      },
      {
        name: "toolbarTitle",
        styles: ReactDOMRe.Style.make(~flexGrow="1", ()),
      },
      {
        name: "toolbarRightBlock",
        styles:
          ReactDOMRe.Style.make(~marginLeft="10px", ~marginRight="-18px", ()),
      },
      {
        name: "tabsIndicator",
        styles:
          ReactDOMRe.Style.make(~backgroundColor="white", ~height="3px", ()),
      },
      {
        name: "fab",
        styles:
          ReactDOMRe.Style.make(
            ~position="absolute",
            ~bottom="16px",
            ~right="16px",
            (),
          ),
      },
    ]
  );

let tabToInt =
  fun
  | Groups => 0
  | Peers => 1
  | General => 2;

let intToTab =
  fun
  | 0 => Groups
  | 1 => Peers
  | 2 => General
  | _ => raise(InternalError);

let component = ReasonReact.reducerComponent("MainScreen");

let make = (~initialActiveTab=Groups, _children) => {
  ...component,
  initialState: () => {
    activeTab: initialActiveTab,
    generalTab: GeneralTab.initialState(),
  },

  reducer: (action, state) => {
    let state =
      switch (action) {
      | ChangedActiveTab(tab) => {...state, activeTab: tab}
      | _ => state
      };

    ReasonReact.Update({
      ...state,
      generalTab: GeneralTab.reducer(action, state.generalTab),
    });
  },
  render: self =>
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <div className=classes##wrapper>
            <AppBar position=`Static>
              <Toolbar variant=`Dense>
                <IconButton color=`Inherit className={classes##toolbarLeftBtn}>
                  <Icons.ArrowBack />
                </IconButton>
                <Typography
                  variant=`H6 color=`Inherit className={classes##toolbarTitle}>
                  {"P2P" |> ReasonReact.string}
                </Typography>
                <div className=classes##toolbarRightBlock>
                  <IconButton color=`Inherit className={classes##leftToolBtn}>
                    <Icons.MoreVert />
                  </IconButton>
                </div>
              </Toolbar>
              <Tabs
                classes=[Indicator(classes##tabsIndicator)]
                value={self.state.activeTab |> tabToInt}
                onChange={(_, tabInt) =>
                  self.send(ChangedActiveTab(tabInt |> intToTab))
                }>
                <Tab label={"Groups" |> ReasonReact.string} />
                <Tab label={"Friends" |> ReasonReact.string} />
                <Tab label={"General" |> ReasonReact.string} />
              </Tabs>
            </AppBar>
            {switch (self.state.activeTab) {
             | Groups => GroupsListTab.render()
             | Peers => PeersListTab.render()
             | General =>
               GeneralTab.render(
                 ~send=self.send,
                 ~state=self.state.generalTab,
               )
             }}
            {let renderer =
               switch (self.state.activeTab) {
               | Groups => GroupsListTab.renderFab
               | Peers => PeersListTab.renderFab
               | General => GeneralTab.renderFab
               }
             renderer(~className=classes##fab)}
          </div>
        }
      />
    ),
};