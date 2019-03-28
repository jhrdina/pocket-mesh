open BlackTea;

type model = {
  // activeTab: Route.mainTab,
  generalTab: GeneralTab.model,
  peersListTab: PeersListTab.model,
  groupsListTab: GroupsListTab.model,
};

exception InternalError;

// type Msg.t +=
//   | ChangedActiveTab(Route.mainTab);

// UPDATE

let init = () => (
  {
    generalTab: GeneralTab.init(),
    peersListTab: PeersListTab.init(),
    groupsListTab: GroupsListTab.init(),
  },
  Cmd.none,
);

let update = (~contentInitializer, msg, model) => {
  let (peersListTab, peersListTabCmd) =
    PeersListTab.update(msg, model.peersListTab);
  let (groupsListTab, groupsListTabCmd) =
    GroupsListTab.update(~contentInitializer, msg, model.groupsListTab);
  (
    {
      generalTab: GeneralTab.update(msg, model.generalTab),
      peersListTab,
      groupsListTab,
    },
    Cmd.batch([peersListTabCmd, groupsListTabCmd]),
  );
};

// VIEW

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
          ReactDOMRe.Style.make(~marginLeft="-16px", ~marginRight="10px", ()),
      },
      {
        name: "toolbarTitle",
        styles: ReactDOMRe.Style.make(~flexGrow="1", ()),
      },
      {
        name: "toolbarRightBlock",
        styles:
          ReactDOMRe.Style.make(~marginLeft="10px", ~marginRight="-16px", ()),
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

let tabToInt: Route.mainTab => int =
  fun
  | Groups => 0
  | Peers => 1
  | General => 2;

let intToTab: int => Route.mainTab =
  fun
  | 0 => Groups
  | 1 => Peers
  | 2 => General
  | _ => raise(InternalError);

let component = ReasonReact.statelessComponent("MainScreen");

let make = (~activeTab, ~dbState, ~runtimeState, ~model, ~pushMsg, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <div className=classes##wrapper>
            <AppBar position=`Static>
              <Toolbar variant=`Dense>
                <IconButton
                  color=`Inherit
                  className={classes##toolbarLeftBtn}
                  onClick={_ => pushMsg(Msg.ClickedGoBackToApp)}>
                  <Icons.ArrowBack />
                </IconButton>
                <Typography
                  variant=`H6 color=`Inherit className={classes##toolbarTitle}>
                  {"P2P" |> ReasonReact.string}
                </Typography>
              </Toolbar>
              // <div className=classes##toolbarRightBlock>
              //   <IconButton color=`Inherit className={classes##leftToolBtn}>
              //     <Icons.MoreVert />
              //   </IconButton>
              // </div>
              <Tabs
                classes=[Indicator(classes##tabsIndicator)]
                value={activeTab |> tabToInt}
                onChange={(_, tabInt) =>
                  pushMsg(Route.ChangeRoute(Main(tabInt |> intToTab)))
                }>
                <Tab label={"Groups" |> ReasonReact.string} />
                <Tab label={"Friends" |> ReasonReact.string} />
                <Tab label={"General" |> ReasonReact.string} />
              </Tabs>
            </AppBar>
            {switch (activeTab) {
             | Groups =>
               GroupsListTab.render(
                 ~dbState,
                 ~runtimeState,
                 ~model=model.groupsListTab,
                 ~pushMsg,
               )
             | Peers =>
               PeersListTab.render(
                 ~dbState,
                 ~runtimeState,
                 ~model=model.peersListTab,
                 ~pushMsg,
               )
             | General =>
               GeneralTab.render(
                 ~model=model.generalTab,
                 ~dbState,
                 ~runtimeState,
                 ~pushMsg,
               )
             }}
            {let renderer =
               switch (activeTab) {
               | Groups => GroupsListTab.renderFab(~pushMsg)
               | Peers => PeersListTab.renderFab(~pushMsg)
               | General => GeneralTab.renderFab
               }
             renderer(~className=classes##fab)}
          </div>
        }
      />
    ),
};