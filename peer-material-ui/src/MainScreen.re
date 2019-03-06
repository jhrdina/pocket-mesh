open BlackTea;

type tab =
  | Groups
  | Peers
  | General;

type model = {
  activeTab: tab,
  generalTab: GeneralTab.model,
};

exception InternalError;

type Msg.t +=
  | ChangedActiveTab(tab);

// UPDATE

let init = initialActiveTab => (
  {activeTab: initialActiveTab, generalTab: GeneralTab.init()},
  Cmd.none,
);

let update = (msg, model) => {
  let model =
    switch (msg) {
    | ChangedActiveTab(tab) => {...model, activeTab: tab}
    | _ => model
    };
  (
    {...model, generalTab: GeneralTab.update(msg, model.generalTab)},
    Cmd.none,
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

let component = ReasonReact.statelessComponent("MainScreen");

let make = (~core, ~model, ~pushMsg, _children) => {
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
                <div className=classes##toolbarRightBlock>
                  <IconButton color=`Inherit className={classes##leftToolBtn}>
                    <Icons.MoreVert />
                  </IconButton>
                </div>
              </Toolbar>
              <Tabs
                classes=[Indicator(classes##tabsIndicator)]
                value={model.activeTab |> tabToInt}
                onChange={(_, tabInt) =>
                  pushMsg(ChangedActiveTab(tabInt |> intToTab))
                }>
                <Tab label={"Groups" |> ReasonReact.string} />
                <Tab label={"Friends" |> ReasonReact.string} />
                <Tab label={"General" |> ReasonReact.string} />
              </Tabs>
            </AppBar>
            {switch (model.activeTab) {
             | Groups => GroupsListTab.render()
             | Peers => PeersListTab.render()
             | General =>
               GeneralTab.render(~model=model.generalTab, ~core, ~pushMsg)
             }}
            {let renderer =
               switch (model.activeTab) {
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