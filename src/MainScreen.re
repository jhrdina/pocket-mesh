let component = ReasonReact.statelessComponent("MainScreen");

type tab =
  | Groups
  | Peers
  | General;

let muiStyles: MaterialUi.Theme.t => list(MaterialUi.WithStyles.style) =
  _theme => [
    {
      name: "wrapper",
      styles: ReactDOMRe.Style.make(~position="relative", ()),
    },
    {
      name: "toolbarLeftBtn",
      styles:
        ReactDOMRe.Style.make(~marginLeft="-18px", ~marginRight="10px", ()),
    },
    {name: "toolbarTitle", styles: ReactDOMRe.Style.make(~flexGrow="1", ())},
    {
      name: "toolbarRightBlock",
      styles:
        ReactDOMRe.Style.make(~marginLeft="10px", ~marginRight="-18px", ()),
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
  ];

module Styles = {
  open Css;
};

let tabToInt =
  fun
  | Groups => 0
  | Peers => 1
  | General => 2;

let make = (~activeTab, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <WithStyles
        classesWithTheme=muiStyles
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
              <Tabs value={activeTab |> tabToInt}>
                <Tab label={"Groups" |> ReasonReact.string} />
                <Tab label={"Friends" |> ReasonReact.string} />
                <Tab label={"General" |> ReasonReact.string} />
              </Tabs>
            </AppBar>
            {switch (activeTab) {
             | Groups => GroupsListTab.render()
             | Peers => PeersListTab.render()
             | General => GeneralTab.render()
             }}
            {let renderer =
               switch (activeTab) {
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