open BlackTea;

// TYPES

[@bs.deriving accessors]
type screen =
  | Main(MainScreen.model)
  | Group
  | PeerInGroup
  | Peer
  | PeerSearch
  | ThisPeer;

type model = screen;

// HELPERS

let updateWith = (toModel, (subModel, subCmd)) => (
  toModel(subModel),
  subCmd,
);

let changeRouteTo = (route: Route.t) => {
  switch (route) {
  | Main => MainScreen.init(Groups) |> updateWith(main)
  | Group => (Group, Cmd.none)
  | PeerInGroup => (PeerInGroup, Cmd.none)
  | Peer => (Peer, Cmd.none)
  | PeerSearch => (PeerSearch, Cmd.none)
  | ThisPeer => (ThisPeer, Cmd.none)
  };
};

// UPDATE

let init = () => {
  let (main, cmd) = MainScreen.init(Groups);
  (Main(main), cmd);
};

let update = (msg, model) => {
  switch (msg) {
  | Route.ChangeRoute(route) => changeRouteTo(route)
  | msg =>
    switch (model) {
    | Main(m) => MainScreen.update(msg, m) |> updateWith(main)
    | Group
    | PeerInGroup
    | Peer
    | PeerSearch
    | ThisPeer => (model, Cmd.none)
    }
  };
};

// VIEW

module Styles = {
  open Css;
  let wrapper = style([display(`flex), children([flex(1)])]);
};

let useStyles =
  MuiStylesHooks.makeWithTheme(_theme =>
    [{name: "root", styles: ReactDOMRe.Style.make()}]
  );

let component = ReasonReact.statelessComponent("PeerScreens");

let make = (~core, ~className="", ~model, ~pushMsg, _children) => {
  ...component,
  render: _self =>
    <ThemeProvider>
      MaterialUi.(
        <UseHook
          hook=useStyles
          render={classes =>
            <div
              className={[Styles.wrapper, className] |> String.concat(" ")}>
              {switch (model) {
               | Main(m) => <MainScreen core model=m pushMsg />
               | Group => <GroupScreen />
               | PeerInGroup => <PeerInGroupScreen />
               | Peer => <PeerScreen />
               | PeerSearch => <PeerSearchScreen />
               | ThisPeer => <ThisPeerScreen />
               }}
            </div>
          }
        />
      )
    </ThemeProvider>,
};