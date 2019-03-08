open BlackTea;

// TYPES

[@bs.deriving accessors]
type screen =
  | Main(Route.mainTab, MainScreen.model)
  | Group
  | PeerInGroup
  | Peer(option(PocketMeshPeer.Peer.Id.t))
  | PeerSearch
  | ThisPeer
  | Loading;

type model = {
  reqRoute: Route.t,
  screen,
};

// HELPERS

let updateWith = (toModel, (subModel, subCmd)) => (
  toModel(subModel),
  subCmd,
);

let routeToNewScreenState = (route: Route.t) => {
  switch (route) {
  | Main(tab) => MainScreen.init() |> updateWith(model => Main(tab, model))
  | Group => (Group, Cmd.none)
  | PeerInGroup => (PeerInGroup, Cmd.none)
  | Peer(peerId) => (Peer(peerId), Cmd.none)
  | PeerSearch => (PeerSearch, Cmd.none)
  | ThisPeer => (ThisPeer, Cmd.none)
  };
};

let updateScreen =
    (~core: PocketMeshPeer.State.taggedT, ~reqRoute: Route.t, msg, screen) => {
  switch (core) {
  | WaitingForDbAndIdentity(_) => (Loading, Cmd.none)
  | HasIdentity(dbState, runtimeState) =>
    switch (screen, reqRoute) {
    | (Main(_currentTab, model), Main(reqTab)) =>
      // Already on wanted screen
      MainScreen.update(msg, model)
      |> updateWith(model => Main(reqTab, model))
    | (_, Main(reqTab)) =>
      // Transitioning from different screen
      MainScreen.init() |> updateWith(model => Main(reqTab, model))
    | (_, Group) => (Group, Cmd.none)
    | (_, PeerInGroup) => (PeerInGroup, Cmd.none)
    | (_, Peer(peerId)) => (Peer(peerId), Cmd.none)
    | (_, PeerSearch) => (PeerSearch, Cmd.none)
    | (_, ThisPeer) => (ThisPeer, Cmd.none)
    }
  };
};

// UPDATE

let init = () => {
  let (main, cmd) = MainScreen.init();
  ({reqRoute: Main(Groups), screen: Main(Groups, main)}, cmd);
};

let update = (~core, msg, model) => {
  let reqRoute =
    switch (msg) {
    | Route.ChangeRoute(route) => route
    | _ => model.reqRoute
    };

  let (screen, screenCmd) =
    updateScreen(~core, ~reqRoute, msg, model.screen);

  ({reqRoute, screen}, screenCmd);
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
              {switch (model.screen) {
               | Main(tab, m) =>
                 <MainScreen activeTab=tab core model=m pushMsg />
               | Group => <GroupScreen />
               | PeerInGroup => <PeerInGroupScreen />
               | Peer(peerId) => <PeerScreen peerId />
               | PeerSearch => <PeerSearchScreen />
               | ThisPeer =>
                 switch (core) {
                 | HasIdentity(dbState, _) =>
                   <ThisPeerScreen
                     thisPeer={dbState |> PocketMeshPeer.DbState.thisPeer}
                     pushMsg
                   />
                 | WaitingForDbAndIdentity(_) => "Bug." |> ReasonReact.string
                 }
               | Loading => <div> {"Loading..." |> ReasonReact.string} </div>
               }}
            </div>
          }
        />
      )
    </ThemeProvider>,
};