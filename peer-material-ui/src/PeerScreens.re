open BlackTea;
open Infix;

// TYPES

[@bs.deriving accessors]
type screen =
  | Main(Route.mainTab, MainScreen.model)
  | Group(PM.PeersGroup.Id.t, GroupScreen.model)
  | PeerInGroup(PM.PeerInGroup.t, PM.PeersGroup.Id.t)
  | Peer(PM.Peer.Id.t, PeerScreen.model)
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
    | (Group(_, model), Group(groupId)) =>
      GroupScreen.update(~groupId, msg, model)
      |> updateWith(m => Group(groupId, m))
    | (_, Group(groupId)) =>
      GroupScreen.init() |> updateWith(m => Group(groupId, m))
    | (_, PeerInGroup(peerId, groupId)) =>
      switch (
        dbState
        |> PM.DbState.groups
        |> PM.PeersGroups.findOpt(groupId)
        |?> PM.PeersGroup.findPeerInGroupOpt(peerId)
      ) {
      | Some(peerInGroup) => (PeerInGroup(peerInGroup, groupId), Cmd.none)
      | None => GroupScreen.init() |> updateWith(m => Group(groupId, m))
      }
    | (Peer(_peerId, m), Peer(peerId)) =>
      PeerScreen.update(~peerId, msg, m) |> updateWith(m => Peer(peerId, m))
    | (_, Peer(peerId)) =>
      PeerScreen.init(~dbState, peerId) |> updateWith(m => Peer(peerId, m))
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
  let wrapper =
    style([
      display(`flex),
      children([flex(1), maxWidth(`percent(100.0))]),
    ]);
};

let useStyles =
  MuiStylesHooks.makeWithTheme(_theme =>
    [{name: "root", styles: ReactDOMRe.Style.make()}]
  );

let renderLoading = () => <div> {"Loading..." |> ReasonReact.string} </div>;

let component = ReasonReact.statelessComponent("PeerScreens");

let make =
    (
      ~core: PocketMeshPeer.State.taggedT,
      ~className="",
      ~model,
      ~pushMsg,
      _children,
    ) => {
  ...component,
  render: _self =>
    <ThemeProvider>
      <UseHook
        hook=useStyles
        render={classes =>
          <div className={[Styles.wrapper, className] |> String.concat(" ")}>
            {switch (core) {
             | HasIdentity(dbState, runtimeState) =>
               switch (model.screen) {
               | Main(tab, m) =>
                 <MainScreen
                   activeTab=tab
                   dbState
                   runtimeState
                   model=m
                   pushMsg
                 />
               | Group(groupId, m) =>
                 <GroupScreen dbState groupId model=m pushMsg />
               | PeerInGroup(peerInGroup, groupId) =>
                 <PeerInGroupScreen dbState groupId pushMsg peerInGroup />
               | Peer(peerId, m) =>
                 <PeerScreen peerId dbState runtimeState model=m pushMsg />
               | ThisPeer =>
                 <ThisPeerScreen
                   thisPeer={dbState |> PocketMeshPeer.DbState.thisPeer}
                   pushMsg
                 />
               | Loading => renderLoading()
               }
             | WaitingForDbAndIdentity(_) => renderLoading()
             }}
          </div>
        }
      />
    </ThemeProvider>,
};