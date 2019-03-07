open BlackTea;

// TYPES

[@bs.deriving accessors]
type screen =
  | Main(Route.mainTab, MainScreen.model)
  | Group
  | PeerInGroup
  | Peer(PocketMeshPeer.Peer.Id.t)
  | PeerSearch
  | ThisPeer;

type model = screen;

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

// UPDATE

let init = () => {
  let (main, cmd) = MainScreen.init();
  (Main(Groups, main), cmd);
};

let handleRedirects =
    (
      ~core: PocketMeshPeer.State.taggedT,
      (suggestedScreenState, suggestedScreenCmd),
    ) =>
  switch (suggestedScreenState) {
  | ThisPeer =>
    switch (core) {
    | HasIdentity(_) => (suggestedScreenState, suggestedScreenCmd)
    | WaitingForDbAndIdentity(_) =>
      MainScreen.init() |> updateWith(model => Main(General, model))
    }
  | _ => (suggestedScreenState, suggestedScreenCmd)
  };

let update = (~core, msg, model) => {
  let suggestedScreen =
    switch (msg) {
    | Route.ChangeRoute(route) => routeToNewScreenState(route)
    | _ => (model, Cmd.none)
    };

  let (model, cmd) = handleRedirects(~core, suggestedScreen);

  // Handle submodel updates
  let (model, subCmd) =
    switch (model) {
    | Main(tab, m) =>
      MainScreen.update(msg, m) |> updateWith(m => Main(tab, m))
    | Group
    | PeerInGroup
    | Peer(_)
    | PeerSearch
    | ThisPeer => (model, Cmd.none)
    };

  (model, Cmd.batch([cmd, subCmd]));
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
               }}
            </div>
          }
        />
      )
    </ThemeProvider>,
};