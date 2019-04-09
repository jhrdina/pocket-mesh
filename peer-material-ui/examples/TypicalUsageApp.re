[%%debugger.chrome];
open BlackTea;

module PMGui = PocketMeshPeerMaterialUi;

type model = {
  p2p: PM.State.t,
  p2pGui: PMGui.PeerScreens.model,
};

[@bs.deriving accessors]
type msg =
  | P2PMsg(PM.Msg.t)
  | P2PGuiMsg(PMGui.Msg.t);

let init = () => {
  let (p2p, p2pCmd) = PM.init(PM.InitConfig.make());
  let (p2pGui, p2pGuiCmd) = PMGui.PeerScreens.init();
  (
    {p2p, p2pGui},
    Cmd.batch([p2pCmd |> Cmd.map(p2PMsg), p2pGuiCmd |> Cmd.map(p2PGuiMsg)]),
  );
};

let update = (model, msg) => {
  switch (msg) {
  | P2PMsg(p2pMsg)
  // Handle cases when PMGui wants to send a msg to PM
  | P2PGuiMsg(PMGui.Msg.ReqP2PMsg(p2pMsg)) =>
    // Js.log(p2pMsg);
    let (p2p, p2pCmd) = PM.update(model.p2p, p2pMsg);

    let (p2pGui, p2pGuiCmd) =
      PMGui.PeerScreens.update(
        ~core=p2p |> PM.State.classify,
        PMGui.Msg.Noop,
        model.p2pGui,
      );

    (
      {p2p, p2pGui},
      Cmd.batch([
        p2pCmd |> Cmd.map(p2PMsg),
        p2pGuiCmd |> Cmd.map(p2PGuiMsg),
      ]),
    );

  | P2PGuiMsg(p2pGuiMsg) =>
    let (p2pGui, p2pGuiCmd) =
      PMGui.PeerScreens.update(
        ~core=model.p2p |> PM.State.classify,
        p2pGuiMsg,
        model.p2pGui,
      );

    ({...model, p2pGui}, p2pGuiCmd |> Cmd.map(p2PGuiMsg));
  };
};

let subscriptions = model =>
  Sub.batch([PM.subscriptions(model.p2p) |> Sub.map(p2PMsg)]);

let store =
  Store.create(~init, ~update, ~subscriptions, ~shutdown=_ => Cmd.none);

module ModelProvider = {
  let make = ReactProvider.createMake(~name="ModelProvider", ~store);
};

module App = {
  let component = ReasonReact.statelessComponent("App");

  let useStyles =
    MuiStylesHooks.makeWithTheme(_theme =>
      [
        {
          name: "root",
          styles:
            ReactDOMRe.Style.make(
              ~position="absolute",
              ~left="0",
              ~right="0",
              ~top="0",
              ~bottom="0",
              (),
            ),
        },
      ]
    );

  let make = (~model, ~pushMsg, _children) => {
    ...component,
    render: _self =>
      <UseHook
        hook=useStyles
        render={classes =>
          <PeerScreens
            core={model.p2p |> PM.State.classify}
            className=classes##root
            model={model.p2pGui}
            pushMsg={msg => msg |> p2PGuiMsg |> pushMsg}
          />
        }
      />,
  };
};

ReactDOMRe.renderToElementWithId(<ModelProvider component=App.make />, "app");

// DEBUG

// type window;
// [@bs.val] external window: window = "";
// [@bs.set_index] external makeGlobal: (window, string, 'a) => unit = "";
// let makeGlobal = (name, value) => makeGlobal(window, name, value);

// store.subscribe(model => Js.log(model));
/* */