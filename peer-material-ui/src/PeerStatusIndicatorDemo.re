type state = {
  signalState: PeerStatusIndicator.signalState,
  inGroup: bool,
  connectionState: PeerStatusIndicator.connectionState,
};

type action =
  | SetSignalState(PeerStatusIndicator.signalState)
  | SetInGroup(bool)
  | SetConnectionState(PeerStatusIndicator.connectionState);

module Styles = {
  open Css;
  let framed = style([border(`px(1), `solid, `hex("ccc"))]);
};

let component = ReasonReact.reducerComponent("PeerStatusIndicatorDemo");

let rs = ReasonReact.string;

let mkConnStateBtn = (self, label, state) =>
  <button onClick={_e => self.ReasonReact.send(SetConnectionState(state))}>
    {label |> rs}
  </button>;

let mkInGroupBtn = (self, label, state) =>
  <button onClick={_e => self.ReasonReact.send(SetInGroup(state))}>
    {label |> rs}
  </button>;

let mkSignalStateBtn = (self, label, state) =>
  <button onClick={_e => self.ReasonReact.send(SetSignalState(state))}>
    {label |> rs}
  </button>;

let make = _children => {
  /* spread the other default fields of component here and override a few */
  ...component,

  initialState: () => {
    signalState: Offline,
    inGroup: false,
    connectionState: Offline,
  },

  /* State transitions */
  reducer: (action, state) =>
    ReasonReact.Update(
      switch (action) {
      | SetSignalState(signalState) => {...state, signalState}
      | SetInGroup(inGroup) => {...state, inGroup}
      | SetConnectionState(connectionState) => {...state, connectionState}
      },
    ),

  render: self => {
    <div>
      <div>
        {"Connection state:" |> rs}
        {mkConnStateBtn(self, "Offline", Offline)}
        {mkConnStateBtn(self, "Connecting", Connecting)}
        {mkConnStateBtn(self, "Connected", Connected)}
        {mkConnStateBtn(self, "Connected transferring", ConnectedTransferring)}
      </div>
      <div>
        {"In group:" |> rs}
        {mkInGroupBtn(self, "Yes", true)}
        {mkInGroupBtn(self, "No", false)}
      </div>
      <div>
        {"Signal state:" |> rs}
        {mkSignalStateBtn(self, "Online", Online)}
        {mkSignalStateBtn(self, "Offline", Offline)}
      </div>
      <div>
        <PeerStatusIndicator
          signalState={self.state.signalState}
          inGroup={self.state.inGroup}
          connectionState={self.state.connectionState}
          className=Styles.framed
        />
      </div>
    </div>;
  },
};