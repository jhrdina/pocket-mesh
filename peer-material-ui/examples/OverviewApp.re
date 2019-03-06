let component = ReasonReact.statelessComponent("App");

module MUI = MaterialUi;

// Dummy pushMsg
let pushMsg = _ => ();

module Styles = {
  open Css;
  let framed = style([border(`px(1), `solid, `hex("ccc"))]);

  let inlineChildren =
    style([
      children([
        display(`inlineBlock),
        marginLeft(`px(8)),
        firstChild([marginLeft(`zero)]),
      ]),
    ]);
};

let renderComponentTitle = title =>
  <MUI.Typography component={`String("h2")} variant=`H5 gutterBottom=true>
    {ReasonReact.string(title)}
  </MUI.Typography>;

let make = _children => {
  ...component,
  render: _self =>
    <ThemeProvider>

        <div>
          <MUI.Typography
            component={`String("h1")} variant=`H3 gutterBottom=true>
            {ReasonReact.string("PocketMesh GUI Showcase")}
          </MUI.Typography>
          {renderComponentTitle("Global Icon")}
          <div>
            <MUI.IconButton>
              <GlobalIcon signalState=Connected peerState=Online />
            </MUI.IconButton>
            <MUI.IconButton>
              <GlobalIcon signalState=Connecting peerState=Offline />
            </MUI.IconButton>
            <MUI.IconButton>
              <GlobalIcon signalState=Connected peerState=Loading />
            </MUI.IconButton>
            <MUI.IconButton>
              <GlobalIcon signalState=Connected peerState=OnlineNoDoc />
            </MUI.IconButton>
          </div>
          {renderComponentTitle("PeerStatusIndicator")}
          <div className=Styles.inlineChildren>
            <PeerStatusIndicatorDemo />
          </div>
          {renderComponentTitle("Selectable Row")}
          <div>
            <SelectableRow selected=true onChange={(_e, b) => ()} />
          </div>
          {renderComponentTitle("List")}
          <div>
            <MUI.List>
              <MUI.ListItem button=true>
                <PeerStatusIndicator
                  signalState=Online
                  inGroup=true
                  connectionState=Connected
                />
                <MUI.Checkbox />
                <MUI.ListItemText
                  primary={"Jan Hrdina" |> ReasonReact.string}
                />
              </MUI.ListItem>
            </MUI.List>
          </div>
          {renderComponentTitle("Members Count")}
          <div> <MembersCount count=42 /> </div>
          {renderComponentTitle("Main Screen")}
          <div
            // <MainScreen model={MainScreen.init(Groups) |> fst} pushMsg />
          />
          <div
            // <MainScreen model={MainScreen.init(Peers) |> fst} pushMsg />
          />
          <div
            // <MainScreen model={MainScreen.init(General) |> fst} pushMsg />
          />
          {renderComponentTitle("Group Screen")}
          <div> <GroupScreen /> </div>
          {renderComponentTitle("Peer in Group Screen")}
          <div> <PeerInGroupScreen className=Styles.framed /> </div>
          {renderComponentTitle("Peer Search Screen")}
          <div> <PeerSearchScreen className=Styles.framed /> </div>
          {renderComponentTitle("Peer Screen")}
          <div> <PeerScreen /> </div>
          {renderComponentTitle("This Peer Screen")}
        </div>
      </ThemeProvider>,
      // <div> <ThisPeerScreen /> </div>
};

ReactDOMRe.renderToElementWithId(ReasonReact.element(make([||])), "app");