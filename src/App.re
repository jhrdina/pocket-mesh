let component = ReasonReact.statelessComponent("App");

module MUI = MaterialUi;

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
    <MUI.ThemeProvider
      theme={MaterialUi_Theme.create(
        MUI.ThemeOptions.(
          make(
            ~typography=Typography.make(~useNextVariants=true, ()),
            ~palette=
              PaletteOptions.make(
                ~primary=
                  Primary.make(
                    ~main="#616161",
                    ~light="#8e8e8e",
                    ~dark="#373737",
                    ~contrastText="#ffffff",
                    (),
                  ),
                ~secondary=
                  Secondary.make(
                    ~main="#7bb241",
                    ~light="#ade470",
                    ~dark="#4a820c",
                    ~contrastText="#ffffff",
                    (),
                  ),
                (),
              ),
            (),
          )
        ),
      )}>
      <div>
        <MUI.Typography
          component={`String("h1")} variant=`H3 gutterBottom=true>
          {ReasonReact.string("PocketMesh GUI Showcase")}
        </MUI.Typography>
        {renderComponentTitle("Global Icon")}
        <div>
          <MUI.IconButton>
            <GlobalIcon signalState=Online peerState=Online />
          </MUI.IconButton>
          <MUI.IconButton>
            <GlobalIcon signalState=Offline peerState=Offline />
          </MUI.IconButton>
          <MUI.IconButton>
            <GlobalIcon signalState=Online peerState=Loading />
          </MUI.IconButton>
          <MUI.IconButton>
            <GlobalIcon signalState=Online peerState=OnlineNoDoc />
          </MUI.IconButton>
        </div>
        {renderComponentTitle("PeerStatusIndicator")}
        <div className=Styles.inlineChildren>
          <PeerStatusIndicatorDemo />
        </div>
        {renderComponentTitle("Selectable Row")}
        <div> <SelectableRow selected=true onChange={(_e, b) => ()} /> </div>
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
              <MUI.ListItemText primary={"Jan Hrdina" |> ReasonReact.string} />
            </MUI.ListItem>
          </MUI.List>
        </div>
        {renderComponentTitle("Members Count")}
        <div> <MembersCount count=42 /> </div>
        {renderComponentTitle("Main Screen")}
        <div> <MainScreen activeTab=Groups /> </div>
        <div> <MainScreen activeTab=Peers /> </div>
        <div> <MainScreen activeTab=General /> </div>
        {renderComponentTitle("Group Screen")}
        <div> <GroupScreen /> </div>
        {renderComponentTitle("Peer in Group Screen")}
        <div> <PeerInGroupScreen className=Styles.framed /> </div>
      </div>
    </MUI.ThemeProvider>,
};