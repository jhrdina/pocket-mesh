let render = () =>
  MaterialUi.(
    <List>
      <PeerRow
        alias="Jan Hrdina"
        signalState=Online
        inGroup=true
        connectionState=Connected
      />
      <PeerRow
        alias="Some Guy"
        signalState=Online
        inGroup=false
        connectionState=Offline
      />
      <PeerRow
        alias="John Brown"
        signalState=Offline
        inGroup=true
        connectionState=Offline
      />
    </List>
  );

let renderFab = (~className) =>
  MaterialUi.(<Fab color=`Secondary className> <Icons.PersonAdd /> </Fab>);