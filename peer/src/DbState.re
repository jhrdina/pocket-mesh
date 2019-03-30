open BlackTea;

type t = {
  thisPeer: ThisPeer.t,
  peersGroups: PeersGroups.t,
  peers: Peers.t,
};

let init = (~thisPeer) => {
  let thisPeer = ThisPeer.init(thisPeer);
  let (peersGroups, peersGroupsCmd) = PeersGroups.init();
  ({thisPeer, peersGroups, peers: Peers.init()}, peersGroupsCmd);
};

let update = (~initContent, ~defaultGroupAlias, msg, model) => {
  let (thisPeer, thisPeerCmd) = ThisPeer.update(model.thisPeer, msg);
  let (peers, peersCmd) = Peers.update(model.peers, msg);
  let (peersGroups, peersGroupsCmd) =
    PeersGroups.update(
      ~thisPeer,
      ~initContent,
      ~defaultAlias=defaultGroupAlias,
      msg,
      model.peersGroups,
    );
  (
    {thisPeer, peersGroups, peers},
    Cmd.batch([thisPeerCmd, peersCmd, peersGroupsCmd]),
  );
};