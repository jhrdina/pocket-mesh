open BlackTea;

type t = {
  thisPeer: ThisPeer.t,
  peersGroups: PeersGroups.t,
  peers: Peers.t,
};

let init = (~thisPeer, ~initContent) => {
  let thisPeer = ThisPeer.init(thisPeer);
  {
    thisPeer,
    peersGroups: PeersGroups.init(~thisPeer, ~initContent),
    peers: Peers.init(),
  };
};

let update = (msg, model) => {
  let (thisPeer, thisPeerCmd) = ThisPeer.update(model.thisPeer, msg);
  let (peers, peersCmd) = Peers.update(model.peers, msg);
  let (peersGroups, peersGroupsCmd) =
    PeersGroups.update(~thisPeer, msg, model.peersGroups);
  (
    {thisPeer, peersGroups, peers},
    Cmd.batch([thisPeerCmd, peersCmd, peersGroupsCmd]),
  );
};