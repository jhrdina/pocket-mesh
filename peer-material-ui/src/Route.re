type mainTab =
  | Groups
  | Peers
  | General;

type t =
  | Main(mainTab)
  | Group(PM.PeersGroup.Id.t)
  | PeerInGroup(PM.Peer.Id.t, PM.PeersGroup.Id.t)
  | Peer(PM.Peer.Id.t)
  | ThisPeer;

type Msg.t +=
  | ChangeRoute(t);