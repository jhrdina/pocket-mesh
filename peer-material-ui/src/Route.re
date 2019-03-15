type mainTab =
  | Groups
  | Peers
  | General;

type t =
  | Main(mainTab)
  | Group(PM.PeersGroup.Id.t)
  | PeerInGroup(PM.PeersGroup.Id.t)
  | Peer(PocketMeshPeer.Peer.Id.t)
  | ThisPeer;

type Msg.t +=
  | ChangeRoute(t);