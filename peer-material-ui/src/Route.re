type mainTab =
  | Groups
  | Peers
  | General;

type t =
  | Main(mainTab)
  | Group(PM.PeersGroup.Id.t)
  | PeerInGroup
  | Peer(PocketMeshPeer.Peer.Id.t)
  | ThisPeer;

type Msg.t +=
  | ChangeRoute(t);