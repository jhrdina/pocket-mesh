type mainTab =
  | Groups
  | Peers
  | General;

type t =
  | Main(mainTab)
  | Group
  | PeerInGroup
  | Peer(PocketMeshPeer.Peer.Id.t)
  | PeerSearch
  | ThisPeer;

type Msg.t +=
  | ChangeRoute(t);