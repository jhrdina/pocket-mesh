type mainTab =
  // Only if has identity
  | Groups
  // Only if has identity
  | Peers
  | General;

type t =
  | Main(mainTab)
  // Only if has identity
  | Group
  // Only if has identity
  | PeerInGroup
  // Only if has identity
  | Peer(PocketMeshPeer.Peer.Id.t)
  // Only if
  | PeerSearch
  | ThisPeer;

type Msg.t +=
  | ChangeRoute(t);