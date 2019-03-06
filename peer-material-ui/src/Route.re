type t =
  | Main
  | Group
  | PeerInGroup
  | Peer
  | PeerSearch
  | ThisPeer;

type Msg.t +=
  | ChangeRoute(t);