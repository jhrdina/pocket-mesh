type mainTab =
  | Groups
  | Peers
  | General;

type t =
  | Main(mainTab)
  | Group
  | PeerInGroup
  | Peer
  | PeerSearch
  | ThisPeer;

type Msg.t +=
  | ChangeRoute(t);