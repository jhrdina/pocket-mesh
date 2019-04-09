type t = ..;

type t +=
  | ClickedOpenGroup(PM.PeersGroup.Id.t)
  | ClickedGoBackToApp
  | ReqP2PMsg(PM.Msg.t)
  | Noop;