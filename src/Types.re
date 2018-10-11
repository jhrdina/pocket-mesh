/* Temporary */

type docType = {. "a": int};

module Automerge =
  Automerge.Make({
    type t = docType;
  });

/* ++++++++++++++++ */

type permission =
  | Read
  | Write
  | ReadWrite;

type groupPermissions = {
  content: permission,
  membersList: permission,
};

type peerInGroup = {
  id: PeerId.t,
  permissions: groupPermissions,
};

type peerGroup = {
  id: string,
  peers: list(peerInGroup),
  content: docType,
};

type connection;

type peerOnlineStatus =
  | Online
  | Offline;

type signalServerState =
  | Connecting
  | SigningIn(SignalServerCmds.t)
  | Connected(
      SignalServerCmds.t,
      /* watchedPeers that are online */ PeerId.Set.t,
    )
  /* time, attemptsMade, lastErrorMessage */
  | FailedRetryingAt(string, int, string)
  | NoNetwork;

type hasIdentity = {
  db: IDBCmds.t,
  thisPeer: ThisPeer.t,
  signalServerState,
  peerGroups: list(peerGroup),
  peers: Peers.t,
};

type rootState =
  | OpeningDB
  | LoadingIdentity(IDBCmds.t)
  | HasIdentity(hasIdentity);