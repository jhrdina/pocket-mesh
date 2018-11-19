/* CONSTANTS */

let defaultSignalServerUrl = Store.defaultSignalServerUrl;

/* MODULES */

module Crdt = PeerGroup.AM;
module InitConfig = Store.InitConfig;

module Peer = {
  module Id = PeerId;

  type signalState =
    | Online
    | Offline;
  type connectionState =
    | NotInGroup(signalState)
    | InGroupWaitingForOnlineSignal
    | InGroupOnlineInitiatingConnection(int)
    | OnlineAcceptingConnection(bool)
    | InGroupOnlineFailedRetryingAt(int, int, string)
    | Connected(bool, signalState);

  type t = Peer.t;

  /* Helpers */

  let decodeSignalState =
    fun
    | Peer.Online(_) => Online
    | Offline => Offline;

  let id = t => t.Peer.id;
  let alias = t => t.Peer.alias;
  let connectionState = t =>
    switch (t.Peer.connectionState) {
    | NotInGroup(signalState) => NotInGroup(signalState |> decodeSignalState)
    | InGroupWaitingForOnlineSignal => InGroupWaitingForOnlineSignal
    | InGroupOnlineCreatingSdpOffer(_, failedAttempts)
    | InGroupOnlineWaitingForAcceptor(_, _, failedAttempts) =>
      InGroupOnlineInitiatingConnection(failedAttempts)
    | InGroupOnlineFailedRetryingAt(_, timeoutSec, failedAttempts, lastErrMsg) =>
      InGroupOnlineFailedRetryingAt(timeoutSec, failedAttempts, lastErrMsg)
    | OnlineCreatingSdpAnswer(inGroup, _)
    | OnlineWaitingForInitiator(inGroup, _, _) =>
      OnlineAcceptingConnection(inGroup)
    | Connected(_, inGroup, signalState) =>
      Connected(inGroup, signalState |> decodeSignalState)
    };
};

module Peers = {
  type t = Peers.t;
  let findOpt = Peers.findOpt;
  let fold = Peers.fold;
};

module PeerInGroup = {
  type membersChangingPerms =
    PeerGroup.membersChangingPerms = | ReadMembers | WriteMembers;
  type groupPermissions =
    PeerGroup.groupPermissions =
      | ReadContentAndMembers | WriteContent(membersChangingPerms);

  type t = PeerGroup.peerInGroup;

  let id = (t: t) => t.id;
  let permissions = (t: t) => t.permissions;
};

module PeersGroup = PeerGroup;

module PeersGroups = PeerGroups;

module ThisPeer = ThisPeer;

module SignalServer = {
  type t = SignalServerState.t;
  type connectionState =
    | Connecting
    | SigningIn
    | FailedRetryingAt(int, int, string)
    | Connected;

  let url = (t: t) => t.url;

  let connectionState = (t: t) =>
    switch (t.connectionState) {
    | Types.Connecting => Connecting
    | SigningIn(_) => SigningIn
    | FailedRetryingAt(intervalSec, failedAttempts, lastErrorMessage) =>
      FailedRetryingAt(intervalSec, failedAttempts, lastErrorMessage)
    | Connected(_, _) => Connected
    };
};

module StateWithId = {
  type t = Store.hasIdentity;
  let groups = t => t.Store.peerGroups;
  let peers = t => t.Store.peers;
  let thisPeer = t => t.Store.thisPeer;
  let signalServer = t => t.Store.signalServer;
};

module State = {
  type t = Store.rootState;
  type taggedT =
    | OpeningDB
    | LoadingDBData
    | GeneratingIdentity
    | FatalError(exn)
    | HasIdentity(StateWithId.t);

  let classify =
    fun
    | Store.OpeningDB(_) => OpeningDB
    | LoadingDBData(_) => LoadingDBData
    | GeneratingIdentity(_) => GeneratingIdentity
    | FatalError(exn) => FatalError(exn)
    | HasIdentity(stateWithId) => HasIdentity(stateWithId);
};

module Msg = Msgs;

let init = Store.init;
let update = Store.update;