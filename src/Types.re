type signalServerState =
  | Connecting
  | SigningIn(SignalServerCmds.conn)
  | Connected(
      SignalServerCmds.conn,
      /* watchedPeers that are online */ PeerId.Set.t,
    )
  /* intervalSec, attemptsMade, lastErrorMessage */
  | FailedRetryingAt(int, int, string);

/* We need these to be here because it is present as Msgs argument */
type peerGroups = PeerGroup.Id.Map.t(PeerGroup.t);
type peerInDb = {
  id: PeerId.t,
  publicKey: option(SimpleCrypto.key),
  alias: string,
};
type peersInDb = PeerId.Map.t(peerInDb);

/* EXCEPTIONS */

exception InternalError;