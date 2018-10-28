type signalServerState =
  | Connecting
  | SigningIn(SignalServerCmds.conn)
  | Connected(
      SignalServerCmds.conn,
      /* watchedPeers that are online */ PeerId.Set.t,
    )
  /* time, attemptsMade, lastErrorMessage */
  | FailedRetryingAt(string, int, string)
  | NoNetwork;

/* We need these to be here because it is present as Msgs argument */
type peerGroups = list(PeerGroup.t);
type peerInDb = {
  id: PeerId.t,
  publicKey: SimpleCrypto.key,
  nickName: string,
};
type peersInDb = PeerId.Map.t(peerInDb);

/* EXCEPTIONS */

exception InternalError;