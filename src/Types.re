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

/* EXCEPTIONS */

exception InternalError;