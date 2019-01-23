open Json.Infix;

/**
  Represents state of a peer,
  describes transitions of the state and related side-effects
 */

/* CONSTANTS */
let waitingTimeoutMs = 10 * 1000;

/* TYPES */

type peerSignalState =
  | Online(SignalServerCmds.conn)
  | Offline;

type sdpMsgType =
  | Offer
  | Answer;

type publicKeyFetchingState =
  | MissingWaitingForOnline
  | Fetching(SignalServerCmds.conn)
  | Fetched(peerSignalState, SimpleCrypto.key);

type peerConnectionState =
  /* bool = online */
  | NotInGroup(peerSignalState)
  | InGroupWaitingForOnlineSignal
  /* ssConn, failedAttempts */
  | InGroupOnlineCreatingSdpOffer(SignalServerCmds.conn, int)
  /* ssConn, rtcConn, failedAttempts */
  | InGroupOnlineWaitingForAcceptor(SignalServerCmds.conn, RTCCmds.t, int)
  /* ssConn, intervalSec, failedAttempts, lastErrorMessage */
  /* | Connecting */
  | InGroupOnlineFailedRetryingAt(SignalServerCmds.conn, int, int, string)
  /* inGroup, ssConn */
  | OnlineCreatingSdpAnswer(bool, SignalServerCmds.conn)
  | OnlineWaitingForInitiator(bool, SignalServerCmds.conn, RTCCmds.t)
  /* RTC conn, inGroup, signal online */
  | Connected(RTCCmds.t, bool, peerSignalState);

type t = {
  id: PeerId.t,
  publicKey: publicKeyFetchingState,
  alias: string,
  connectionState: peerConnectionState,
};

[@bs.deriving accessors]
type msgs =
  /*
   Group membership changes
   */
  | AddedToGroup
  | RemovedFromLastGroup
  /*
   Online/Offline
   */
  | WentOnline(SignalServerCmds.conn)
  | WentOffline
  /*
   Signal receiving & verification
   */
  | ReceivedSignal(string, Message.peerToPeerMsg)
  | ReceivedVerifiedSignal(Message.peerToPeerMsg)
  | SignalVerificationFailed(exn)
  | PrepareKeyRequestFinished(Result.t(string, exn))
  | VerifyKeyResponseFinished(Result.t(SimpleCrypto.key, exn))
  | VerifyKeyRequestFinished(Result.t(SimpleCrypto.key, exn))
  | PrepareKeyResponseFinished(Result.t(string, exn))
  /*
   RTC connection
   */
  | RtcRetryConnection
  | RtcClose
  /* t, sdp */
  | RtcAnswerReady(RTCCmds.t, string)
  | RtcOfferReady(RTCCmds.t, string)
  | RtcConnected
  /*
   Alias changes
   */
  | UpdateAlias(string);

exception InvalidState;
exception InvalidKeyResponse;
exception ReceivedMessageSignatureMismatch;

/* HELPERS */

let shouldAccept = (thisPeer: ThisPeer.t, srcPeerId) =>
  thisPeer.id < srcPeerId;

let createAcceptorFromOffer = (src, sdp) =>
  RTCCmds.createAcceptor(
    src,
    sdp,
    Msgs.rtcAnswerReady,
    Msgs.rtcConnected,
    Msgs.rtcGotData,
    Msgs.rtcError,
    Msgs.rtcClose,
  );

let createInitiator = peerId =>
  RTCCmds.createInitiator(
    peerId,
    Msgs.rtcOfferReady,
    Msgs.rtcConnected,
    Msgs.rtcGotData,
    Msgs.rtcError,
    Msgs.rtcClose,
  );

let sendSignalMessage = (thisPeer, tgPeer, msg, ssConn) =>
  Cmds.wrapPromise(
    () =>
      SignalServerCmds.signAndSendMsg(
        PeerToPeer(thisPeer.ThisPeer.id, tgPeer, msg),
        thisPeer.ThisPeer.privateKey,
        ssConn,
      ),
    _ => Msgs.noop,
    Msgs.cryptoFatalError,
  );

let peerSignalStateOfSignalServerState = peerId =>
  fun
  | Types.Connected(ssConn, onlinePeers)
      when PeerId.Set.mem(peerId, onlinePeers) =>
    Online(ssConn)
  | _ => Offline;

let verifyMessageSignature = (srcPublicKey, signature, msg) =>
  Json.Object(Message.encodeSignedMsg(msg))
  |> Json.stringify
  |> SimpleCrypto.verify(srcPublicKey, signature);

let verifyMessageSignatureCmd = (srcPublicKey, signature, src, tg, msg) => {
  let msgToVerify = Message.PeerToPeer(src, tg, msg);
  Cmds.wrapPromise(
    () =>
      verifyMessageSignature(srcPublicKey, signature, msgToVerify)
      |> Js.Promise.then_(valid =>
           valid ?
             Js.Promise.resolve(msg) :
             Js.Promise.reject(ReceivedMessageSignatureMismatch)
         ),
    Msgs.receivedVerifiedSignal(src),
    Msgs.signalVerificationFailed(src),
  );
};

let exportThisPeerKeyCmd = (resultToMsg, thisPeer: ThisPeer.t, peerId) =>
  Cmds.wrapPromise(
    () =>
      thisPeer.publicKey
      |> SimpleCrypto.publicKeyToJwk
      |> Js.Promise.then_(jwk =>
           jwk |> SimpleCrypto.jwkToString |> Js.Promise.resolve
         ),
    keyStr => resultToMsg(peerId, Result.Ok(keyStr)),
    exn => resultToMsg(peerId, Error(exn)),
  );

let verifyKeyMessageCmd =
    (
      resultToMsg,
      thisPeer: ThisPeer.t,
      src,
      signature,
      p2pMsg: Message.peerToPeerMsg,
    ) =>
  switch (p2pMsg) {
  | KeyResponse(srcKeyStr)
  | KeyRequest(srcKeyStr) =>
    Cmds.wrapPromise(
      () => {
        let srcJwk = srcKeyStr |> SimpleCrypto.stringToJwk;
        srcJwk
        |> SimpleCrypto.jwkToPublicKey
        |> Js.Promise.then_(srcKey =>
             Js.Promise.all2((
               verifyMessageSignature(
                 srcKey,
                 signature,
                 PeerToPeer(src, thisPeer.id, p2pMsg),
               ),
               SimpleCrypto.fingerprintForRSAJWK(srcJwk)
               |> Js.Promise.then_(fingerprint =>
                    fingerprint
                    |> PeerId.ofString
                    |?>> (==)(src)
                    |? false
                    |> Js.Promise.resolve
                  ),
             ))
             |> Js.Promise.then_(((signatureOk, srcMatchesKey)) =>
                  if (signatureOk && srcMatchesKey) {
                    srcKey |> Js.Promise.resolve;
                  } else {
                    Js.Promise.reject(InvalidKeyResponse);
                  }
                )
           );
      },
      srcKey => resultToMsg(src, Result.Ok(srcKey)),
      exn => resultToMsg(src, Error(exn)),
    )
  | Offer(_)
  | Answer(_) =>
    Cmds.log(
      "Invalid usage: verifyKeyMessageCmd called with weird message type.",
    )
  };

/* UPDATES */

let initConnState = (peerId, inGroup, peerSignalState) =>
  switch (inGroup, peerSignalState) {
  | (false, peerSignalState) => (NotInGroup(peerSignalState), Cmds.none)
  | (true, Offline) => (InGroupWaitingForOnlineSignal, Cmds.none)
  | (true, Online(ssConn)) => (
      InGroupOnlineCreatingSdpOffer(ssConn, 0),
      createInitiator(peerId),
    )
  };

let updateConnState = (thisPeer, peerId, peerKey, prevState, msg) =>
  switch (msg, prevState) {
  | (AddedToGroup, NotInGroup(Online(ssConn)))
  | (WentOnline(ssConn), InGroupWaitingForOnlineSignal) => (
      InGroupOnlineCreatingSdpOffer(ssConn, 0),
      createInitiator(peerId),
    )

  | (
      RtcRetryConnection,
      InGroupOnlineFailedRetryingAt(ssConn, _, failedAttempts, _),
    ) => (
      InGroupOnlineCreatingSdpOffer(ssConn, failedAttempts),
      createInitiator(peerId),
    )

  /* Waiting timeout */
  | (
      RtcRetryConnection,
      InGroupOnlineWaitingForAcceptor(ssConn, rtcConn, failedAttempts),
    ) => (
      InGroupOnlineCreatingSdpOffer(ssConn, failedAttempts + 1),
      Cmds.batch([RTCCmds.destroy(rtcConn), createInitiator(peerId)]),
    )

  | (AddedToGroup, NotInGroup(Offline))
  | (
      WentOffline,
      InGroupOnlineCreatingSdpOffer(_) | InGroupOnlineFailedRetryingAt(_) |
      OnlineCreatingSdpAnswer(true, _),
    ) => (
      InGroupWaitingForOnlineSignal,
      Cmds.none,
    )

  | (RtcClose, Connected(rtcConn, true, Offline))
  | (
      WentOffline,
      InGroupOnlineWaitingForAcceptor(_, rtcConn, _) |
      OnlineWaitingForInitiator(true, _, rtcConn),
    ) => (
      InGroupWaitingForOnlineSignal,
      RTCCmds.destroy(rtcConn),
    )

  | (
      RtcOfferReady(rtcConn, sdp),
      InGroupOnlineCreatingSdpOffer(ssConn, failedAttempts),
    ) => (
      InGroupOnlineWaitingForAcceptor(ssConn, rtcConn, failedAttempts),
      Cmds.batch([
        sendSignalMessage(thisPeer, peerId, Offer(sdp), ssConn),
        /* TODO: Exponential backoff */
        Cmds.timeout(Msgs.rtcRetryConnection(peerId), waitingTimeoutMs),
      ]),
    )

  | (
      ReceivedVerifiedSignal(Answer(sdp)),
      InGroupOnlineWaitingForAcceptor(_ssConn, rtcConn, _),
    ) => (
      /* TODO: Check Signature, maybe move upwards to Peer.update */
      prevState,
      Cmds.batch([
        RTCCmds.signal(rtcConn, sdp),
        /* TODO: Exponential backoff */
        Cmds.timeout(Msgs.rtcRetryConnection(peerId), waitingTimeoutMs),
      ]),
    )

  | (ReceivedVerifiedSignal(Offer(sdp)), NotInGroup(Online(ssConn))) => (
      OnlineCreatingSdpAnswer(false, ssConn),
      createAcceptorFromOffer(peerId, sdp),
    )

  | (
      ReceivedVerifiedSignal(Offer(sdp)),
      InGroupOnlineCreatingSdpOffer(ssConn, _) |
      InGroupOnlineWaitingForAcceptor(ssConn, _, _) |
      InGroupOnlineFailedRetryingAt(ssConn, _, _, _),
    )
      when shouldAccept(thisPeer, peerId) =>
    let destroyRtcCmd =
      switch (prevState) {
      | InGroupOnlineWaitingForAcceptor(_, rtcConn, _) =>
        RTCCmds.destroy(rtcConn)
      | _ => Cmds.none
      };
    (
      OnlineCreatingSdpAnswer(true, ssConn),
      Cmds.batch([destroyRtcCmd, createAcceptorFromOffer(peerId, sdp)]),
    );

  | (
      ReceivedVerifiedSignal(Offer(sdp)),
      InGroupOnlineWaitingForAcceptor(ssConn, rtcConn, _),
    )
      when shouldAccept(thisPeer, peerId) => (
      OnlineCreatingSdpAnswer(true, ssConn),
      Cmds.batch([
        RTCCmds.destroy(rtcConn),
        createAcceptorFromOffer(peerId, sdp),
      ]),
    )

  | (AddedToGroup, OnlineCreatingSdpAnswer(false, ssConn)) => (
      OnlineCreatingSdpAnswer(true, ssConn),
      Cmds.none,
    )

  | (RtcConnected, OnlineWaitingForInitiator(inGroup, ssConn, rtcConn)) => (
      Connected(rtcConn, inGroup, Online(ssConn)),
      Cmds.none,
    )

  | (RtcConnected, InGroupOnlineWaitingForAcceptor(ssConn, rtcConn, _)) => (
      Connected(rtcConn, true, Online(ssConn)),
      Cmds.none,
    )

  | (WentOnline(ssConn), Connected(rtcConn, inGroup, Offline)) => (
      Connected(rtcConn, inGroup, Online(ssConn)),
      Cmds.none,
    )

  | (WentOffline, Connected(rtcConn, true, Online(_))) => (
      Connected(rtcConn, true, Offline),
      Cmds.none,
    )

  | (AddedToGroup, Connected(rtcConn, false, signalState)) => (
      Connected(rtcConn, true, signalState),
      Cmds.none,
    )

  | (RtcAnswerReady(rtcConn, sdp), OnlineCreatingSdpAnswer(inGroup, ssConn)) => (
      OnlineWaitingForInitiator(inGroup, ssConn, rtcConn),
      Cmds.batch([
        sendSignalMessage(thisPeer, peerId, Answer(sdp), ssConn),
        /* TODO: Exponential backoff */
        Cmds.timeout(Msgs.rtcRetryConnection(peerId), waitingTimeoutMs),
      ]),
    )

  | (AddedToGroup, OnlineWaitingForInitiator(false, ssConn, rtcConn)) => (
      OnlineWaitingForInitiator(true, ssConn, rtcConn),
      Cmds.none,
    )

  | (RtcClose, OnlineCreatingSdpAnswer(true, ssConn)) =>
    let newFailedAttempts = 1;
    let timeoutMs = Retry.getTimeoutMs(newFailedAttempts);
    (
      InGroupOnlineFailedRetryingAt(
        ssConn,
        timeoutMs |> Retry.msToSec,
        newFailedAttempts,
        "",
      ),
      Cmds.timeout(Msgs.rtcRetryConnection(peerId), timeoutMs),
    );

  | (RtcClose, InGroupOnlineCreatingSdpOffer(ssConn, failedAttempts)) =>
    let newFailedAttempts = failedAttempts + 1;
    let timeoutMs = Retry.getTimeoutMs(newFailedAttempts);
    (
      InGroupOnlineFailedRetryingAt(
        ssConn,
        timeoutMs |> Retry.msToSec,
        newFailedAttempts,
        "",
      ),
      Cmds.timeout(Msgs.rtcRetryConnection(peerId), timeoutMs),
    );

  | (
      RtcClose,
      OnlineWaitingForInitiator(true, ssConn, rtcConn) |
      Connected(rtcConn, true, Online(ssConn)),
    ) =>
    let newFailedAttempts = 1;
    let timeoutMs = Retry.getTimeoutMs(newFailedAttempts);
    (
      InGroupOnlineFailedRetryingAt(
        ssConn,
        timeoutMs |> Retry.msToSec,
        newFailedAttempts,
        "",
      ),
      Cmds.batch([
        RTCCmds.destroy(rtcConn),
        Cmds.timeout(Msgs.rtcRetryConnection(peerId), timeoutMs),
      ]),
    );

  | (
      RtcClose,
      InGroupOnlineWaitingForAcceptor(ssConn, rtcConn, failedAttempts),
    ) =>
    let newFailedAttempts = failedAttempts + 1;
    let timeoutMs = Retry.getTimeoutMs(newFailedAttempts);
    (
      InGroupOnlineFailedRetryingAt(
        ssConn,
        timeoutMs |> Retry.msToSec,
        newFailedAttempts,
        "",
      ),
      Cmds.batch([
        RTCCmds.destroy(rtcConn),
        Cmds.timeout(Msgs.rtcRetryConnection(peerId), timeoutMs),
      ]),
    );

  | (RtcClose | WentOffline, OnlineCreatingSdpAnswer(false, ssConn)) => (
      NotInGroup(Online(ssConn)),
      Cmds.none,
    )

  | (
      RtcClose | WentOffline,
      OnlineWaitingForInitiator(false, ssConn, rtcConn),
    ) => (
      NotInGroup(Online(ssConn)),
      RTCCmds.destroy(rtcConn),
    )

  | (RtcClose | WentOffline, Connected(rtcConn, false, peerSignalState)) => (
      NotInGroup(peerSignalState),
      RTCCmds.destroy(rtcConn),
    )

  | (RemovedFromLastGroup, Connected(rtcConn, true, online))
  /* TODO: Really? Don't we need to tell him first? */
  | (RemovedFromLastGroup, Connected(rtcConn, false, online)) => (
      NotInGroup(online),
      RTCCmds.destroy(rtcConn),
    )
  | (RemovedFromLastGroup, InGroupWaitingForOnlineSignal)
  | (WentOffline, NotInGroup(Online(_))) => (
      NotInGroup(Offline),
      Cmds.none,
    )

  | (WentOnline(ssConn), NotInGroup(Offline)) => (
      NotInGroup(Online(ssConn)),
      Cmds.none,
    )

  | (
      RemovedFromLastGroup,
      InGroupOnlineCreatingSdpOffer(ssConn, _) |
      InGroupOnlineFailedRetryingAt(ssConn, _, _, _) |
      OnlineCreatingSdpAnswer(true, ssConn),
    ) => (
      NotInGroup(Online(ssConn)),
      Cmds.none,
    )

  | (
      RemovedFromLastGroup,
      InGroupOnlineWaitingForAcceptor(ssConn, rtcConn, _) |
      OnlineWaitingForInitiator(true, ssConn, rtcConn),
    ) => (
      NotInGroup(Online(ssConn)),
      RTCCmds.destroy(rtcConn),
    )

  /* Matches with no change */

  | (
      ReceivedVerifiedSignal(Answer(_)),
      NotInGroup(_) | InGroupWaitingForOnlineSignal |
      InGroupOnlineCreatingSdpOffer(_) |
      InGroupOnlineFailedRetryingAt(_, _, _, _) |
      OnlineCreatingSdpAnswer(_, _) |
      OnlineWaitingForInitiator(_, _, _) |
      Connected(_, _, _),
    )
  | (
      ReceivedVerifiedSignal(Offer(_)),
      InGroupWaitingForOnlineSignal | InGroupOnlineCreatingSdpOffer(_) |
      InGroupOnlineWaitingForAcceptor(_, _, _) |
      InGroupOnlineFailedRetryingAt(_, _, _, _) |
      OnlineCreatingSdpAnswer(_, _) |
      OnlineWaitingForInitiator(_, _, _) |
      Connected(_, _, _) |
      NotInGroup(Offline),
    )
  | (
      WentOffline,
      NotInGroup(Offline) | InGroupWaitingForOnlineSignal |
      Connected(_, _, Offline),
    )
  | (
      WentOnline(_),
      OnlineCreatingSdpAnswer(_, _) | OnlineWaitingForInitiator(_, _, _) |
      InGroupOnlineCreatingSdpOffer(_) |
      InGroupOnlineWaitingForAcceptor(_) |
      InGroupOnlineFailedRetryingAt(_) |
      Connected(_, _, Online(_)) |
      NotInGroup(Online(_)),
    )
  | (
      RtcRetryConnection,
      OnlineCreatingSdpAnswer(_, _) | OnlineWaitingForInitiator(_, _, _) |
      InGroupWaitingForOnlineSignal |
      InGroupOnlineCreatingSdpOffer(_) |
      Connected(_, _, _) |
      NotInGroup(_),
    )
  | (
      RtcOfferReady(_, _),
      OnlineCreatingSdpAnswer(_, _) | OnlineWaitingForInitiator(_, _, _) |
      InGroupOnlineFailedRetryingAt(_) |
      Connected(_, _, _) |
      InGroupOnlineWaitingForAcceptor(_) |
      InGroupWaitingForOnlineSignal |
      NotInGroup(_),
    )
  | (
      RtcAnswerReady(_, _),
      NotInGroup(_) | InGroupWaitingForOnlineSignal |
      InGroupOnlineCreatingSdpOffer(_) |
      InGroupOnlineWaitingForAcceptor(_, _, _) |
      InGroupOnlineFailedRetryingAt(_, _, _, _) |
      OnlineWaitingForInitiator(_, _, _) |
      Connected(_, _, _),
    )
  | (
      RtcConnected,
      OnlineCreatingSdpAnswer(_, _) | InGroupOnlineFailedRetryingAt(_) |
      InGroupOnlineCreatingSdpOffer(_) |
      Connected(_, _, _) |
      InGroupWaitingForOnlineSignal |
      NotInGroup(_),
    )
  | (
      RemovedFromLastGroup,
      OnlineCreatingSdpAnswer(false, _) |
      OnlineWaitingForInitiator(false, _, _) |
      NotInGroup(_),
    )
  | (
      AddedToGroup,
      OnlineCreatingSdpAnswer(true, _) | Connected(_, true, _) |
      OnlineWaitingForInitiator(true, _, _) |
      InGroupWaitingForOnlineSignal |
      InGroupOnlineCreatingSdpOffer(_) |
      InGroupOnlineWaitingForAcceptor(_) |
      InGroupOnlineFailedRetryingAt(_),
    )
  | (
      RtcClose,
      InGroupOnlineFailedRetryingAt(_) | InGroupWaitingForOnlineSignal |
      NotInGroup(_),
    )
  | (
      UpdateAlias(_) | ReceivedVerifiedSignal(KeyRequest(_) | KeyResponse(_)),
      _,
    )
  | (ReceivedSignal(_), _)
  | (
      SignalVerificationFailed(_) | PrepareKeyRequestFinished(_) |
      VerifyKeyResponseFinished(_) |
      VerifyKeyRequestFinished(_) |
      PrepareKeyResponseFinished(_),
      _,
    ) => (
      prevState,
      Cmds.none,
    )
  };

let updateAlias = alias =>
  fun
  | UpdateAlias(newAlias) => newAlias
  | _ => alias;

let initPublicKey =
    (thisPeer, peerId, peerSignalState: peerSignalState, maybePublicKey) =>
  switch (maybePublicKey, peerSignalState) {
  | (Some(key), _) => (Fetched(peerSignalState, key), Cmds.none)
  | (None, Online(ssConn)) => (
      Fetching(ssConn),
      exportThisPeerKeyCmd(Msgs.prepareKeyRequestFinished, thisPeer, peerId),
    )
  | (None, Offline) => (MissingWaitingForOnline, Cmds.none)
  };

let updatePublicKey =
    (thisPeer: ThisPeer.t, peerId, model: publicKeyFetchingState, msg) =>
  switch (msg, model) {
  /* Request peer's public key if missing */
  | (WentOnline(ssConn), MissingWaitingForOnline) => (
      Fetching(ssConn),
      exportThisPeerKeyCmd(Msgs.prepareKeyRequestFinished, thisPeer, peerId),
    )
  | (WentOnline(ssConn), Fetched(Offline, key)) => (
      Fetched(Online(ssConn), key),
      Cmds.none,
    )

  | (PrepareKeyRequestFinished(Ok(keyStr)), Fetching(ssConn)) => (
      model,
      sendSignalMessage(thisPeer, peerId, KeyRequest(keyStr), ssConn),
    )
  | (PrepareKeyRequestFinished(Error(_)), _) => (
      model,
      Cmds.log("PrepareKeyRequest failed"),
    )

  | (WentOffline, Fetching(_) | Fetched(_, _) | MissingWaitingForOnline) => (
      MissingWaitingForOnline,
      Cmds.none,
    )

  /* Verify public key response */
  | (
      ReceivedSignal(signature, KeyResponse(_srcKeyStr) as p2pMsg),
      Fetching(_),
    ) => (
      model,
      verifyKeyMessageCmd(
        Msgs.verifyKeyResponseFinished,
        thisPeer,
        peerId,
        signature,
        p2pMsg,
      ),
    )

  | (VerifyKeyResponseFinished(Ok(srcKey)), Fetching(ssConn)) => (
      Fetched(Online(ssConn), srcKey),
      Cmds.none,
    )

  | (VerifyKeyResponseFinished(Ok(srcKey)), MissingWaitingForOnline) => (
      Fetched(Offline, srcKey),
      Cmds.none,
    )

  | (
      VerifyKeyResponseFinished(Error(_)),
      MissingWaitingForOnline | Fetching(_),
    ) => (
      MissingWaitingForOnline,
      Cmds.log("VerifyKeyResponse failed"),
    )

  /* Verify public key request */

  | (ReceivedSignal(signature, KeyRequest(_) as p2pMsg), _) => (
      model,
      verifyKeyMessageCmd(
        Msgs.verifyKeyRequestFinished,
        thisPeer,
        peerId,
        signature,
        p2pMsg,
      ),
    )

  | (
      VerifyKeyRequestFinished(Ok(srcKey)),
      Fetched(Online(ssConn), _) | Fetching(ssConn),
    ) => (
      Fetched(Online(ssConn), srcKey),
      /* TODO: Send response */
      exportThisPeerKeyCmd(Msgs.prepareKeyResponseFinished, thisPeer, peerId),
    )
  | (
      VerifyKeyRequestFinished(Ok(srcKey)),
      MissingWaitingForOnline | Fetched(Offline, _),
    ) => (
      Fetched(Offline, srcKey),
      /* TODO: Send response */
      exportThisPeerKeyCmd(Msgs.prepareKeyResponseFinished, thisPeer, peerId),
    )
  | (VerifyKeyRequestFinished(Error(_)), _) => (
      model,
      Cmds.log("VerifyKeyRequest failed"),
    )

  | (
      PrepareKeyResponseFinished(Ok(thisPeerKeyStr)),
      Fetching(ssConn) | Fetched(Online(ssConn), _),
    ) => (
      model,
      sendSignalMessage(
        thisPeer,
        peerId,
        KeyResponse(thisPeerKeyStr),
        ssConn,
      ),
    )

  | (PrepareKeyResponseFinished(Error(_)), _) => (
      model,
      Cmds.log("PrepareKeyResponse failed"),
    )

  /* Check signature if we have peer's public key */
  | (
      ReceivedSignal(signature, (Offer(_) | Answer(_)) as msg),
      Fetched(_, srcKey),
    ) => (
      /* TODO: Messages shouldn't get lost during key exchange, should they? */
      model,
      verifyMessageSignatureCmd(srcKey, signature, peerId, thisPeer.id, msg),
    )

  | (ReceivedSignal(_, Offer(_) | Answer(_)), MissingWaitingForOnline) => (
      model,
      Cmds.log("Received Offer, but I don't have a key yet."),
    )

  | (SignalVerificationFailed(exn), _) => (model, Cmds.log(exn))

  /* No action */
  | (WentOnline(_), Fetching(_) | Fetched(Online(_), _))
  | (PrepareKeyRequestFinished(Ok(_)), MissingWaitingForOnline | Fetched(_))
  | (
      AddedToGroup | RemovedFromLastGroup | ReceivedVerifiedSignal(_) |
      RtcRetryConnection |
      RtcClose |
      RtcAnswerReady(_, _) |
      RtcOfferReady(_, _) |
      RtcConnected |
      UpdateAlias(_),
      _,
    )
  | (ReceivedSignal(_, Offer(_) | Answer(_)), Fetching(_))
  | (ReceivedSignal(_, KeyResponse(_)), Fetched(_) | MissingWaitingForOnline)
  | (
      PrepareKeyResponseFinished(Ok(_)),
      MissingWaitingForOnline | Fetched(Offline, _),
    )
  | (VerifyKeyResponseFinished(_), Fetched(_)) => (model, Cmds.none)
  };

/* MAIN */

let init = (~thisPeer, ~id, ~publicKey, ~alias, ~inGroup, ~peerSignalState) => {
  let (connectionState, connStateCmd) =
    initConnState(id, inGroup, peerSignalState);
  let (publicKey, publicKeyCmd) =
    initPublicKey(thisPeer, id, peerSignalState, publicKey);
  (
    {id, publicKey, alias, connectionState},
    Cmds.batch([connStateCmd, publicKeyCmd]),
  );
};

let update = (thisPeer, model: t, msg) => {
  let newAlias = updateAlias(model.alias, msg);

  let (newPublicKey, publicKeyCmd) =
    updatePublicKey(thisPeer, model.id, model.publicKey, msg);

  let (newConnectionState, connectionStateCmd) =
    updateConnState(
      thisPeer,
      model.id,
      model.publicKey,
      model.connectionState,
      msg,
    );

  (
    {
      ...model,
      publicKey: newPublicKey,
      alias: newAlias,
      connectionState: newConnectionState,
    },
    Cmds.batch([publicKeyCmd, connectionStateCmd]),
  );
};

/* PERSISTENCY */

let initFromDb =
    (thisPeer, {Types.id, publicKey, alias}, inGroup, peerSignalState) =>
  init(~thisPeer, ~id, ~publicKey, ~alias, ~inGroup, ~peerSignalState);

let toDb = ({id, publicKey: publicKeyFetchingState, alias, _}: t) => {
  let publicKey =
    switch (publicKeyFetchingState) {
    | Fetched(_, key) => Some(key)
    | MissingWaitingForOnline
    | Fetching(_) => None
    };
  {Types.id, publicKey, alias};
};

/* QUERIES */

let getActiveConnection = peer =>
  switch (peer.connectionState) {
  | Connected(rtcConn, _, _) => Some(rtcConn)
  | _ => None
  };