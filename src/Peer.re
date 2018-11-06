/* TYPES */

type peerSignalState =
  | Online(SignalServerCmds.conn)
  | Offline;

type sdpMsg =
  | Offer(Message.sdpMessage)
  | Answer(Message.sdpMessage);

type peerConnectionState =
  /* bool = online */
  | NotInGroup(peerSignalState)
  | InGroupWaitingForOnlineSignal
  | InGroupOnlineCreatingSdpOffer(SignalServerCmds.conn)
  | InGroupOnlineWaitingForAcceptor(SignalServerCmds.conn, RTCCmds.t)
  /* time, attemptsMade, lastErrorMessage */
  /* | Connecting */
  | InGroupOnlineFailedRetryingAt(SignalServerCmds.conn, string, int, string)
  /* inGroup */
  | OnlineCreatingSdpAnswer(bool, SignalServerCmds.conn)
  | OnlineWaitingForInitiator(bool, SignalServerCmds.conn, RTCCmds.t)
  /* RTC conn, inGroup, signal online */
  | Connected(RTCCmds.t, bool, peerSignalState);

/* Another alternative */

/* type peerConnectionOnlineState =
     | CreatingSdpOffer
     | WaitingForSdpAnswer(RTCCmds.t)
     | FailedRetryingAt(string, int, string);

   type signallingState =
     | WaitingForOnline
     | Online(peerConnectionOnlineState);

   type rtcConnState =
     | Signalling(signallingState)
     | Connected(RTCCmds.t);

   type peerConnectionState =
     | NotInGroup(peerSignalState)
     | InGroup(rtcConnState); */

type t = {
  id: PeerId.t,
  publicKey: SimpleCrypto.key,
  nickName: string,
  connectionState: peerConnectionState,
};

[@bs.deriving accessors]
type msgs =
  /* external */
  | AddedToGroup
  | RemovedFromLastGroup
  | WentOnline(SignalServerCmds.conn)
  | WentOffline
  | ReceivedSdp(sdpMsg)
  /* internal */
  | RtcRetryConnection
  | RtcClose
  /* t, sdp */
  | RtcAnswerReady(RTCCmds.t, string)
  | RtcOfferReady(RTCCmds.t, string)
  | RtcConnected;

type effect =
  | None
  | SendSignal(string)
  | RTCConnect
  | RTCPlanRetry;

exception InvalidState;

/* HELPERS */

let shouldAccept = (thisPeer: ThisPeer.t, srcPeerId) =>
  thisPeer.id < srcPeerId;

let createAcceptorFromOffer = (offer: Message.sdpMessage) =>
  RTCCmds.createAcceptor(
    offer.src,
    offer.sdp,
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

let peerSignalStateOfSignalServerState = peerId =>
  fun
  | Types.Connected(ssConn, onlinePeers)
      when PeerId.Set.mem(peerId, onlinePeers) =>
    Online(ssConn)
  | _ => Offline;

/* UPDATES */

let initConnState = (peerId, inGroup, peerSignalState) =>
  switch (inGroup, peerSignalState) {
  | (false, peerSignalState) => (NotInGroup(peerSignalState), Cmds.none)
  | (true, Offline) => (InGroupWaitingForOnlineSignal, Cmds.none)
  | (true, Online(ssConn)) => (
      InGroupOnlineCreatingSdpOffer(ssConn),
      RTCCmds.createInitiator(
        peerId,
        Msgs.rtcOfferReady,
        Msgs.rtcConnected,
        Msgs.rtcGotData,
        Msgs.rtcError,
        Msgs.rtcClose,
      ),
    )
  };

let updateConnState = (thisPeer, peerId, prevState, msg) =>
  switch (msg, prevState) {
  | (AddedToGroup, NotInGroup(Online(ssConn)))
  | (WentOnline(ssConn), InGroupWaitingForOnlineSignal)
  | (RtcRetryConnection, InGroupOnlineFailedRetryingAt(ssConn, _, _, _)) => (
      InGroupOnlineCreatingSdpOffer(ssConn),
      createInitiator(peerId),
    )

  /* Waiting timeout */
  | (RtcRetryConnection, InGroupOnlineWaitingForAcceptor(ssConn, rtcConn)) => (
      InGroupOnlineCreatingSdpOffer(ssConn),
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
      InGroupOnlineWaitingForAcceptor(_, rtcConn) |
      OnlineWaitingForInitiator(true, _, rtcConn),
    ) => (
      InGroupWaitingForOnlineSignal,
      RTCCmds.destroy(rtcConn),
    )

  | (RtcOfferReady(rtcConn, sdp), InGroupOnlineCreatingSdpOffer(ssConn)) => (
      InGroupOnlineWaitingForAcceptor(ssConn, rtcConn),
      Cmds.batch([
        SignalServerCmds.sendMsg(
          Offer({
            src: thisPeer.ThisPeer.id,
            tg: peerId,
            sdp,
            /* TODO: Sign message */
            signature: "",
          }),
          ssConn,
        ),
        Cmds.timeout(Msgs.rtcRetryConnection(peerId), 10000),
      ]),
    )

  | (
      ReceivedSdp(Answer(msg)),
      InGroupOnlineWaitingForAcceptor(_ssConn, rtcConn),
    ) => (
      /* TODO: Check Signature, maybe move upwards to Peer.update */
      prevState,
      RTCCmds.signal(rtcConn, msg.sdp),
    )

  | (ReceivedSdp(Offer(offer)), NotInGroup(Online(ssConn))) => (
      OnlineCreatingSdpAnswer(false, ssConn),
      createAcceptorFromOffer(offer),
    )

  | (
      ReceivedSdp(Offer(offer)),
      InGroupOnlineCreatingSdpOffer(ssConn) |
      InGroupOnlineWaitingForAcceptor(ssConn, _) |
      InGroupOnlineFailedRetryingAt(ssConn, _, _, _),
    )
      when shouldAccept(thisPeer, offer.src) =>
    let destroyRtcCmd =
      switch (prevState) {
      | InGroupOnlineWaitingForAcceptor(_, rtcConn) =>
        RTCCmds.destroy(rtcConn)
      | _ => Cmds.none
      };
    (
      OnlineCreatingSdpAnswer(true, ssConn),
      Cmds.batch([destroyRtcCmd, createAcceptorFromOffer(offer)]),
    );

  | (
      ReceivedSdp(Offer(offer)),
      InGroupOnlineWaitingForAcceptor(ssConn, rtcConn),
    )
      when shouldAccept(thisPeer, offer.src) => (
      OnlineCreatingSdpAnswer(true, ssConn),
      Cmds.batch([
        RTCCmds.destroy(rtcConn),
        createAcceptorFromOffer(offer),
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

  | (RtcConnected, InGroupOnlineWaitingForAcceptor(ssConn, rtcConn)) => (
      Connected(rtcConn, true, Online(ssConn)),
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
      SignalServerCmds.sendMsg(
        Answer({
          src: thisPeer.id,
          tg: peerId,
          sdp,
          /* TODO: Sign message */
          signature: "",
        }),
        ssConn,
      ),
    )

  | (AddedToGroup, OnlineWaitingForInitiator(false, ssConn, rtcConn)) => (
      OnlineWaitingForInitiator(true, ssConn, rtcConn),
      Cmds.none,
    )

  | (
      RtcClose,
      InGroupOnlineCreatingSdpOffer(ssConn) |
      OnlineCreatingSdpAnswer(true, ssConn),
    ) => (
      /* TODO: Store attemptsMade */
      InGroupOnlineFailedRetryingAt(ssConn, "", 0, ""),
      Cmds.timeout(Msgs.rtcRetryConnection(peerId), Retry.getTimeoutMs(0)),
    )

  | (
      RtcClose,
      InGroupOnlineWaitingForAcceptor(ssConn, rtcConn) |
      OnlineWaitingForInitiator(true, ssConn, rtcConn) |
      Connected(rtcConn, true, Online(ssConn)),
    ) => (
      /* TODO: Store attemptsMade */
      InGroupOnlineFailedRetryingAt(ssConn, "", 0, ""),
      Cmds.batch([
        RTCCmds.destroy(rtcConn),
        Cmds.timeout(
          Msgs.rtcRetryConnection(peerId),
          Retry.getTimeoutMs(0),
        ),
      ]),
    )

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

  | (
      RemovedFromLastGroup,
      InGroupOnlineCreatingSdpOffer(ssConn) |
      InGroupOnlineFailedRetryingAt(ssConn, _, _, _) |
      OnlineCreatingSdpAnswer(true, ssConn),
    ) => (
      NotInGroup(Online(ssConn)),
      Cmds.none,
    )

  | (
      RemovedFromLastGroup,
      InGroupOnlineWaitingForAcceptor(ssConn, rtcConn) |
      OnlineWaitingForInitiator(true, ssConn, rtcConn),
    ) => (
      NotInGroup(Online(ssConn)),
      RTCCmds.destroy(rtcConn),
    )

  /* Matches with no change */

  | (
      ReceivedSdp(Answer(_)),
      NotInGroup(_) | InGroupWaitingForOnlineSignal |
      InGroupOnlineCreatingSdpOffer(_) |
      InGroupOnlineFailedRetryingAt(_, _, _, _) |
      OnlineCreatingSdpAnswer(_, _) |
      OnlineWaitingForInitiator(_, _, _) |
      Connected(_, _, _),
    )
  | (
      ReceivedSdp(Offer(_)),
      InGroupWaitingForOnlineSignal | InGroupOnlineCreatingSdpOffer(_) |
      InGroupOnlineWaitingForAcceptor(_, _) |
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
      Connected(_, _, _) |
      NotInGroup(_),
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
      InGroupOnlineWaitingForAcceptor(_, _) |
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
    ) => (
      prevState,
      Cmds.none,
    )
  };

let init = (~id, ~publicKey, ~nickName, ~inGroup, ~peerSignalState) => {
  let (connectionState, cmd) = initConnState(id, inGroup, peerSignalState);
  ({id, publicKey, nickName, connectionState}, cmd);
};

let update = (thisPeer, model: t, msg) => {
  let (newConnectionState, connectionStateCmd) =
    updateConnState(thisPeer, model.id, model.connectionState, msg);
  ({...model, connectionState: newConnectionState}, connectionStateCmd);
};

/* PERSISTENCY */

let initFromDb = ({Types.id, publicKey, nickName}, inGroup, peerSignalState) =>
  init(~id, ~publicKey, ~nickName, ~inGroup, ~peerSignalState);

let toDb = ({id, publicKey, nickName, _}: t) => {
  Types.id,
  publicKey,
  nickName,
};

/* QUERIES */

let getActiveConnection = peer =>
  switch (peer.connectionState) {
  | Connected(rtcConn, _, _) => Some(rtcConn)
  | _ => None
  };