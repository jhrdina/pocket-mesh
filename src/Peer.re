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

type sdpMsg =
  | Offer(Message.sdpMessage)
  | Answer(Message.sdpMessage);

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
  publicKey: option(SimpleCrypto.key),
  alias: string,
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
  | RtcConnected
  /* Not influencing connection state */
  | UpdateAlias(string);

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
      InGroupOnlineCreatingSdpOffer(ssConn, 0),
      createInitiator(peerId),
    )
  };

let updateConnState = (thisPeer, peerId, prevState, msg) =>
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
        /* TODO: Exponential backoff */
        Cmds.timeout(Msgs.rtcRetryConnection(peerId), waitingTimeoutMs),
      ]),
    )

  | (
      ReceivedSdp(Answer(msg)),
      InGroupOnlineWaitingForAcceptor(_ssConn, rtcConn, _),
    ) => (
      /* TODO: Check Signature, maybe move upwards to Peer.update */
      prevState,
      Cmds.batch([
        RTCCmds.signal(rtcConn, msg.sdp),
        /* TODO: Exponential backoff */
        Cmds.timeout(Msgs.rtcRetryConnection(peerId), waitingTimeoutMs),
      ]),
    )

  | (ReceivedSdp(Offer(offer)), NotInGroup(Online(ssConn))) => (
      OnlineCreatingSdpAnswer(false, ssConn),
      createAcceptorFromOffer(offer),
    )

  | (
      ReceivedSdp(Offer(offer)),
      InGroupOnlineCreatingSdpOffer(ssConn, _) |
      InGroupOnlineWaitingForAcceptor(ssConn, _, _) |
      InGroupOnlineFailedRetryingAt(ssConn, _, _, _),
    )
      when shouldAccept(thisPeer, offer.src) =>
    let destroyRtcCmd =
      switch (prevState) {
      | InGroupOnlineWaitingForAcceptor(_, rtcConn, _) =>
        RTCCmds.destroy(rtcConn)
      | _ => Cmds.none
      };
    (
      OnlineCreatingSdpAnswer(true, ssConn),
      Cmds.batch([destroyRtcCmd, createAcceptorFromOffer(offer)]),
    );

  | (
      ReceivedSdp(Offer(offer)),
      InGroupOnlineWaitingForAcceptor(ssConn, rtcConn, _),
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
  | (UpdateAlias(_), _) => (prevState, Cmds.none)
  };

let updateAlias = alias =>
  fun
  | UpdateAlias(newAlias) => newAlias
  | _ => alias;

let init = (~id, ~publicKey, ~alias, ~inGroup, ~peerSignalState) => {
  let (connectionState, cmd) = initConnState(id, inGroup, peerSignalState);
  ({id, publicKey, alias, connectionState}, cmd);
};

let update = (thisPeer, model: t, msg) => {
  let newAlias = updateAlias(model.alias, msg);

  let (newConnectionState, connectionStateCmd) =
    updateConnState(thisPeer, model.id, model.connectionState, msg);

  (
    {...model, alias: newAlias, connectionState: newConnectionState},
    connectionStateCmd,
  );
};

/* PERSISTENCY */

let initFromDb = ({Types.id, publicKey, alias}, inGroup, peerSignalState) =>
  init(~id, ~publicKey, ~alias, ~inGroup, ~peerSignalState);

let toDb = ({id, publicKey, alias, _}: t) => {Types.id, publicKey, alias};

/* QUERIES */

let getActiveConnection = peer =>
  switch (peer.connectionState) {
  | Connected(rtcConn, _, _) => Some(rtcConn)
  | _ => None
  };