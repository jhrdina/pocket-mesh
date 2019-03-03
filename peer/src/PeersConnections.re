open BlackTea;

// CONSTANTS

let waitingTimeoutMs = 10 * 1000;

/* TYPES */

type connectionState =
  /* failedAttempts */
  | CreatingSdpOffer(int)
  /* rtcConn, failedAttempts */
  | WaitingForAcceptor(RTCSub.conn, int)
  /* sdp */
  | CreatingSdpAnswer(string)
  | WaitingForInitiator(string, RTCSub.conn)
  /* RTC conn, inGroup, signal online */
  | Connected(RTCSub.conn, RTCSub.role, int);

type t = PeerId.Map.t(connectionState);

type Msgs.t +=
  // Inputs
  | Send(PeerId.t, RTCSub.payload)
  // Internal
  | RTCMsg(PeerId.t, option(RTCSub.msg))
  | WaitingTimeoutExpired(PeerId.t)
  // Outputs
  | GotData(PeerId.t, RTCSub.payload);

let rtcMsg = (peerId, msg) => RTCMsg(peerId, msg);
let waitingTimeoutExpired = peerId => WaitingTimeoutExpired(peerId);

// QUERIES

let foldActiveConnections = (f, acc, t) =>
  PeerId.Map.fold(
    (peerId, connState, acc) =>
      switch (connState) {
      | Connected(conn, _role, _a) => f(acc, peerId, conn)
      | _ => acc
      },
    t,
    acc,
  );

let getPeerConnectionState = (peerId, t) => PeerId.Map.findOpt(peerId, t);

let diffGotConnected = diffRes =>
  switch (diffRes) {
  | PeerId.Map.Right(Connected(_))
  | Unequal(
      CreatingSdpOffer(_) | WaitingForAcceptor(_) | CreatingSdpAnswer(_) |
      WaitingForInitiator(_),
      Connected(_),
    ) =>
    true
  | Left(_)
  | Right(_)
  | Unequal(Connected(_, _, _), _)
  | Unequal(
      CreatingSdpOffer(_) | WaitingForAcceptor(_) | CreatingSdpAnswer(_) |
      WaitingForInitiator(_),
      CreatingSdpOffer(_) | WaitingForAcceptor(_, _) | CreatingSdpAnswer(_) |
      WaitingForInitiator(_, _),
    ) =>
    false
  };

// HELPERS

let updatePeerState = PeerId.Map.add;

let shouldAccept = (thisPeer: ThisPeer.t, srcPeerId) =>
  thisPeer.id < srcPeerId;

let unionMergerRightWins = (key, a, b) =>
  switch (a, b) {
  | (Some(v), None)
  | (None, Some(v))
  | (Some(_), Some(v)) => Some(v)
  | (None, None) => None
  };

/* UPDATE */

let derive =
    (
      ~peers: Peers.t,
      ~peersGroups: PeersGroups.t,
      ~peersStatuses: PeersStatuses.t,
      model,
    ) => {
  let onlinePeers = peersStatuses |> PeersStatuses.getOnlinePeers;

  PeerId.Map.fold(
    (peerId, _peer, connStates) =>
      switch (
        onlinePeers |> PeerId.Set.mem(peerId),
        peersGroups |> PeersGroups.isPeerInAGroup(peerId),
        model |> PeerId.Map.findOpt(peerId),
      ) {
      | (true, true, None) =>
        connStates |> PeerId.Map.add(peerId, CreatingSdpOffer(0))
      | (true, true, Some(state))
      | (false, true, Some(Connected(_) as state)) =>
        connStates |> PeerId.Map.add(peerId, state)
      | (true | false, false, _)
      | (false, true, _) => connStates
      },
    peers.byId,
    PeerId.Map.empty,
  );
};

let init = (~peers, ~peersStatuses, ~peersGroups) => {
  derive(~peers, ~peersStatuses, ~peersGroups, PeerId.Map.empty);
};

let handleReceivedSignalMessage =
    (~thisPeer, ~src, ~msg: Message.peerToPeerMsg, model) => {
  switch (msg, model |> PeerId.Map.findOpt(src)) {
  | (Answer(sdp), Some(WaitingForAcceptor(rtcConn, a))) => (
      model,
      RTCSub.signalCmd(rtcConn, sdp),
    )
  | (Offer(sdp), None) => (
      model |> updatePeerState(src, CreatingSdpAnswer(sdp)),
      Cmd.none,
    )
  | (Offer(sdp), Some(CreatingSdpOffer(a) | WaitingForAcceptor(_, a)))
      when shouldAccept(thisPeer, src) => (
      model |> updatePeerState(src, CreatingSdpAnswer(sdp)),
      Cmd.none,
    )

  | (Offer(_), Some(_))
  | (
      Answer(_),
      None |
      Some(
        CreatingSdpOffer(_) | CreatingSdpAnswer(_) | WaitingForInitiator(_) |
        Connected(_, _, _),
      ),
    )
  | (KeyRequest(_) | KeyResponse(_), _) => (model, Cmd.none)
  };
};

let handleRTCSubMsg =
    (~thisPeer: ThisPeer.t, ~peerId, msg: option(RTCSub.msg), model) => {
  switch (msg, model |> PeerId.Map.findOpt(peerId)) {
  | (Some(Signal(rtcConn, Offer, sdp)), Some(CreatingSdpOffer(a))) => (
      model |> updatePeerState(peerId, WaitingForAcceptor(rtcConn, a)),
      Cmd.msg(
        SignalVerifier.SignAndSendMsg(
          PeerToPeer(thisPeer.id, peerId, Offer(sdp)),
        ),
      ),
    )

  | (
      Some(Signal(rtcConn, Answer, sdpAnswer)),
      Some(CreatingSdpAnswer(sdpOffer)),
    ) => (
      model
      |> updatePeerState(peerId, WaitingForInitiator(sdpOffer, rtcConn)),
      Cmd.msg(
        SignalVerifier.SignAndSendMsg(
          PeerToPeer(thisPeer.id, peerId, Answer(sdpAnswer)),
        ),
      ),
    )

  | (None, _) => (
      model |> updatePeerState(peerId, CreatingSdpOffer(0)),
      Cmd.none,
    )

  | (Some(Connected(rtcConn)), Some(WaitingForAcceptor(_, a))) => (
      model |> updatePeerState(peerId, Connected(rtcConn, Initiator, a)),
      Cmd.none,
    )

  | (Some(Connected(rtcConn)), Some(WaitingForInitiator(_, _))) => (
      model |> updatePeerState(peerId, Connected(rtcConn, Acceptor, 0)),
      Cmd.none,
    )

  | (Some(GotData(data)), _) =>
    /* DEBUG */
    /* TODO: Remove me */
    let logCmd =
      switch (data) {
      | String(strData) =>
        Cmds.log(
          "[PeersConnections] Got data from "
          ++ (peerId |> PeerId.toString)
          ++ ": "
          ++ strData,
        )
      | ArrayBuffer(_) => Cmd.none
      };
    (model, Cmd.batch([logCmd, Cmd.msg(GotData(peerId, data))]));

  | (Some(Error(str)), _) => (
      model,
      Cmds.log(
        "RTC w/ peer " ++ (peerId |> PeerId.toString) ++ " error: \n" ++ str,
      ),
    )

  | (Some(Signal(_, Offer, _)), Some(_) | None)
  | (Some(Signal(_, Answer, _)), Some(_) | None)
  | (
      Some(Connected(_)),
      None |
      Some(CreatingSdpOffer(_) | CreatingSdpAnswer(_) | Connected(_, _, _)),
    ) => (
      model,
      Cmd.none,
    )
  };
};

let update =
    (
      ~thisPeer: ThisPeer.t,
      ~peers: Peers.t,
      ~peersGroups,
      ~peersStatuses,
      msg: Msgs.t,
      model: t,
    ) => {
  let (model, cmd) =
    switch (msg) {
    | SignalVerifier.CompletedMessageVerification(
        Ok(PeerToPeer(src, tg, p2pSignalMsg)),
      )
        when tg == thisPeer.id =>
      handleReceivedSignalMessage(~thisPeer, ~src, ~msg=p2pSignalMsg, model)

    | RTCMsg(peerId, rtcMsg) =>
      handleRTCSubMsg(~thisPeer, ~peerId, rtcMsg, model)

    | WaitingTimeoutExpired(peerId) =>
      switch (model |> PeerId.Map.findOpt(peerId)) {
      | Some(WaitingForInitiator(_)) => (
          model |> updatePeerState(peerId, CreatingSdpOffer(0)),
          Cmd.none,
        )
      | Some(WaitingForAcceptor(_, a)) => (
          model |> updatePeerState(peerId, CreatingSdpOffer(a + 1)),
          Cmd.none,
        )
      | Some(_)
      | None => (model, Cmd.none)
      }

    | Send(peerId, data) =>
      switch (model |> PeerId.Map.findOpt(peerId)) {
      | Some(Connected(rtcConn, _role, _a)) => (
          model,
          RTCSub.sendCmd(rtcConn, data),
        )
      | _ => (
          model,
          Cmds.log(
            "[PeersConnections] Trying to send data to peer that is not connected.",
          ),
        )
      }

    | _ => (model, Cmd.none)
    };

  let model = model |> derive(~peers, ~peersStatuses, ~peersGroups);
  (model, cmd);
};

let waitingTimeoutSub = (peerId, myRole: RTCSub.role) => {
  let str =
    switch (myRole) {
    | Initiator => "waitForAccept"
    | Acceptor => "waitForInit"
    };
  Subs.timeout(
    "PeersConnections/" ++ str ++ "/" ++ (peerId |> PeerId.toString),
    waitingTimeoutMs,
    waitingTimeoutExpired(peerId),
  );
};

let rtcSub = attempt =>
  RTCSub.sub("PeersConnections/" ++ string_of_int(attempt));

let subscriptions = model =>
  PeerId.Map.fold(
    (peerId, connState, subs) =>
      switch (connState) {
      | CreatingSdpOffer(a)
      | WaitingForAcceptor(_, a) => [
          rtcSub(a, peerId, Initiator, rtcMsg),
          switch (connState) {
          | WaitingForAcceptor(_, _) => waitingTimeoutSub(peerId, Initiator)
          | _ => Sub.none
          },
          ...subs,
        ]
      | CreatingSdpAnswer(sdp)
      | WaitingForInitiator(sdp, _) => [
          rtcSub(0, peerId, Acceptor, ~initSignal=sdp, rtcMsg),
          switch (connState) {
          | WaitingForInitiator(_, _) => waitingTimeoutSub(peerId, Acceptor)
          | _ => Sub.none
          },
          ...subs,
        ]
      | Connected(_, role, a) => [rtcSub(a, peerId, role, rtcMsg), ...subs]
      },
    model,
    [],
  )
  |> Sub.batch;