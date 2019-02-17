open BlackTea;

// CONSTANTS

let waitingTimeoutMs = 10 * 1000;

/* TYPES */

type connectionState =
  /* failedAttempts */
  | CreatingSdpOffer(int)
  /* rtcConn, failedAttempts */
  | WaitingForAcceptor(RtcSub.conn, int)
  /* sdp */
  | CreatingSdpAnswer(string)
  | WaitingForInitiator(string, RtcSub.conn)
  /* RTC conn, inGroup, signal online */
  | Connected(RtcSub.conn, RtcSub.role);

type t = PeerId.Map.t(connectionState);

type Msgs.t +=
  | RtcMsg(PeerId.t, option(RtcSub.msg))
  | WaitingTimeoutExpired(PeerId.t);

let rtcMsg = (peerId, msg) => RtcMsg(peerId, msg);
let waitingTimeoutExpired = peerId => WaitingTimeoutExpired(peerId);

// QUERIES

let foldActiveConnections = (f, acc, t) =>
  PeerId.Map.fold(
    (peerId, connState, acc) =>
      switch (connState) {
      | Connected(conn, _role) => f(acc, peerId, conn)
      | _ => acc
      },
    t,
    acc,
  );

let getPeerConnectionState = (peerId, t) => PeerId.Map.findOpt(peerId, t);

// HELPERS

let updatePeerState = PeerId.Map.add;

let shouldAccept = (thisPeer: ThisPeer.t, srcPeerId) =>
  thisPeer.id < srcPeerId;

/* UPDATE */

let derive =
    (~peersStatuses: PeersStatuses.t, ~peersGroups: PeersGroups.t, model) => {
  let onlinePeers = peersStatuses |> PeersStatuses.getOnlinePeers;
  PeerId.Set.fold(
    (peerId, connStates) =>
      switch (
        peersGroups |> PeersGroups.isPeerInAGroup(peerId),
        model |> PeerId.Map.findOpt(peerId),
      ) {
      | (true, None) =>
        connStates |> PeerId.Map.add(peerId, CreatingSdpOffer(0))
      | (true | false, Some(state)) =>
        connStates |> PeerId.Map.add(peerId, state)
      | (false, None) => connStates
      },
    onlinePeers,
    PeerId.Map.empty,
  );
};

let init = (~peersStatuses, ~peersGroups) => {
  derive(~peersStatuses, ~peersGroups, PeerId.Map.empty);
};

let handleReceivedSignalMessage =
    (~thisPeer, ~src, ~msg: Message.peerToPeerMsg, model) => {
  switch (msg, model |> PeerId.Map.findOpt(src)) {
  | (Answer(sdp), Some(WaitingForAcceptor(rtcConn, a))) => (
      model,
      RtcSub.signalCmd(rtcConn, sdp),
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
        Connected(_, _),
      ),
    )
  | (KeyRequest(_) | KeyResponse(_), _) => (model, Cmd.none)
  };
};

let handleRtcSubMsg =
    (~thisPeer: ThisPeer.t, ~peerId, msg: option(RtcSub.msg), model) => {
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

  | (None, _) =>
    // TODO: Wrap with current state check
    (model |> updatePeerState(peerId, CreatingSdpOffer(0)), Cmd.none)

  | (Some(Connected(rtcConn)), Some(WaitingForAcceptor(_, _a))) => (
      model |> updatePeerState(peerId, Connected(rtcConn, Initiator)),
      Cmd.none,
    )

  | (Some(Connected(rtcConn)), Some(WaitingForInitiator(_, _))) => (
      model |> updatePeerState(peerId, Connected(rtcConn, Acceptor)),
      Cmd.none,
    )

  | (Some(Signal(_, Offer, _)), Some(_) | None)
  | (Some(Signal(_, Answer, _)), Some(_) | None)
  | (Some(GotData(ArrayBuffer(_))), _)
  | (
      Some(Connected(_)),
      None |
      Some(CreatingSdpOffer(_) | CreatingSdpAnswer(_) | Connected(_, _)),
    ) => (
      model,
      Cmd.none,
    )

  | (Some(Error(str)), _) => (
      model,
      Cmds.log(
        "RTC w/ peer " ++ (peerId |> PeerId.toString) ++ " error: \n" ++ str,
      ),
    )

  /* DEBUG */
  /* TODO: Remove me */
  | (Some(GotData(String(data))), _) => (
      model,
      Cmds.log(
        "Store: Got data from " ++ (peerId |> PeerId.toString) ++ ": " ++ data,
      ),
    )
  };
};

let update =
    (
      ~thisPeer: ThisPeer.t,
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

    | RtcMsg(peerId, rtcMsg) =>
      handleRtcSubMsg(~thisPeer, ~peerId, rtcMsg, model)

    | WaitingTimeoutExpired(peerId) =>
      switch (model |> PeerId.Map.findOpt(peerId)) {
      | Some(WaitingForAcceptor(_) | WaitingForInitiator(_)) => (
          model |> updatePeerState(peerId, CreatingSdpOffer(0)),
          Cmd.none,
        )
      | Some(_)
      | None => (model, Cmd.none)
      }

    | _ => (model, Cmd.none)
    };

  let model = model |> derive(~peersStatuses, ~peersGroups);
  (model, cmd);
};

let waitingTimeoutSub = (peerId, myRole: RtcSub.role) => {
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

let subscriptions = model =>
  PeerId.Map.fold(
    (peerId, connState, subs) =>
      switch (connState) {
      | CreatingSdpOffer(_a)
      | WaitingForAcceptor(_, _a) => [
          RtcSub.sub("PeersConnections", peerId, Initiator, rtcMsg),
          switch (connState) {
          | WaitingForAcceptor(_, _) => waitingTimeoutSub(peerId, Initiator)
          | _ => Sub.none
          },
          ...subs,
        ]
      | CreatingSdpAnswer(sdp)
      | WaitingForInitiator(sdp, _) => [
          RtcSub.sub(
            "PeersConnections",
            peerId,
            Acceptor,
            ~initSignal=sdp,
            rtcMsg,
          ),
          switch (connState) {
          | WaitingForInitiator(_, _) => waitingTimeoutSub(peerId, Acceptor)
          | _ => Sub.none
          },
          ...subs,
        ]
      | Connected(_, role) => [
          RtcSub.sub("PeersConnections", peerId, role, rtcMsg),
          ...subs,
        ]
      },
    model,
    [],
  )
  |> Sub.batch;