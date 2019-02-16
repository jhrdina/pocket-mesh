open BlackTea;

/* TYPES */

type connectionState =
  /* failedAttempts */
  | CreatingSdpOffer(int)
  /* rtcConn, failedAttempts */
  | WaitingForAcceptor(RtcSub.conn, int)
  /* intervalSec, failedAttempts, lastErrorMessage */
  | FailedRetryingAt(int, int, string)
  /* sdp */
  | CreatingSdpAnswer(string)
  | WaitingForInitiator(string, RtcSub.conn)
  /* RTC conn, inGroup, signal online */
  | Connected(RtcSub.conn, RtcSub.role);

type t = PeerId.Map.t(connectionState);

type Msgs.t +=
  | RtcMsg(PeerId.t, option(RtcSub.msg));

let rtcMsg = (peerId, msg) => RtcMsg(peerId, msg);

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
      switch (p2pSignalMsg, model |> PeerId.Map.findOpt(src)) {
      | (Answer(sdp), Some(WaitingForAcceptor(rtcConn, a))) => (
          model,
          RtcSub.signalCmd(rtcConn, sdp),
        )
      | (Offer(sdp), None) => (
          model |> updatePeerState(src, CreatingSdpAnswer(sdp)),
          Cmd.none,
        )
      | (
          Offer(sdp),
          Some(
            CreatingSdpOffer(a) | WaitingForAcceptor(_, a) |
            FailedRetryingAt(_, a, _),
          ),
        )
          when shouldAccept(thisPeer, src) => (
          model |> updatePeerState(src, CreatingSdpAnswer(sdp)),
          Cmd.none,
        )

      | (Offer(_), Some(_))
      | (
          Answer(_),
          None |
          Some(
            CreatingSdpOffer(_) | FailedRetryingAt(_, _, _) |
            CreatingSdpAnswer(_) |
            WaitingForInitiator(_) |
            Connected(_, _),
          ),
        )
      | (KeyRequest(_) | KeyResponse(_), _) => (model, Cmd.none)
      }

    | RtcMsg(peerId, Some(Signal(rtcConn, Offer, sdp))) =>
      switch (model |> PeerId.Map.findOpt(peerId)) {
      | Some(CreatingSdpOffer(a)) => (
          model |> updatePeerState(peerId, WaitingForAcceptor(rtcConn, a)),
          Cmd.msg(
            SignalVerifier.SignAndSendMsg(
              PeerToPeer(thisPeer.id, peerId, Offer(sdp)),
            ),
          ),
        )
      | Some(_)
      | None => (model, Cmd.none)
      }

    /* DEBUG */
    /* TODO: Remove me */
    | RtcMsg(peerId, Some(GotData(String(data)))) => (
        model,
        Cmds.log(
          "Store: Got data from "
          ++ (peerId |> PeerId.toString)
          ++ ": "
          ++ data,
        ),
      )
    | _ => (model, Cmd.none)
    };

  let model = model |> derive(~peersStatuses, ~peersGroups);
  (model, cmd);
};

let subscriptions = model =>
  PeerId.Map.fold(
    (peerId, connState, subs) =>
      switch (connState) {
      | CreatingSdpOffer(_a)
      | WaitingForAcceptor(_, _a) => [
          RtcSub.sub("PeersConnections", peerId, Initiator, rtcMsg),
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
          ...subs,
        ]
      | Connected(_, role) => [
          RtcSub.sub("PeersConnections", peerId, role, rtcMsg),
          ...subs,
        ]
      | FailedRetryingAt(_, _, _) => subs
      },
    model,
    [],
  )
  |> Sub.batch;