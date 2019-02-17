open BlackTea;

type conn = {
  simpleRtc: SimpleRTC.t,
  mutable chunkerState: SimpleRTCChunker.t,
};

type sdpType =
  | Offer
  | Answer;

type msg =
  | Signal(conn, sdpType, string)
  | Connected(conn)
  | GotData(SimpleRTCChunker.payload)
  | Error(string);

type role = SimpleRTC.role;

let sdpTypeSentByRole =
  fun
  | SimpleRTC.Initiator => Offer
  | Acceptor => Answer;

let rtcSource = (~initSignal, role) =>
  Wonka.make((. observer: Wonka_types.observerT(option(msg))) => {
    let t = {
      simpleRtc: SimpleRTC.create({role: role}),
      chunkerState: SimpleRTCChunker.make(),
    };
    t.simpleRtc
    ->SimpleRTC.setOnSignal(sdp =>
        observer.next(Some(Signal(t, sdpTypeSentByRole(role), sdp)))
      );
    t.simpleRtc
    ->SimpleRTC.setOnConnect(() => observer.next(Some(Connected(t))));
    t.simpleRtc
    ->SimpleRTC.setOnData(data => {
        let (newChunkerState, maybePayload) =
          t.chunkerState |> SimpleRTCChunker.recv(data);
        t.chunkerState = newChunkerState;
        switch (maybePayload) {
        | Some(payload) => observer.next(Some(GotData(payload)))
        | None => ()
        };
      });
    t.simpleRtc
    ->SimpleRTC.setOnError(error => observer.next(Some(Error(error))));
    t.simpleRtc->SimpleRTC.setOnClose(() => observer.next(None));

    switch (initSignal) {
    | Some(initSignal) => t.simpleRtc->SimpleRTC.signal(initSignal)
    | None => ()
    };

    (.) => t.simpleRtc->SimpleRTC.destroy;
  });

let stringOfRole =
  fun
  | SimpleRTC.Initiator => "init"
  | Acceptor => "accept";

let sub = (key, peerId, ~initSignal=?, role, rtcMsgToMsg) => {
  Subs.ofStream(
    key ++ "/" ++ stringOfRole(role) ++ "/" ++ (peerId |> PeerId.toString), () =>
    rtcSource(~initSignal, role)
    |> StreamOps.retryWhen(Retry.getTimeoutMs)
    |> Wonka.map((. s) => rtcMsgToMsg(peerId, s))
  );
};

let signalCmd = (conn, signal) =>
  Cmd.call(_callbacks => conn.simpleRtc->SimpleRTC.signal(signal));

let sendCmd = (conn, payload) =>
  Cmd.call(_callbacks => SimpleRTCChunker.send(conn.simpleRtc, payload));