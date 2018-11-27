open BlackTea;

/**
  Wraps SimpleRTC into Cmds and adds chunking support
*/

/* TYPES */

type t = {
  simpleRtc: SimpleRTC.t,
  mutable chunkerState: SimpleRTCChunker.t,
};

/* FUNCTIONS */

let _create =
    (
      role,
      tag,
      maybeInitSignal,
      sdpToMsg,
      connectedToMsg,
      dataToMsg,
      errorToMsg,
      closeToMsg,
    ) =>
  Cmd.call(callbacks => {
    let t = {
      simpleRtc: SimpleRTC.create({role: role}),
      chunkerState: SimpleRTCChunker.make(),
    };
    t.simpleRtc
    ->SimpleRTC.setOnSignal(sdp =>
        callbacks^.enqueue(sdpToMsg(t, sdp, tag))
      );
    t.simpleRtc
    ->SimpleRTC.setOnConnect(() =>
        callbacks^.enqueue(connectedToMsg(t, tag))
      );
    t.simpleRtc
    ->SimpleRTC.setOnData(data => {
        let (newChunkerState, maybePayload) =
          t.chunkerState |> SimpleRTCChunker.recv(data);
        t.chunkerState = newChunkerState;
        switch (maybePayload) {
        | Some(payload) => callbacks^.enqueue @@ dataToMsg(t, tag, payload)
        | None => ()
        };
      });
    t.simpleRtc
    ->SimpleRTC.setOnError(error =>
        error |> errorToMsg(t, tag) |> callbacks^.enqueue
      );
    t.simpleRtc
    ->SimpleRTC.setOnClose(() => closeToMsg(t, tag) |> callbacks^.enqueue);

    switch (maybeInitSignal) {
    | Some(initSignal) => t.simpleRtc->SimpleRTC.signal(initSignal)
    | None => ()
    };
  });

let createInitiator =
    (tag, offerToMsg, connectedToMsg, dataToMsg, errorToMsg, closeToMsg) =>
  _create(
    Initiator,
    tag,
    None,
    offerToMsg,
    connectedToMsg,
    dataToMsg,
    errorToMsg,
    closeToMsg,
  );
let createAcceptor =
    (
      tag,
      sdpOffer,
      answerToMsg,
      connectedToMsg,
      dataToMsg,
      errorToMsg,
      closeToMsg,
    ) =>
  _create(
    Acceptor,
    tag,
    Some(sdpOffer),
    answerToMsg,
    connectedToMsg,
    dataToMsg,
    errorToMsg,
    closeToMsg,
  );

let signal = (t, sdpAnswer) =>
  Cmd.call(_callbacks => t.simpleRtc->SimpleRTC.signal(sdpAnswer));

let destroy = t => Cmd.call(_callbacks => t.simpleRtc->SimpleRTC.destroy);

let send = (t, payload) =>
  Cmd.call(_callbacks => SimpleRTCChunker.send(t.simpleRtc, payload));