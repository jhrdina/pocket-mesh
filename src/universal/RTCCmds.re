open BlackTea;

type t = SimpleRTC.t;

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
    let t = SimpleRTC.create({role: role});
    t->SimpleRTC.setOnSignal(sdp =>
      callbacks^.enqueue(sdpToMsg(t, sdp, tag))
    );
    t->SimpleRTC.setOnConnect(() =>
      callbacks^.enqueue(connectedToMsg(t, tag))
    );
    t->SimpleRTC.setOnData(data =>
      callbacks^.enqueue @@ dataToMsg(t, tag, data)
    );
    t->SimpleRTC.setOnError(error =>
      error |> errorToMsg(t, tag) |> callbacks^.enqueue
    );
    t->SimpleRTC.setOnClose(() => closeToMsg(t, tag) |> callbacks^.enqueue);

    switch (maybeInitSignal) {
    | Some(initSignal) => t->SimpleRTC.signal(initSignal)
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
  Cmd.call(_callbacks => t->SimpleRTC.signal(sdpAnswer));

let destroy = t => Cmd.call(_callbacks => t->SimpleRTC.destroy);

let send = (t, str) => Cmd.call(_callbacks => t->SimpleRTC.send(str));