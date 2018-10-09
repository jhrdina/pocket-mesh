open BlackTea;

type t = SimpleRTC.t;

let _create =
    (role, tag, maybeInitSignal, sdpToMsg, connectedToMsg, dataToMsg) =>
  Cmd.call(callbacks => {
    let t = SimpleRTC.create({role: role});
    t->SimpleRTC.setOnSignal(sdp =>
      callbacks^.enqueue(sdpToMsg(t, sdp, tag))
    );
    t->SimpleRTC.setOnConnect(() =>
      callbacks^.enqueue(connectedToMsg(t, tag))
    );
    t->SimpleRTC.setOnData(data => callbacks^.enqueue @@ dataToMsg(t, data));
    switch (maybeInitSignal) {
    | Some(initSignal) => t->SimpleRTC.signal(initSignal)
    | None => ()
    };
  });

let createInitiator = (tag, offerToMsg, connectedToMsg, dataToMsg) =>
  _create(Initiator, tag, None, offerToMsg, connectedToMsg, dataToMsg);
let createAcceptor = (tag, sdpOffer, answerToMsg, connectedToMsg, dataToMsg) =>
  _create(
    Acceptor,
    tag,
    Some(sdpOffer),
    answerToMsg,
    connectedToMsg,
    dataToMsg,
  );

let signal = (t, sdpAnswer) =>
  Cmd.call(_callbacks => t->SimpleRTC.signal(sdpAnswer));

let destroy = t =>
  Cmd.call(_callbacks
    /* TODO: Implement */
    => Js.log("Destroying RTC connection: Not implemented!!"));

let send = (t, str) => Cmd.call(_callbacks => t->SimpleRTC.send(str));