open WebapiExtra.Dom;

/* TODO: Move somewhere else */
let unsafeDescFromString: string => RTCSessionDescription.t = [%bs.raw
  msg => "{return JSON.parse(msg);}"
];

type role =
  | Initiator
  | Acceptor;

type data =
  RTCMessageEvent.data =
    | String(string) | ArrayBuffer(Js.Typed_array.array_buffer);

type t = {
  role,
  connection: RTCPeerConnection.t,
  mutable iceComplete: bool,
  mutable waitingOffer: option(RTCSessionDescription.t),
  mutable waitingAnswer: option(RTCSessionDescription.t),
  mutable dataChannel: option(RTCDataChannel.t),
  mutable connected: bool,
  mutable destroyed: bool,
  mutable onSignal: option(string => unit),
  mutable onData: option(data => unit),
  mutable onConnect: option(unit => unit),
  mutable onError: option(string => unit),
  mutable onClose: option(unit => unit),
};

type options = {role};

let logDebug = s => Js.log2("[SimpleRTC]", s);

let promiseFailed = (name, _) =>
  Js_promise.resolve @@ logDebug(name ++ " FAILED");

let emitSignal = (s, str) =>
  switch (s.onSignal) {
  | Some(cb) => cb(str)
  | None => ()
  };

let emitError = (s, str) =>
  switch (s.onError) {
  | Some(cb) => cb(str)
  | None => ()
  };

let emitData = (s, str) =>
  switch (s.onData) {
  | Some(cb) => cb(str)
  | None => ()
  };

let emitConnect = s =>
  switch (s.onConnect) {
  | Some(cb) => cb()
  | None => ()
  };

let emitClose = s =>
  switch (s.onClose) {
  | Some(cb) => cb()
  | None => ()
  };

let sendAnswer = (s, answer) =>
  /* Only type and sdp */
  switch (Js.Json.stringifyAny(answer)) {
  | Some(descAsStr) => s->emitSignal(descAsStr)
  | None => s->emitError("Cannot stringify ANSWER")
  };

let sendOffer = (s, offer) => {
  logDebug("send Offer");
  /* Only type and sdp */
  switch (Js.Json.stringifyAny(offer)) {
  | Some(descAsStr) => s->emitSignal(descAsStr)
  | None => s->emitError("Cannot stringify OFFER")
  };
};

let onIceCandidate = (s, e) => {
  logDebug("OnIceCandidate");
  let candidate = RTCPeerConnectionIceEvent.getCandidate(e);
  if (Js.Null.return(candidate) != Js.null) {
    ();
      /* logDebug(candidate); */
  } else {
    /* No more candidates, ICE is complete. */
    s.iceComplete = true;

    switch (s.waitingAnswer) {
    /* | Some(waitingAnswer) => send(SendAnswer(waitingAnswer)) */
    | Some(_waitingAnswer) =>
      s->sendAnswer(RTCPeerConnection.localDescription(s.connection))
    | None => logDebug("no waiting answer")
    };
    switch (s.waitingOffer) {
    /* | Some(waitingOffer) => send(SendOffer(waitingOffer)) */
    | Some(_) =>
      s->sendOffer(RTCPeerConnection.localDescription(s.connection))
    | None => logDebug("no waiting offer")
    };
  };
};

let onChannelMessage = (s, e) =>
  switch (RTCMessageEvent.getData(e)) {
  | Some(data) => s->emitData(data)
  | None => logDebug("Ignoring unknown incomming message type.")
  };

let onChannelOpen = s => {
  /* TODO: Do magic to workaround https://github.com/js-platform/node-webrtc/issues/339 */
  s.connected = true;
  s->emitConnect;
};

let onChannelClose = _s =>
  /* TODO */
  logDebug("ChannelClose");

let setupDataChannel = (s, ch) => {
  s.dataChannel = Some(ch);
  RTCDataChannel.setOnMessage(ch, Some(e => s->onChannelMessage(e)));
  /* onbufferedamountlow */
  RTCDataChannel.setOnOpen(ch, Some(() => s->onChannelOpen));
  RTCDataChannel.setOnClose(ch, Some(() => s->onChannelClose));
  RTCDataChannel.setOnError(
    ch,
    Some(
      _ =>
        /* self.destroyAndNotify(makeError(err, 'ERR_DATA_CHANNEL')) */
        logDebug("there was an error somewhere"),
    ),
  );
};

let createOfferFinished = (s, offer) =>
  if (s.iceComplete) {
    s->sendOffer(offer);
  } else {
    s.waitingOffer = Some(offer);
  };

let createOffer = s => {
  let offerConstraints =
    RTCOffer.makeOptions(
      ~offerToReceiveAudio=false,
      ~offerToReceiveVideo=false,
    );

  /* if !s.destroyed... */
  ignore(
    RTCPeerConnection.createOffer(s.connection, ~options=offerConstraints)
    |> Js.Promise.then_(offer => {
         logDebug("createOffer success!");

         RTCPeerConnection.setLocalDescription(s.connection, offer)
         |> Js.Promise.then_(() => {
              logDebug("setLocalDescription");
              s->createOfferFinished(offer);
              Js.Promise.resolve();
            });
       })
    |> Js.Promise.catch(promiseFailed("createOffer or setLocalDescription")),
  );
};

let negotiate = s => s->createOffer;

let createAnswerFinished = (s, answer) =>
  if (s.iceComplete) {
    s->sendAnswer(answer);
  } else {
    s.waitingAnswer = Some(answer);
  };

let createAnswer = s =>
  ignore(
    RTCPeerConnection.createAnswer(s.connection)
    |> Js.Promise.then_(answer => {
         logDebug("createAnswer went well");
         RTCPeerConnection.setLocalDescription(s.connection, answer)
         |> Js.Promise.then_(() => {
              logDebug("setLocalDescription");
              s->createAnswerFinished(answer);
              Js.Promise.resolve();
            });
       })
    |> Js.Promise.catch(
         promiseFailed("setRemoteDescription or createAnswer"),
       ),
  );
let onRemoteDescriptionUpdated = s =>
  switch (
    RTCPeerConnection.remoteDescription(s.connection)
    ->RTCSessionDescription.getType
  ) {
  | Offer =>
    /* extra check */
    s->createAnswer
  | _ => ()
  };

let signal = (s, str) => {
  /* TODO: Write a better parser */
  let desc = unsafeDescFromString(str);

  /* Only if contains .sdp */
  ignore(
    RTCPeerConnection.setRemoteDescription(s.connection, desc)
    |> Js.Promise.then_(() => {
         s->onRemoteDescriptionUpdated;
         Js.Promise.resolve();
       })
    |> Js.Promise.catch(promiseFailed("addIceCandidate")),
  );
};

let destroy = s =>
  if (!s.destroyed) {
    switch (s.dataChannel) {
    | Some(ch) =>
      RTCDataChannel.setOnMessage(ch, None);
      RTCDataChannel.setOnOpen(ch, None);
      RTCDataChannel.setOnClose(ch, None);
      RTCDataChannel.setOnError(ch, None);
      try (RTCDataChannel.close(ch)) {
      | Js.Exn.Error(_) => ()
      };
      s.dataChannel = None;
    | None => ()
    };

    s.connection->RTCPeerConnection.setOnIceConnectionStateChange(None);
    s.connection->RTCPeerConnection.setOnIceCandidate(None);
    s.connection->RTCPeerConnection.setOnDataChannel(None);
    try (s.connection->RTCPeerConnection.close) {
    | Js.Exn.Error(_) => ()
    };

    s.destroyed = true;
  };

let destroyAndNotify = s => {
  s->destroy;
  s->emitClose;
};

let onIceStateChange = s =>
  switch (RTCPeerConnection.iceConnectionState(s.connection)) {
  | Connected
  | Completed => logDebug("Ice connection Completed")
  | Failed =>
    logDebug("Ice connection Failed");
    s->destroyAndNotify;
  | Closed
  | Disconnected => s->destroyAndNotify
  | New => logDebug("Ice connection New")
  | Checking => logDebug("Ice connection Checking")
  | Unknown => logDebug("Ice connection Unknown state")
  /* | _ => logDebug("Not completed") */
  };
let _onSignalingStateChange = () =>
  logDebug(
    "OnSignalingStateChange",
    /* TODO */
  );

let send = (s, msg) =>
  switch (s.dataChannel, msg) {
  | (Some(dataChannel), String(msgStr)) =>
    dataChannel->RTCDataChannel.sendString(msgStr)
  | (Some(dataChannel), ArrayBuffer(msgBuf)) =>
    dataChannel->RTCDataChannel.sendArrayBuffer(msgBuf)
  | (None, _) =>
    s->emitError("Cannot send data: Data channel doesn't exist yet.")
  };

let create = options => {
  let s = {
    role: options.role,
    connection:
      RTCPeerConnection.createWithConfig({
        "iceServers": [|{urls: "stun:stun.l.google.com:19302"}|],
      }),
    connected: false,
    iceComplete: false,
    dataChannel: None,
    waitingOffer: None,
    waitingAnswer: None,
    /* TODO: Model using variants */
    destroyed: false,
    onSignal: None,
    onData: None,
    onConnect: None,
    onError: None,
    onClose: None,
  };

  let c = s.connection;
  RTCPeerConnection.setOnIceConnectionStateChange(
    c,
    Some(_ => s->onIceStateChange),
  );
  /* RTCPeerConnection.setOnIceGatheringStateChange(c, _ => s->onIceStateChange);
     RTCPeerConnection.setOnSignalingStateChange(c, _ =>
       s->onSignalingStateChange
     ); */
  RTCPeerConnection.setOnIceCandidate(c, Some(e => s->onIceCandidate(e)));

  switch (s.role) {
  | Initiator =>
    let channelName = "somenamehere";
    let options =
      RTCDataChannel.makeOptions(~ordered=true, ~maxPacketLifeTime=3000);
    /* let channelName = randombytes(20).toString('hex') */
    s->setupDataChannel(
      RTCPeerConnection.createDataChannel(c, ~channelName, ~options),
    );
  | Acceptor =>
    RTCPeerConnection.setOnDataChannel(
      c,
      Some(e => s->setupDataChannel(RTCDataChannelEvent.getChannel(e))),
    )
  };

  switch (s.role) {
  | Initiator => s->negotiate
  | Acceptor => ()
  };

  s;
};

let setOnSignal = (s, callback) => s.onSignal = Some(callback);
let setOnData = (s, callback) => s.onData = Some(callback);
let setOnConnect = (s, callback) => s.onConnect = Some(callback);
let setOnError = (s, callback) => s.onError = Some(callback);
let setOnClose = (s, callback) => s.onClose = Some(callback);