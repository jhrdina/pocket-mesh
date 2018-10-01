open WebapiExtra.Dom;

/* TODO: Move somewhere else */
let unsafeDescFromString: string => RTCSessionDescription.t = [%bs.raw
  msg => "{return JSON.parse(msg);}"
];

type role =
  | Initiator
  | Acceptor;

type t = {
  role,
  connection: RTCPeerConnection.t,
  mutable iceComplete: bool,
  mutable waitingOffer: option(RTCSessionDescription.t),
  mutable waitingAnswer: option(RTCSessionDescription.t),
  mutable dataChannel: option(RTCDataChannel.t),
  mutable connected: bool,
  mutable onSignal: option(string => unit),
  mutable onData: option(string => unit),
  mutable onConnect: option(unit => unit),
  mutable onError: option(string => unit),
};

let promiseFailed = (name, _) =>
  Js_promise.resolve @@ Js.log(name ++ " FAILED");

type options = {role};

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

let sendAnswer = (s, answer) =>
  /* Only type and sdp */
  switch (Js.Json.stringifyAny(answer)) {
  | Some(descAsStr) => s->emitSignal(descAsStr)
  | None => s->emitError("Cannot stringify ANSWER")
  };

let sendOffer = (s, offer) => {
  Js.log("send Offer");
  /* Only type and sdp */
  switch (Js.Json.stringifyAny(offer)) {
  | Some(descAsStr) => s->emitSignal(descAsStr)
  | None => s->emitError("Cannot stringify OFFER")
  };
};

let onIceCandidate = (s, e) => {
  Js.log("OnIceCandidate");
  let candidate = RTCPeerConnectionIceEvent.getCandidate(e);
  if (Js.Null.return(candidate) != Js.null) {
    Js.log(candidate);
    ();
  } else {
    /* No more candidates, ICE is complete. */
    s.iceComplete = true;

    switch (s.waitingAnswer) {
    /* | Some(waitingAnswer) => send(SendAnswer(waitingAnswer)) */
    | Some(_waitingAnswer) =>
      s->sendAnswer(RTCPeerConnection.localDescription(s.connection))
    | None => Js.log("no waiting answer")
    };
    switch (s.waitingOffer) {
    /* | Some(waitingOffer) => send(SendOffer(waitingOffer)) */
    | Some(_) =>
      s->sendOffer(RTCPeerConnection.localDescription(s.connection))
    | None => Js.log("no waiting offer")
    };
  };
};

let onChannelMessage = (s, e) => s->emitData(RTCMessageEvent.getData(e));

let onChannelOpen = s => {
  /* TODO: Do magic to workaround https://github.com/js-platform/node-webrtc/issues/339 */
  s.connected = true;
  s->emitConnect;
};

let onChannelClose = _s =>
  /* TODO */
  Js.log("ChannelClose");

let setupDataChannel = (s, ch) => {
  s.dataChannel = Some(ch);
  RTCDataChannel.setOnMessage(ch, e => s->onChannelMessage(e));
  /* onbufferedamountlow */
  RTCDataChannel.setOnOpen(ch, () => s->onChannelOpen);
  RTCDataChannel.setOnClose(ch, () => s->onChannelClose);
  RTCDataChannel.setOnError(ch, _
    /* self.destroy(makeError(err, 'ERR_DATA_CHANNEL')) */
    => Js.log("there was an error somewhere"));
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
         Js.log("createOffer success!");

         RTCPeerConnection.setLocalDescription(s.connection, offer)
         |> Js.Promise.then_(() => {
              Js.log("setLocalDescription");
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
         Js.log("createAnswer went well");
         RTCPeerConnection.setLocalDescription(s.connection, answer)
         |> Js.Promise.then_(() => {
              Js.log("setLocalDescription");
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

let onIceStateChange = s => {
  Js.log("OnIceStateChange");
  switch (RTCPeerConnection.iceConnectionState(s.connection)) {
  | Connected
  | Completed => Js.log("Ice connection Completed")
  | Failed => Js.log("Ice connection Failed")
  | Closed => Js.log("Ice connection Closed")

  | New => Js.log("Ice connection New")
  | Checking => Js.log("Ice connection Checking")
  | Disconnected => Js.log("Ice connection Disconnected")
  | Unknown => Js.log("Ice connection Unknown state")
  /* | _ => Js.log("Not completed") */
  };
};
let _onSignalingStateChange = () =>
  Js.log(
    "OnSignalingStateChange",
    /* TODO */
  );
let send = (s, msg) =>
  switch (s.dataChannel) {
  | Some(dataChannel) => RTCDataChannel.send(dataChannel, msg)
  | None => s->emitError("Cannot send data: Data channel doesn't exist yet.")
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
    onSignal: None,
    onData: None,
    onConnect: None,
    onError: None,
  };

  let c = s.connection;
  RTCPeerConnection.setOnIceConnectionStateChange(c, _ => s->onIceStateChange);
  /* RTCPeerConnection.setOnIceGatheringStateChange(c, _ => s->onIceStateChange);
     RTCPeerConnection.setOnSignalingStateChange(c, _ =>
       s->onSignalingStateChange
     ); */
  RTCPeerConnection.setOnIceCandidate(c, e => s->onIceCandidate(e));

  switch (s.role) {
  | Initiator =>
    let channelName = "somenamehere";
    let options =
      RTCDataChannel.makeOptions(~ordered=true, ~maxRetransmitTime=3000.0);
    /* let channelName = randombytes(20).toString('hex') */
    s
    ->setupDataChannel(
        RTCPeerConnection.createDataChannel(c, ~channelName, ~options),
      );
  | Acceptor =>
    RTCPeerConnection.setOnDataChannel(c, e =>
      s->setupDataChannel(RTCDataChannelEvent.getChannel(e))
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