open WebapiExtra.Dom;

type role =
  | Initiator
  | Acceptor;

type t = {
  role,
  connection: RTCPeerConnection.t,
  iceComplete: bool,
  waitingOffer: option(RTCSessionDescription.t),
  waitingAnswer: option(RTCSessionDescription.t),
  dataChannel: option(RTCDataChannel.t),
  connected: bool,
};

let promiseFailed = (name, _) =>
  Js_promise.resolve @@ Js.log(name ++ " FAILED");

type options = {role};

type event =
  | Error(string)
  | Signal(string)
  | Connect
  | Data(string);

type action =
  | CreateAnswer
  | CreateAnswerFinished(RTCSessionDescription.t)
  | CreateOffer
  | CreateOfferFinished(RTCSessionDescription.t)
  | Negotiate
  | OnChannelMessage(RTCMessageEvent.t)
  | OnIceCandidate(RTCPeerConnectionIceEvent.t)
  | OnIceStateChange
  | OnRemoteDescriptionUpdated
  | OnSignalingStateChange
  | OnChannelOpen
  | OnChannelClose
  | SendAnswer(RTCSessionDescription.t)
  | SendOffer(RTCSessionDescription.t)
  | SetupDataChannel(RTCDataChannel.t)
  | Signal(string)
  | Start
  | Send(string);

type cmd =
  | F((action => unit, event => unit) => unit)
  | None;

let unsafeDescFromString: string => RTCSessionDescription.t = [%bs.raw
  msg => "{return JSON.parse(msg);}"
];

let reducer: (action, t) => (t, cmd) =
  (action, state) =>
    switch (action) {
    | Start => (
        state,
        F(
          (
            (send, _) => {
              let c = state.connection;
              RTCPeerConnection.setOnIceConnectionStateChange(c, _ =>
                send(OnIceStateChange)
              );
              /* RTCPeerConnection.setOnIceGatheringStateChange(c, _ =>
                   send(OnIceStateChange)
                 ); */
              /* RTCPeerConnection.setOnSignalingStateChange(c, _ =>
                   send(OnSignalingStateChange)
                 ); */
              RTCPeerConnection.setOnIceCandidate(c, e =>
                send(OnIceCandidate(e))
              );

              switch (state.role) {
              | Initiator =>
                let channelName = "somenamehere";
                let options =
                  RTCDataChannel.makeOptions(
                    ~ordered=true,
                    ~maxRetransmitTime=3000.0,
                  );
                /* let channelName = randombytes(20).toString('hex') */
                send(
                  SetupDataChannel(
                    RTCPeerConnection.createDataChannel(
                      c,
                      ~channelName,
                      ~options,
                    ),
                  ),
                );
              | Acceptor =>
                RTCPeerConnection.setOnDataChannel(c, e =>
                  send(SetupDataChannel(RTCDataChannelEvent.getChannel(e)))
                )
              };

              switch (state.role) {
              | Initiator => send(Negotiate)
              | Acceptor => ()
              };
            }
          ),
        ),
      )
    | OnIceCandidate(e) =>
      Js.log("OnIceCandidate");
      let candidate = RTCPeerConnectionIceEvent.getCandidate(e);
      if (Js.Null.return(candidate) != Js.null) {
        Js.log(candidate);
        (state, None);
      } else {
        (
          /* No more candidates, ICE is complete. */
          {...state, iceComplete: true},
          F(
            (
              (send, _) => {
                switch (state.waitingAnswer) {
                /* | Some(waitingAnswer) => send(SendAnswer(waitingAnswer)) */
                | Some(waitingAnswer) =>
                  send(
                    SendAnswer(
                      RTCPeerConnection.localDescription(state.connection),
                    ),
                  )
                | None => Js.log("no waiting answer")
                };
                switch (state.waitingOffer) {
                /* | Some(waitingOffer) => send(SendOffer(waitingOffer)) */
                | Some(_) =>
                  send(
                    SendOffer(
                      RTCPeerConnection.localDescription(state.connection),
                    ),
                  )
                | None => Js.log("no waiting offer")
                };
              }
            ),
          ),
        );
      };
    | SetupDataChannel(ch) => (
        {...state, dataChannel: Some(ch)},
        F(
          (
            (send, _) => {
              RTCDataChannel.setOnMessage(ch, e =>
                send(OnChannelMessage(e))
              );
              /* onbufferedamountlow */
              RTCDataChannel.setOnOpen(ch, () => send(OnChannelOpen));
              RTCDataChannel.setOnClose(ch, () => send(OnChannelClose));
              RTCDataChannel.setOnError(ch, _
                /* self.destroy(makeError(err, 'ERR_DATA_CHANNEL')) */
                => Js.log("there was an error somewhere"));
            }
          ),
        ),
      )
    | OnChannelOpen =>
      /* TODO: Do magic to workaround https://github.com/js-platform/node-webrtc/issues/339 */
      (
        {...state, connected: true},
        F(((_, sendEvent) => sendEvent(Connect))),
      )
    | OnChannelClose =>
      Js.log("ChannelClose");
      /* TODO */
      (state, None);
    | OnChannelMessage(e) =>
      /* TODO */
      let msgContent = RTCMessageEvent.getData(e);
      Js.log("there was a message somewhere " ++ msgContent);
      (state, F(((_, sendEvent) => sendEvent(Data(msgContent)))));
    | Negotiate => (state, F(((send, _) => send(CreateOffer))))
    | CreateOffer =>
      let offerConstraints =
        RTCOffer.makeOptions(
          ~offerToReceiveAudio=false,
          ~offerToReceiveVideo=false,
        );
      (
        state,
        F(
          (
            (send, _) =>
              /* if !state.destroyed... */
              ignore(
                RTCPeerConnection.createOffer(
                  state.connection,
                  ~options=offerConstraints,
                )
                |> Js_promise.then_(offer => {
                     Js.log("createOffer success!");

                     RTCPeerConnection.setLocalDescription(
                       state.connection,
                       offer,
                     )
                     |> Js_promise.then_(() => {
                          Js.log("setLocalDescription");
                          send(CreateOfferFinished(offer));
                          Js_promise.resolve();
                        });
                   })
                |> Js_promise.catch(
                     promiseFailed("createOffer or setLocalDescription"),
                   ),
              )
          ),
        ),
      );
    | CreateOfferFinished(offer) =>
      if (state.iceComplete) {
        (state, F(((send, _) => send(SendOffer(offer)))));
      } else {
        ({...state, waitingOffer: Some(offer)}, None);
      }
    | SendOffer(offer) =>
      Js.log("send Offer");
      /* Only type and sdp */
      switch (Js.Json.stringifyAny(offer)) {
      | Some(descAsStr) => (
          state,
          F(((_, sendEvent) => sendEvent(Signal(descAsStr)))),
        )
      | None => (
          state,
          F(
            ((_, sendEvent) => sendEvent(Error("Cannot stringify OFFER"))),
          ),
        )
      };
    | Signal(str) =>
      let desc = unsafeDescFromString(str);

      /* Only if contains .sdp */
      (
        state,
        F(
          (
            (send, _) =>
              ignore(
                RTCPeerConnection.setRemoteDescription(state.connection, desc)
                |> Js_promise.then_(() => {
                     send(OnRemoteDescriptionUpdated);
                     Js_promise.resolve();
                   })
                |> Js_promise.catch(promiseFailed("addIceCandidate")),
              )
          ),
        ),
      );
    | OnRemoteDescriptionUpdated =>
      /* state.pendingCandidates
         |> Array.iter(candidate =>
             RTCPeerConnection.addIceCandidate(
               state.connection,
               candidate,
             )
           ) */
      switch (
        RTCPeerConnection.remoteDescription(state.connection)
        ->RTCSessionDescription.getType
      ) {
      | Offer =>
        /* extra check */
        (state, F(((send, _) => send(CreateAnswer))))
      | _ => (state, None)
      }

    | CreateAnswer => (
        state,
        F(
          (
            (send, _) =>
              ignore(
                RTCPeerConnection.createAnswer(state.connection)
                |> Js_promise.then_(answer => {
                     Js.log("createAnswer went well");
                     RTCPeerConnection.setLocalDescription(
                       state.connection,
                       answer,
                     )
                     |> Js_promise.then_(() => {
                          Js.log("setLocalDescription");
                          send(CreateAnswerFinished(answer));
                          Js_promise.resolve();
                        });
                   })
                |> Js_promise.catch(
                     promiseFailed("setRemoteDescription or createAnswer"),
                   ),
              )
          ),
        ),
      )
    | CreateAnswerFinished(answer) =>
      if (state.iceComplete) {
        (state, F(((send, _) => send(SendAnswer(answer)))));
      } else {
        ({...state, waitingAnswer: Some(answer)}, None);
      }
    | SendAnswer(answer) =>
      /* Only type and sdp */
      switch (Js.Json.stringifyAny(answer)) {
      | Some(descAsStr) => (
          state,
          F(((_, sendEvent) => sendEvent(Signal(descAsStr)))),
        )
      | None =>
        Js.log("Cannot stringify OFFER");
        (state, None);
      }
    | OnIceStateChange =>
      Js.log("OnIceStateChange");
      switch (RTCPeerConnection.iceConnectionState(state.connection)) {
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
      (state, None);
    | OnSignalingStateChange =>
      Js.log("OnSignalingStateChange");
      /* TODO */
      (state, None);
    | Send(msg) =>
      switch (state.dataChannel) {
      | Some(dataChannel) =>
        RTCDataChannel.send(dataChannel, msg);
        (state, None);
      | None => (
          state,
          F(
            (
              (_, sendEvent) =>
                sendEvent(
                  Error("Cannot send data: Data channel doesn't exist yet."),
                )
            ),
          ),
        )
      }
    };

let create = options => {
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
};