open Rex_json.Json.Infix;
open BlackTea;

/* TYPES */

type conn = {ws: WebapiExtra.Dom.WebSocket.t};

type peerChange = Message.peerChange;

/* HELPERS */

let send = (msg: Message.t, ws: WebapiExtra.Dom.WebSocket.t) =>
  WebapiExtra.Dom.(
    msg |> Message.encode |> Json.stringify |> ws->WebSocket.sendString
  );

/* COMMANDS */

exception SignatureError;

let signMsg = (msg: Message.signedMsg, privateKey) =>
  Json.Object(msg |> Message.encodeSignedMsg)
  |> Json.stringify
  |> SimpleCrypto.sign(privateKey)
  |> Js.Promise.then_(signature =>
       Message.Signed(signature, msg) |> Js.Promise.resolve
     );

let signAndSendMsg = (msg: Message.signedMsg, privateKey, t: conn) =>
  /* TODO: Sort fields */
  /* TODO: Can Websocket.send throw or does it only fire onError event? */
  signMsg(msg, privateKey)
  |> Js.Promise.then_(signedMsg =>
       send(signedMsg, t.ws) |> Js.Promise.resolve
     )
  |> Js.Promise.catch(e => {
       Js.log2("[SignalServerCmds] Cannot sign and send message:", e);
       Js.Promise.reject(SignatureError);
     });

let connect =
    (
      url,
      thisPeer: ThisPeer.t,
      watchedPeers,
      openedToMsg,
      signalChannelMsgToMsg,
      errorMsg,
    ) =>
  Cmd.call(callbacks => {
    open WebapiExtra.Dom;
    let t = {ws: WebSocket.create(url)};

    t.ws
    ->WebSocket.setOnOpen(_ => {
        callbacks^.enqueue(openedToMsg(t));
        t
        |> signAndSendMsg(
             PeerToServer(thisPeer.id, Login(watchedPeers)),
             thisPeer.privateKey,
           )
        |> Js.Promise.catch(exn =>
             callbacks^.enqueue(errorMsg) |> Js.Promise.resolve
           )
        |> ignore;
      });
    t.ws
    ->WebSocket.setOnMessage(event =>
        switch (
          event->MessageEvent.data |> JsonUtils.parseOpt |?>> Message.decode
        ) {
        | Some(Ok(msg)) => callbacks^.enqueue(signalChannelMsgToMsg(msg))
        | Some(Error(msg)) => Js.log(msg)
        | None => Js.log("Received message that is not valid JSON. Skipping.")
        }
      );
    /* t.ws
       ->WebSocket.setOnError(_ => {
           Js.log("WS Error");
           callbacks^.enqueue(errorMsg);
         }); */
    t.ws->WebSocket.setOnClose(_ => callbacks^.enqueue(errorMsg));
  });

let close = (t: conn) =>
  Cmd.call(_callbacks => t.ws->WebapiExtra.Dom.WebSocket.close);