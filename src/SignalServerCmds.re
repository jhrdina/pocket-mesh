open Rex_json.Json.Infix;
open BlackTea;

/* TYPES */

type conn = {ws: WebapiExtra.Dom.WebSocket.t};

type peerChange = Message.peerChange;

let send = (msg: Message.t, ws: WebapiExtra.Dom.WebSocket.t) =>
  WebapiExtra.Dom.(
    msg |> Message.encode |> Json.stringify |> ws->WebSocket.sendString
  );

/* COMMANDS */

let sendMsg = (msg: Message.t, t: conn) =>
  Cmd.call(_callbacks =>
    msg
    |> Message.encode
    |> Json.stringify
    |> t.ws->WebapiExtra.Dom.WebSocket.sendString
  );

exception SignatureError;

let signAndSendMsg = (msg: Message.signedMsg, privateKey, t: conn) =>
  /* TODO: Sort fields */
  /* TODO: Can Websocket.send throw or does it only fire onError event? */
  Json.Object(msg |> Message.encodeSignedMsg)
  |> Json.stringify
  |> SimpleCrypto.sign(privateKey)
  |> Js.Promise.then_(signature =>
       Message.Signed(signature, msg)
       |> Message.encode
       |> Json.stringify
       |> t.ws->WebapiExtra.Dom.WebSocket.sendString
       |> Js.Promise.resolve
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
      signalServerMsgToMsg,
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
        | Some(Ok(msg)) => callbacks^.enqueue(signalServerMsgToMsg(msg))
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