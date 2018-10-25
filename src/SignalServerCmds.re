open BlackTea;

/* CONSTANTS */

let defaultSignalServerUrl = "ws://localhost:7777";

/* TYPES */

type conn = {ws: WebapiExtra.Dom.WebSocket.t};

type peerChange = Message.peerChange;

let send = (msg: Message.t, ws: WebapiExtra.Dom.WebSocket.t) =>
  WebapiExtra.Dom.(msg |> Message.toJSON |> ws->WebSocket.sendString);

/* COMMANDS */

let sendMsg = (msg: Message.t, t: conn) =>
  Cmd.call(_callbacks =>
    msg |> Message.toJSON |> t.ws->WebapiExtra.Dom.WebSocket.sendString
  );

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
        /* TODO: Sign */
        t.ws
        |> send(
             Login({src: thisPeer.id, watch: watchedPeers, signature: ""}),
           );
      });
    t.ws
    ->WebSocket.setOnMessage(event =>
        switch (Message.fromJSON(event->MessageEvent.data)) {
        | Ok(msg) => callbacks^.enqueue(signalServerMsgToMsg(msg))
        | Error(msg) => Js.log(msg)
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