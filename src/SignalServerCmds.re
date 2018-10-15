open BlackTea;

type t = {ws: WebapiExtra.Dom.WebSocket.t};

let send = (msg: Message.t, ws: WebapiExtra.Dom.WebSocket.t) =>
  WebapiExtra.Dom.(msg |> Message.toJSON |> ws->WebSocket.sendString);

let sendMsg = (msg: Message.t, t: t) =>
  Cmd.call(_callbacks =>
    msg |> Message.toJSON |> t.ws->WebapiExtra.Dom.WebSocket.sendString
  );

let connect =
    (url, thisPeer: ThisPeer.t, openedToMsg, signalServerMsgToMsg, errorToMsg) =>
  Cmd.call(callbacks => {
    open WebapiExtra.Dom;
    let t = {ws: WebSocket.create(url)};

    t.ws
    ->WebSocket.setOnOpen(_ => {
        callbacks^.enqueue(openedToMsg(t));
        /* TODO: Sign */
        t.ws
        |> send(
             Login({
               src: thisPeer.id,
               watch: PeerId.Set.empty,
               signature: "",
             }),
           );
      });
    t.ws
    ->WebSocket.setOnMessage(event =>
        switch (Message.fromJSON(event->MessageEvent.data)) {
        | Ok(msg) => callbacks^.enqueue(signalServerMsgToMsg(t, msg))
        | Error(msg) => Js.log(msg)
        }
      );
    t.ws->WebSocket.setOnClose(_ => callbacks^.enqueue(errorToMsg()));
  });