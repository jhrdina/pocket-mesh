open BlackTea;

/* TYPES */

type conn = WebapiExtra.Dom.WebSocket.t;
type msg =
  | Connected(conn)
  | Received(string);

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
  /*
   : Can Websocket.send throw or does it only fire onError event? */
  signMsg(msg, privateKey)
  |> Js.Promise.then_(signedMsg => send(signedMsg, t) |> Js.Promise.resolve)
  |> Js.Promise.catch(e => {
       Js.log2("[WebSocketsSub] Cannot sign and send message:", e);
       Js.Promise.reject(SignatureError);
     });

let webSocketSource = url =>
  Wonka.make((. observer: Wonka_types.observerT(option(msg))) =>
    WebapiExtra.Dom.(
      switch (WebSocket.create(url)) {
      | t =>
        let completed = ref(false);

        t->WebSocket.setOnOpen(_ => observer.next(Some(Connected(t))));

        t->WebSocket.setOnMessage(event =>
          observer.next(Some(Received(event->MessageEvent.data)))
        );

        let onDisconnect = () =>
          if (! completed^) {
            completed := true;
            observer.next(None);
            observer.complete();
          };

        let onOffline = _ => {
          // After calling close(), there is a 60s timeout after which onClose is
          // fired and resources are released (onDisconnect are called twice)
          t->WebSocket.close;
          onDisconnect();
        };

        t->WebSocket.setOnClose(_ => onDisconnect());

        // TODO: Make this optional
        Webapi.Dom.window |> addOfflineEventListener(onOffline);

        (
          (.) => {
            t->WebSocket.close;
            Webapi.Dom.window |> removeOfflineEventListener(onOffline);
          }
        );
      | exception (Js.Exn.Error(_)) =>
        // Probably invalid URL Address...
        observer.next(None);
        observer.complete();
        ((.) => ());
      }
    )
  );

let send = (str, conn) => str |> conn->WebapiExtra.Dom.WebSocket.sendString;

let sendCmd = (str, conn) => Cmd.call(_callbacks => send(str, conn));

let fromWebsocketWithRetry = (url, f) =>
  webSocketSource(url) |> StreamOps.retryWhen(f);

let sub = (key, url, delayStrategy, wsStateToMsg) =>
  Subs.ofStream(key, () =>
    fromWebsocketWithRetry(url, delayStrategy)
    |> Wonka.map((. s) => s |> wsStateToMsg)
  );