open BlackTea;
open Rex_json.Json.Infix;

/**
  Signalling channel connection state representation, its changes and related
  global messages handling.
 */

/* TYPES */
type connectionState =
  | Connecting
  | Connected(WebSocketsSub.conn);

type t = {
  url: string,
  connectionState,
};

type Msgs.t +=
  | UpdateUrl(string)
  | WebSocketsMsg(option(WebSocketsSub.msg))
  /* Output: */
  | GotMessage(Message.t)
  /* Input: */
  | Send(Message.t);

let updateUrl = url => UpdateUrl(url);
let connected = ssConn => Connected(ssConn);
let gotMessage = msg => GotMessage(msg);
let webSocketsMsg = msg => WebSocketsMsg(msg);

/* INIT, UPDATE */
let init = url => {
  Js.log("reseting");
  {url, connectionState: Connecting};
};

let update = (model, msg) => {
  switch (msg, model.connectionState) {
  | (WebSocketsMsg(Some(Connected(conn))), _) => (
      {...model, connectionState: Connected(conn)},
      Cmd.none,
    )

  | (WebSocketsMsg(Some(Received(strMsg))), Connected(_)) =>
    switch (strMsg |> JsonUtils.parseOpt |?>> Message.decode) {
    | Some(Ok(msg)) => (model, Cmd.msg(GotMessage(msg)))
    | Some(Error(msg)) => (
        model,
        Cmds.log("Cannot parse received message: " ++ msg),
      )
    | None => (
        model,
        Cmds.log("Received message is not a valid JSON. Skipping."),
      )
    }

  | (WebSocketsMsg(None), _) => (
      {...model, connectionState: Connecting},
      Cmd.none,
    )

  | (UpdateUrl(url), _) => ({url, connectionState: Connecting}, Cmd.none)

  | (Send(msg), Connected(conn)) =>
    let msgAsStr = msg |> Message.encode |> Json.stringify;
    (model, WebSocketsSub.sendCmd(msgAsStr, conn));

  | (_, _) => (model, Cmd.none)
  };
};

let subscriptions = model =>
  WebSocketsSub.sub(
    "SignalChannel/connection/" ++ model.url,
    model.url,
    Retry.getTimeoutMs,
    webSocketsMsg,
  );