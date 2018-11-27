open Rex_json;
open Rex_json.Json.Infix;

let latestVersion = 1;

type sdpMessage = {
  src: PeerId.t,
  tg: PeerId.t,
  sdp: string,
  signature: string,
};

type loginOrChangeWatchedPeers = {
  src: PeerId.t,
  watch: PeerId.Set.t,
  signature: string,
};

type logoff = {
  src: PeerId.t,
  signature: string,
};

type peerChange =
  | WentOnline(PeerId.t)
  | WentOffline(PeerId.t);

type error =
  | TargetNotOnline
  | SourceNotOnline
  | InvalidMessage(string);

type t =
  | Login(loginOrChangeWatchedPeers)
  | Offer(sdpMessage)
  | Answer(sdpMessage)
  | Error(error)
  | Logoff(logoff)
  | Ok(PeerId.Set.t)
  | WatchedPeersChanged(list(peerChange))
  | ChangeWatchedPeers(loginOrChangeWatchedPeers)
  | Unknown;

type clientToServer = t;
type serverToClient = t;

let toJSON =
  fun
  | Offer(msg) as v
  | Answer(msg) as v => {
      let typeString =
        switch (v) {
        | Offer(_) => "offer"
        | _ => "answer"
        };
      Json.(
        stringify(
          Object([
            ("type", String(typeString)),
            ("src", String(msg.src |> PeerId.toString)),
            ("tg", String(msg.tg |> PeerId.toString)),
            ("sdp", String(msg.sdp)),
            ("signature", String(msg.signature)),
          ]),
        )
      );
    }
  | Error(error) =>
    Json.(
      stringify(
        Object([
          ("type", String("error")),
          ...switch (error) {
             | TargetNotOnline => [("code", String("TargetNotOnline"))]
             | SourceNotOnline => [("code", String("SourceNotOnline"))]
             | InvalidMessage(explanation) => [
                 ("code", String("InvalidMessage")),
                 ("explanation", String(explanation)),
               ]
             },
        ]),
      )
    )
  | Ok(onlinePeers) =>
    Json.(
      stringify(
        Object([
          ("type", String("ok")),
          (
            "onlinePeers",
            Array(
              onlinePeers
              |> PeerId.Set.elements
              |> List.rev_map(id => String(id |> PeerId.toString)),
            ),
          ),
        ]),
      )
    )
  | Login(msg) =>
    Json.(
      stringify(
        Object([
          ("type", String("login")),
          ("src", String(msg.src |> PeerId.toString)),
          (
            "watch",
            Array(
              msg.watch
              |> PeerId.Set.elements
              |> List.rev_map(id => String(id |> PeerId.toString)),
            ),
          ),
          ("signature", String(msg.signature)),
        ]),
      )
    )
  | WatchedPeersChanged(changes) =>
    Json.(
      stringify(
        Object([
          ("type", String("watchedPeersChanged")),
          (
            "changes",
            Array(
              changes
              |> List.rev_map(
                   fun
                   | WentOnline(peerId) =>
                     Array([
                       String(peerId |> PeerId.toString),
                       String("online"),
                     ])
                   | WentOffline(peerId) =>
                     Array([
                       String(peerId |> PeerId.toString),
                       String("offline"),
                     ]),
                 ),
            ),
          ),
        ]),
      )
    )
  | ChangeWatchedPeers(msg) =>
    Json.(
      stringify(
        Object([
          ("type", String("changeWatchedPeers")),
          ("src", String(msg.src |> PeerId.toString)),
          (
            "watch",
            Array(
              msg.watch
              |> PeerId.Set.elements
              |> List.rev_map(id => String(id |> PeerId.toString)),
            ),
          ),
          ("signature", String(msg.signature)),
        ]),
      )
    )
  | _ => "decoder not implemented";

type parsingResult('a) =
  | Ok('a)
  | Error(string);

/* f = i => i |> Json.string */
let decodeList = (f, json) =>
  json
  |> Json.array
  |?> (
    items =>
      List.fold_right(
        (itemJson, acc) =>
          switch (f(itemJson), acc) {
          | (Some(i), Some(list)) => Some([i, ...list])
          | _ => None
          },
        items,
        Some([]),
      )
  );

let decodePeerIdSet = json =>
  json
  |> Json.array
  |?> (
    items =>
      List.fold_left(
        (peerIdSet, peerId) =>
          switch (peerIdSet, peerId |> Json.string |?> PeerId.ofString) {
          | (Some(peerIdSet), Some(peerId)) =>
            Some(peerIdSet |> PeerId.Set.add(peerId))
          | _ => None
          },
        Some(PeerId.Set.empty),
        items,
      )
  );

let decodeLoginMsg = json =>
  switch (
    json |> Json.get("src") |?> Json.string |?> PeerId.ofString,
    json |> Json.get("signature") |?> Json.string,
    json |> Json.get("watch") |?> decodePeerIdSet,
  ) {
  | (Some(src), Some(signature), Some(watch)) =>
    Ok(Login({src, signature, watch}))
  | _ => Error("Login message invalid format")
  };

let decodeChangeWatchedPeers = json =>
  switch (
    json |> Json.get("src") |?> Json.string |?> PeerId.ofString,
    json |> Json.get("signature") |?> Json.string,
    json |> Json.get("watch") |?> decodePeerIdSet,
  ) {
  | (Some(src), Some(signature), Some(watch)) =>
    Ok(ChangeWatchedPeers({src, signature, watch}))
  | _ => Error("ChangeWatchedPeers message invalid format")
  };

let decodeLogoffMsg = json =>
  switch (
    json |> Json.get("src") |?> Json.string |?> PeerId.ofString,
    json |> Json.get("signature") |?> Json.string,
  ) {
  | (Some(src), Some(signature)) => Ok(Logoff({src, signature}))
  | _ => Error("Logoff message invalid format")
  };

let decodeOfferOrAnswer = json =>
  switch (
    json |> Json.get("src") |?> Json.string |?> PeerId.ofString,
    json |> Json.get("tg") |?> Json.string |?> PeerId.ofString,
    json |> Json.get("sdp") |?> Json.string,
    json |> Json.get("signature") |?> Json.string,
  ) {
  | (Some(src), Some(tg), Some(sdp), Some(signature)) =>
    Ok({src, tg, sdp, signature})
  | _ => Error("Offer message invalid format")
  };

let decodeOkMsg: Rex_json.Json.t => parsingResult(t) =
  json =>
    switch (json |> Json.get("onlinePeers") |?> decodePeerIdSet) {
    | Some(onlinePeers) => Ok(Ok(onlinePeers))
    | None => Error("Ok message invalid format")
    };

let decodeWatchedPeersChanged = json =>
  switch (
    json
    |> Json.get("changes")
    |?> decodeList(ch =>
          switch (
            ch |> Json.nth(0) |?> Json.string |?> PeerId.ofString,
            ch |> Json.nth(1) |?> Json.string,
          ) {
          | (Some(peerId), Some("offline")) => Some(WentOffline(peerId))
          | (Some(peerId), Some(_)) => Some(WentOnline(peerId))
          | _ => None
          }
        )
  ) {
  | Some(changes) => Ok(WatchedPeersChanged(changes))
  | None => Error("WatchedPeersChanged message invalid format")
  };

let fromJSON = str =>
  switch (str |> JsonUtils.parseOpt) {
  | Some(json) =>
    let maybeVersion =
      switch (json |> Json.get("version")) {
      | None => Some(latestVersion)
      | Some(v) => v |> Json.number |?>> int_of_float
      };
    switch (maybeVersion) {
    | Some(_ver) =>
      /* TODO: Support different versions */
      switch (json |> Json.get("type") |?> Json.string) {
      | Some(typeStr) =>
        switch (typeStr) {
        | "login" => decodeLoginMsg(json)
        | "offer" =>
          switch (decodeOfferOrAnswer(json)) {
          | Ok(offerPayload) => Ok(Offer(offerPayload))
          | Error(_) as e => e
          }
        | "answer" =>
          switch (decodeOfferOrAnswer(json)) {
          | Ok(answerPayload) => Ok(Answer(answerPayload))
          | Error(_) as e => e
          }
        | "logoff" => decodeLogoffMsg(json)
        | "ok" => decodeOkMsg(json)
        | "watchedPeersChanged" => decodeWatchedPeersChanged(json)
        | "changeWatchedPeers" => decodeChangeWatchedPeers(json)
        | _ => Error("Type: Unknown message type.")
        }
      | None => Error("Type: Missing or not string")
      }
    | None => Error("Version: Not a number.")
    };
  | None => Error("Not a valid JSON")
  };