open Rex_json;
open Rex_json.Json.Infix;

/* CONSTANTS */

let latestVersion = 1;

/* TYPES */

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

type keyRequest = {
  src: PeerId.t,
  tg: PeerId.t,
  signature: string,
};

type keyResponse = {
  src: PeerId.t,
  tg: PeerId.t,
  key: string,
  signature: string,
};

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
  | KeyRequest(keyRequest)
  | KeyResponse(keyResponse);

type clientToServer = t;
type serverToClient = t;

type parsingResult('a) =
  | Ok('a)
  | Error(string);

/* SERIALIZATION */

let typeToJsonKeyVal = v => ("type", Json.String(v));

let srcToJsonKeyVal = v => ("src", Json.String(v |> PeerId.toString));
let srcOfJson = json =>
  json |> Json.get("src") |?> Json.string |?> PeerId.ofString;

let tgToJsonKeyVal = v => ("tg", Json.String(v |> PeerId.toString));
let tgOfJson = json =>
  json |> Json.get("tg") |?> Json.string |?> PeerId.ofString;

let signatureToJsonKeyVal = v => ("signature", Json.String(v));
let signatureOfJson = json => json |> Json.get("signature") |?> Json.string;

let encode =
  Json.(
    fun
    | Offer(msg) as v
    | Answer(msg) as v => {
        let typeString =
          switch (v) {
          | Offer(_) => "offer"
          | _ => "answer"
          };
        Object([
          typeString |> typeToJsonKeyVal,
          msg.src |> srcToJsonKeyVal,
          msg.tg |> tgToJsonKeyVal,
          ("sdp", String(msg.sdp)),
          msg.signature |> signatureToJsonKeyVal,
        ]);
      }
    | Error(error) =>
      Object([
        "error" |> typeToJsonKeyVal,
        ...switch (error) {
           | TargetNotOnline => [("code", String("TargetNotOnline"))]
           | SourceNotOnline => [("code", String("SourceNotOnline"))]
           | InvalidMessage(explanation) => [
               ("code", String("InvalidMessage")),
               ("explanation", String(explanation)),
             ]
           },
      ])
    | Ok(onlinePeers) =>
      Object([
        "ok" |> typeToJsonKeyVal,
        (
          "onlinePeers",
          Array(
            onlinePeers
            |> PeerId.Set.elements
            |> List.rev_map(id => String(id |> PeerId.toString)),
          ),
        ),
      ])
    | Login(msg) =>
      Object([
        "login" |> typeToJsonKeyVal,
        msg.src |> srcToJsonKeyVal,
        (
          "watch",
          Array(
            msg.watch
            |> PeerId.Set.elements
            |> List.rev_map(id => String(id |> PeerId.toString)),
          ),
        ),
        msg.signature |> signatureToJsonKeyVal,
      ])
    | WatchedPeersChanged(changes) =>
      Object([
        "watchedPeersChanged" |> typeToJsonKeyVal,
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
      ])
    | ChangeWatchedPeers(msg) =>
      Object([
        "changeWatchedPeers" |> typeToJsonKeyVal,
        msg.src |> srcToJsonKeyVal,
        (
          "watch",
          Array(
            msg.watch
            |> PeerId.Set.elements
            |> List.rev_map(id => String(id |> PeerId.toString)),
          ),
        ),
        msg.signature |> signatureToJsonKeyVal,
      ])
    | KeyRequest(msg) =>
      Object([
        "keyRequest" |> typeToJsonKeyVal,
        msg.src |> srcToJsonKeyVal,
        msg.tg |> tgToJsonKeyVal,
        msg.signature |> signatureToJsonKeyVal,
      ])
    | KeyResponse(msg) =>
      Object([
        "keyRequest" |> typeToJsonKeyVal,
        msg.src |> srcToJsonKeyVal,
        msg.tg |> tgToJsonKeyVal,
        ("key", String(msg.key)),
        msg.signature |> signatureToJsonKeyVal,
      ])
    | Logoff(msg) =>
      Object([
        "logoff" |> typeToJsonKeyVal,
        msg.src |> srcToJsonKeyVal,
        msg.signature |> signatureToJsonKeyVal,
      ])
  );

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
    json |> srcOfJson,
    json |> signatureOfJson,
    json |> Json.get("watch") |?> decodePeerIdSet,
  ) {
  | (Some(src), Some(signature), Some(watch)) =>
    Ok(Login({src, signature, watch}))
  | _ => Error("Login message invalid format")
  };

let decodeChangeWatchedPeers = json =>
  switch (
    json |> srcOfJson,
    json |> signatureOfJson,
    json |> Json.get("watch") |?> decodePeerIdSet,
  ) {
  | (Some(src), Some(signature), Some(watch)) =>
    Ok(ChangeWatchedPeers({src, signature, watch}))
  | _ => Error("ChangeWatchedPeers message invalid format")
  };

let decodeLogoffMsg = json =>
  switch (json |> srcOfJson, json |> signatureOfJson) {
  | (Some(src), Some(signature)) => Ok(Logoff({src, signature}))
  | _ => Error("Logoff message invalid format")
  };

let decodeOfferOrAnswer = json =>
  switch (
    json |> srcOfJson,
    json |> tgOfJson,
    json |> Json.get("sdp") |?> Json.string,
    json |> signatureOfJson,
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

let decodeKeyRequest = json =>
  switch (json |> srcOfJson, json |> tgOfJson, json |> signatureOfJson) {
  | (Some(src), Some(tg), Some(signature)) =>
    Ok(KeyRequest({src, tg, signature}))
  | _ => Error("KeyRequest message invalid format")
  };

let decodeKeyResponse = json =>
  switch (
    json |> srcOfJson,
    json |> tgOfJson,
    json |> Json.get("key") |?> Json.string,
    json |> signatureOfJson,
  ) {
  | (Some(src), Some(tg), Some(key), Some(signature)) =>
    Ok(KeyResponse({src, tg, key, signature}))
  | _ => Error("KeyResponse message invalid format")
  };

let decode = json => {
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
      | "keyRequest" => decodeKeyRequest(json)
      | "keyResponse" => decodeKeyResponse(json)
      | _ => Error("Type: Unknown message type.")
      }
    | None => Error("Type: Missing or not string")
    }
  | None => Error("Version: Not a number.")
  };
};