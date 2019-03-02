open Rex_json;
open Rex_json.Json.Infix;

/* CONSTANTS */

let latestVersion = 1;

/* TYPES */

type peerChange =
  | WentOnline(PeerId.t)
  | WentOffline(PeerId.t);

type error =
  | TargetNotOnline
  | SourceNotOnline
  | InvalidMessage(string);

type sdp = string;
type key = string;

/*
       BEST-CASE SEQUENCE DIAGRAM
       (edit at https://textik.com/#cb03768deea5c0f6)

       +--------+                    +---------------+                +--------+
       | NODE A |                    | SIGNAL SERVER |                | NODE B |
       +--------+                    +---------------+                +--------+
           |                                |                                |
           | Login([B]), sig_A, id_A        |                                |
           |------------------------------->|                                |
           |                         Ok([]) |                                |
           |<-------------------------------|        Login([A]), sig_B, id_B |
           |                                |<-------------------------------|
           |            WatchedPeersChanged | Ok([A])                        |
           |              ([[B, 'online']]) |------------------------------->|
           |<-------------------------------|                                |
 +-----------------------------------------------------------------------------+
 |IF       | KeyRequest(Pub_A),             |                                | |
 |!Pub_B   | sig_A, id_A, id_B              | KeyRequest(pub_A),             | |
 |         |------------------------------->| sig_A, id_A, id_B              | |
 |         |                                |------------------------------->| |
 |         |                                |            KeyResponse(pub_B), | |
 |         |            KeyResponse(pub_B), |              sig_B, id_B, id_A | |
 |         |              sig_B, id_B, id_A |<-------------------------------| |
 |         |<-------------------------------|                                | |
 +-----------------------------------------------------------------------------+
 +-----------------------------------------------------------------------------+
 |IF       | Offer(sdp),                    |                                | |
 |B in a   | sig_A, id_A, id_B              | Offer(sdp),                    | |
 | group   |------------------------------->| sig_A, id_A, id_B              | |
 |&&       |                                |------------------------------->| |
 |has      |                                |                   Answer(sig), | |
 | Pub_B   |                   Answer(sdp), |              sig_B, id_B, id_A | |
 |         |              sig_B, id_B, id_A |<-------------------------------| |
 |         |<-------------------------------|                                | |
 +-----------------------------------------------------------------------------+
           |                                |                                |
           |                                |                                |
   */

type peerToServerMsg =
  | Login(/* Watched peers */ PeerId.Set.t)
  | Logoff
  | ChangeWatchedPeers(/* Watched peers */ PeerId.Set.t);

type peerToPeerMsg =
  | Offer(sdp)
  | Answer(sdp)
  | KeyRequest(key)
  | KeyResponse(key);

type serverToPeerMsg =
  | Error(error)
  | Ok(PeerId.Set.t)
  | WatchedPeersChanged(list(peerChange));

type signedMsg =
  | PeerToServer(PeerId.t, peerToServerMsg)
  | PeerToPeer(PeerId.t, PeerId.t, peerToPeerMsg);

type t =
  | Signed(string, signedMsg)
  | Unsigned(serverToPeerMsg);

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

let keyToJsonKeyVal = v => ("key", Json.String(v));
let keyOfJson = json => json |> Json.get("key") |?> Json.string;

let encodePeerToPeerMsg = msg =>
  switch (msg) {
  | Offer(sdp) => ["offer" |> typeToJsonKeyVal, ("sdp", Json.String(sdp))]
  | Answer(sdp) => ["answer" |> typeToJsonKeyVal, ("sdp", Json.String(sdp))]
  | KeyRequest(key) => [
      "keyRequest" |> typeToJsonKeyVal,
      key |> keyToJsonKeyVal,
    ]
  | KeyResponse(key) => [
      "keyResponse" |> typeToJsonKeyVal,
      key |> keyToJsonKeyVal,
    ]
  };

let encodePeerToServerMsg = msg =>
  switch (msg) {
  | Login(watch) => [
      "login" |> typeToJsonKeyVal,
      (
        "watch",
        Array(
          watch
          |> PeerId.Set.elements
          |> List.rev_map(id => Json.String(id |> PeerId.toString)),
        ),
      ),
    ]
  | Logoff => ["logoff" |> typeToJsonKeyVal]
  | ChangeWatchedPeers(watch) => [
      "changeWatchedPeers" |> typeToJsonKeyVal,
      (
        "watch",
        Array(
          watch
          |> PeerId.Set.elements
          |> List.rev_map(id => Json.String(id |> PeerId.toString)),
        ),
      ),
    ]
  };

let encodeSignedMsg = msg =>
  switch (msg) {
  | PeerToServer(src, payload) => [
      src |> srcToJsonKeyVal,
      ...payload |> encodePeerToServerMsg,
    ]
  | PeerToPeer(src, tg, payload) => [
      src |> srcToJsonKeyVal,
      tg |> tgToJsonKeyVal,
      ...payload |> encodePeerToPeerMsg,
    ]
  };

let encodeServerToPeerMsg = msg =>
  Json.(
    switch (msg) {
    | WatchedPeersChanged(changes) => [
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
      ]
    | Error(error) => [
        "error" |> typeToJsonKeyVal,
        ...switch (error) {
           | TargetNotOnline => [("code", String("TargetNotOnline"))]
           | SourceNotOnline => [("code", String("SourceNotOnline"))]
           | InvalidMessage(explanation) => [
               ("code", String("InvalidMessage")),
               ("explanation", String(explanation)),
             ]
           },
      ]
    | Ok(onlinePeers) => [
        "ok" |> typeToJsonKeyVal,
        (
          "onlinePeers",
          Array(
            onlinePeers
            |> PeerId.Set.elements
            |> List.rev_map(id => String(id |> PeerId.toString)),
          ),
        ),
      ]
    }
  );

let encode = msg => {
  open Json;
  let fields =
    switch (msg) {
    | Signed(signature, msg) => [
        ("signature", String(signature)),
        ...encodeSignedMsg(msg),
      ]
    | Unsigned(msg) => encodeServerToPeerMsg(msg)
    };
  Object(fields);
};

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
  switch (json |> Json.get("watch") |?> decodePeerIdSet) {
  | Some(watch) => Some(Login(watch))
  | _ => None
  };

let decodeChangeWatchedPeers = json =>
  switch (json |> Json.get("watch") |?> decodePeerIdSet) {
  | Some(watch) => Some(ChangeWatchedPeers(watch))
  | _ => None
  };

let decodeLogoffMsg = _ => Some(Logoff);

let decodeSdp = json => json |> Json.get("sdp") |?> Json.string;

let decodeOffer = json =>
  switch (json |> decodeSdp) {
  | Some(sdp) => Some(Offer(sdp))
  | _ => None
  };

let decodeAnswer = json =>
  switch (json |> decodeSdp) {
  | Some(sdp) => Some(Answer(sdp))
  | _ => None
  };

let decodeOkMsg: Json.t => option(serverToPeerMsg) =
  json =>
    switch (json |> Json.get("onlinePeers") |?> decodePeerIdSet) {
    | Some(onlinePeers) => Some(Ok(onlinePeers))
    | None => None
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
  | Some(changes) => Some(WatchedPeersChanged(changes))
  | None => None
  };

let decodeKeyMsg = json =>
  switch (json |> Json.get("key") |?> Json.string) {
  | Some(key) => Some(key)
  | _ => None
  };

let decodeKeyRequest = json =>
  switch (keyOfJson(json)) {
  | Some(key) => Some(KeyRequest(key))
  | _ => None
  };

let decodeKeyResponse = json =>
  switch (keyOfJson(json)) {
  | Some(key) => Some(KeyResponse(key))
  | _ => None
  };

let decodePeerToServerMsg = (decodeNext, json) =>
  switch (
    json |> Json.get("src") |?> Json.string |?> PeerId.ofString,
    decodeNext(json),
  ) {
  | (Some(src), Some(peerToServerPayload)) =>
    Some(PeerToServer(src, peerToServerPayload))
  | _ => None
  };

let decodeSignedMsg = (decodeNext, json) =>
  switch (json |> Json.get("signature") |?> Json.string, decodeNext(json)) {
  | (Some(signature), Some(signedMsg)) =>
    Some(Signed(signature, signedMsg))
  | _ => None
  };

let decodeUnsignedMsg = (decodeNext, json) =>
  switch (decodeNext(json)) {
  | Some(payload) => Some(Unsigned(payload))
  | None => None
  };

let decodePeerToPeerMsg = (decodeNext, json) =>
  switch (
    json |> Json.get("src") |?> Json.string |?> PeerId.ofString,
    json |> Json.get("tg") |?> Json.string |?> PeerId.ofString,
    decodeNext(json),
  ) {
  | (Some(src), Some(tg), Some(payload)) =>
    Some(PeerToPeer(src, tg, payload))
  | _ => None
  };

let optToResult = ();

let decode = json => {
  let maybeVersion =
    switch (json |> Json.get("version")) {
    | None => Some(latestVersion)
    | Some(v) => v |> Json.number |?>> int_of_float
    };
  let msgOptToResult =
    fun
    | Some(m) => Ok(m)
    | None => Error("Invalid fields or format");

  switch (maybeVersion) {
  | Some(_ver) =>
    /* TODO: Support different versions */
    switch (json |> Json.get("type") |?> Json.string) {
    | Some(typeStr) =>
      switch (typeStr) {
      | "login" =>
        json
        |> decodeSignedMsg @@
        decodePeerToServerMsg @@
        decodeLoginMsg
        |> msgOptToResult
      | "offer" =>
        json
        |> decodeSignedMsg @@
        decodePeerToPeerMsg @@
        decodeOffer
        |> msgOptToResult
      | "answer" =>
        json
        |> decodeSignedMsg @@
        decodePeerToPeerMsg @@
        decodeAnswer
        |> msgOptToResult
      | "logoff" =>
        json
        |> decodeSignedMsg @@
        decodePeerToServerMsg @@
        decodeLogoffMsg
        |> msgOptToResult
      | "ok" => json |> decodeUnsignedMsg @@ decodeOkMsg |> msgOptToResult
      | "watchedPeersChanged" =>
        json
        |> decodeUnsignedMsg @@
        decodeWatchedPeersChanged
        |> msgOptToResult
      | "changeWatchedPeers" =>
        json
        |> decodeSignedMsg @@
        decodePeerToServerMsg @@
        decodeChangeWatchedPeers
        |> msgOptToResult
      | "keyRequest" =>
        json
        |> decodeSignedMsg @@
        decodePeerToPeerMsg @@
        decodeKeyRequest
        |> msgOptToResult
      | "keyResponse" =>
        json
        |> decodeSignedMsg @@
        decodePeerToPeerMsg @@
        decodeKeyResponse
        |> msgOptToResult
      | _ => Error("Type: Unknown message type.")
      }
    | None => Error("Type: Missing or not string")
    }
  | None => Error("Version: Not a number.")
  };
};