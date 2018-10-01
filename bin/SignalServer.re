module WS: DreamWSType.T = DreamWSCohttp;
open Rex_json;

let t = BlackTea.Cmd.none;

module Fingerprint = String;

/* module FingerprintMap = Map.Make(Fingerprint); */
/* module FingerprintSet = Set.Make(Fingerprint); */
type client = {
  socket: WS.Socket.t,
  isAuthenticated: bool,
  protocolVersion: int,
  /* <<FK>> */
  fingerprint: Fingerprint.t,
};

module PeerWatching = {
  type t = {
    /* <<PK>> */
    watchedPeer: Fingerprint.t,
    /* <<FK to activeConnections>>  */
    watcherPeer: Fingerprint.t,
  };
  let compare = (a, b) => {
    let watchedRes = String.compare(a.watchedPeer, b.watchedPeer);
    if (watchedRes == 0) {
      String.compare(a.watcherPeer, b.watcherPeer);
    } else {
      watchedRes;
    };
  };
};

module PeerWatchingSet = Set.Make(PeerWatching);

type serverState = {
  connectedPeers: Hashtbl.t(Fingerprint.t, client),
  watcherToWatched: Hashtbl.t(Fingerprint.t, PeerWatchingSet.t),
  watchedToWatchers: Hashtbl.t(Fingerprint.t, PeerWatchingSet.t),
};

let init = () => {
  connectedPeers: Hashtbl.create(10),
  watcherToWatched: Hashtbl.create(10),
  watchedToWatchers: Hashtbl.create(10),
};

let getWatchedByWatcher = (state, watcher) =>
  Hashtbl.find(state.watcherToWatched, watcher);

let getWatchersByWatched = (state, watched) =>
  Hashtbl.find(state.watchedToWatchers, watched);

let addWatching = (state, watching: PeerWatching.t) => {
  Hashtbl.replace(
    state.watcherToWatched,
    watching.watcherPeer,
    Hashtbl.find(state.watcherToWatched, watching.watcherPeer)
    |> PeerWatchingSet.add(watching),
  );
  Hashtbl.replace(
    state.watchedToWatchers,
    watching.watchedPeer,
    Hashtbl.find(state.watchedToWatchers, watching.watchedPeer)
    |> PeerWatchingSet.add(watching),
  );
};

let addPeer = (state, client) =>
  Hashtbl.replace(state.connectedPeers, client.fingerprint, client);

let removePeer = (state, fingerprint) =>
  Hashtbl.remove(state.connectedPeers, fingerprint);

let findPeer = (state, fingerprint) =>
  Hashtbl.find_opt(state.connectedPeers, fingerprint);

let latestVersion = 1;

module Messages = {
  open Json.Infix;

  type offerOrAnswer = {
    src: string,
    tg: string,
    sdp: string,
    signature: string,
  };
  type login = {
    src: string,
    watch: list(string),
    signature: string,
  };
  type logoff = {
    src: string,
    signature: string,
  };
  type error =
    | TargetNotOnline
    | InvalidMessage(string);
  type t =
    | Login(login)
    | Offer(offerOrAnswer)
    | Answer(offerOrAnswer)
    | Error(error)
    | Logoff(logoff)
    | Ok
    | Unknown;

  type clientToServer = t;
  type serverToClient = t;
  module DumbJson = {
    let obj = items =>
      "{"
      ++ (
        items
        |> List.fold_left(
             (acc, item) =>
               if (acc == "") {
                 item;
               } else {
                 acc ++ "," ++ item;
               },
             "",
           )
      )
      ++ "}";
    let str = value => "\"" ++ value ++ "\"";
    let keyVal = (key, value) => str(key) ++ ":" ++ value;
    let num = value => Printf.sprintf("%d", value);
  };

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
              ("src", String(msg.src)),
              ("tg", String(msg.tg)),
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
               | InvalidMessage(explanation) => [
                   ("code", String("InvalidMessage")),
                   ("explanation", String(explanation)),
                 ]
               },
          ]),
        )
      )
    | Ok => Json.(stringify(Object([("type", String("ok"))])))
    | _ => "";
  type parsingResult('a) =
    | Ok('a)
    | Error(string);

  /* f = i => i |> Json.string */
  let decodeList = (f, json) =>
    Json.Infix.(
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
      )
    );
  let decodeLoginMsg = json =>
    switch (
      json |> Json.get("src") |?> Json.string,
      json |> Json.get("signature") |?> Json.string,
      json |> Json.get("watch") |?> decodeList(i => i |> Json.string),
    ) {
    | (Some(src), Some(signature), Some(watch)) =>
      Ok(Login({src, signature, watch}))
    | _ => Error("Login message invalid format")
    };
  let decodeLogoffMsg = json =>
    switch (
      json |> Json.get("src") |?> Json.string,
      json |> Json.get("signature") |?> Json.string,
    ) {
    | (Some(src), Some(signature)) => Ok(Logoff({src, signature}))
    | _ => Error("Logoff message invalid format")
    };

  let decodeOfferOrAnswer = json =>
    switch (
      json |> Json.get("src") |?> Json.string,
      json |> Json.get("tg") |?> Json.string,
      json |> Json.get("sdp") |?> Json.string,
      json |> Json.get("signature") |?> Json.string,
    ) {
    | (Some(src), Some(tg), Some(sdp), Some(signature)) =>
      Ok({src, tg, sdp, signature})
    | _ => Error("Offer message invalid format")
    };

  let fromJSON = str => {
    let jsonOption =
      switch (Json.parse(str)) {
      | json => Some(json)
      | exception _ => None
      };
    Json.Infix.(
      switch (jsonOption) {
      | Some(json) =>
        let version =
          switch (json |> Json.get("version")) {
          | None => Some(latestVersion)
          | Some(v) => v |> Json.number |?>> int_of_float
          };
        switch (version) {
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
            | _ => Error("Type: Unknown message type.")
            }
          | None => Error("Type: Missing or not string")
          }
        | None => Error("Version: Not a number.")
        };
      | None => Error("Not a valid JSON")
      }
    );
  };
};

type effect =
  | Emit(WS.Socket.t, Messages.t)
  | Db(serverState);

let handleMessage = (srcSocket, msgStr, state) =>
  /* ignore(Lwt_io.eprintf("asdf\n%!")); */
  switch (Messages.fromJSON(msgStr)) {
  | Ok(message) =>
    switch (message) {
    | Login(msg) =>
      /* TODO: Check fingerprint */
      addPeer(
        state,
        {
          socket: srcSocket,
          isAuthenticated: false, /* TODO */
          protocolVersion: 1, /* TODO */
          fingerprint: msg.src,
        },
      );
      [Emit(srcSocket, Ok)];
    | Offer(payload) as msg
    | Answer(payload) as msg =>
      /* TODO: Check fingerprint */
      Printf.eprintf("Got offer... ooooo\n");
      switch (findPeer(state, payload.tg)) {
      | Some(tgClient) => [Emit(tgClient.socket, msg)]
      | None => [Emit(srcSocket, Error(TargetNotOnline))]
      };
    | Logoff(msg) =>
      /* TODO: Check fingerprint */
      removePeer(state, msg.src);
      [Emit(srcSocket, Ok)];
    | _ =>
      Printf.eprintf("Unknown message\n");
      [];
    }
  | Error(str) => [Emit(srcSocket, Error(InvalidMessage(str)))]
  };

let state = ref(init());

let handleEffect =
  fun
  | Emit(socket, msg) =>
    ignore(WS.Socket.emit(socket, msg |> Messages.toJSON))
  | Db(newState) => state := newState;

WS.run(
  ~port=7777,
  ~onConnection=socket => {
    open WS;
    Printf.eprintf("Got a connection!\n%!");
    socket
    |> Socket.setOnMessage((_, msg) =>
         handleMessage(socket, msg, state^) |> List.iter(handleEffect)
       )
    |> Socket.setOnDisconnect(() => Printf.eprintf("Disconnected\n%!"));
  },
);