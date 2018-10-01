module WS: DreamWSType.T = DreamWSCohttp;

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

let findPeer = (state, fingerprint) =>
  Hashtbl.find_opt(state.connectedPeers, fingerprint);

let latestVersion = 1;

module Messages = {
  type offerOrAnswer = {
    src: string,
    tg: string,
    sdp: string,
    signature: string,
  };
  type login = {
    src: string,
    signature: string,
    watch: list(string),
  };
  type error =
    | TargetNotOnline
    | InvalidMessage(string);
  type t =
    | Login(login)
    | Offer(offerOrAnswer)
    | Answer(offerOrAnswer)
    | Error(error)
    | Logoff
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
  let encodeErrorMsg = error =>
    Yojson.Basic.(
      to_string(
        `Assoc([
          ("version", `Int(latestVersion)),
          ("type", `String("error")),
          ...switch (error) {
             | TargetNotOnline => [("code", `String("TargetNotOnline"))]
             | InvalidMessage(explanation) => [
                 ("code", `String("InvalidMessage")),
                 ("explanation", `String(explanation)),
               ]
             },
        ]),
      )
    );

  let toJSON =
    fun
    | Login(_msg) => ""
    | Offer(msg) =>
      DumbJson.(
        obj([
          keyVal("type", str("offer")),
          keyVal("src", str(msg.src)),
          keyVal("watch", str(msg.tg)),
          keyVal("sdp", str(msg.sdp)),
          keyVal("signature", str(msg.signature)),
        ])
      )
    | Answer(msg) =>
      DumbJson.(
        obj([
          keyVal("type", str("answer")),
          keyVal("src", str(msg.src)),
          keyVal("tg", str(msg.tg)),
          keyVal("sdp", str(msg.sdp)),
          keyVal("signature", str(msg.signature)),
        ])
      )
    | Logoff => "logoff"
    | Error(error) => encodeErrorMsg(error)
    | Unknown => "unknown";
  type parsingResult =
    | Ok(t)
    | Error(string);

  let decodeLoginMsg = json =>
    Yojson.Basic.Util.(
      try (
        Ok(
          Login({
            src: json |> member("src") |> to_string,
            signature: json |> member("signature") |> to_string,
            watch: json |> member("watch") |> to_list |> filter_string,
          }),
        )
      ) {
      | Type_error(msg, _) => Error(msg)
      }
    );

  let fromJSON = str => {
    let jsonOption =
      switch (Yojson.Basic.from_string(str)) {
      | json => Some(json)
      | exception _ => None
      };
    Yojson.Basic.Util.(
      switch (jsonOption) {
      | Some(json) =>
        let version =
          switch (json |> member("version")) {
          | `Null => Some(latestVersion)
          | _ as v => v |> to_int_option
          };
        switch (version) {
        | Some(_ver) =>
          /* TODO: Support different versions */
          switch (json |> member("type") |> to_string_option) {
          | Some(typeStr) =>
            switch (typeStr) {
            | "login" => decodeLoginMsg(json)
            | "offer" =>
              Ok(
                Offer({
                  src: "aaa",
                  tg: "bbb",
                  sdp: "sdp_offer",
                  signature: "",
                }),
              )
            | "answer" =>
              Ok(
                Answer({
                  src: "bbb",
                  tg: "aaa",
                  sdp: "sdp_answer",
                  signature: "",
                }),
              )
            | "logoff" => Ok(Logoff)
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
      addPeer(
        state,
        {
          socket: srcSocket,
          isAuthenticated: false, /* TODO */
          protocolVersion: 1, /* TODO */
          fingerprint: msg.src,
        },
      );
      [];
    | Offer(payload) as msg
    | Answer(payload) as msg =>
      Printf.eprintf("Got offer... ooooo\n");
      switch (findPeer(state, payload.tg)) {
      | Some(tgClient) => [Emit(tgClient.socket, msg)]
      | None => [Emit(srcSocket, Error(TargetNotOnline))]
      };
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