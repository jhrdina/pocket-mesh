module WS: DreamWSType.T = DreamWSCohttp;

module Fingerprint = String;
/* module FingerprintMap = Map.Make(Fingerprint); */
/* module FingerprintSet = Set.Make(Fingerprint); */

type client = {
  connection: WS.Socket.t,
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

module Messages = {
  type offer = {
    src: string,
    tg: string,
    sdp: string,
    signature: string,
  };

  type t =
    | Login
    | Offer(offer)
    | Answer
    | Logoff
    | Unknown;

  type clientToServer = t;
  type serverToClient = t;

  let toJSON =
    fun
    | Login => "login"
    | Offer(_) => "offer"
    | Answer => "answer"
    | Logoff => "logoff"
    | Unknown => "unknown";

  let fromJSON =
    fun
    | "login" => Login
    | "offer" => Offer({src: "", tg: "", sdp: "", signature: ""})
    | "answer" => Answer
    | "logoff" => Logoff
    | _ => Unknown;
};

type effect =
  | Emit(WS.Socket.t, Messages.t)
  | Db(serverState);

let handleMessage = (srcSocket, msgStr) => {
  let message = Messages.fromJSON(msgStr);
  switch (message) {
  | Offer(_) =>
    Printf.eprintf("Got offer... ooooo\n");
    [Emit(srcSocket, Answer)];
  | _ =>
    Printf.eprintf("Unknown message\n");
    [];
  }
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
    Printf.eprintf("Got a connection!\n");
    socket
    |> Socket.setOnMessage((_, msg) =>
         handleMessage(socket, msg) |> List.iter(handleEffect)
       )
    |> Socket.setOnDisconnect(() => Printf.eprintf("Disconnected\n%!"));
  },
);