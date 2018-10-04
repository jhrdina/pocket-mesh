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
    watchedRes != 0 ?
      watchedRes : String.compare(a.watcherPeer, b.watcherPeer);
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

type effect =
  | Emit(WS.Socket.t, Message.t)
  | Db(serverState);

let handleMessage = (srcSocket, msgStr, state) =>
  /* ignore(Lwt_io.eprintf("asdf\n%!")); */
  switch (Message.fromJSON(msgStr)) {
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
    ignore(WS.Socket.emit(socket, msg |> Message.toJSON))
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