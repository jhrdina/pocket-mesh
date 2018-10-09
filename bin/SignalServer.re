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
  Hashtbl.find_opt(state.watcherToWatched, watcher);

let getWatchersByWatched = (state, watched) =>
  Hashtbl.find_opt(state.watchedToWatchers, watched);

let addWatching = (state, watching: PeerWatching.t) => {
  Hashtbl.replace(
    state.watcherToWatched,
    watching.watcherPeer,
    /* TODO: Eliminate this check */
    (
      switch (Hashtbl.find_opt(state.watcherToWatched, watching.watcherPeer)) {
      | Some(watchingSet) => watchingSet
      | None => PeerWatchingSet.empty
      }
    )
    |> PeerWatchingSet.add(watching),
  );
  /* TODO: remove duplicate code */
  Hashtbl.replace(
    state.watchedToWatchers,
    watching.watchedPeer,
    /* TODO: Eliminate this check */
    (
      switch (Hashtbl.find_opt(state.watchedToWatchers, watching.watchedPeer)) {
      | Some(watchingSet) => watchingSet
      | None => PeerWatchingSet.empty
      }
    )
    |> PeerWatchingSet.add(watching),
  );
};

let removeWatching = (state, watching: PeerWatching.t) => {
  let newWatcherToWatched =
    Hashtbl.find(state.watcherToWatched, watching.watcherPeer)
    |> PeerWatchingSet.remove(watching);
  if (newWatcherToWatched |> PeerWatchingSet.is_empty) {
    Hashtbl.remove(state.watcherToWatched, watching.watcherPeer);
  } else {
    Hashtbl.replace(
      state.watcherToWatched,
      watching.watcherPeer,
      newWatcherToWatched,
    );
  };

  /* TODO: Remove duplicate code */
  let newWatchedToWatcher =
    Hashtbl.find(state.watchedToWatchers, watching.watchedPeer)
    |> PeerWatchingSet.remove(watching);
  if (newWatchedToWatcher |> PeerWatchingSet.is_empty) {
    Hashtbl.remove(state.watchedToWatchers, watching.watchedPeer);
  } else {
    Hashtbl.replace(
      state.watchedToWatchers,
      watching.watchedPeer,
      newWatchedToWatcher,
    );
  };
};

let removeAllWatchings = (state, watcher) =>
  switch (getWatchedByWatcher(state, watcher)) {
  | Some(watchedSet) =>
    watchedSet |> PeerWatchingSet.iter(removeWatching(state))
  | None =>
    Printf.eprintf(
      "Internal error: trying to remove watchings of watcher that doesn't have record in watcherToWatched.",
    )
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
      /* TODO: Check signature */
      addPeer(
        state,
        {
          socket: srcSocket,
          /* TODO: Check signature */
          isAuthenticated: false,
          /* TODO: Support multiple protocol versions */
          protocolVersion: 1,
          fingerprint: msg.src,
        },
      );
      /* Add my watches */
      msg.watch
      |> List.iter(peerId =>
           addWatching(state, {watcherPeer: msg.src, watchedPeer: peerId})
         );
      /* Populate states of peers I'm interested in */
      let onlinePeers =
        msg.watch
        |> List.filter(peerId =>
             switch (findPeer(state, peerId)) {
             | Some(_) => true
             | None => false
             }
           );
      /* Notify others that are interested in my arrival */
      let notifications =
        switch (getWatchersByWatched(state, msg.src)) {
        | Some(watchings) =>
          PeerWatchingSet.fold(
            ({PeerWatching.watcherPeer, watchedPeer: _}, acc) =>
              switch (findPeer(state, watcherPeer)) {
              | Some(peer) => [
                  Emit(
                    peer.socket,
                    WatchedPeersChanged([WentOnline(msg.src)]),
                  ),
                  ...acc,
                ]
              | None =>
                Printf.eprintf(
                  "Internal error: found a watching of watcher %s who is not online though.",
                  watcherPeer,
                );
                acc;
              },
            watchings,
            [],
          )
        | None => []
        };

      [Emit(srcSocket, Ok(onlinePeers)), ...notifications];
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
      [];
    /* [Emit(srcSocket, Ok)]; */
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