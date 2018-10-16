module WS: DreamWSType.T = DreamWSCohttp;

type client = {
  id: PeerId.t,
  socket: WS.Socket.t,
  isAuthenticated: bool,
  protocolVersion: int,
};

module PeerWatching = {
  type t = {
    watchedPeer: PeerId.t,
    watcherPeer: PeerId.t,
  };
  let compare = compare;
};

module PeerWatchingSet = Set.Make(PeerWatching);
module PeerWatchings = IndexedCollection.Make(PeerWatching);

type serverState = {
  connectedPeers: Hashtbl.t(PeerId.t, client),
  watchings: PeerWatchings.t(PeerWatching.t),
  watchingsByWatcher: PeerWatchings.index(PeerWatching.t, PeerId.t),
  watchingsByWatched: PeerWatchings.index(PeerWatching.t, PeerId.t),
};

type effect =
  | Emit(WS.Socket.t, Message.t)
  | Db(serverState);

let init = () => {
  let watchingsByWatcher =
    PeerWatchings.makeIndex(({PeerWatching.watcherPeer, _}) => watcherPeer);
  let watchingsByWatched =
    PeerWatchings.makeIndex(({PeerWatching.watchedPeer, _}) => watchedPeer);
  {
    connectedPeers: Hashtbl.create(10),
    watchings:
      PeerWatchings.make(
        a => a,
        [I(watchingsByWatcher), I(watchingsByWatched)],
      ),
    watchingsByWatcher,
    watchingsByWatched,
  };
};

/* Watchings */

let getWatchedByWatcher = (state, watcher) =>
  state.watchingsByWatcher |> PeerWatchings.get(watcher);

let getWatchersByWatched = (state, watched) =>
  state.watchingsByWatched |> PeerWatchings.get(watched);

let addWatching = (state, watching: PeerWatching.t) =>
  state.watchings |> PeerWatchings.add(watching);

let removeWatching = (state, watching: PeerWatching.t) =>
  state.watchings |> PeerWatchings.remove(watching);

let removeAllWatchings = (state, watcher) =>
  getWatchedByWatcher(state, watcher)
  |> PeerWatchingSet.iter(removeWatching(state));

let addWatchings = (state, watcher, watchedPeers) =>
  watchedPeers
  |> PeerId.Set.iter(peerId =>
       addWatching(state, {watcherPeer: watcher, watchedPeer: peerId})
     );

let updateWatchings = (state, watcher, watchedPeers) => {
  removeAllWatchings(state, watcher);
  addWatchings(state, watcher, watchedPeers);
};

/* Connected peers */

let addPeer = (s, client) =>
  Hashtbl.replace(s.connectedPeers, client.id, client);

let removePeer = (s, id) => Hashtbl.remove(s.connectedPeers, id);

let findPeer = (s, id) => Hashtbl.find_opt(s.connectedPeers, id);

let memPeer = (s, id) => Hashtbl.mem(s.connectedPeers, id);

/* Helpers */

let makeNotificationsForInterestedPeers = (state, peerChange) => {
  let Message.WentOnline(peerId) | WentOffline(peerId) = peerChange;
  PeerWatchingSet.fold(
    ({PeerWatching.watcherPeer, watchedPeer: _}, acc) =>
      switch (findPeer(state, watcherPeer)) {
      | Some(peer) => [
          Emit(peer.socket, WatchedPeersChanged([peerChange])),
          ...acc,
        ]
      | None =>
        Printf.eprintf(
          "Internal error: found a watching of watcher %s who is not online though.",
          watcherPeer,
        );
        acc;
      },
    getWatchersByWatched(state, peerId),
    [],
  );
};

let handleMessage = (srcSocket, msgStr, state) =>
  /* ignore(Lwt_io.eprintf("asdf\n%!")); */
  switch (Message.fromJSON(msgStr)) {
  | Ok(message) =>
    switch (message) {
    | Login(msg) =>
      /* Existing peer */
      switch (findPeer(state, msg.src)) {
      | Some(_) =>
        /* There is already an existing connected peer with the same ID. */
        /* ...Let's kick him out... */
        /* TODO: Disconnect an existing connected peer */
        ()
      | None => ()
      };

      /* TODO: Check signature */
      addPeer(
        state,
        {
          socket: srcSocket,
          /* TODO: Check signature */
          isAuthenticated: false,
          /* TODO: Support multiple protocol versions */
          protocolVersion: 1,
          id: msg.src,
        },
      );

      /* Add my watches */
      updateWatchings(state, msg.src, msg.watch);

      /* Populate states of peers I'm interested in */
      let onlinePeers = msg.watch |> PeerId.Set.filter(memPeer(state));
      /* Notify others that are interested in my arrival */
      let notifications =
        makeNotificationsForInterestedPeers(state, WentOnline(msg.src));

      [Emit(srcSocket, Ok(onlinePeers)), ...notifications];
    | ChangeWatchedPeers(msg) =>
      /* TODO: Check signature */
      switch (findPeer(state, msg.src)) {
      | Some(_) =>
        updateWatchings(state, msg.src, msg.watch);

        /* Populate states of peers I'm interested in */
        let onlinePeersIds = msg.watch |> PeerId.Set.filter(memPeer(state));
        let peerChanges =
          PeerId.Set.fold(
            (peerId, acc) => [Message.WentOnline(peerId), ...acc],
            onlinePeersIds,
            [],
          );
        [Emit(srcSocket, WatchedPeersChanged(peerChanges))];
      | None => [Emit(srcSocket, Error(SourceNotOnline))]
      }
    | Offer(payload) as msg
    | Answer(payload) as msg =>
      /* TODO: Check signature */
      switch (findPeer(state, payload.tg)) {
      | Some(tgClient) => [Emit(tgClient.socket, msg)]
      | None => [Emit(srcSocket, Error(TargetNotOnline))]
      }
    | Logoff(msg) =>
      /* TODO: Check signature */
      removeAllWatchings(state, msg.src);
      removePeer(state, msg.src);
      [];
    /* [Emit(srcSocket, Ok)]; */
    | Error(_)
    | Ok(_)
    | WatchedPeersChanged(_) =>
      Printf.eprintf(
        "Got message type that should be sent only from server to client\n",
      );
      [];
    | Unknown =>
      Printf.eprintf("Unknown message type\n");
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