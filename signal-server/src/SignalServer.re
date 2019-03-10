open Json.Infix;

/* MODULES */

module WS: DreamWSType.T = DreamWSCohttp;

module PeerWatching = {
  type t = {
    watchedPeer: PeerId.t,
    watcherPeer: PeerId.t,
  };
  let compare = compare;
};

module PeerWatchingSet = Set.Make(PeerWatching);
module PeerWatchings = IndexedCollection.Make(PeerWatching);
module Peers = IndexedCollection.Make(PeerId);

/* TYPES */

type client = {
  id: PeerId.t,
  socket: WS.Socket.t,
  isAuthenticated: bool,
  protocolVersion: int,
  key: SimpleCrypto.key,
};

type serverState = {
  peers: Peers.t(client),
  /*
    TODO:
      Use WS.Socket.compare by file_descr by implementing
      Functorial Interface instead of comparison by the whole record value.
      See https://caml.inria.fr/pub/docs/manual-ocaml/libref/Hashtbl.html#1_Functorialinterface
   */
  peersBySocket: Peers.Index.t(client, WS.Socket.t),
  watchings: PeerWatchings.t(PeerWatching.t),
  watchingsByWatcher: PeerWatchings.Index.t(PeerWatching.t, PeerId.t),
  watchingsByWatched: PeerWatchings.Index.t(PeerWatching.t, PeerId.t),
};

type effect =
  | Emit(WS.Socket.t, Message.t)
  | Db(serverState);

/* WATCHINGS */

let getWatchedByWatcher = (state, watcher) =>
  state.watchingsByWatcher |> PeerWatchings.Index.get(watcher);

let getWatchersByWatched = (state, watched) =>
  state.watchingsByWatched |> PeerWatchings.Index.get(watched);

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

let socketEqual = (a, b) => WS.Socket.compare(a, b) == 0;

/* Connected peers */

let addPeer = (s, client) => s.peers |> Peers.add(client);

let removePeer = (s, id) => s.peers |> Peers.remove(id);

let findPeer = (s, id) => s.peers |> Peers.findOpt(id);

let checkPeerSocket = (connectedSocket, client) =>
  socketEqual(connectedSocket, client.socket) ? Some(client) : None;

let findPeerBySocket = (s, socket) =>
  s.peersBySocket |> Peers.Index.get(socket) |> Peers.PrimarySet.choose_opt;

let memPeer = (s, id) => s.peers |> Peers.mem(id);

/* Helpers */

let makeNotificationsForInterestedPeers = (state, peerChange) => {
  let Message.WentOnline(peerId) | WentOffline(peerId) = peerChange;
  PeerWatchingSet.fold(
    ({PeerWatching.watcherPeer, watchedPeer: _}, acc) =>
      switch (findPeer(state, watcherPeer)) {
      | Some(peer) => [
          Emit(peer.socket, Unsigned(WatchedPeersChanged([peerChange]))),
          ...acc,
        ]
      | None =>
        Printf.eprintf(
          "Internal error: found a watching of watcher %s who is not online though.",
          watcherPeer |> PeerId.toString,
        );
        acc;
      },
    getWatchersByWatched(state, peerId),
    [],
  );
};

/* Main server logic */

let init = () => {
  let watchingsByWatcher =
    PeerWatchings.Index.create(({PeerWatching.watcherPeer, _}) =>
      watcherPeer
    );
  let watchingsByWatched =
    PeerWatchings.Index.create(({PeerWatching.watchedPeer, _}) =>
      watchedPeer
    );
  let peersBySocket = Peers.Index.create(({socket, _}) => socket);
  {
    peers: Peers.create(({id, _}) => id, [I(peersBySocket)]),
    peersBySocket,
    watchings:
      PeerWatchings.create(
        a => a,
        [I(watchingsByWatcher), I(watchingsByWatched)],
      ),
    watchingsByWatcher,
    watchingsByWatched,
  };
};

let verifyMessageSignature = (srcPublicKey, signature, msg) =>
  Json.Object(Message.encodeSignedMsg(msg))
  |> Json.stringify
  |> SimpleCrypto.verify(srcPublicKey, signature);

let verifyLoginMessage = (signature, src, srcKeyStr, watch) =>
  srcKeyStr
  |> SimpleCrypto.stringToJwk
  |?> (
    srcJwk => {
      let srcMatchesKey =
        SimpleCrypto.fingerprintForRSAJWK(srcJwk)
        |> PeerId.ofString
        |?>> (==)(src)
        |? false;

      let signatureOk =
        srcJwk
        |> SimpleCrypto.jwkToPublicKey
        |?>> (
          srcKey =>
            verifyMessageSignature(
              srcKey,
              signature,
              PeerToServer(src, Login(srcKeyStr, watch)),
            )
              ? Some(srcKey) : None
        )
        |? None;

      srcMatchesKey ? signatureOk : None;
    }
  );

let handleMessage = (srcSocket, message: Message.t, state) =>
  switch (message) {
  | Signed(signature, PeerToServer(src, Login(srcKeyStr, watch))) =>
    switch (verifyLoginMessage(signature, src, srcKeyStr, watch)) {
    | Some(srcKey) =>
      /* Existing peer */
      switch (findPeer(state, src)) {
      | Some(_) =>
        /* There is already an existing connected peer with the same ID. */
        /* ...Let's kick him out... */
        /* TODO: Disconnect an existing connected peer */
        ()
      | None => ()
      };

      addPeer(
        state,
        {
          socket: srcSocket,
          isAuthenticated: false,
          /* TODO: Support multiple protocol versions */
          protocolVersion: 1,
          id: src,
          key: srcKey,
        },
      );

      /* Add my watches */
      updateWatchings(state, src, watch);

      /* Populate states of peers I'm interested in */
      let onlinePeers = watch |> PeerId.Set.filter(memPeer(state));
      /* Notify others that are interested in my arrival */
      let notifications =
        makeNotificationsForInterestedPeers(state, WentOnline(src));

      [Emit(srcSocket, Unsigned(Ok(onlinePeers))), ...notifications];
    | None => []
    }

  | Signed(_signature, PeerToServer(src, ChangeWatchedPeers(watch))) =>
    /* TODO: Check signature (optional precaution) */
    switch (findPeer(state, src) |?> checkPeerSocket(srcSocket)) {
    | Some(_) =>
      /* Populate states of peers I'm interested in */
      let onlinePeersIds = watch |> PeerId.Set.filter(memPeer(state));
      let peerChanges =
        PeerId.Set.fold(
          (peerId, acc) => [Message.WentOnline(peerId), ...acc],
          onlinePeersIds,
          [],
        );
      [Emit(srcSocket, Unsigned(WatchedPeersChanged(peerChanges)))];

    | None => [Emit(srcSocket, Unsigned(Error(SourceNotOnline)))]
    }

  | Signed(_signature, PeerToServer(src, Logoff)) =>
    /* TODO: Check signature (optional precaution) */
    switch (findPeer(state, src) |?> checkPeerSocket(srcSocket)) {
    | Some(_) =>
      removeAllWatchings(state, src);
      removePeer(state, src);
    | None => ()
    };
    [];

  | Signed(
      _signature,
      PeerToPeer(
        src,
        tg,
        KeyRequest(_) | KeyResponse(_) | Offer(_) | Answer(_),
      ),
    ) as msg =>
    /* TODO: Check signature (optional precaution) */
    switch (findPeer(state, src) |?> checkPeerSocket(srcSocket)) {
    | Some(_) =>
      switch (findPeer(state, tg)) {
      | Some(tgClient) => [Emit(tgClient.socket, msg)]
      | None => [Emit(srcSocket, Unsigned(Error(TargetNotOnline)))]
      }
    | None => []
    }

  | Unsigned(Error(_) | Ok(_) | WatchedPeersChanged(_)) =>
    /* [Emit(srcSocket, Ok)]; */
    Printf.eprintf(
      "Got message type that should be sent only from server to client\n",
    );
    [];
  };

let handleStringMessage = (srcSocket, msgStr, state) =>
  switch (msgStr |> JsonUtils.parseOpt |?>> Message.decode) {
  | Some(Ok(msg)) => handleMessage(srcSocket, msg, state)
  | Some(Error(str)) => [
      Emit(srcSocket, Unsigned(Error(InvalidMessage(str)))),
    ]
  | None => [
      Emit(srcSocket, Unsigned(Error(InvalidMessage("Not a valid JSON")))),
    ]
  };

let handleDisconnect = (socket, state) =>
  switch (findPeerBySocket(state, socket)) {
  | Some(peerId) =>
    removeAllWatchings(state, peerId);
    removePeer(state, peerId);
    /* Notify others that are interested in my disconnect */
    let notifications =
      makeNotificationsForInterestedPeers(state, WentOffline(peerId));

    Printf.eprintf("Disconnected peer %s\n%!", peerId |> PeerId.toString);
    notifications;
  | None => []
  };

let state = ref(init());

let handleEffect =
  fun
  | Emit(socket, msg) =>
    ignore(WS.Socket.emit(socket, msg |> Message.encode |> Json.stringify))
  | Db(newState) => state := newState;

WS.run(
  ~port=7777,
  ~onConnection=socket => {
    open WS;
    Printf.eprintf("Got a connection!\n%!");
    socket
    |> Socket.setOnMessage((_, msg) =>
         handleStringMessage(socket, msg, state^) |> List.iter(handleEffect)
       )
    |> Socket.setOnDisconnect(() =>
         handleDisconnect(socket, state^) |> List.iter(handleEffect)
       );
  },
);