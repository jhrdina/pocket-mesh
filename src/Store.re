open BlackTea;

/* Constants */

let defaultSignalServerUrl = "ws://localhost:7777";

module Subs = {
  let createStateLogger = () => {
    let prevModelRef = ref(None);
    model => {
      let changed =
        switch (prevModelRef^) {
        | None => true
        | Some(prevModel) when prevModel !== model => true
        | Some(_prevModel) => false
        };
      if (changed) {
        Js.log(model);
      };
      Sub.none;
    };
  };
  /* let externalNotifier = model => model.Sub.none; */
};

/* Helpers */

let createDefaultPeerGroups = myId =>
  PeerGroups.empty
  |> PeerGroups.addPeerGroup(
       PeerGroup.make("aaa", myId)
       |> PeerGroup.addPeer({
            id: myId,
            permissions: {
              content: ReadWrite,
              membersList: ReadWrite,
            },
          }),
     );

let applyPeerStatusChanges = (onlinePeers, changes) =>
  List.fold_left(
    (prevOnlinePeers, change) =>
      /* TODO: Care only about those I have in my contacts */
      switch (change) {
      | Message.WentOnline(peerId) =>
        prevOnlinePeers |> PeerId.Set.add(peerId)
      | WentOffline(peerId) =>
        prevOnlinePeers |> PeerId.Set.filter(oldPeer => oldPeer !== peerId)
      },
    onlinePeers,
    changes,
  );

let cmdConnectToSignalServer = thisPeer =>
  SignalServerCmds.connect(
    defaultSignalServerUrl,
    thisPeer,
    Msgs.connectToSignalServerSuccess,
    Msgs.signalServerMessage,
    Msgs.signalServerConnectionError,
  );

exception InternalError;

let connectWaitingPeers = allPeers => {
  let waitingPeerIds =
    allPeers |> Peers.findAllIdsWithConnectionState(WaitingForOnlineSignal);

  let (peers, cmds) =
    PeerId.Set.fold(
      (peerId, (peers, cmds)) => {
        let peer =
          switch (peers |> Peers.findOpt(peerId)) {
          | Some(peer) => peer
          | None => raise(InternalError)
          };
        let newPeers =
          peers
          |> Peers.add(peerId, {...peer, connectionState: CreatingSdpOffer});
        let newCmds = [
          RTCCmds.createInitiator(
            peerId,
            Msgs.rtcOfferReady,
            Msgs.rtcConnected,
            Msgs.rtcGotData,
          ),
          ...cmds,
        ];
        (newPeers, newCmds);
      },
      waitingPeerIds,
      (allPeers, []),
    );
  (peers, Cmds.batch(cmds));
};

/* Updates */

let init: unit => (Types.rootState, BlackTea.Cmd.t(Msgs.t)) =
  () => (
    OpeningDB,
    IDBCmds.open_("pocketMesh", "all", Msgs.openDbSuccess, Msgs.dbFatalError),
  );

let update:
  (Types.rootState, Msgs.t) => (Types.rootState, BlackTea.Cmd.t(Msgs.t)) =
  model =>
    fun
    /********/
    /* Init */
    /********/
    | OpenDbSuccess(db) => (
        LoadingIdentity(db),
        db
        |> IDBCmds.getKey(
             "thisPeer",
             Msgs.loadIdentityFromDBSuccess,
             Msgs.dbFatalError,
           ),
      )
    | DbFatalError(_exn) => (
        DbFatalError("Cannot open database."),
        Cmds.none,
      )
    | LoadIdentityFromDBSuccess(maybeThisPeer) =>
      switch (model) {
      | LoadingIdentity(db) =>
        switch (maybeThisPeer) {
        | Some(thisPeer) => (
            HasIdentity({
              db,
              thisPeer,
              signalServerState: Connecting,
              /* TODO: Also load these from DB */
              peerGroups: createDefaultPeerGroups(thisPeer.id),
              /* TODO: Also load these from DB */
              peers: Peers.empty,
            }),
            /* TODO: Remove duplicate code */
            Cmd.batch([
              cmdConnectToSignalServer(thisPeer),
              /* DEBUG */
              Cmds.log("My ID: " ++ thisPeer.id),
              CryptoCmds.exportPublicKey(
                thisPeer.publicKey, Msgs.logMyPublicKey, _ =>
                Msgs.noop
              ),
            ]),
          )
        | None => (
            model,
            CryptoCmds.generateKeyPair(Msgs.myKeyPairGenSuccess, _ =>
              Msgs.noop
            ),
          )
        }
      | OpeningDB
      | DbFatalError(_)
      | HasIdentity(_) => (model, Cmds.none)
      }
    | MyKeyPairGenSuccess(keyPair) =>
      switch (model) {
      | LoadingIdentity(db) =>
        let thisPeer = {
          ThisPeer.id: keyPair.fingerprint,
          publicKey: keyPair.publicKey,
          privateKey: keyPair.privateKey,
        };
        (
          HasIdentity({
            db,
            thisPeer,
            signalServerState: Connecting,
            peerGroups: createDefaultPeerGroups(keyPair.fingerprint),
            peers: Peers.empty,
          }),
          Cmd.batch([
            cmdConnectToSignalServer(thisPeer),
            db
            |> IDBCmds.setKey(
                 "thisPeer",
                 thisPeer,
                 _ => Msgs.noop,
                 _ => Msgs.noop,
               ),
            /* DEBUG */
            Cmds.log("My ID: " ++ keyPair.fingerprint),
            CryptoCmds.exportPublicKey(
              keyPair.publicKey, Msgs.logMyPublicKey, _ =>
              Msgs.noop
            ),
          ]),
        );
      | OpeningDB
      | DbFatalError(_)
      | HasIdentity(_) => (model, Cmds.none)
      }

    /*****************************************/
    /* Peers & groups management, connecting */
    /*****************************************/
    | AddPeerToGroup(jwkStr, groupId) => (
        model,
        CryptoCmds.importAndFingerprintKey(
          jwkStr |> SimpleCrypto.stringToJwk,
          (id, key) =>
            Msgs.addPeerWithIdAndPublicKeyToGroup(id, key, groupId),
          _ => Msgs.noop,
        ),
      )

    | AddPeerWithIdAndPublicKeyToGroup(id, key, groupId) =>
      switch (model) {
      | HasIdentity(stateWithId) =>
        let peerGroups =
          stateWithId.peerGroups
          |> PeerGroups.update(groupId, group =>
               group
               |> PeerGroup.addPeer({
                    id,
                    /* TODO: Really? */
                    permissions: {
                      content: ReadWrite,
                      membersList: ReadWrite,
                    },
                  })
             );

        let (peersPeer, isNewPeer) =
          switch (stateWithId.peers |> Peers.findOpt(id)) {
          | Some(existingPeer) => (existingPeer, false)
          | None => (Peer.make(id, key), true)
          };
        let (newConnectionState, shouldConnect) =
          switch (peersPeer.connectionState, stateWithId.signalServerState) {
          | (NoNeedToConnect, Connected(_conn, onlinePeers))
              when PeerId.Set.mem(id, onlinePeers) => (
              Peer.CreatingSdpOffer,
              true,
            )
          | (NoNeedToConnect, Connected(_, _))
          | (NoNeedToConnect, _) => (WaitingForOnlineSignal, false)
          | (oldState, _) => (oldState, false)
          };
        let peersPeer = {...peersPeer, connectionState: newConnectionState};
        let peers = stateWithId.peers |> Peers.add(id, peersPeer);

        (
          HasIdentity({...stateWithId, peerGroups, peers}),
          Cmds.batch([
            switch (isNewPeer, stateWithId.signalServerState) {
            | (true, Connected(conn, _)) =>
              SignalServerCmds.sendMsg(
                ChangeWatchedPeers({
                  src: stateWithId.thisPeer.id,
                  watch: peers |> Peers.getAllIds,
                  /* TODO: Sign */
                  signature: "",
                }),
                conn,
              )
            | (
                true,
                Connecting | SigningIn(_) | FailedRetryingAt(_, _, _) |
                NoNetwork,
              )
            | (false, _) => Cmds.none
            },
            shouldConnect ?
              RTCCmds.createInitiator(
                id,
                Msgs.rtcOfferReady,
                Msgs.rtcConnected,
                Msgs.rtcGotData,
              ) :
              Cmds.none,
          ]),
        );
      | OpeningDB
      | DbFatalError(_)
      | LoadingIdentity(_) => (model, Cmds.none)
      }

    | ConnectToSignalServerSuccess(connection) => (
        switch (model) {
        | HasIdentity(stateWithId) =>
          HasIdentity({
            ...stateWithId,
            signalServerState: SigningIn(connection),
          })
        | OpeningDB
        | DbFatalError(_)
        | LoadingIdentity(_) => model
        },
        Cmds.none,
      )

    | SignalServerConnectionError =>
      switch (model) {
      | HasIdentity(stateWithId) =>
        let (newSignalServerState, attemptsMade) =
          switch (stateWithId.signalServerState) {
          | FailedRetryingAt(time, attemptsMade, lastErr) => (
              Types.FailedRetryingAt(time, attemptsMade + 1, lastErr),
              attemptsMade,
            )
          | _ => (FailedRetryingAt("", 1, ""), 1)
          };
        (
          HasIdentity({
            ...stateWithId,
            signalServerState: newSignalServerState,
          }),
          Cmds.timeout(
            Msgs.signalServerRetryConnection,
            SignalServerCmds.getRetryTimeoutMs(attemptsMade),
          ),
        );
      | OpeningDB
      | LoadingIdentity(_)
      | DbFatalError(_) => (model, Cmds.none)
      }

    | SignalServerRetryConnection =>
      switch (model) {
      | HasIdentity({
          thisPeer,
          signalServerState: FailedRetryingAt(_, _, _),
          _,
        }) => (
          model,
          cmdConnectToSignalServer(thisPeer),
        )
      | HasIdentity({
          signalServerState:
            Connecting | SigningIn(_) | Connected(_, _) | NoNetwork,
          _,
        })
      | OpeningDB
      | LoadingIdentity(_)
      | DbFatalError(_) => (model, Cmds.none)
      }

    | SignalServerMessage(connection, msg) =>
      switch (model) {
      | HasIdentity(stateWithId) =>
        switch (msg) {
        | Ok(onlinePeers) =>
          let (peers, connectPeersCmd) =
            connectWaitingPeers(stateWithId.peers);
          (
            HasIdentity({
              ...stateWithId,
              signalServerState: Connected(connection, onlinePeers),
              peers,
            }),
            connectPeersCmd,
          );
        | Offer(offer) =>
          /* Check that I have the src peer added */
          switch (stateWithId.peers |> Peers.findOpt(offer.src)) {
          | Some(_peer) =>
            /* TODO: Check signature */
            (
              model,
              RTCCmds.createAcceptor(
                offer.src,
                offer.sdp,
                Msgs.rtcAnswerReady,
                Msgs.rtcConnected,
                Msgs.rtcGotData,
              ),
            )
          | None => (
              model,
              Cmds.log("Ignoring SDP offer from unknown peer " ++ offer.src),
            )
          }
        | Answer(answer) =>
          /* Check that I have the src peer added */
          switch (stateWithId.peers |> Peers.findOpt(answer.src)) {
          | Some(peer) =>
            /* TODO: Check signature */
            switch (peer.connectionState) {
            | WaitingForSdpAnswer(rtcConn) => (
                model,
                RTCCmds.signal(rtcConn, answer.sdp),
              )
            | NoNeedToConnect
            | WaitingForOnlineSignal
            | CreatingSdpOffer
            | FailedRetryingAt(_, _, _)
            | Connected(_) => (
                model,
                Cmds.log(
                  "Got SDP answer from "
                  ++ answer.src
                  ++ " even though I'm not waiting for one.",
                ),
              )
            }
          | None => (
              model,
              Cmds.log(
                "Ignoring SDP answer from unknown peer " ++ answer.src,
              ),
            )
          }
        | WatchedPeersChanged(changes) =>
          switch (stateWithId.signalServerState) {
          | Connected(conn, onlinePeers) =>
            let (peers, connectPeersCmd) =
              connectWaitingPeers(stateWithId.peers);
            (
              HasIdentity({
                ...stateWithId,
                signalServerState:
                  Connected(
                    conn,
                    applyPeerStatusChanges(onlinePeers, changes),
                  ),
                peers,
              }),
              connectPeersCmd,
            );
          | _ => (model, Cmds.none)
          }
        | _ => (model, Cmds.log("Received unhandled Signal message"))
        }
      | OpeningDB
      | DbFatalError(_)
      | LoadingIdentity(_) => (model, Cmds.none)
      }

    | RtcAnswerReady(_rtcConn, sdp, initiatorId) =>
      switch (model) {
      | HasIdentity(stateWithId) =>
        switch (stateWithId.signalServerState) {
        | Connected(sigServConn, _) => (
            model,
            SignalServerCmds.sendMsg(
              Answer({
                src: stateWithId.thisPeer.id,
                tg: initiatorId,
                sdp,
                /* TODO: Sign message */
                signature: "",
              }),
              sigServConn,
            ),
          )
        | Connecting
        | SigningIn(_)
        | FailedRetryingAt(_, _, _)
        | NoNetwork => (model, Cmds.none)
        }

      | OpeningDB
      | DbFatalError(_)
      | LoadingIdentity(_) => (model, Cmds.none)
      }

    | RtcOfferReady(rtcConn, sdp, _acceptorId) =>
      /* TODO: Pair it with a specified acceptor ID */
      switch (model) {
      | HasIdentity(stateWithId) =>
        switch (stateWithId.signalServerState) {
        | Connected(sigServConn, _) =>
          switch (
            stateWithId.peers |> Peers.findByConnectionState(CreatingSdpOffer)
          ) {
          | Some(peer) => (
              HasIdentity({
                ...stateWithId,
                peers:
                  stateWithId.peers
                  |> Peers.add(
                       peer.id,
                       {
                         ...peer,
                         connectionState: WaitingForSdpAnswer(rtcConn),
                       },
                     ),
              }),
              SignalServerCmds.sendMsg(
                Offer({
                  src: stateWithId.thisPeer.id,
                  tg: peer.id,
                  sdp,
                  /* TODO: Sign message */
                  signature: "",
                }),
                sigServConn,
              ),
            )
          | None => (model, RTCCmds.destroy(rtcConn))
          }
        | Connecting
        | SigningIn(_)
        | FailedRetryingAt(_, _, _)
        | NoNetwork => (model, Cmds.none)
        }

      | OpeningDB
      | DbFatalError(_)
      | LoadingIdentity(_) => (model, Cmds.none)
      }

    | RtcConnected(rtcConn, peerId) =>
      switch (model) {
      | HasIdentity(stateWithId) =>
        switch (stateWithId.peers |> Peers.findOpt(peerId)) {
        | Some(peer) => (
            HasIdentity({
              ...stateWithId,
              peers:
                stateWithId.peers
                |> Peers.add(
                     peerId,
                     {...peer, connectionState: Connected(rtcConn)},
                   ),
            }),
            Cmds.log("Store: Peer " ++ peerId ++ "connected"),
          )
        | None => (
            model,
            Cmds.batch([
              Cmds.log(
                "Connected to peer, that was meanwhile removed. Destroying connection...",
              ),
              RTCCmds.destroy(rtcConn),
            ]),
          )
        }
      | OpeningDB
      | DbFatalError(_)
      | LoadingIdentity(_) => (model, Cmds.none)
      }

    | RtcGotData(_rtcConn, data) => (
        model,
        Cmds.log("Store: Got data: " ++ data),
      )

    /*********/
    /* Debug */
    /*********/

    | LogMyPublicKey(jwk) => (
        model,
        Cmds.log("My JWK: " ++ SimpleCrypto.jwkToString(jwk)),
      )
    | SendToPeer(id, msgStr) =>
      switch (model) {
      | HasIdentity(stateWithId) =>
        switch (stateWithId.peers |> Peers.findOpt(id)) {
        | Some(peer) =>
          switch (peer.connectionState) {
          | Connected(rtcConn) => (model, RTCCmds.send(rtcConn, msgStr))
          | NoNeedToConnect
          | WaitingForOnlineSignal
          | CreatingSdpOffer
          | WaitingForSdpAnswer(_)
          | FailedRetryingAt(_, _, _) => (
              model,
              Cmds.log(
                "Cannot send message to peer because connection is not established",
              ),
            )
          }
        | None => (model, Cmds.log("Cannot find public key for peer."))
        }
      | OpeningDB
      | DbFatalError(_)
      | LoadingIdentity(_) => (model, Cmds.none)
      }
    | Noop => (model, Cmds.none);

[@bs.set_index]
external makeGlobal: (Webapi.Dom.Window.t, string, 'a) => unit = "";
let makeGlobal = (name, value) => makeGlobal(Webapi.Dom.window, name, value);

let create = () => {
  let stateLogger = Subs.createStateLogger();
  let app =
    BlackTea.Store.create(
      ~init,
      ~update,
      ~subscriptions=model => stateLogger(model),
      ~shutdown=_model => Cmds.none,
    );

  makeGlobal("addFriend", jwk => app.pushMsg(AddPeerToGroup(jwk, "aaa")));
  makeGlobal("sendToPeer", (id, msg) => app.pushMsg(SendToPeer(id, msg)));
};

open Types;

let getMyId = model =>
  switch (model) {
  | HasIdentity(state) => Some(state.thisPeer.id)
  | OpeningDB
  | DbFatalError(_)
  | LoadingIdentity(_) => None
  };