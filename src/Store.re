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

let init: unit => (Types.rootState, BlackTea.Cmd.t(Msgs.t)) =
  () => (
    OpeningDB,
    IDBCmds.open_("pocketMesh", "all", Msgs.openDbSuccess, _ => Msgs.noop),
  );

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
        |> IDBCmds.getKey("thisPeer", Msgs.loadIdentityFromDBSuccess, _ =>
             Msgs.noop
           ),
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
              SignalServerCmds.connect(
                defaultSignalServerUrl,
                thisPeer,
                Msgs.connectToSignalServerSuccess,
                Msgs.signalServerMessage,
                () =>
                Msgs.noop
              ),
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
            SignalServerCmds.connect(
              defaultSignalServerUrl,
              thisPeer,
              Msgs.connectToSignalServerSuccess,
              Msgs.signalServerMessage,
              () =>
              Msgs.noop
            ),
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
        let (peersNewPeer, shouldConnect) =
          switch (stateWithId.peers |> Peers.findOpt(id)) {
          | None =>
            let (newConnectionState, shouldConnect) =
              switch (stateWithId.signalServerState) {
              | NoNetwork
              | Connecting
              | SigningIn(_)
              | FailedRetryingAt(_, _, _) => (
                  Peer.WaitingForOnlineSignal,
                  false,
                )
              | Connected(_conn, _onlinePeers) =>
                /* TODO */
                /* PeerId.Set.mem(id, onlinePeers) ? */
                (CreatingSdpOffer, true) /* :
                  (WaitingForOnlineSignal, false)*/
              };
            (
              {
                Peer.id,
                publicKey: key,
                nickName: "",
                connectionState: newConnectionState,
              },
              shouldConnect,
            );
          | Some(existingPeer) =>
            let (newConnectionState, shouldConnect) =
              switch (
                existingPeer.connectionState,
                stateWithId.signalServerState,
              ) {
              | (NoNeedToConnect, Connected(_conn, _onlinePeers)) =>
                /* TODO */
                /* PeerId.Set.mem(id, onlinePeers) ? */
                (Peer.CreatingSdpOffer, true) /*:
                  (WaitingForOnlineSignal, false)*/

              | (oldState, _) => (oldState, false)
              };
            (
              {...existingPeer, connectionState: newConnectionState},
              shouldConnect,
            );
          };
        (
          HasIdentity({
            ...stateWithId,
            peerGroups:
              stateWithId.peerGroups
              |> PeerGroups.update(groupId, group =>
                   group
                   |> PeerGroup.addPeer({
                        id,
                        permissions: {
                          content: ReadWrite,
                          membersList: ReadWrite,
                        },
                      })
                 ),
            peers: stateWithId.peers |> Peers.add(id, peersNewPeer),
          }),
          if (shouldConnect) {
            Js.log("aaa");
            RTCCmds.createInitiator(
              id,
              Msgs.rtcOfferReady,
              Msgs.rtcConnected,
              Msgs.rtcGotData,
            );
          } else {
            Cmds.none;
          },
        );
      | OpeningDB
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
        | LoadingIdentity(_) => model
        },
        Cmds.none,
      )

    | SignalServerMessage(connection, msg) =>
      switch (model) {
      | HasIdentity(stateWithId) =>
        switch (msg) {
        | Ok(onlinePeers) => (
            HasIdentity({
              ...stateWithId,
              signalServerState: Connected(connection, onlinePeers),
            }),
            /* TODO: initialize connection for peers that are waiting for online signal */
            Cmds.none,
          )
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
          | Connected(conn, onlinePeers) => (
              HasIdentity({
                ...stateWithId,
                signalServerState:
                  Connected(
                    conn,
                    List.fold_left(
                      (prevOnlinePeers, change) =>
                        /* TODO: Care only about those I have in my contacts */
                        switch (change) {
                        | Message.WentOnline(peerId) =>
                          prevOnlinePeers |> PeerId.Set.add(peerId)
                        | WentOffline(peerId) =>
                          prevOnlinePeers
                          |> PeerId.Set.filter(oldPeer => oldPeer !== peerId)
                        },
                      onlinePeers,
                      changes,
                    ),
                  ),
              }),
              Cmds.none,
            )
          | _ => (model, Cmds.none)
          }
        | _ => (model, Cmds.log("Received unhandled Signal message"))
        }
      | OpeningDB
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
  | LoadingIdentity(_) => None
  };