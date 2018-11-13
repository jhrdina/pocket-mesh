[%%debugger.chrome];

open BlackTea;

/* TYPES */

type hasIdentity = {
  db: Db.t,
  thisPeer: ThisPeer.t,
  signalServerState: Types.signalServerState,
  peerGroups: Types.peerGroups,
  peers: Peers.t,
};

type rootState =
  | OpeningDB
  | LoadingDBData(Db.t)
  | GeneratingIdentity(Db.t, Db.allData)
  | FatalError(exn)
  | HasIdentity(hasIdentity);

exception CannotOpenDatabase;
exception CannotCreateIdentity;

/* HELPERS */

let logIdAndJWT = (thisPeer: ThisPeer.t) =>
  Cmds.wrapPromise(
    () =>
      SimpleCrypto.publicKeyToJwk(thisPeer.publicKey)
      |> Js.Promise.then_(jwk => {
           Js.log("My ID: " ++ thisPeer.id);
           Js.log("My JWK: " ++ SimpleCrypto.jwkToString(jwk));
           Js.Promise.resolve();
         }),
    _ => Msgs.noop,
    _ => Msgs.noop,
  );

/* UPDATE */

let initStateWithId = (db, thisPeer: ThisPeer.t, allDbData: Db.allData) => {
  let (peerGroups, peerGroupsCmd) =
    PeerGroups.init(db, thisPeer.id, allDbData.peerGroups);
  let (peers, peersCmd) =
    Peers.init(
      db,
      peerGroups,
      SignalServerState.initialModel,
      allDbData.peers,
    );
  let (ssState, ssStateCmd) = SignalServerState.init(thisPeer, peers);
  (
    {db, thisPeer, signalServerState: ssState, peerGroups, peers},
    Cmds.batch([ssStateCmd, peersCmd, peerGroupsCmd]),
  );
};

let updateStateWithId = (model, msg) => {
  let cryptoCmd =
    switch (msg, model) {
    | (Msgs.AddPeerToGroup(jwkStr, groupId), _) =>
      CryptoCmds.importAndFingerprintKey(
        jwkStr |> SimpleCrypto.stringToJwk,
        (id, key) => Msgs.addPeerWithIdAndPublicKeyToGroup(id, key, groupId),
        _ => Msgs.noop,
      )
    | _ => Cmds.none
    };
  let debugCmd =
    switch (msg) {
    | Msgs.OfferChanges =>
      Cmds.batch(
        Peers.foldActiveConnections(
          (peerId, rtcConn, cmdList) => {
            let groupsStatuses =
              PeerGroups.getGroupsStatusesForPeer(peerId, model.peerGroups);
            let msgForPeer = P2PMsg.ChangesOffer(groupsStatuses);
            [
              rtcConn->RTCCmds.send(
                msgForPeer |> P2PMsg.encode |> Json.stringify,
              ),
              ...cmdList,
            ];
          },
          model.peers,
          [],
        ),
      )
    | RtcGotData(_rtcConn, peerId, data) =>
      Cmds.log("Store: Got data from " ++ peerId ++ ": " ++ data)
    | _ => Cmds.none
    };
  let (ssState, ssStateCmd) =
    SignalServerState.update(
      model.thisPeer,
      model.peers,
      model.signalServerState,
      msg,
    );
  let (peers, peersCmd) =
    Peers.update(
      model.db,
      model.thisPeer,
      model.signalServerState,
      model.peers,
      msg,
    );
  let (peerGroups, peerGroupsCmd) =
    PeerGroups.update(model.db, model.peerGroups, msg);
  (
    {
      db: model.db,
      thisPeer: model.thisPeer,
      signalServerState: ssState,
      peerGroups,
      peers,
    },
    Cmd.batch([ssStateCmd, peersCmd, cryptoCmd, peerGroupsCmd, debugCmd]),
  );
};

let init: unit => (rootState, BlackTea.Cmd.t(Msgs.t)) =
  () => (OpeningDB, Db.open_(Msgs.openDbSuccess, Msgs.dbFatalError));

let update: (rootState, Msgs.t) => (rootState, BlackTea.Cmd.t(Msgs.t)) =
  (model, msg) =>
    switch (msg, model) {
    /********/
    /* Init */
    /********/
    | (OpenDbSuccess(db), OpeningDB) => (
        LoadingDBData(db),
        Db.getAll(db, Msgs.loadDataFromDBSuccess, Msgs.dbFatalError),
      )

    | (DbFatalError(_exn), _) => (
        FatalError(CannotOpenDatabase),
        Cmds.none,
      )

    | (LoadDataFromDBSuccess(allDbData), LoadingDBData(db)) =>
      switch (allDbData.thisPeer) {
      | Some(thisPeer) =>
        let (stateWithId, stateWithIdCmd) =
          initStateWithId(db, thisPeer, allDbData);
        (
          HasIdentity(stateWithId),
          Cmds.batch([stateWithIdCmd, logIdAndJWT(thisPeer)]),
        );
      | None => (
          GeneratingIdentity(db, allDbData),
          CryptoCmds.generateKeyPair(
            Msgs.myKeyPairGenSuccess,
            Msgs.myKeyPairGenError,
          ),
        )
      }

    | (MyKeyPairGenSuccess(keyPair), GeneratingIdentity(db, allDbData)) =>
      let thisPeer = {
        ThisPeer.id: keyPair.fingerprint,
        publicKey: keyPair.publicKey,
        privateKey: keyPair.privateKey,
      };
      let (stateWithId, stateWithIdCmd) =
        initStateWithId(db, thisPeer, allDbData);
      (
        HasIdentity(stateWithId),
        Cmd.batch([
          stateWithIdCmd,
          db
          |> Db.setThisPeer(
               {
                 ThisPeer.id: keyPair.fingerprint,
                 publicKey: keyPair.publicKey,
                 privateKey: keyPair.privateKey,
               },
               _ => Msgs.noop,
               _ => Msgs.noop,
             ),
          /* Debug */
          logIdAndJWT(thisPeer),
        ]),
      );

    | (MyKeyPairGenError(_exn), GeneratingIdentity(_)) => (
        FatalError(CannotCreateIdentity),
        Cmds.none,
      )

    /*****************************************/
    /* Peers & groups management, connecting */
    /*****************************************/

    | (RtcError(_rtcConn, peerId, msg), _) => (
        model,
        Cmds.log("RTC Error (peer " ++ peerId ++ "): " ++ msg),
      )

    /*********/
    /* Debug */
    /*********/
    | (SendToPeer(id, msgStr), HasIdentity({peers})) =>
      switch (peers |> Peers.findOpt(id)) {
      | Some({connectionState: Connected(rtcConn, _, _), _}) => (
          model,
          RTCCmds.send(rtcConn, msgStr),
        )
      | Some({connectionState: _, _}) => (
          model,
          Cmds.log(
            "Cannot send message to peer because connection is not established",
          ),
        )
      | None => (
          model,
          Cmds.log("Peer " ++ id ++ " is not added in friends."),
        )
      }
    | (msg, HasIdentity(stateWithId)) =>
      let (newStateWithId, cmd) = updateStateWithId(stateWithId, msg);
      (HasIdentity(newStateWithId), cmd);
    | (Noop, _) => (model, Cmds.none)
    | (_, _) => (model, Cmds.none)
    };

[@bs.set_index]
external makeGlobal: (Webapi.Dom.Window.t, string, 'a) => unit = "";
let makeGlobal = (name, value) => makeGlobal(Webapi.Dom.window, name, value);

let create = () => {
  /* let stateLogger = StateLogger.create(); */
  let app =
    BlackTea.Store.create(
      ~init,
      ~update,
      /* ~subscriptions=model => stateLogger(model), */
      ~subscriptions=
        model => {
          makeGlobal("model", model);
          Sub.none;
        },
      ~shutdown=_model => Cmds.none,
    );

  makeGlobal("addFriend", jwk => app.pushMsg(AddPeerToGroup(jwk, "aaa")));
  makeGlobal("sendToPeer", (id, msg) => app.pushMsg(SendToPeer(id, msg)));
  makeGlobal("offerChanges", () => app.pushMsg(OfferChanges));
  makeGlobal("addPeerToGroupWithPerms", (peerId, groupId, perms) =>
    app.pushMsg(AddPeerToGroupWithPerms(peerId, groupId, perms))
  );
  makeGlobal("addItem", text => app.pushMsg(AddItem(text)));
  makeGlobal("printData", () => app.pushMsg(PrintData));
};

let getMyId = model =>
  switch (model) {
  | HasIdentity(state) => Some(state.thisPeer.id)
  | OpeningDB
  | FatalError(_)
  | LoadingDBData(_)
  | GeneratingIdentity(_) => None
  };