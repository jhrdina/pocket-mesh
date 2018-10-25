open BlackTea;

/* CONSTANTS */
let dbName = "pocketMesh";

/* TYPES */

type hasIdentity = {
  db: IDBCmds.t,
  thisPeer: ThisPeer.t,
  signalServerState: Types.signalServerState,
  peerGroups: PeerGroups.t,
  peers: Peers.t,
};

type rootState =
  | OpeningDB
  | LoadingIdentity(IDBCmds.t)
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
    () => Msgs.noop,
    _ => Msgs.noop,
  );

/* UPDATE */

let initStateWithId = (db, thisPeer) => {
  let (peers, peersCmd) = Peers.init();
  let (ssState, ssStateCmd) = SignalServerState.init(thisPeer, peers);
  (
    {
      db,
      thisPeer,
      signalServerState: ssState,
      peerGroups: PeerGroups.init(thisPeer.id),
      peers,
    },
    Cmds.batch([ssStateCmd, peersCmd]),
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
  let (ssState, ssStateCmd) =
    SignalServerState.update(
      model.thisPeer,
      model.peers,
      model.signalServerState,
      msg,
    );
  let (peers, peersCmd) =
    Peers.update(model.thisPeer, model.signalServerState, model.peers, msg);
  (
    {
      db: model.db,
      thisPeer: model.thisPeer,
      signalServerState: ssState,
      peerGroups: PeerGroups.update(model.peerGroups, msg),
      peers,
    },
    Cmd.batch([ssStateCmd, peersCmd, cryptoCmd]),
  );
};

let init: unit => (rootState, BlackTea.Cmd.t(Msgs.t)) =
  () => (
    OpeningDB,
    IDBCmds.open_(dbName, "all", Msgs.openDbSuccess, Msgs.dbFatalError),
  );

let update: (rootState, Msgs.t) => (rootState, BlackTea.Cmd.t(Msgs.t)) =
  (model, msg) =>
    switch (msg, model) {
    /********/
    /* Init */
    /********/
    | (OpenDbSuccess(db), OpeningDB) => (
        LoadingIdentity(db),
        Cmds.batch([
          db
          |> IDBCmds.getKey(
               "thisPeer",
               Msgs.loadIdentityFromDBSuccess,
               Msgs.dbFatalError,
             ),
          /* db
             |> IDBCmds.getKey(
                  "peers",
                  Msgs.loadPeersFromDBSuccess,
                  Msgs.dbFatalError,
                ), */
        ]),
      )

    | (DbFatalError(_exn), _) => (
        FatalError(CannotOpenDatabase),
        Cmds.none,
      )

    | (LoadIdentityFromDBSuccess(maybeThisPeer), LoadingIdentity(db)) =>
      switch (maybeThisPeer) {
      | Some(thisPeer) =>
        let (stateWithId, stateWithIdCmd) = initStateWithId(db, thisPeer);
        (
          HasIdentity(stateWithId),
          Cmds.batch([stateWithIdCmd, logIdAndJWT(thisPeer)]),
        );
      | None => (
          LoadingIdentity(db),
          CryptoCmds.generateKeyPair(
            Msgs.myKeyPairGenSuccess,
            Msgs.myKeyPairGenError,
          ),
        )
      }

    | (MyKeyPairGenSuccess(keyPair), LoadingIdentity(db)) =>
      let thisPeer = {
        ThisPeer.id: keyPair.fingerprint,
        publicKey: keyPair.publicKey,
        privateKey: keyPair.privateKey,
      };
      let (stateWithId, stateWithIdCmd) = initStateWithId(db, thisPeer);
      (
        HasIdentity(stateWithId),
        Cmd.batch([
          stateWithIdCmd,
          db
          |> IDBCmds.setKey(
               "thisPeer",
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

    | (MyKeyPairGenError(_exn), LoadingIdentity(_db)) => (
        FatalError(CannotCreateIdentity),
        Cmds.none,
      )

    /*****************************************/
    /* Peers & groups management, connecting */
    /*****************************************/

    | (RtcGotData(_rtcConn, data), _) => (
        model,
        Cmds.log("Store: Got data: " ++ data),
      )

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
  let stateLogger = StateLogger.create();
  let app =
    BlackTea.Store.create(
      ~init,
      ~update,
      /* ~subscriptions=model => stateLogger(model), */
      ~subscriptions=_ => Sub.none,
      ~shutdown=_model => Cmds.none,
    );

  makeGlobal("addFriend", jwk => app.pushMsg(AddPeerToGroup(jwk, "aaa")));
  makeGlobal("sendToPeer", (id, msg) => app.pushMsg(SendToPeer(id, msg)));
};

let getMyId = model =>
  switch (model) {
  | HasIdentity(state) => Some(state.thisPeer.id)
  | OpeningDB
  | FatalError(_)
  | LoadingIdentity(_) => None
  };