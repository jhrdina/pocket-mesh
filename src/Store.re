/* TODO: Remove for production */
[%%debugger.chrome];

open BlackTea;
open Json.Infix;

/* CONSTANTS */

let defaultSignalServerUrl = "ws://localhost:7777";

/* MODULES */

module InitConfig = {
  type t = {
    contentInitializer: PeerGroup.AM.t => PeerGroup.AM.t,
    signalServerUrl: string,
  };

  let make =
      (
        ~contentInitializer=crdt => crdt,
        ~signalServerUrl=defaultSignalServerUrl,
        (),
      ) => {
    contentInitializer,
    signalServerUrl,
  };
};

/* TYPES */

type hasIdentity = {
  db: Db.t,
  thisPeer: ThisPeer.t,
  signalServer: SignalServerState.t,
  peerGroups: Types.peerGroups,
  peers: Peers.t,
  offerChangesDebouncer: Debouncer.t(Msgs.t),
};

type rootState =
  | OpeningDB(InitConfig.t)
  | LoadingDBData(InitConfig.t, Db.t)
  | GeneratingIdentity(InitConfig.t, Db.t, Db.allData)
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
           Js.log("My ID: " ++ (thisPeer.id |> PeerId.toString));
           Js.log("My JWK: " ++ SimpleCrypto.jwkToString(jwk));
           Js.Promise.resolve();
         }),
    _ => Msgs.noop,
    _ => Msgs.noop,
  );

/* UPDATE */

let initStateWithId =
    (
      db,
      thisPeer: ThisPeer.t,
      allDbData: Db.allData,
      {contentInitializer, signalServerUrl}: InitConfig.t,
    ) => {
  let (peerGroups, peerGroupsCmd) =
    PeerGroups.init(
      db,
      thisPeer.id,
      allDbData.peerGroups,
      contentInitializer,
    );
  let (peers, peersCmd) =
    Peers.init(
      db,
      peerGroups,
      SignalServerState.initialConnectionState,
      allDbData.peers,
    );
  let (signalServer, signalServerCmd) =
    SignalServerState.init(thisPeer, peers, signalServerUrl);
  let offerChangesDebouncer = Debouncer.init();
  (
    {db, thisPeer, signalServer, peerGroups, peers, offerChangesDebouncer},
    Cmds.batch([signalServerCmd, peersCmd, peerGroupsCmd]),
  );
};

let updateStateWithId = (model, msg) => {
  /* let cryptoCmd =
     switch (msg, model) {
     | (Msgs.AddPeerToGroup(jwkStr, groupId), _) =>
       CryptoCmds.importAndFingerprintKey(
         jwkStr |> SimpleCrypto.stringToJwk,
         (id, key) => Msgs.addPeerWithIdAndPublicKeyToGroup(id, key, groupId),
         _ => Msgs.noop,
       )
     | _ => Cmds.none
     }; */
  let debugCmd =
    switch (msg) {
    | Msgs.OfferChangesFromGroupsDebounced(_) =>
      Peers.foldActiveConnections(
        (cmdList, peerId, rtcConn) => {
          let groupsStatuses =
            PeerGroups.getGroupsStatusesForPeer(peerId, model.peerGroups);
          let msgForPeer = P2PMsg.ChangesOffer(groupsStatuses);
          [
            rtcConn->RTCCmds.send(
              String(msgForPeer |> P2PMsg.encode |> Json.stringify),
            ),
            ...cmdList,
          ];
        },
        [],
        model.peers,
      )
      |> Cmds.batch
    | RtcGotData(_rtcConn, peerId, String(data)) =>
      Cmds.log(
        "Store: Got data from " ++ (peerId |> PeerId.toString) ++ ": " ++ data,
      )
    | _ => Cmds.none
    };
  let (signalServer, signalServerCmd) =
    SignalServerState.update(
      model.thisPeer,
      model.peers,
      model.signalServer,
      msg,
    );
  let (peers, peersCmd) =
    Peers.update(
      model.db,
      model.thisPeer,
      model.signalServer.connectionState,
      model.peerGroups,
      model.peers,
      msg,
    );
  let (peerGroups, peerGroupsCmd) =
    PeerGroups.update(model.db, model.thisPeer.id, model.peerGroups, msg);
  let (offerChangesDebouncer, offerChangesDebouncerCmd) =
    switch (msg) {
    | OfferChangesDebouncerMsg(msg) =>
      Debouncer.update(
        d => d,
        model.offerChangesDebouncer,
        msg,
        Msgs.offerChangesDebouncerMsg,
      )
    | _ => (model.offerChangesDebouncer, Cmds.none)
    };
  (
    {
      db: model.db,
      thisPeer: model.thisPeer,
      signalServer,
      peerGroups,
      peers,
      offerChangesDebouncer,
    },
    Cmd.batch([
      signalServerCmd,
      peersCmd,
      peerGroupsCmd,
      debugCmd,
      offerChangesDebouncerCmd,
    ]),
  );
};

let init: InitConfig.t => (rootState, BlackTea.Cmd.t(Msgs.t)) =
  initConfig => (
    OpeningDB(initConfig),
    Db.open_(Msgs.openDbSuccess, Msgs.dbFatalError),
  );

let update: (rootState, Msgs.t) => (rootState, BlackTea.Cmd.t(Msgs.t)) =
  (model, msg) =>
    switch (msg, model) {
    /********/
    /* Init */
    /********/
    | (OpenDbSuccess(db), OpeningDB(initConfig)) => (
        LoadingDBData(initConfig, db),
        Db.getAll(db, Msgs.loadDataFromDBSuccess, Msgs.dbFatalError),
      )

    | (DbFatalError(_exn), _) => (
        FatalError(CannotOpenDatabase),
        Cmds.none,
      )

    | (LoadDataFromDBSuccess(allDbData), LoadingDBData(initConfig, db)) =>
      switch (allDbData.thisPeer) {
      | Some(thisPeer) =>
        let (stateWithId, stateWithIdCmd) =
          initStateWithId(db, thisPeer, allDbData, initConfig);
        (
          HasIdentity(stateWithId),
          Cmds.batch([stateWithIdCmd, logIdAndJWT(thisPeer)]),
        );
      | None => (
          GeneratingIdentity(initConfig, db, allDbData),
          CryptoCmds.generateKeyPair(
            Msgs.myKeyPairGenSuccess,
            Msgs.myKeyPairGenError,
          ),
        )
      }

    | (
        MyKeyPairGenSuccess(keyPair),
        GeneratingIdentity(initConfig, db, allDbData),
      ) =>
      switch (PeerId.ofString(keyPair.fingerprint)) {
      | Some(thisPeerId) =>
        let thisPeer = {
          ThisPeer.id: thisPeerId,
          publicKey: keyPair.publicKey,
          privateKey: keyPair.privateKey,
        };
        let (stateWithId, stateWithIdCmd) =
          initStateWithId(db, thisPeer, allDbData, initConfig);
        (
          HasIdentity(stateWithId),
          Cmd.batch([
            stateWithIdCmd,
            db |> Db.setThisPeer(thisPeer, _ => Msgs.noop, _ => Msgs.noop),
            /* Debug */
            logIdAndJWT(thisPeer),
          ]),
        );
      | None => (FatalError(CannotCreateIdentity), Cmds.none)
      }

    | (MyKeyPairGenError(_exn), GeneratingIdentity(_)) => (
        FatalError(CannotCreateIdentity),
        Cmds.none,
      )

    /*****************************************/
    /* Peers & groups management, connecting */
    /*****************************************/

    | (RtcError(_rtcConn, peerId, msg), _) => (
        model,
        Cmds.log(
          "RTC Error (peer " ++ (peerId |> PeerId.toString) ++ "): " ++ msg,
        ),
      )

    /*********/
    /* Debug */
    /*********/
    | (SendToPeer(strId, msgStr), HasIdentity({peers})) =>
      switch (PeerId.ofString(strId) |?> (id => peers |> Peers.findOpt(id))) {
      | Some({connectionState: Connected(rtcConn, _, _), _}) => (
          model,
          RTCCmds.send(rtcConn, String(msgStr)),
        )
      | Some({connectionState: _, _}) => (
          model,
          Cmds.log(
            "Cannot send message to peer because connection is not established",
          ),
        )
      | None => (
          model,
          Cmds.log(
            "Peer '" ++ strId ++ "' has invalid ID or is not added in friends.",
          ),
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
  let initConfig =
    InitConfig.make(
      ~contentInitializer=
        crdt =>
          crdt
          |> PeerGroup.AM.change("Init", root =>
               PeerGroup.AM.Json.(
                 root |> Map.add("items", List.create() |> List.toJson)
               )
             ),
      (),
    );
  let app =
    BlackTea.Store.create(
      ~init=() => init(initConfig),
      ~update,
      /* ~subscriptions=model => stateLogger(model), */
      ~subscriptions=
        model => {
          makeGlobal("model", model);
          Sub.none;
        },
      ~shutdown=_model => Cmds.none,
    );

  makeGlobal("addPeer", (id, alias) => app.pushMsg(AddPeer(id, alias)));
  makeGlobal("addPeerToGroup", id =>
    app.pushMsg(
      AddPeerToGroup(
        id,
        PeerGroups.defaultGroupId,
        PeerGroup.ReadContentAndMembers,
      ),
    )
  );
  makeGlobal("sendToPeer", (id, msg) => app.pushMsg(SendToPeer(id, msg)));
  makeGlobal("addPeerToGroupWithPerms", (peerId, groupId, permsStr) =>
    switch (permsStr |> PeerGroup.decodePermissions) {
    | Some(perms) => app.pushMsg(AddPeerToGroup(peerId, groupId, perms))
    | None => Js.log("Invalid permissions, try crmr, crwmr, or crwmrw")
    }
  );
};