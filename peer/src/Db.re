open BlackTea;
open Json.Infix;

/* CONSTANTS */
let dbName = "pocketMesh";

let peersGroupsKey = "peersGroups";
let thisPeerKey = "thisPeer";
let peersKey = "peers";

type openedDb = IDBPromise.t;

type allData = {
  thisPeer: option(ThisPeer.t),
  peersGroups: option(PeersGroups.t),
  peers: option(Peers.t),
};

type loaded =
  | WithoutDb
  // db, lastDbStateForDiffs
  | WithDb(openedDb, allData);

type t =
  | Opening
  | LoadingData(openedDb)
  | GeneratingIdentity(loaded, allData)
  | Loaded(loaded)
  | FatalError(exn);

exception CannotOpenDatabase;
exception CannotCreateIdentity;
exception CannotSave(list(exn));

/* MESSAGES */

type Msgs.t +=
  // Internal
  | CompletedOpenDb(Result.t(openedDb, exn))
  | CompletedLoadDataFromDb(Result.t(allData, exn))
  | CompletedKeyPairGeneration(Result.t(SimpleCrypto.keyPair, exn))
  | CompletedSaveData(DbState.t, Result.t(unit, exn))
  // Output
  | InitializationComplete(DbState.t);

let completedOpenDb = res => CompletedOpenDb(res);
let completedLoadDataFromDb = allData => CompletedLoadDataFromDb(allData);
let completedKeyPairGeneration = r => CompletedKeyPairGeneration(r);

// HELPERS

let thisPeerOfKeyPair = (keyPair: SimpleCrypto.keyPair) =>
  PeerId.ofString(keyPair.fingerprint)
  |?>> (
    thisPeerId => {
      ThisPeer.id: thisPeerId,
      publicKey: keyPair.publicKey,
      privateKey: keyPair.privateKey,
    }
  );

/* COMMANDS */

let open_ = resultToMsg =>
  Cmds.fromPromise(() => IDBPromise.open_(dbName, "all"), resultToMsg);

let getAll = db =>
  Cmds.fromPromise(() =>
    Js.Promise.all3((
      db |> IDBPromise.getKey(thisPeerKey),
      db |> IDBPromise.getKey(peersGroupsKey),
      db |> IDBPromise.getKey(peersKey),
    ))
    |> Js.Promise.then_(((thisPeer, peersGroups, peers)) =>
         Js.Promise.resolve({
           thisPeer,
           peersGroups: peersGroups |?> PeersGroups.decode,
           peers,
         })
       )
  );

let setThisPeer = (thisPeer: ThisPeer.t, db) =>
  IDBPromise.setKey(thisPeerKey, thisPeer, db);

let setPeers = (peers: Peers.t, db) =>
  IDBPromise.setKey(peersKey, peers, db);

let setPeersGroups = (peersGroups: PeersGroups.t, db) =>
  IDBPromise.setKey(peersGroupsKey, peersGroups |> PeersGroups.encode, db);

let someEqual = (aOpt, b) =>
  switch (aOpt) {
  | Some(a) => a === b
  | None => false
  };

let writeRequestToPromise = (request, setter, db) =>
  switch (request) {
  | Some(data) => setter(data, db)
  | None => JsUtils.emptyPromise()
  };

let saveDbStateIfChanged = (dbState: DbState.t, lastAllData, db) => {
  let writeRequests = {
    thisPeer:
      !someEqual(lastAllData.thisPeer, dbState.thisPeer) ?
        Some(dbState.thisPeer) : None,
    peersGroups:
      !someEqual(lastAllData.peersGroups, dbState.peersGroups) ?
        Some(dbState.peersGroups) : None,
    peers:
      !someEqual(lastAllData.peers, dbState.peers) ?
        Some(dbState.peers) : None,
  };

  let writeNeeded =
    writeRequests.thisPeer != None
    || writeRequests.peersGroups != None
    || writeRequests.peers != None;

  // DEBUG
  // if (writeNeeded) {
  //   Js.log("[Db] Writing to DB");
  // };

  let writeCmd =
    if (writeNeeded) {
      Cmds.fromPromise(
        () =>
          Js.Promise.all([|
            writeRequestToPromise(writeRequests.thisPeer, setThisPeer, db),
            writeRequestToPromise(
              writeRequests.peersGroups,
              setPeersGroups,
              db,
            ),
            writeRequestToPromise(writeRequests.peers, setPeers, db),
          |]),
        res => CompletedSaveData(dbState, res |> Result.mapOk(_ => ())),
      );
    } else {
      Cmd.none;
    };

  (
    {
      thisPeer: Some(dbState.thisPeer),
      peersGroups: Some(dbState.peersGroups),
      peers: Some(dbState.peers),
    },
    writeCmd,
  );
};

let initStateWithIdentity = (~allData, ~thisPeer, ~initContent) => {
  // We need to detect state when some data has to be repaired
  DbState.thisPeer,
  peersGroups:
    PeersGroups.(allData.peersGroups |? init(~thisPeer, ~initContent)),
  peers: allData.peers |? Peers.init(),
};

let allDataEmpty = {thisPeer: None, peersGroups: None, peers: None};

let finishInit = (~db, ~allData, ~thisPeer, ~initContent) => {
  let dbState = initStateWithIdentity(~allData, ~thisPeer, ~initContent);
  (
    Loaded(db),
    Some(dbState),
    Cmd.batch([
      Cmds.log("My ID: " ++ (thisPeer.id |> PeerId.toString)),
      Cmd.msg(InitializationComplete(dbState)),
    ]),
  );
};

// UPDATE

let init = () => (Opening, open_(completedOpenDb));

let update = (~dbState, ~initContent, msg, model) => {
  let (model, dbState, cmd) =
    switch (msg, model) {
    | (CompletedOpenDb(Ok(db)), Opening) => (
        LoadingData(db),
        None,
        getAll(db, completedLoadDataFromDb),
      )
    | (CompletedOpenDb(Error(_exn)), Opening | LoadingData(_)) => (
        GeneratingIdentity(WithoutDb, allDataEmpty),
        None,
        CryptoCmds.generateKeyPair(completedKeyPairGeneration),
      )

    | (CompletedLoadDataFromDb(Ok(allData)), LoadingData(db)) =>
      switch (allData.thisPeer) {
      | Some(thisPeer) =>
        finishInit(
          ~db=WithDb(db, allData),
          ~allData,
          ~thisPeer,
          ~initContent,
        )
      | None => (
          GeneratingIdentity(WithDb(db, allData), allData),
          None,
          CryptoCmds.generateKeyPair(completedKeyPairGeneration),
        )
      }

    | (CompletedLoadDataFromDb(Error(_exn)), LoadingData(_)) => (
        GeneratingIdentity(WithoutDb, allDataEmpty),
        None,
        Cmds.batch([
          Cmds.log("Error loading DB data"),
          CryptoCmds.generateKeyPair(completedKeyPairGeneration),
        ]),
      )
    | (
        CompletedKeyPairGeneration(Ok(keyPair)),
        GeneratingIdentity(mDb, allData),
      ) =>
      switch (thisPeerOfKeyPair(keyPair)) {
      | Some(thisPeer) =>
        finishInit(~db=mDb, ~allData, ~thisPeer, ~initContent)
      | None => (FatalError(CannotCreateIdentity), None, Cmds.none)
      }

    | (CompletedKeyPairGeneration(Error(exn)), LoadingData(_)) => (
        FatalError(exn),
        None,
        Cmds.none,
      )
    | (_, _) => (model, dbState, Cmd.none)
    };

  let (model, derivedCmd) =
    switch (model, dbState) {
    | (Loaded(WithDb(db, lastAllData)), Some(dbState)) =>
      let (allData, saveCmd) =
        saveDbStateIfChanged(dbState, lastAllData, db);
      (Loaded(WithDb(db, allData)), saveCmd);
    | _ => (model, Cmd.none)
    };

  (model, Cmd.batch([cmd, derivedCmd]));
};