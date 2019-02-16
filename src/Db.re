open BlackTea;
open Rex_json.Json.Infix;

/* CONSTANTS */
let dbName = "pocketMesh";

let peersGroupsKey = "peersGroups";
let thisPeerKey = "thisPeer";
let peersKey = "peers";

type openedDb = IDBPromise.t;

type allData = {
  thisPeer: option(ThisPeer.t),
  peersGroups: option(Json.t),
  peers: option(Peers.t),
};

type t =
  | Opening
  | LoadingData(openedDb)
  | GeneratingIdentity(option(openedDb), allData)
  | Loaded(option(openedDb))
  | FatalError(exn);

exception CannotOpenDatabase;
exception CannotCreateIdentity;

/* MESSAGES */

type Msgs.t +=
  // Internal
  | CompletedOpenDb(Result.t(openedDb, exn))
  | CompletedLoadDataFromDb(Result.t(allData, exn))
  | CompletedKeyPairGeneration(Result.t(SimpleCrypto.keyPair, exn))
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
  Cmds.wrapResPromise(() => IDBPromise.open_(dbName, "all"), resultToMsg);

let getAll = db =>
  Cmds.wrapResPromise(() =>
    Js.Promise.all3((
      db |> IDBPromise.getKey(thisPeerKey),
      db |> IDBPromise.getKey(peersGroupsKey),
      db |> IDBPromise.getKey(peersKey),
    ))
    |> Js.Promise.then_(((thisPeer, peersGroups, peers)) =>
         Js.Promise.resolve({thisPeer, peersGroups, peers})
       )
  );

let setThisPeer = (thisPeer: ThisPeer.t, resultToMsg, t) =>
  Cmds.wrapPromise(
    () => IDBPromise.setKey(thisPeerKey, thisPeer, t),
    resultToMsg,
  );

let setPeers = (peers: Peers.t, resultToMsg, t) =>
  Cmds.wrapPromise(() => IDBPromise.setKey(peersKey, peers, t), resultToMsg);

let setPeerGroups = (peerGroups: Json.t, resultToMsg, t) =>
  Cmds.wrapPromise(
    () => IDBPromise.setKey(peersGroupsKey, peerGroups, t),
    resultToMsg,
  );

let initStateWithIdentity = (~allData, ~thisPeer, ~initContent) => {
  DbState.thisPeer,
  peersGroups:
    PeersGroups.(
      allData.peersGroups |?> decode |? init(~thisPeer, ~initContent)
    ),
  peers: allData.peers |? Peers.init(),
};

let allDataEmpty = {thisPeer: None, peersGroups: None, peers: None};

// UPDATE

let init = () => (Opening, open_(completedOpenDb));

let update = (~dbState, ~initContent, msg, model) =>
  switch (msg, model) {
  | (CompletedOpenDb(Ok(db)), Opening) => (
      LoadingData(db),
      getAll(db, completedLoadDataFromDb),
    )
  | (CompletedOpenDb(Error(_exn)), Opening | LoadingData(_)) => (
      GeneratingIdentity(None, allDataEmpty),
      CryptoCmds.generateKeyPair(completedKeyPairGeneration),
    )

  | (CompletedLoadDataFromDb(Ok(allData)), LoadingData(db)) =>
    switch (allData.thisPeer) {
    | Some(thisPeer) => (
        Loaded(Some(db)),
        Cmd.msg(
          InitializationComplete(
            initStateWithIdentity(~allData, ~thisPeer, ~initContent),
          ),
        ),
      )
    | None => (
        GeneratingIdentity(Some(db), allData),
        CryptoCmds.generateKeyPair(completedKeyPairGeneration),
      )
    }

  | (CompletedLoadDataFromDb(Error(_exn)), LoadingData(_)) => (
      GeneratingIdentity(None, allDataEmpty),
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
    | Some(thisPeer) => (
        Loaded(mDb),
        // DEBUG
        Cmd.batch([
          Cmds.log("My ID: " ++ (thisPeer.id |> PeerId.toString)),
          Cmd.msg(
            InitializationComplete(
              initStateWithIdentity(~allData, ~thisPeer, ~initContent),
            ),
          ),
        ]),
      )
    | None => (FatalError(CannotCreateIdentity), Cmds.none)
    }

  | (CompletedKeyPairGeneration(Error(exn)), LoadingData(_)) => (
      FatalError(exn),
      Cmds.none,
    )
  | (_, _) => (model, Cmd.none)
  };