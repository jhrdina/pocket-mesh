/* CONSTANTS */
let dbName = "pocketMesh";

type t = IDBPromise.t;

type allData = {
  thisPeer: option(ThisPeer.t),
  peerGroups: option(Json.t),
  peers: option(Types.peersInDb),
};

/* COMMANDS */

let open_ = (succToMsg, errToMsg) =>
  Cmds.wrapPromise(
    () => IDBPromise.open_(dbName, "all"),
    succToMsg,
    errToMsg,
  );

let getAll = db =>
  Cmds.wrapPromise(() =>
    Js.Promise.all3((
      db |> IDBPromise.getKey("thisPeer"),
      db |> IDBPromise.getKey("peerGroups"),
      db |> IDBPromise.getKey("peers"),
    ))
    |> Js.Promise.then_(((thisPeer, peerGroups, peers)) =>
         Js.Promise.resolve({thisPeer, peerGroups, peers})
       )
  );

let setThisPeer = (thisPeer: ThisPeer.t, succToMsg, errToMsg, t) =>
  Cmds.wrapPromise(
    () => IDBPromise.setKey("thisPeer", thisPeer, t),
    succToMsg,
    errToMsg,
  );

let setPeers = (peers: Types.peersInDb, succToMsg, errToMsg, t) =>
  Cmds.wrapPromise(
    () => IDBPromise.setKey("peers", peers, t),
    succToMsg,
    errToMsg,
  );

let setPeerGroups = (peerGroups: Json.t, succToMsg, errToMsg, t) =>
  Cmds.wrapPromise(
    () => IDBPromise.setKey("peerGroups", peerGroups, t),
    succToMsg,
    errToMsg,
  );