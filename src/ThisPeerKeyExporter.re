open BlackTea;

/*
  Watches changes of this peer's public key and caches it's stringified representation.
 */

type t =
  | ExportingIdentity(ThisPeer.t)
  | ExportedIdentity(ThisPeer.t, string);

type Msgs.t +=
  // Internal
  | CompletedExport(Result.t(string, exn));

let completedExport = result => CompletedExport(result);

/* QUERIES */
let getKey =
  fun
  | ExportingIdentity(_) => None
  | ExportedIdentity(_, keyStr) => Some(keyStr);

/* HELPERS */

let exportThisPeerKeySub = (resultToMsg, thisPeer: ThisPeer.t) =>
  Subs.ofStream(
    "ThisPeerKeyExporter/export/" ++ (thisPeer.id |> PeerId.toString), () =>
    StreamOps.fromPromise(thisPeer.publicKey |> SimpleCrypto.publicKeyToJwk)
    |> Wonka.map((. res) =>
         res
         |> Result.mapOk(jwk => jwk |> SimpleCrypto.jwkToString)
         |> resultToMsg
       )
  );

let init = (~thisPeer) => ExportingIdentity(thisPeer);

let update = (~thisPeer, msg, model) => {
  let (model, cmd) =
    switch (msg, model) {
    | (CompletedExport(Ok(keyStr)), ExportingIdentity(oldThisPeer)) => (
        ExportedIdentity(oldThisPeer, keyStr),
        // DEBUG
        Cmds.log("My JWK: " ++ keyStr),
      )
    /* TODO: Handle error */
    | _ => (model, Cmds.none)
    };

  (
    switch (model) {
    | ExportingIdentity(oldThisPeer)
    | ExportedIdentity(oldThisPeer, _) when oldThisPeer != thisPeer =>
      ExportingIdentity(thisPeer)
    | ExportingIdentity(_)
    | ExportedIdentity(_) => model
    },
    cmd,
  );
};

let subscriptions = model =>
  switch (model) {
  | ExportingIdentity(thisPeer) =>
    exportThisPeerKeySub(completedExport, thisPeer)
  | ExportedIdentity(_, _) => Sub.none
  };