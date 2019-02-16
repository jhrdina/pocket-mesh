open BlackTea;
open Rex_json.Json.Infix;

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// WARNING: We presume that only thisPeer can be the target in incomming msgs
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
/*
  - Fetches public keys from peers as soon as they are available.
  - Verifies incomming key requests and responds to them
 */

type Msgs.t +=
  | CompletedVerifyKeyResponse(PeerId.t, Result.t(SimpleCrypto.key, exn))
  | CompletedVerifyKeyRequest(PeerId.t, Result.t(SimpleCrypto.key, exn))
  /* Public */
  | ReceivedKeyForPeer(PeerId.t, SimpleCrypto.key);

let completedVerifyKeyResponse = (src, res) =>
  CompletedVerifyKeyResponse(src, res);
let completedVerifyKeyRequest = (src, res) =>
  CompletedVerifyKeyRequest(src, res);

// type keyFetchingState =
//   | Fetching;

type t = {
  lastOnlinePeers: PeerId.Set.t,
  fetchingPeers: PeerId.Set.t,
  // Queue of peers (with verified KeyRequests) that are waiting for ThisPeerKeyExporter to export ThisPeer's key.
  responseQueue: PeerId.Set.t,
};

exception InvalidKeyResponse;

/* HELPERS */

let verifyKeyMessageCmd =
    (
      resultToMsg,
      thisPeer: ThisPeer.t,
      src,
      signature,
      p2pMsg: Message.peerToPeerMsg,
    ) =>
  switch (p2pMsg) {
  | KeyResponse(srcKeyStr)
  | KeyRequest(srcKeyStr) =>
    Cmds.wrapPromise(
      () => {
        let srcJwk = srcKeyStr |> SimpleCrypto.stringToJwk;
        srcJwk
        |> SimpleCrypto.jwkToPublicKey
        |> Js.Promise.then_(srcKey =>
             Js.Promise.all2((
               SignalVerifier.verifyMessageSignature(
                 srcKey,
                 signature,
                 // WARNING: We presume that only we can be the target
                 PeerToPeer(src, thisPeer.id, p2pMsg),
               ),
               SimpleCrypto.fingerprintForRSAJWK(srcJwk)
               |> Js.Promise.then_(fingerprint =>
                    fingerprint
                    |> PeerId.ofString
                    |?>> (==)(src)
                    |? false
                    |> Js.Promise.resolve
                  ),
             ))
             |> Js.Promise.then_(((signatureOk, srcMatchesKey)) =>
                  if (signatureOk && srcMatchesKey) {
                    srcKey |> Js.Promise.resolve;
                  } else {
                    Js.Promise.reject(InvalidKeyResponse);
                  }
                )
           );
      },
      srcKey => resultToMsg(src, Result.Ok(srcKey)),
      exn => resultToMsg(src, Error(exn)),
    )
  | Offer(_)
  | Answer(_) =>
    Cmds.log(
      "Invalid usage: verifyKeyMessageCmd called with weird message type.",
    )
  };

// QUERIES

let isFetchingKeyForPeer = (peerId, t) =>
  t.fetchingPeers |> PeerId.Set.mem(peerId);

let removeFetchingForPeer = (peerId, t) => {
  ...t,
  fetchingPeers: t.fetchingPeers |> PeerId.Set.remove(peerId),
};

let removePeerFromResponseQueue = (peerId, t) => {
  ...t,
  responseQueue: t.responseQueue |> PeerId.Set.remove(peerId),
};

/* MAIN */

let derive =
    (
      ~thisPeer: ThisPeer.t,
      ~thisPeerKeyExporter: ThisPeerKeyExporter.t,
      ~peersStatuses: PeersStatuses.t,
      ~peers: Peers.t,
      model: t,
    ) => {
  /* Request peer's public key if missing */
  let (model, reqCmd) =
    switch (thisPeerKeyExporter |> ThisPeerKeyExporter.getKey) {
    | Some(thisPeerKeyStr) =>
      let onlinePeers = peersStatuses |> PeersStatuses.getOnlinePeers;
      let (model, cmdsList) =
        PeerId.Set.symmetric_diff(
          ~f=
            (diffRes, (model, cmdsList)) =>
              switch (diffRes) {
              | Left(peerId) => (
                  model
                  |> removeFetchingForPeer(peerId)
                  |> removePeerFromResponseQueue(peerId),
                  cmdsList,
                )
              | Right(peerId)
                  when
                    peers
                    |> Peers.findOpt(peerId)
                    |?> (peer => peer.publicKey) == None => (
                  {
                    ...model,
                    fetchingPeers:
                      model.fetchingPeers |> PeerId.Set.add(peerId),
                  },
                  [
                    Cmd.msg(
                      SignalVerifier.SignAndSendMsg(
                        Message.PeerToPeer(
                          thisPeer.id,
                          peerId,
                          KeyRequest(thisPeerKeyStr),
                        ),
                      ),
                    ),
                    ...cmdsList,
                  ],
                )
              | Right(_) => (model, cmdsList)
              },
          model.lastOnlinePeers,
          onlinePeers,
          ~acc=(model, []),
        );
      ({...model, lastOnlinePeers: onlinePeers}, Cmd.batch(cmdsList));
    | None => ({...model, fetchingPeers: PeerId.Set.empty}, Cmd.none)
    };

  let (model, respCmd) =
    switch (thisPeerKeyExporter |> ThisPeerKeyExporter.getKey) {
    | Some(thisPeerKeyStr) => (
        {...model, responseQueue: PeerId.Set.empty},
        model.responseQueue
        |> PeerId.Set.elements
        |> List.map(peerId =>
             Cmd.msg(
               SignalVerifier.SignAndSendMsg(
                 Message.PeerToPeer(
                   thisPeer.id,
                   peerId,
                   KeyResponse(thisPeerKeyStr),
                 ),
               ),
             )
           )
        |> Cmd.batch,
      )
    | None => (model, Cmd.none)
    };
  (model, Cmd.batch([reqCmd, respCmd]));
};

let init =
    (
      ~thisPeer: ThisPeer.t,
      ~peers: Peers.t,
      ~thisPeerKeyExporter,
      ~peersStatuses,
    ) => {
  {
    lastOnlinePeers: PeerId.Set.empty,
    fetchingPeers: PeerId.Set.empty,
    responseQueue: PeerId.Set.empty,
  }
  |> derive(~thisPeer, ~peers, ~thisPeerKeyExporter, ~peersStatuses);
};

let update =
    (
      ~thisPeer: ThisPeer.t,
      ~peers: Peers.t,
      ~thisPeerKeyExporter: ThisPeerKeyExporter.t,
      ~peersStatuses: PeersStatuses.t,
      msg,
      model: t,
    ) => {
  // Handle messages
  let (model, verifCmd) =
    switch (msg) {
    /* Verify public key response */
    | SignalServerState.GotMessage(
        Signed(
          signature,
          PeerToPeer(src, _tg, KeyResponse(_srcKeyStr) as p2pMsg),
        ),
      )
        when model |> isFetchingKeyForPeer(src) => (
        model,
        verifyKeyMessageCmd(
          completedVerifyKeyResponse,
          thisPeer,
          src,
          signature,
          p2pMsg,
        ),
      )

    | CompletedVerifyKeyResponse(src, Ok(srcKey))
        when model |> isFetchingKeyForPeer(src) => (
        model |> removeFetchingForPeer(src),
        Cmd.msg(ReceivedKeyForPeer(src, srcKey)),
      )

    | CompletedVerifyKeyResponse(src, Error(_)) => (
        model,
        Cmds.log(
          "VerifyKeyResponse from peer "
          ++ (src |> PeerId.toString)
          ++ " failed",
        ),
      )

    /* Verify public key request */

    | SignalServerState.GotMessage(
        Signed(signature, PeerToPeer(src, _tg, KeyRequest(_) as p2pMsg)),
      ) => (
        model,
        verifyKeyMessageCmd(
          completedVerifyKeyRequest,
          thisPeer,
          src,
          signature,
          p2pMsg,
        ),
      )

    | CompletedVerifyKeyRequest(src, Ok(srcKey)) =>
      let isFetching = model |> isFetchingKeyForPeer(src);
      let model = isFetching ? model |> removeFetchingForPeer(src) : model;
      (
        {...model, responseQueue: model.responseQueue |> PeerId.Set.add(src)},
        isFetching ? Cmd.msg(ReceivedKeyForPeer(src, srcKey)) : Cmd.none,
      );

    | CompletedVerifyKeyRequest(src, Error(_)) => (
        model,
        Cmds.log(
          "VerifyKeyRequest from peer "
          ++ (src |> PeerId.toString)
          ++ " failed",
        ),
      )

    | ReceivedKeyForPeer(peerId, publicKey) => (
        model,
        Cmd.msg(Peers.UpdatePublicKey(peerId, Some(publicKey))),
      )

    | _ => (model, Cmds.none)
    };

  let (model, derivedCmd) =
    model |> derive(~thisPeer, ~peers, ~thisPeerKeyExporter, ~peersStatuses);

  (model, Cmd.batch([verifCmd, derivedCmd]));
};

/* let subscriptions = model =>
   Sub.batch(PeerId.Set.fold(() => (), model.fetchingStates, []));*/