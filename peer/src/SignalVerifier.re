open BlackTea;
open Json.Infix;

type Msgs.t +=
  /* Input */
  | SignAndSendMsg(Message.signedMsg)
  /* Internal */
  | CompletedSignMessage(Result.t(Message.t, exn))
  /* Output */
  | CompletedMessageVerification(Result.t(Message.signedMsg, exn));

exception ReceivedMessageSignatureMismatch;

let completedMessageVerification = result =>
  CompletedMessageVerification(result);

let completedSignMessage = result => CompletedSignMessage(result);

/* VERIFICATION */

let verifyMessageSignature = (srcPublicKey, signature, msg) =>
  Json.Object(Message.encodeSignedMsg(msg))
  |> Json.stringify
  |> SimpleCrypto.verify(srcPublicKey, signature);

let verifyMessageSignatureCmd = (srcPublicKey, signature, msg) =>
  switch (msg) {
  | Message.PeerToPeer(_) =>
    Cmds.fromPromise(
      () =>
        verifyMessageSignature(srcPublicKey, signature, msg)
        |> Js.Promise.then_(valid =>
             valid ?
               Js.Promise.resolve(msg) :
               Js.Promise.reject(ReceivedMessageSignatureMismatch)
           ),
      completedMessageVerification,
    )
  | _ => Cmds.none
  };

/* SIGNING */

exception SignatureError;

let signMsg = (msg: Message.signedMsg, privateKey) =>
  Json.Object(msg |> Message.encodeSignedMsg)
  |> Json.stringify
  |> SimpleCrypto.sign(privateKey)
  |> Js.Promise.then_(signature =>
       Message.Signed(signature, msg) |> Js.Promise.resolve
     );

let update = (~thisPeer: ThisPeer.t, ~peers: Peers.t, msg) =>
  switch (msg) {
  | SignAndSendMsg(signedMsgPayload) =>
    Cmds.fromPromise(
      () => signMsg(signedMsgPayload, thisPeer.privateKey),
      completedSignMessage,
    )
  | CompletedSignMessage(Ok(signedMsg)) =>
    Cmd.msg(SignalChannel.Send(signedMsg))
  | CompletedSignMessage(Error(_e)) => Cmds.log("Could not sign message")

  | SignalChannel.GotMessage(
      Signed(signature, PeerToPeer(src, tg, Offer(_) | Answer(_)) as msg),
    )
      when PeerId.equal(tg, thisPeer.id) =>
    // KeyRequest and KeyResponse are verified directly in PeersKeysFetcherAndSender
    peers
    |> Peers.findOpt(src)
    |?> (peer => peer.publicKey)
    |?>> (srcKey => verifyMessageSignatureCmd(srcKey, signature, msg))
    // TODO: Write down rules of messages dropping (e.g. in the times when keys are not exchanged yet)
    |? Cmds.log("Received Offer or Answer, but I don't have a key yet.")

  | _ => Cmd.none
  };