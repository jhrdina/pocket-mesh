open BlackTea;

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
    Cmds.wrapResPromise(
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

let update = (~thisPeer: ThisPeer.t, msg) =>
  switch (msg) {
  | SignAndSendMsg(signedMsgPayload) =>
    Cmds.wrapResPromise(
      () => signMsg(signedMsgPayload, thisPeer.privateKey),
      completedSignMessage,
    )
  | CompletedSignMessage(Ok(signedMsg)) =>
    Cmd.msg(SignalServerState.Send(signedMsg))
  | CompletedSignMessage(Error(_e)) => Cmds.log("Could not sign message")
  | _ => Cmd.none
  };