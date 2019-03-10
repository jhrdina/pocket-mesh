open BlackTea;

type t =
  | WaitingForSignalChannel
  | SigningIn
  | SignedIn;

let applyMsg = (msg, model) => {
  switch (msg, model) {
  | (
      SignalChannel.GotMessage(Unsigned(Ok(onlinePeers))),
      SigningIn | SignedIn,
    ) =>
    SignedIn
  | _ => model
  };
};

let init = () => WaitingForSignalChannel;

let update =
    (
      ~peers: Peers.t,
      ~signalChannel: SignalChannel.t,
      ~thisPeer: ThisPeer.t,
      ~thisPeerKeyExporter: ThisPeerKeyExporter.t,
      msg,
      model,
    ) => {
  let model = model |> applyMsg(msg);
  switch (
    signalChannel.connectionState,
    thisPeerKeyExporter |> ThisPeerKeyExporter.getKey,
    model,
  ) {
  | (Connected(_), Some(thisPeerKeyStr), WaitingForSignalChannel) => (
      SigningIn,
      Cmd.msg(
        SignalVerifier.SignAndSendMsg(
          PeerToServer(
            thisPeer.id,
            Login(thisPeerKeyStr, peers |> Peers.getAllIds),
          ),
        ),
      ),
    )
  | (Connecting, _, _) => (WaitingForSignalChannel, Cmd.none)
  | (Connected(_), None, WaitingForSignalChannel)
  | (Connected(_), _, SigningIn | SignedIn) => (model, Cmd.none)
  };
};