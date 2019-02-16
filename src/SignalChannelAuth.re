open BlackTea;

type t =
  | WaitingForSignalChannel
  | SigningIn
  | SignedIn;

let applyMsg = (msg, model) => {
  switch (msg, model) {
  | (
      SignalServerState.GotMessage(Unsigned(Ok(onlinePeers))),
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
      ~signalChannel: SignalServerState.t,
      ~thisPeer: ThisPeer.t,
      msg,
      model,
    ) => {
  let model = model |> applyMsg(msg);
  switch (signalChannel.connectionState, model) {
  | (Connected(_), WaitingForSignalChannel) => (
      SigningIn,
      Cmd.msg(
        SignalVerifier.SignAndSendMsg(
          PeerToServer(thisPeer.id, Login(peers |> Peers.getAllIds)),
        ),
      ),
    )
  | (Connecting, _) => (WaitingForSignalChannel, Cmd.none)
  | (Connected(_), SigningIn | SignedIn) => (model, Cmd.none)
  };
};