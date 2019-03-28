open BlackTea;

type t =
  | WaitingForSignalChannel
  | SigningIn
  | SignedIn;

let applyMsg = (~thisPeer: ThisPeer.t, ~peers, msg, model) => {
  switch (msg, model) {
  | (SignalChannel.GotMessage(Unsigned(Challenge(challenge))), SigningIn) => (
      SigningIn,
      Cmd.msg(
        SignalVerifier.SignAndSendMsg(
          PeerToServer(
            thisPeer.id,
            Login(challenge, peers |> Peers.getAllIds),
          ),
        ),
      ),
    )
  | (
      SignalChannel.GotMessage(Unsigned(Ok(_onlinePeers))),
      SigningIn | SignedIn,
    ) => (
      SignedIn,
      Cmd.none,
    )
  | _ => (model, Cmd.none)
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
  let (model, cmd1) = model |> applyMsg(~thisPeer, ~peers, msg);
  let (model, cmd2) =
    switch (
      signalChannel.connectionState,
      thisPeerKeyExporter |> ThisPeerKeyExporter.getKey,
      model,
    ) {
    | (Connected(_), Some(thisPeerKeyStr), WaitingForSignalChannel) => (
        SigningIn,
        Cmd.msg(
          SignalVerifier.SignAndSendMsg(
            PeerToServer(thisPeer.id, LoginReq(thisPeerKeyStr)),
          ),
        ),
      )
    | (Connecting, _, _) => (WaitingForSignalChannel, Cmd.none)
    | (Connected(_), None, WaitingForSignalChannel)
    | (Connected(_), _, SigningIn | SignedIn) => (model, Cmd.none)
    };
  (model, Cmd.batch([cmd1, cmd2]));
};