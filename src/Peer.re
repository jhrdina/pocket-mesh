type peerConnectionState =
  | NoNeedToConnect
  | WaitingForOnlineSignal
  | CreatingSdpOffer
  | WaitingForSdpAnswer(RTCCmds.t)
  /* time, attemptsMade, lastErrorMessage */
  /* | Connecting */
  | FailedRetryingAt(string, int, string)
  | Connected(RTCCmds.t);

type t = {
  id: PeerId.t,
  publicKey: SimpleCrypto.key,
  nickName: string,
  connectionState: peerConnectionState,
};