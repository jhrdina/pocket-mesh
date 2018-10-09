[@bs.deriving accessors]
type t =
  | MyKeyPairGenSuccess(SimpleCrypto.keyPair)
  | LogMyPublicKey(SimpleCrypto.jwk)
  | AddPeerToGroup(string, string)
  | AddPeerWithIdAndPublicKeyToGroup(string, SimpleCrypto.key, string)
  /*            */
  /* Signalling */
  | ConnectToSignalServerSuccess(SignalServerCmds.t)
  | SignalServerMessage(SignalServerCmds.t, Message.t)
  /*     */
  /* RTC */
  /* _, SDP, Acceptor ID */
  | RtcOfferReady(RTCCmds.t, string, string)
  /* _, SDP, Initiator ID */
  | RtcAnswerReady(RTCCmds.t, string, string)
  | RtcConnected(RTCCmds.t, string)
  | RtcGotData(RTCCmds.t, string)
  /*       */
  /* Debug */
  | SendToPeer(string, string)
  | Noop;