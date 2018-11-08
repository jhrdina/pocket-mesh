[@bs.deriving accessors]
type t =
  | OpenDbSuccess(Db.t)
  | LoadDataFromDBSuccess(Db.allData)
  | DbFatalError(exn)
  | MyKeyPairGenSuccess(SimpleCrypto.keyPair)
  | MyKeyPairGenError(exn)
  | AddPeerToGroup(string, string)
  | AddPeerWithIdAndPublicKeyToGroup(string, SimpleCrypto.key, string)
  /* ========== */
  /* Signalling */
  /* ========== */
  | ConnectToSignalServerSuccess(SignalServerCmds.conn)
  | SignalServerConnectionError
  | SignalServerRetryConnection
  | SignalServerMessage(Message.t)
  /* =========== */
  /* Peers - RTC */
  /* =========== */
  /* _, SDP, Acceptor ID */
  | RtcOfferReady(SimpleRTC.t, string, string)
  /* _, SDP, Initiator ID */
  | RtcAnswerReady(SimpleRTC.t, string, string)
  | RtcConnected(SimpleRTC.t, string)
  /* _, tag, data */
  | RtcGotData(SimpleRTC.t, string, string)
  /* _, PeerID, errMsg */
  | RtcError(SimpleRTC.t, string, string)
  /* _, PeerID */
  | RtcClose(SimpleRTC.t, string)
  /* PeerID */
  | RtcRetryConnection(string)
  /* ===== */
  /* Debug */
  /* ===== */
  | PrintData
  | AddItem(string)
  | SendToPeer(string, string)
  | AddPeerToGroupWithPerms(PeerId.t, PeerGroup.Id.t, string)
  | OfferChanges
  | Noop;