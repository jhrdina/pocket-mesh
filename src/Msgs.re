[@bs.deriving accessors]
type t =
  | OpenDbSuccess(Db.t)
  | LoadDataFromDBSuccess(Db.allData)
  | OpenDbError(exn)
  | CryptoFatalError(exn)
  | MyKeyPairGenSuccess(SimpleCrypto.keyPair)
  | MyKeyPairGenError(exn)
  /* ========== */
  /* Signalling */
  /* ========== */
  | ConnectToSignalServerSuccess(SignalServerCmds.conn)
  | SignalServerConnectionError
  | SignalServerRetryConnection
  | SignalServerMessage(Message.t)
  | ReceivedVerifiedSignal(PeerId.t, Message.peerToPeerMsg)
  | SignalVerificationFailed(PeerId.t, exn)
  | PrepareKeyRequestFinished(PeerId.t, Result.t(string, exn))
  | VerifyKeyResponseFinished(PeerId.t, Result.t(SimpleCrypto.key, exn))
  | VerifyKeyRequestFinished(PeerId.t, Result.t(SimpleCrypto.key, exn))
  | PrepareKeyResponseFinished(PeerId.t, Result.t(string, exn))
  /* =========== */
  /* Peers - RTC */
  /* =========== */
  /* _, SDP, Acceptor ID */
  | RtcOfferReady(RTCCmds.t, string, PeerId.t)
  /* _, SDP, Initiator ID */
  | RtcAnswerReady(RTCCmds.t, string, PeerId.t)
  | RtcConnected(RTCCmds.t, PeerId.t)
  /* _, tag, data */
  | RtcGotData(RTCCmds.t, PeerId.t, SimpleRTCChunker.payload)
  /* _, PeerID, errMsg */
  | RtcError(RTCCmds.t, PeerId.t, string)
  /* _, PeerID */
  | RtcClose(RTCCmds.t, PeerId.t)
  /* PeerID */
  | RtcRetryConnection(PeerId.t)
  /* ====== */
  /* Global */
  /* ====== */
  /* Peers */
  | AddPeer(PeerId.t, string)
  | UpdatePeer(PeerId.t, string)
  | RemovePeer(PeerId.t)
  /* Groups */
  | AddGroup(PeerGroup.Id.t, string, PeerGroup.AM.t => PeerGroup.AM.t)
  | UpdateGroupAlias(PeerGroup.Id.t, string)
  | UpdateGroupContent(PeerGroup.Id.t, PeerGroup.AM.t)
  | RemoveGroup(PeerGroup.Id.t)
  /* Peers in groups */
  | AddPeerToGroup(PeerId.t, PeerGroup.Id.t, PeerGroup.groupPermissions)
  | UpdatePeerPermissions(
      PeerId.t,
      PeerGroup.Id.t,
      PeerGroup.groupPermissions,
    )
  | RemovePeerFromGroup(PeerId.t, PeerGroup.Id.t)
  /* Others */
  | UpdateSignalServerUrl(string)
  | RemoveThisPeerAndAllData
  /* ===== */
  /* Debug */
  /* ===== */
  | SendToPeer(string, string)
  | OfferChangesDebouncerMsg(Debouncer.msg(t))
  | OfferChangesFromGroupsDebounced(PeerGroup.Id.Set.t)
  | Noop;