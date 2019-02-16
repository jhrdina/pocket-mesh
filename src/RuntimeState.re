open BlackTea;

/* CONSTANTS */

let defaultSignalServerUrl = "ws://localhost:7777";

/* MODULES */

module InitConfig = {
  type t = {
    contentInitializer: PeerGroup.AM.t => PeerGroup.AM.t,
    signalServerUrl: string,
  };

  let make =
      (
        ~contentInitializer=crdt => crdt,
        ~signalServerUrl=defaultSignalServerUrl,
        (),
      ) => {
    contentInitializer,
    signalServerUrl,
  };
};

// TYPES

type t = {
  initConfig: InitConfig.t,
  signalServer: SignalServerState.t,
  signalChannelAuth: SignalChannelAuth.t,
  // peersConnections,
  // peersGroupsSynchronizer,
  // peersKeysFetcherAndSender,
  peersStatuses: PeersStatuses.t,
  // signalVerifierAndSigner,
  thisPeerKeyExporter: ThisPeerKeyExporter.t,
};

let init = (~dbState: DbState.t, ~signalServer, ~initConfig) => {
  initConfig,
  signalServer,
  signalChannelAuth: SignalChannelAuth.init(),
  // peersConnections,
  // peersGroupsSynchronizer,
  // peersKeysFetcherAndSender,
  peersStatuses: PeersStatuses.init(),
  // signalVerifierAndSigner,
  thisPeerKeyExporter: ThisPeerKeyExporter.init(~thisPeer=dbState.thisPeer),
};

let update = (dbState: DbState.t, msg, model) => {
  let (signalServer, signalServerCmd) =
    SignalServerState.update(model.signalServer, msg);
  let (signalChannelAuth, signalChannelAuthCmd) =
    SignalChannelAuth.update(
      ~peers=dbState.peers,
      ~signalChannel=signalServer,
      ~thisPeer=dbState.thisPeer,
      msg,
      model.signalChannelAuth,
    );
  let signalVerifierCmd =
    SignalVerifier.update(~thisPeer=dbState.thisPeer, msg);
  let (peersStatuses, peersStatusesCmd) =
    PeersStatuses.update(
      ~thisPeer=dbState.thisPeer,
      ~peers=dbState.peers,
      msg,
      model.peersStatuses,
    );
  let (thisPeerKeyExporter, thisPeerKeyExporterCmd) =
    ThisPeerKeyExporter.update(
      ~thisPeer=dbState.thisPeer,
      msg,
      model.thisPeerKeyExporter,
    );
  // let (peersConnections, peersCmd) =
  //   PeersConnections.update(
  //     model.maybeDb,
  //     model.thisPeer,
  //     model.signalServer.connectionState,
  //     model.peerGroups,
  //     model.peers,
  //     msg,
  //   );

  // let (offerChangesDebouncer, offerChangesDebouncerCmd) =
  //   switch (msg) {
  //   | OfferChangesDebouncerMsg(msg) =>
  //     Debouncer.update(
  //       d => d,
  //       model.offerChangesDebouncer,
  //       msg,
  //       Msgs.offerChangesDebouncerMsg,
  //     )
  //   | _ => (model.offerChangesDebouncer, Cmds.none)
  //   };
  (
    {
      initConfig: model.initConfig,
      signalServer,
      signalChannelAuth,
      // peersConnections,
      // peersGroupsSynchronizer,
      // peersKeysFetcherAndSender,
      peersStatuses,
      // signalVerifierAndSigner,
      thisPeerKeyExporter,
    },
    Cmd.batch([
      signalServerCmd,
      signalChannelAuthCmd,
      signalVerifierCmd,
      peersStatusesCmd,
      // debugCmd,
      // offerChangesDebouncerCmd,
      thisPeerKeyExporterCmd,
    ]),
  );
};

let subscriptions = model =>
  Sub.batch([
    SignalServerState.subscriptions(model.signalServer),
    ThisPeerKeyExporter.subscriptions(model.thisPeerKeyExporter),
  ]);