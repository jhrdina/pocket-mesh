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
  peersConnections: PeersConnections.t,
  // peersGroupsSynchronizer,
  peersKeysFetcherAndSender: PeersKeysFetcherAndSender.t,
  peersStatuses: PeersStatuses.t,
  // signalVerifierAndSigner,
  thisPeerKeyExporter: ThisPeerKeyExporter.t,
};

let init = (~dbState: DbState.t, ~signalServer, ~initConfig) => {
  let thisPeerKeyExporter =
    ThisPeerKeyExporter.init(~thisPeer=dbState.thisPeer);
  let peersStatuses = PeersStatuses.init();
  let (peersKeysFetcherAndSender, peersKeysFetcherAndSenderCmd) =
    PeersKeysFetcherAndSender.init(
      ~thisPeer=dbState.thisPeer,
      ~peers=dbState.peers,
      ~thisPeerKeyExporter,
      ~peersStatuses,
    );
  let peersConnections =
    PeersConnections.init(
      ~peers=dbState.peers,
      ~peersStatuses,
      ~peersGroups=dbState.peersGroups,
    );
  (
    {
      initConfig,
      signalServer,
      signalChannelAuth: SignalChannelAuth.init(),
      peersConnections,
      // peersGroupsSynchronizer,
      peersKeysFetcherAndSender,
      peersStatuses,
      // signalVerifierAndSigner,
      thisPeerKeyExporter,
    },
    peersKeysFetcherAndSenderCmd,
  );
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
    SignalVerifier.update(
      ~thisPeer=dbState.thisPeer,
      ~peers=dbState.peers,
      msg,
    );
  let (peersStatuses, peersStatusesCmd) =
    PeersStatuses.update(
      ~thisPeer=dbState.thisPeer,
      ~peers=dbState.peers,
      ~signalServer,
      msg,
      model.peersStatuses,
    );
  let (thisPeerKeyExporter, thisPeerKeyExporterCmd) =
    ThisPeerKeyExporter.update(
      ~thisPeer=dbState.thisPeer,
      msg,
      model.thisPeerKeyExporter,
    );
  let (peersKeysFetcherAndSender, peersKeysFetcherAndSenderCmd) =
    PeersKeysFetcherAndSender.update(
      ~thisPeer=dbState.thisPeer,
      ~peers=dbState.peers,
      ~thisPeerKeyExporter,
      ~peersStatuses,
      msg,
      model.peersKeysFetcherAndSender,
    );
  let (peersConnections, peersConnectionsCmd) =
    PeersConnections.update(
      ~peers=dbState.peers,
      ~thisPeer=dbState.thisPeer,
      ~peersGroups=dbState.peersGroups,
      ~peersStatuses,
      msg,
      model.peersConnections,
    );

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
      peersConnections,
      // peersGroupsSynchronizer,
      peersKeysFetcherAndSender,
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
      peersConnectionsCmd,
      thisPeerKeyExporterCmd,
      peersKeysFetcherAndSenderCmd,
    ]),
  );
};

let subscriptions = model =>
  Sub.batch([
    SignalServerState.subscriptions(model.signalServer),
    ThisPeerKeyExporter.subscriptions(model.thisPeerKeyExporter),
    PeersConnections.subscriptions(model.peersConnections),
  ]);